#' @author \Yuqing \Dai
#'
#' Perform Weather Normalization
#'
#' The wenorm function applies weather normalization to a given data. It is particularly useful for energy usage data where weather variables are often key resample_variables.
#'
#' @param data A data frame containing the data to be normalized.
#' @param model A trained model used for prediction. Default is NULL.
#' @param response_variable The variable to be predicted by the model. Default is NULL.
#' @param predictor_variables The resample_variables for the model. Default includes wind speed (ws), wind direction (wd), temperature (temp), and relative humidity (rh).
#' @param constant_variables The variables to be excluded as resample_variables for the model. Default excludes datetime and unixtime.
#' @param num_iterations The number of iterations to perform. Default is 1.
#' @param enable_diff If set to TRUE, a new column containing the weather difference is added to the output. Default is TRUE.
#' @param seed Seed for reproducibility. Default is NULL.
#' @param cdp  Logical. If TRUE, smoothed residuals are calculated. Default is FALSE.
#' @param window Numeric. Specifies the window size for smoothing. Default is 10.
#' @return A list containing the final_data (all data frames combined) and summary_data (data summarized by datetime).
#' @export
#' @examples
#' \dontrun{
#' wenorm(df = mydata, response_variable = "target", model = mymodel, num_iterations = 10)
#' }
#'
wenorm <- function(data,
                   model = NULL,
                   response_variable = NULL,
                   predictor_variables = NULL,
                   constant_variables = NULL,
                   num_iterations = 1,
                   seed = NULL,
                   cpd = T,
                   window = 10,
                   ...) {

    # Check input
    if (!all(c(constant_variables, predictor_variables, response_variable) %in% names(data))) {
      stop("All elements of constant_variables, predictor_variables and response_variable must be column names in data.")
    }

    if (!(is.numeric(num_iterations) & length(num_iterations) == 1 & num_iterations > 0 & floor(num_iterations) == num_iterations)) {
      stop("num_iterations must be a single positive integer.")
    }


  # Set the seed for reproducibility
  set.seed(seed)

  constant_variables <- c(constant_variables, "datetime")
  resample_variables <- setdiff(predictor_variables, constant_variables)

  # Load parallel processing packages
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package doParallel needed for this function is not installed. Please install it.")
  }

  #' Step 1
  # Register parallel backend
  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

  # Perform operations in parallel
  randomized_dfs1 <- foreach(i = 1:num_iterations, .packages = "dplyr") %dopar% {
    new_data <- data[, c(constant_variables, response_variable), drop = FALSE]
      # Sample the row indices from the original dataframe
      sampled_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)

    new_data[, resample_variables] <- data[sampled_indices, resample_variables]
    return(new_data)
  }

  randomized_dfs2 <- foreach(i = 1:num_iterations, .packages = "dplyr") %dopar% {
    new_data <- data[, c("datetime", response_variable), drop = FALSE]
    # Sample the row indices from the original dataframe
    sampled_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)

    new_data[, predictor_variables] <- data[sampled_indices, predictor_variables]
    return(new_data)
  }

  # Stop the cluster
  stopCluster(cl)

  # Combine all data frames into one
  final_data1 <- dplyr::bind_rows(randomized_dfs1) %>%
    as.h2o() %>%
    h2o::h2o.predict(model, .) %>%
    as.data.frame() %>%
    cbind(., bind_rows(randomized_dfs1))

  final_data2 <- dplyr::bind_rows(randomized_dfs2) %>%
    as.h2o() %>%
    h2o::h2o.predict(model, .) %>%
    as.data.frame() %>%
    cbind(., bind_rows(randomized_dfs2))

  summary_data1 <- final_data1 %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(
      predict_mean = mean(predict),
      predict_sd = sd(predict)
    )

  summary_data2 <- final_data2 %>%
    dplyr::group_by(datetime) %>%
    dplyr::summarise(
      predict_mean = mean(predict),
      predict_sd = sd(predict)
    )

  data_new <- data %>%
    dplyr::select(all_of(c("datetime", response_variable, predictor_variables)))

  summary_data1 <- summary_data1 %>%
    left_join(data_new, by = "datetime") %>%
    setNames(c("datetime", paste(response_variable, "_wn", sep = ""), paste(response_variable, "_sd", sep = ""),
               response_variable, predictor_variables))

  summary_data2 <- summary_data2 %>%
    left_join(data_new, by = "datetime") %>%
    setNames(c("datetime", paste(response_variable, "_wn", sep = ""), paste(response_variable, "_sd", sep = ""),
               response_variable, predictor_variables))

  # Create the new variable name as a string
  new_var_name <- paste(response_variable, "_wn", sep = "")

  # Replace the existing column with 'we2'
  summary_data3 <- summary_data1 %>%
    mutate(across(.cols = all_of(new_var_name),
                  .fns = ~ summary_data1[[response_variable]] * summary_data2[[new_var_name]] / summary_data1[[new_var_name]],
                  .names = new_var_name))

  summary_data4 <- summary_data3

  #' Step 2
  # If cpd is TRUE, calculate smoothed residuals
  if (cpd) {
    # Load necessary libraries
    library(bsts)

    # Helper function to find optimal window size for smoothing
    find_optimal_window <- function(residuals, target_period_sd, max_window = 2000) {
      min_difference <- Inf  # initialize minimum difference as infinity
      optimal_window <- 1  # initialize optimal window size as 1

      for (window_size in 1:max_window) {
        if (window_size > length(residuals)) break

        smoothed_residuals <- stats::filter(residuals, rep(1/window_size, window_size), sides = 2)
        difference <- abs(sd(smoothed_residuals, na.rm = TRUE) - target_period_sd)

        if (!is.na(difference) && difference < min_difference) {
          min_difference <- difference
          optimal_window <- window_size
        }
      }
      return(optimal_window)
    }

    # Helper function to extend a vector with mirrored values at the ends
    extend_with_mirror <- function(x, n) {
      c(rev(x[2:(n+1)]), x, rev(x[(length(x)-n):(length(x)-1)]))
    }

    df_temp <- data.frame(summary_data3[,c("datetime", paste(response_variable, "_wn", sep =""))])
    df_temp1   <- data.frame(summary_data3[,c("datetime", response_variable)])

    colnames_to_loop <- setdiff(names(df_temp), "datetime")

    for (i in colnames_to_loop) {
      ss <- bsts::AddLocalLevel(list(), df_temp[[i]])
      BSS <- bsts(df_temp[[i]], state.specification = ss, niter = 300, ping = 0, seed = 1)
      pred_means <- predict(BSS, horizon = 1)$mean

      residuals <- df_temp[[i]] - pred_means
      residuals_extended <- extend_with_mirror(residuals, window)
      smoothed_residuals_extended <- stats::filter(residuals_extended, rep(1/window, window), sides = 2)
      smoothed_residuals <- smoothed_residuals_extended[(window+1):(length(residuals)+window)]

      df_temp[[i]] <- pred_means + smoothed_residuals
    }
    merged_df <- merge(df_temp, df_temp1, by="datetime", all=TRUE)

    summary_data4 <- na.omit(merged_df)
  }

  return(list(final_data_detrend    = final_data1,
              final_data_base       = final_data2,
              summary_data_detrend  = summary_data1,
              summary_data_baseline = summary_data2,
              summary_data_wenormed = summary_data3,
              summary_data_final = summary_data4))
}
