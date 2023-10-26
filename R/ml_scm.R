#' Machine Learning Enhanced Synthetic Control Method (ML-SCM)
#'
#' This function implements a variation of the synthetic control method (SCM) for time series data,
#' enhanced with machine learning for better prediction accuracy.
#'
#' @param df A dataframe containing the time series data.
#' @param params A list containing parameters that dictate the behavior of the model.
#' @param cpd Logical flag indicating if residual smoothing and decomposition should be done. Default is FALSE.
#' @param window Numeric value indicating the window size for smoothing. Default is 10.
#' @param added_time Character string specifying which date related columns to add. Default is "unixtime".
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function processes the input dataframe, optionally decomposing and smoothing the residuals.
#' The core of the function applies a machine learning model to predict outcomes and then computes the differences
#' between the actual and predicted values. The efficiency of the model is also calculated.
#' The function returns a list containing the processed dataframe, differences, efficiencies, and the machine learning model.
#'
#' @return
#' A list containing the processed dataframe, differences between observed and predicted values, efficiency,
#' and the machine learning model used.
#'
#' @author [Yuqing Dai, Chengxu Tong]
#'
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' df <- data.frame(
#'   datetime = seq(from = as.POSIXct("2020-01-01"), to = as.POSIXct("2020-01-10"), by = "hour"),
#'   metric1 = rnorm(217),
#'   metric2 = rnorm(217)
#' )
#'
#' # Define params list
#' params <- list(
#'   treatment_group = "A",
#'   start_time = as.POSIXct("2020-01-03 00:00"),
#'   end_time = as.POSIXct("2020-01-07 23:00"),
#'   split_proportion = 0.7,
#'   max_models = 10,
#'   max_runtime_secs = 60,
#'   algorithm = "GBM",
#'   criterion = "RMSE"
#' )
#'
#' results <- ml_scm(df, params, rsd = TRUE, window = 5)
#' }
#'
#' @export
#'
ml_scm <- function(df,
                  params,
                  cpd = F,
                  window = 10,
                  added_time = "unixtime",
                  ...) {
  # treatment_select is a function to select the treatment group from the input data frame

  if(cpd) {
  df_new <- df

  find_optimal_window <- function(residuals, target_period_sd, max_window = 2000) {
    min_difference <- Inf  # initialize minimum difference as infinity
    optimal_window <- 1  # initialize optimal window size as 1

    for (window_size in 1:max_window) {
      # Check if window_size is greater than the length of residuals, if it is, break the loop
      if (window_size > length(residuals)) {
        break
      }

      # Smooth the residuals
      smoothed_residuals <- stats::filter(residuals, rep(1/window_size, window_size), sides = 2)

      # Calculate the difference between the standard deviation of the smoothed residuals and the target period standard deviation
      difference <- abs(sd(smoothed_residuals, na.rm = TRUE) - target_period_sd)

      # Check if difference is NA, if it is, skip this iteration
      if (is.na(difference)) {
        next
      }

      # If this difference is smaller than the current minimum difference, update the minimum difference and the optimal window size
      if (difference < min_difference) {
        min_difference <- difference
        optimal_window <- window_size
      }
    }
    return(optimal_window)
  }

  # Function to extend a vector with mirrored values at the ends
  extend_with_mirror <- function(x, n) {
    c(rev(x[2:(n+1)]), x, rev(x[(length(x)-n):(length(x)-1)]))
  }

  # Get a vector of column names excluding 'datetime'
  colnames_to_loop <- setdiff(names(df_new), "datetime")

  require(bsts)
  require(augsynth)
  # Loop over the columns
  for (i in colnames_to_loop) {
    ss <- bsts::AddLocalLevel(list(), df_new[[i]])
    BSS <- bsts(df_new[[i]], state.specification = ss, niter = 300, ping=0, seed=1)
    predicted_values <- predict.bsts(BSS, horizon = 1)
    pred_means <- predicted_values$mean
    residuals <- df_new[[i]] - pred_means
    target_period_sd <- 0.1 * sd(as.numeric(df_new[[i]]), na.rm = TRUE)

    # Use the function to find the optimal window size
    # optimal_window <- find_optimal_window(residuals, target_period_sd)
    # Use the function to find the optimal window size
    optimal_window <- window

    # Extend residuals with mirrored values
    residuals_extended <- extend_with_mirror(residuals, optimal_window)
    smoothed_residuals_extended <- stats::filter(residuals_extended, rep(1/optimal_window, optimal_window), sides = 2)
    smoothed_residuals <- smoothed_residuals_extended[(optimal_window+1):(length(residuals)+optimal_window)]

    # Update the weather-normalized response variable in the processed dataframe
    df_new[[i]] <- pred_means + smoothed_residuals
  }

    df <- na.omit(df_new)
  }

  # Define the treatment_group based on the mode
  tg <- params$treatment_group

  df_preprocessed <- pick_treatment(df, treatment_group = tg)

  # Initialize list structures to store processed dataframes and results
  df_processed <- list()
  df_list_all <- setNames(vector("list", length(df_preprocessed)), names(df_preprocessed))
  df_list <- setNames(vector("list", length(df_preprocessed)), names(df_preprocessed))

  # Process each dataframe in df_preprocessed
  # tidyr::gather is used for converting wide format to long format
  # add_date_cols is a function to add date related columns to the data frame
  for(i in seq_len(length(df_preprocessed))) {
    df_processed[[i]] <- df_preprocessed[[i]] %>%
      add_date_cols(date_col = datetime, stats = added_time) %>%
      split_policy_time(params$start_time, params$end_time) %>%
      purrr::map(~.x %>% dplyr::select(-datetime))
  }

  names(df_processed) <- names(df_preprocessed)

  for(i in seq_len(length(df_processed))){
  data_temp <- df_processed[[i]][[length(params$start_time) + 1]]

  explanatory_vars <- setdiff(colnames(data_temp), "y")

  # Apply the automatic machine learning model
  auto_ml_model <- autoMod(data_temp,
                           response_variable = "y",
                           predictor_variables = explanatory_vars,
                           split_proportion = params$split_proportion,
                           max_models = params$max_models,
                           max_runtime_secs = params$max_runtime_secs,
                           algorithm = params$algorithm,
                           criterion = params$criterion)

  # Predict using the model and bind the prediction to the processed dataframe
  mlscm_data <- cbind(df_preprocessed[[i]], prediction = as.data.frame(h2o.predict(auto_ml_model[[1]], as.h2o(df_preprocessed[[i]]))))
  mlscm_data["difference"] <- mlscm_data["y"] - mlscm_data["predict"]
  mlscm_data["efficiency"] <- 100 * mlscm_data["difference"] / mlscm_data["predict"]
  df_list[[i]] <- mlscm_data
  }

  # Extract the 'difference' column from each list and cbind
  diff <- data.frame(datetime = df_list[[1]][["datetime"]], do.call(cbind, lapply(df_list, function(x) x$difference)))
  effi <- data.frame(datetime = df_list[[1]][["datetime"]], do.call(cbind, lapply(df_list, function(x) x$efficiency)))

  # Add the result dataframe to df_list
  df_list$difference <- diff
  df_list$efficiency <- effi
  df_list$model <- auto_ml_model

  return(df_list)
}
