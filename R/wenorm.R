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
#' @param constant_variables The subset of `predictor_variables` to hold fixed
#'   (i.e. NOT resampled) -- should be the time variables, not the weather
#'   variables, or no weather-normalisation will actually occur.
#' @param num_iterations The number of iterations to perform. Default is 1.
#' @param enable_diff If set to TRUE, a new column containing the weather difference is added to the output. Default is TRUE.
#' @param seed Seed for reproducibility. Default is NULL.
#' @param cdp  Logical. If TRUE, smoothed residuals are calculated. Default is FALSE. Ignored when wenorm_method = "TuanVu".
#' @param window Numeric. Specifies the window size for smoothing. Default is 10.
#' @param wenorm_method One of "default" (resample_variables reshuffled
#'   across randomly chosen rows from the whole series) or "TuanVu" (Vu et
#'   al. 2019: resample_variables drawn from rows within +/-14 days of year
#'   and the same hour-of-day; skips the baseline/cpd stages below entirely).
#'
#'   Performance note (v0.2.1): for "TuanVu", the original implementation
#'   recomputed, for every single row, a fresh O(N) distance scan over the
#'   *entire* series to find its +/-14-day/same-hour candidates -- and did so
#'   again from scratch on every one of the `num_iterations` Monte Carlo
#'   passes (O(num_iterations * N^2) overall). This version builds the
#'   per-row candidate index once, up front, via a per-hour binary search
#'   (O(N log N) total), then reuses it for every iteration; each iteration
#'   then draws one sampled row per candidate set and assigns the resampled
#'   weather columns in a single vectorised operation instead of row-by-row.
#'   Results are statistically equivalent to the original (same +/-14-day,
#'   same-hour candidate pool; same fallback rules), just computed far less
#'   redundantly.
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
                   wenorm_method = "default",
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
  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cluster)

  # --- "TuanVu" candidate-index precomputation (done once, not per-iteration) ---
  #
  # For every row we need all rows that share its hour-of-day and fall within
  # a circular +/-14-day window of its day-of-year. Build that lookup once
  # here via a per-hour binary search instead of, as before, re-scanning the
  # whole series for every row on every Monte Carlo iteration.
  build_candidate_index <- function(doy, hr) {

    n <- length(doy)
    idx_all <- seq_len(n)
    by_hour <- split(idx_all, hr)
    candidates <- vector("list", n)

    for (rows_h in by_hour) {

      doy_h <- doy[rows_h]
      ord <- order(doy_h)
      rows_sorted <- rows_h[ord]
      doy_sorted  <- doy_h[ord]

      # Triplicate the per-hour timeline (-366, +0, +366) so a circular
      # +/-14-day window never needs special-casing at the year boundary
      # (e.g. day 3 correctly matches day 364 of the previous "lap").
      doy_ext  <- c(doy_sorted - 366, doy_sorted, doy_sorted + 366)
      rows_ext <- rep(rows_sorted, 3)
      ord_ext  <- order(doy_ext)
      doy_ext  <- doy_ext[ord_ext]
      rows_ext <- rows_ext[ord_ext]

      # +/- 0.5 offsets make the inclusive <= 14 day boundary exact under
      # findInterval()'s half-open interval semantics.
      lo_pos <- findInterval(doy_sorted - 14.5, doy_ext) + 1L
      hi_pos <- findInterval(doy_sorted + 14.5, doy_ext)

      for (k in seq_along(rows_sorted)) {
        candidates[[rows_sorted[k]]] <- rows_ext[lo_pos[k]:hi_pos[k]]
      }
    }

    # Defensive fallbacks mirroring the original row-loop's rules. In
    # practice every row always matches itself (distance 0 from its own
    # day-of-year), so these branches should not trigger.
    empty <- which(lengths(candidates) == 0L)
    for (row_i in empty) {
      idx <- which(hr == hr[row_i])
      if (length(idx) == 0L) idx <- idx_all
      candidates[[row_i]] <- idx
    }

    candidates
  }

  if (identical(wenorm_method, "TuanVu")) {

    work_data <- data
    work_data$doy <- as.integer(format(work_data$datetime, "%j"))
    work_data$hr  <- as.integer(format(work_data$datetime, "%H"))

    candidate_idx      <- build_candidate_index(work_data$doy, work_data$hr)
    new_data_template  <- work_data[, c(constant_variables, response_variable), drop = FALSE]
  }

  randomized_dfs1 <- foreach(iter = 1:num_iterations, .packages = "dplyr") %dopar% {

    if (identical(wenorm_method, "TuanVu")) {

      # One sampled candidate row per row, drawn from the precomputed
      # index, then a single vectorised assignment of the resampled
      # weather columns (replacing the original's row-by-row
      # new_data[row_i, ...] <- ... assignment inside an O(N) row loop).
      sampled_rows <- vapply(candidate_idx, function(idx) {
        if (length(idx) == 1L) idx else idx[sample.int(length(idx), 1L)]
      }, integer(1))

      new_data <- new_data_template
      new_data[, resample_variables] <- work_data[sampled_rows, resample_variables]

      return(new_data)

    } else {

      new_data <- data[, c(constant_variables, response_variable), drop = FALSE]
      sampled_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)
      new_data[, resample_variables] <- data[sampled_indices, resample_variables]
      return(new_data)
    }
  }

  # Baseline resample (and the ratio correction + bsts cpd smoothing derived
  # from it, further down) is not part of Vu et al. (2019)'s method and is
  # unused by run_wenorm() for "TuanVu" -- skip to avoid a wasted h2o.predict()
  # pass and bsts fit on every call.
  if (!identical(wenorm_method, "TuanVu")) {
    randomized_dfs2 <- foreach(i = 1:num_iterations, .packages = "dplyr") %dopar% {
      # Define the columns to keep. Check if 'trend' column exists and include it if present.
      cols_to_keep <- c("datetime", response_variable)
      if ("trend" %in% names(data)) {
        cols_to_keep <- c(cols_to_keep, "trend")
      }

      new_data <- data[, cols_to_keep, drop = FALSE]

      # Sample the row indices from the original dataframe
      sampled_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)

      # Shuffle only the predictor variables based on the sampled indices
      new_data[, predictor_variables] <- data[sampled_indices, predictor_variables]

      return(new_data)
    }
  }

  # Clean up
  parallel::stopCluster(cluster)

  # Combine all data frames into one
  final_data1 <- dplyr::bind_rows(randomized_dfs1) %>%
    as.h2o() %>%
    h2o::h2o.predict(model, .) %>%
    as.data.frame() %>%
    cbind(., bind_rows(randomized_dfs1))

  summary_data1 <- final_data1 %>%
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

  if (!identical(wenorm_method, "TuanVu")) {

    final_data2 <- dplyr::bind_rows(randomized_dfs2) %>%
      as.h2o() %>%
      h2o::h2o.predict(model, .) %>%
      as.data.frame() %>%
      cbind(., bind_rows(randomized_dfs2))

    summary_data2 <- final_data2 %>%
      dplyr::group_by(datetime) %>%
      dplyr::summarise(
        predict_mean = mean(predict),
        predict_sd = sd(predict)
      )

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

  } else {
    # "TuanVu": not computed above, so leave NULL
    final_data2   <- NULL
    summary_data2 <- NULL
    summary_data3 <- NULL
    summary_data4 <- NULL
  }

  return(list(final_data_detrend    = final_data1,
              final_data_base       = final_data2,
              summary_data_detrend  = summary_data1,
              summary_data_baseline = summary_data2,
              summary_data_wenormed = summary_data3,
              summary_data_final = summary_data4))
}
