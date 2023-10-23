#' Machine Learning Enhanced Event Model
#'
#' The function combines time-series processing with machine learning to provide a predictive model
#' for an event in the data, returning the processed data, predictions, differences, and efficiencies.
#'
#' @param df A dataframe containing the time series data.
#' @param params A list containing parameters that dictate the behavior of the model. Expected fields are
#' `start_time`, `end_time`, `split_proportion`, `max_models`, `max_runtime_secs`, `algorithm`, and `criterion`.
#' @param cpd Logical flag indicating if residual smoothing and decomposition should be done. Default is FALSE.
#' @param window Numeric value indicating the window size for smoothing. Default is 10.
#' @param added_time Character vector specifying which date related columns to add. Default includes elements like
#' "trend", "year", "month", etc.
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function begins by optionally processing the input dataframe, decomposing and smoothing residuals using the
#' BSTS (Bayesian Structural Time Series) method. If residual smoothing is enabled, it applies smoothing based on the
#' given window.
#' Subsequently, it transforms the time-series data by adding time-related columns and prepares the data for modeling.
#' Finally, an automatic machine learning model is applied, followed by predictions.
#' The differences and efficiencies between the observed and predicted values are also calculated.
#' The function returns a list containing the processed data, predictions, differences, efficiencies, and the
#' machine learning model.
#'
#' @return
#' A list containing:
#'   - `data`: A dataframe with processed data, predictions, differences, and efficiencies.
#'   - `model`: The machine learning model used for predictions.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' df <- data.frame(
#'   datetime = seq(from = as.POSIXct("2020-01-01"), to = as.POSIXct("2020-01-10"), by = "hour"),
#'   metric1 = rnorm(240),
#'   metric2 = rnorm(240)
#' )
#'
#' # Define params list
#' params <- list(
#'   start_time = as.POSIXct("2020-01-03 00:00"),
#'   end_time = as.POSIXct("2020-01-07 23:00"),
#'   split_proportion = 0.7,
#'   max_models = 10,
#'   max_runtime_secs = 60,
#'   algorithm = "GBM",
#'   criterion = "RMSE"
#' )
#'
#' results <- ml_event2(df, params, rsd = TRUE, window = 5)
#' }
#'
#' @export
#'
ml_event2 <- function(df,
                  params,
                  cpd = F,
                  window = 10,
                  added_time = c("trend", "year", "month", "day", "hour", "doy", "dow", "unixtime"),
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

  # Process each dataframe in df_preprocessed
  # tidyr::gather is used for converting wide format to long format
  # add_date_cols is a function to add date related columns to the data frame
  df_new <- df %>%
    add_date_cols(date_col = datetime, stats = added_time)

  df_processed <- df %>%
      add_date_cols(date_col = datetime, stats = added_time) %>%
      split_policy_time2(params$start_time, params$end_time) %>%
      purrr::map(~.x %>% dplyr::select(-datetime))

  data_pre  <- df_processed[[1]]
  data_post <- df_processed[[3]]

  response_var <- setdiff(colnames(data_pre), added_time)
  colnames(data_pre)[colnames(data_pre) == response_var]   <- "y"
  colnames(data_post)[colnames(data_post) == response_var] <- "y"
  explanatory_vars <- setdiff(colnames(data_pre), "y")
  explanatory_vars <- setdiff(colnames(data_post), "y")

  auto_ml_pre <- autoMod(data_pre,
                           response_variable = "y",
                           predictor_variables = explanatory_vars,
                           split_proportion = params$split_proportion,
                           max_models = params$max_models,
                           max_runtime_secs = params$max_runtime_secs,
                           algorithm = params$algorithm,
                           criterion = params$criterion)

  auto_ml_post <- autoMod(data_post,
                         response_variable = "y",
                         predictor_variables = explanatory_vars,
                         split_proportion = params$split_proportion,
                         max_models = params$max_models,
                         max_runtime_secs = params$max_runtime_secs,
                         algorithm = params$algorithm,
                         criterion = params$criterion)

  # Predict using the model and bind the prediction to the processed dataframe
  mlevent_pre   <- as.data.frame(h2o.predict(auto_ml_pre[[1]], as.h2o(df_new)))
  mlevent_post  <- as.data.frame(h2o.predict(auto_ml_post[[1]], as.h2o(df_new)))

  # Rename the columns of the predictions
  colnames(mlevent_pre)[colnames(mlevent_pre) == "predict"] <- "predict_pre_model"
  colnames(mlevent_post)[colnames(mlevent_post) == "predict"] <- "predict_post_model"

  mlevent_data  <- cbind(df_new, mlevent_pre, mlevent_post)
  # Apply the automatic machine learning model

  df_list <- list()
  df_list$data <- mlevent_data
  df_list$model_pre <- auto_ml_pre
  df_list$model_post <- auto_ml_post

  return(df_list)
}
