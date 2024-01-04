#' Augmented Synthetic Control Method for Causal Effect Estimation
#'
#' The function estimates the causal effect of an intervention using the augmented
#' synthetic control method. The data is preprocessed, synthetic control methods are applied,
#' and effects are calculated.
#'
#' If the `cpd` parameter is set to TRUE, Bayesian structural time series (`bsts`) is
#' used to compute smoothed residuals. These residuals are then utilized to update the dataframe.
#'
#' The `augsynth` function is subsequently applied to estimate the causal effect.
#'
#' @param df Data frame containing the raw data.
#' @param params List of parameters. Contains the treatment_group and start_time.
#'
#' @return Returns a list of dataframes with the estimated effects and upper/lower bound, and average effect.
#'
#' @author [Yuqing Dai, Chengxu Tong]
#'
#' @examples
#' \dontrun{
#' # Assuming df is your data frame and params is your parameters list:
#' results <- a_scm(df, params)
#' }
#'
#' @importFrom bsts AddLocalLevel
#' @importFrom augsynth augsynth
#' @importFrom tidyr gather
#' @importFrom dplyr mutate if_else
#'
#' @export
#'
a_scm <- function(df, params, cpd = FALSE, window = 10) {

  # Load necessary libraries
  library(augsynth)
  library(tidyr)
  library(dplyr)

  # If cpd is TRUE, calculate smoothed residuals
  if (cpd) {
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

    df_new <- df
    colnames_to_loop <- setdiff(names(df_new), "datetime")

    for (i in colnames_to_loop) {
      ss <- bsts::AddLocalLevel(list(), df_new[[i]])
      BSS <- bsts(df_new[[i]], state.specification = ss, niter = 300, ping = 0, seed = 1)
      pred_means <- predict(BSS, horizon = 1)$mean

      residuals <- df_new[[i]] - pred_means
      residuals_extended <- extend_with_mirror(residuals, window)
      smoothed_residuals_extended <- stats::filter(residuals_extended, rep(1/window, window), sides = 2)
      smoothed_residuals <- smoothed_residuals_extended[(window+1):(length(residuals)+window)]

      df_new[[i]] <- pred_means + smoothed_residuals
    }
    df <- na.omit(df_new)
  }

  # Define treatment group and preprocess the dataframe
  tg <- params$treatment_group
  df_preprocessed <- pick_treatment(df, treatment_group = tg)

  # Initialize lists to store processed dataframes and results
  df_processed <- list()
  df_list_all <- vector("list", length(df_preprocessed))
  df_list <- vector("list", length(df_preprocessed))
  names(df_list_all) <- names(df_preprocessed)
  names(df_list) <- names(df_preprocessed)

  # Process each dataframe in df_preprocessed
  for(i in seq_len(length(df_preprocessed))) {
    df_processed[[i]] <- df_preprocessed[[i]] %>%
      gather(key = "site", value = "value", -datetime) %>%
      mutate(trt = if_else(site == 'y' & datetime > params$start_time, 1, 0)) %>%
      add_date_cols(date_col = datetime, stats = "unixtime")
  }

  # Apply augsynth for each dataframe in df_processed
  for(i in seq_len(length(df_processed))) {
    df_list_all[[i]] <- augsynth(
      value ~ trt,
      unit = site,
      unixtime,
      data = df_processed[[i]],
      progfunc = "Ridge",
      scm = TRUE
    ) %>% summary(inf_type = "jackknife+")

    df_list[[i]] <- df_list_all[[i]]$att %>%
      cbind(df$datetime) %>%
      rename_with(~replace(., length(.), "datetime"))

    df_list[[i]]$Effect <- -100 * df_list[[i]]$Estimate / (-df_list[[i]]$Estimate + df_preprocessed[[i]]$y)
    df_list[[i]]$Effect_lower_bound <- -100 * df_list[[i]]$lower_bound / (-df_list[[i]]$Estimate + df_preprocessed[[i]]$y)
    df_list[[i]]$Effect_upper_bound <- -100 * df_list[[i]]$upper_bound / (-df_list[[i]]$Estimate  + df_preprocessed[[i]]$y)
    df_list[[i]][1, "average_att"] <- df_list_all[[i]]$average_att[1, 1]
  }

  return(df_list)
}
