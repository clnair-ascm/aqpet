#' Function to Calculate Common Model Evaluation Statistics
#'
#' mod_stats
#'
#' The `mod_stats` function is used to evaluate the performance of a model
#' by comparing it with observed data. The function calculates a variety of statistical metrics,
#' including the number of samples (n), fractional bias (FB), geometric mean bias (MG),
#' factor of two (FAC2), coefficient of determination (r), normalized mean square error (NMSE),
#' and others. These metrics provide a comprehensive quantitative assessment of the model's
#' performance and accuracy.
#'
#' @param mydata A data frame containing the model and observed data. It should include at
#' least two columns, one for the modelled data and one for the observed data.
#'
#' @param mod A character string specifying the name of the model data column in `mydata`.
#' Default is "mod".
#'
#' @param obs A character string specifying the name of the observed data column in `mydata`.
#' Default is "obs".
#'
#' @param stats A character vector specifying the evaluation metrics to be computed. Available
#' metrics include "n", "FB", "MG", "FAC2", "VG", "r", "RMSEs", "RMSEu", "RMSE", "COE", "IOA",
#' "IOAr". If "default" is included in `stats` or if `stats` is not provided, all metrics will
#' be computed. Default is "default".
#'
#' @return A data frame with the computed evaluation metrics. Each column corresponds to a
#' metric and contains the computed values.
#'
#' @author [Yuqing Dai]
#'
#'
#' @examples
#' creat a dataframe with data
#' df <- data.frame("mod"= c(1,2,3,4,5,6,7,8,9), "obs"= c(12,31,53,54,12,53,12,45,12))
#' print IOA and IOAr
#' mod_stats(df, mod = "mod", obs = "obs", stats = c("n","IOA","IOAr","r"))
#' print default statistics
#' mod_stats(df, mod = "mod", obs = "obs", stats = "default")
#'
#' df <- data.frame(
#'   mod = rnorm(100, mean = 5, sd = 2),  # modelled data
#'   obs = rnorm(100, mean = 5, sd = 2)   # observed data
#' )
#'
#' # Compute all metrics
#' model_stats <- mod_stats(df)
#' print(model_stats)
#'
#' # Compute specific metrics
#' model_stats <- mod_stats(df, stats = c("n", "FB", "MG"))
#' print(model_stats)
#'
#' @note The function requires the `dplyr` and `purrr` packages. If you wish to modify the
#' metrics computed by the function, you can do so by adjusting the `evaluation_metrics` list.
#'
#' @seealso \code{\link[dplyr]{dplyr}}, \code{\link[purrr]{purrr}}
#'
#' @export
#'
# Load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
  library(purrr)
}

# Define evaluation metric functions
evaluation_metrics <- list(
  n = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- nrow(x)
    data.frame(n = res)
  },
  FB = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- 2 * mean(x[[mod]] - x[[obs]]) / mean(x[[mod]] + x[[obs]])
    data.frame(FB = res)
  },
  MG = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- exp(mean(log(x[[mod]])) - mean(log(x[[obs]])))
    data.frame(MG = res)
  },
  FAC2 = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    ratio <- x[[mod]] / x[[obs]]
    ratio <- na.omit(ratio)
    len <- length(ratio)
    if (len > 0) {
      res <- length(which(ratio >= 0.5 & ratio <= 2)) / len
    } else {
      res <- NA
    }
    data.frame(FAC2 = res)
  },
  COE = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- 1 - sum(abs(x[[mod]] - x[[obs]])) / sum(abs(x[[obs]] - mean(x[[obs]])))
    data.frame(COE = res)
  },
  r = function(x, mod = "mod", obs = "obs", ...) {
    x <- na.omit(x[, c(mod, obs)])
    res <- suppressWarnings(cor(x[[mod]], x[[obs]], ...))
    data.frame(r = res)
  },
  IOAr = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    LHS <- sum(abs(x[[mod]] - x[[obs]]))
    RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))
    if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1
    data.frame(IOAr = res)
  },
  IOA = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    LHS <- sum(abs(x[[mod]] - x[[obs]]))
    RHS <- sum(abs(x[[mod]] - mean(x[[obs]]) + abs(x[[obs]] - mean(x[[obs]]))))
    res <- 1 - LHS / RHS
    data.frame(IOA = res)
  },
  RMSE = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- mean((x[[mod]] - x[[obs]]) ^ 2) ^ 0.5
    data.frame(RMSE = res)
  },
  RMSEu = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- mean((fitted.values(lm(formula = x[[mod]] ~ x[[obs]])) - x[[mod]]) ^ 2) ^ 0.5
    data.frame(RMSEu = res)
  },
  RMSEs = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- mean((fitted.values(lm(formula = x[[mod]] ~ x[[obs]])) - x[[obs]]) ^ 2) ^ 0.5
    data.frame(RMSEs = res)
  },
  VG = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    res <- exp(mean((log(x[[obs]] - log(x[[mod]]))) ^ 2))
    data.frame(VG = res)
  }
)
#'
#' Evaluate the performance of a model compared to observed data.
#'
mod_stats <- function(mydata, mod = "mod", obs = "obs",
                     stats = c("n", "FB", "MG", "FAC2", "VG", "r",
                               "RMSEs", "RMSEu", "RMSE", "COE", "IOA", "IOAr", "default")) {
  # Check if provided stats are valid
  valid_stats <- c(names(evaluation_metrics), "default")
  if (any(!stats %in% valid_stats)) {
    stop(cat("Can't find the statistic(s)", stats[!stats %in% valid_stats], "\n"))
  }

  # If stats is "default", output all other parameters
  if ("default" %in% stats) {
    stats <- c("n","FAC2","r","IOAr","RMSE","RMSEu","RMSEs")
  }

  # Calculate the requested statistics
  results <- purrr::map_dfr(stats, function(stat) {
    evaluation_metrics[[stat]](x = mydata, mod = mod, obs = obs)
  })

  df_clean <- data.frame(lapply(results, na.omit))

  return(df_clean)
}
#'

