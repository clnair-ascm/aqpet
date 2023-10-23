#' Handle Missing Values in a Data Frame Using Various Imputation Techniques
#'
#' This function provides a versatile approach to handling missing data in a given data frame. Depending on the method chosen,
#' it can remove rows with missing values, use linear interpolation, replace with mean/median, or utilize the sophisticated
#' capabilities of H2O's autoML for imputation.
#'
#' @param df A data.frame with potential missing values.
#' @param method The method to handle missing values. Default is "rm" (remove rows with missing values).
#'        It can take values like "h2o", "linear", "mean", "median", etc.
#' @param response_variable The target/response variable(s) for imputation. Default is NULL.
#' @param predictor_variables Predictor variables for the imputation. Default is NULL.
#' @param split_by_time Logical. If TRUE, data splitting considers time-based order. Default is FALSE.
#' @param split_proportion Proportion of data to be used for training in case of H2O imputation. Default is 0.8.
#' @param seed Seed for reproducibility. Default is 1234.
#' @param patch_data Logical. If TRUE, replaces the missing values in the original dataframe with the predicted values. Default is TRUE.
#' @param max_models Maximum number of models to be trained using H2O's autoML. Default is 10.
#' @param algorithm Algorithm choice for H2O's autoML imputation. Default is "gbm".
#' @param criterion Criterion choice for H2O's autoML imputation. Default is "AUTO".
#' @param max_runtime_secs Maximum time (in seconds) to run H2O's autoML. If NULL, there's no limit. Default is NULL.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A data frame with imputed values.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   datetime = seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-10 00:00:00"), by = "hour"),
#'   A = sample(c(0:100), size = 217, replace = TRUE),
#'   B = sample(c(0:100), size = 217, replace = TRUE),
#'   C = sample(c(0:100), size = 217, replace = TRUE),
#'   D = sample(c(0:100), size = 217, replace = TRUE),
#'   E = sample(c(0:100), size = 217, replace = TRUE),
#'   G = sample(c(0:100), size = 217, replace = TRUE))
#'
#' Randomly replace some values with NA
#' df$A[sample(1:217, 50)] <- NA
#' df$B[sample(1:217, 30)] <- NA
#' df$C[sample(1:217, 20)] <- NA
#'
#' predictor_variables <- c("D","E","G")
#'
#' test <- miss_imp(df, method = "h2o", response_variable = c("A", "B"), predictor_variables = c("D","E","G"), max_runtime_secs = 5, seed = 123, patch_data = T, split_proportion = 0.8)
#' test1 <- miss_imp(df, method = "mean", response_variable = c("A", "B"))
#' test2 <- miss_imp(df, method = "rm", response_variable = c("A", "B"))
#' response_variable <- c("A","B")
#' }
#'
#' @export
#'
miss_imp <- function(df,
                     method = "rm",
                     response_variable = NULL,
                     predictor_variables = NULL,
                     split_by_time = F,
                     split_proportion = 0.8,
                     seed = 1234,
                     patch_data = T,
                     max_models = 10,
                     algorithm = "gbm",
                     criterion = "AUTO",
                     max_runtime_secs = NULL,
                     ...) {

  #' Check if df is a data.frame
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  #' Impute specific columns only
  impute_columns <- function(imputed_data, original_data, response_variable) {
    if (!is.null(response_variable)) {
      not_columns <- setdiff(colnames(original_data), response_variable)
      imputed_data <- imputed_data[, response_variable]
      original_data <- original_data[, not_columns]
      return(cbind(original_data, imputed_data))
    } else {
      return(imputed_data)
    }
  }

  df_all <- xy_select(df, response_variable = response_variable, predictor_variables = predictor_variables)

  if (method == "h2o") {

    # Create parallel backend
    n_cores <- detectCores()
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)

    df_list <- foreach(i = 1:length(response_variable), .combine = cbind, .packages = c("tidymodels", "h2o", "magrittr")) %dopar% {
      # Start H2O server within each parallel worker
    source("autoMod.R")

      # Process each column in parallel
      df_prep <- data.frame(df_all[[i]]) %>% drop_na(all_of("y"))
      df_pred <- data.frame(df_all[[i]])

      aml_AQ <- autoMod(df_all[[i]],
                        response_variable = "y",
                        split_by_time = split_by_time,
                        predictor_variables = predictor_variables,
                        split_proportion = split_proportion,
                        seed = seed,
                        max_models = max_models,
                        algorithm = algorithm,
                        criterion = criterion,
                        max_runtime_secs = max_runtime_secs
      )

      combined_data_h2o <- as.h2o(df_all[[i]])

      predictions <- as.data.frame(h2o.predict(aml_AQ[[1]], combined_data_h2o))
      colnames(predictions)[colnames(predictions) == "predict"] <- "y.pred"
      #
      #'=========================================================================================
      #'===============================    prediction    ========================================
      #'=========================================================================================
      #'
      # Use the pipe operator to create a new column using the ifelse() function
      local_df <- cbind(predictions, df_pred["y"])

      # Use the pipe operator to create a new column using the ifelse() function
      if (patch_data) {
        local_df <- local_df %>% mutate(!!paste("y.cp", sep = "") :=
                                                  ifelse(is.na(.[["y"]]), .[[paste("y.pred", sep = "")]], .[["y"]]))
        # %>% select(-paste(yvars, ".pred", sep = ""), -yvars)

        local_df <- local_df[, !(names(local_df) %in% c("y"))]}

      # Replace "y" with "a" in column names
      colnames(local_df) <- gsub("y", response_variable[i], colnames(local_df))
      local_df
    }

    # Stop parallel backend
    stopCluster(cl)

    df_final <- data.frame(cbind(df_list, df))

    return(df_final)
  } else if (method == "rm") {
    if (is.null(response_variable)) {
      return(na.omit(df))
    } else {
      return(df[complete.cases(df[, response_variable]), ])
    }
  } else {
    imputed_data <- df[, response_variable, drop = FALSE]

    if (method == "linear") {
      imputed_data <- na.approx(imputed_data)
    } else if (method == "mean") {
      imputed_data <- base::apply(imputed_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    } else if (method == "median") {
      imputed_data <- base::apply(imputed_data, 2, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
    } else {
      stop("Invalid method specified.")
    }

    return(impute_columns(imputed_data, df, response_variable))
  }
}
