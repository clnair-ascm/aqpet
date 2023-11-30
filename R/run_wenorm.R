#' Execute Data Preprocessing and Model Training Using Various Methods
#'
#' This function preprocesses the provided dataset, handles missing values, and trains a model based on
#' the method provided in the model parameters. It offers three methods - "aml", "default", and "revised".
#'
#' @param df A data frame that contains the data to be processed and modeled.
#' @param model_params A list that provides various model parameters such as the response variable,
#'                     predictor variables, method for handling missing data, and various parameters
#'                     for the modeling process.
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function starts by checking and converting the response and predictor variables to numeric, if necessary.
#' It then processes the data based on the method provided in `model_params`. The "aml" method processes
#' and models the data differently than the "default" and "revised" methods. After preprocessing, the function
#' utilizes the `autoMod` function to train models, followed by the `wenorm` function to further process
#' the dataset. The final output includes the trained model and processed datasets.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `df_wenorm`: A data frame resulting from the wenorm processing.
#'   \item `df_base`: A base data frame resulting from the wenorm processing.
#'   \item `df_detrend`: A data frame resulting from the detrend wenorm processing.
#'   \item `df_final`: A data frame resulting from the final wenorm processing.
#'   \item `aqmod`: A list that contains details about the trained model, including model statistics and a leaderboard.
#'}
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' sample_data <- data.frame(response = c(1, 2, 3, 4, 5),
#'                           predictor1 = c(1, NA, 3, 4, 5),
#'                           predictor2 = c(NA, 2, 3, 4, NA),
#'                           datetime = as.POSIXct(c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", "2022-01-05")))
#' params <- list(response_variable = "response",
#'                predictor_variables = c("predictor1", "predictor2"),
#'                wenorm_method = "default",
#'                ...)
#' result <- run_wenorm(sample_data, model_params = params)
#' print(result$aqmod$aqmod_stat)
#'}
#'
#' @export
#'
run_wenorm <- function(df,
                       model_params,
                       ...) {
  df_new       <- df
  df_treat     <- data.frame()
  df_detrend   <- data.frame()
  df_wenorm    <- data.frame()
  df_base      <- data.frame()
  df_final     <- data.frame()
  # Make sure 'response_variable' is a column in df_new
  if (model_params$response_variable %in% colnames(df_new)) {
    # Convert to numeric
    df_new[[model_params$response_variable]] <- as.numeric(df_new[[model_params$response_variable]])
  }

  # Make sure 'predictor_variables' are columns in df_new
  for (var in model_params$predictor_variables) {
    if (var %in% colnames(df_new)) {
      # Convert to numeric
      df_new[[var]] <- as.numeric(df_new[[var]])
    }
  }

    switch (model_params$wenorm_method,
            "aml" = {
              my_df <- df_new %>%
                missing_treat(method = model_params$miss_data_treat, response_variable = model_params$response_variable) %>%
                add_date_cols(date_col = datetime, stats = "all")

              aml_AQ <- autoMod(my_df, response_variable = model_params$response_variable,
                                split_by_time = model_params$split_by_time,
                                predictor_variables = model_params$predictor_variables,
                                split_proportion = model_params$split_proportion,
                                seed = model_params$seed,
                                max_models = model_params$max_models,
                                algorithm = model_params$algorithm,
                                criterion = model_params$criterion,
                                max_runtime_secs = model_params$max_runtime_secs)
            },

            "default" = {
              my_df <- df_new %>%
                missing_treat(method = "rm",
                         response_variable = model_params$response_variable) %>%
                add_date_cols(date_col = datetime, stats = "all")

              aml_AQ <- autoMod(my_df,
                                response_variable = model_params$response_variable,
                                split_by_time = model_params$split_by_time,
                                predictor_variables = model_params$predictor_variables,
                                split_proportion = model_params$split_proportion,
                                seed = model_params$seed,
                                max_models = model_params$max_models,
                                algorithm = model_params$algorithm,
                                criterion = model_params$criterion,
                                max_runtime_secs = model_params$max_runtime_secs
                                )

              my_df_wn <- wenorm(my_df,
                                 model = aml_AQ[[1]],
                                 response_variable = model_params$response_variable,
                                 predictor_variables = model_params$predictor_variables,
                                 constant_variables = model_params$constant_variables,
                                 num_iterations = model_params$num_iterations,
                                 seed = model_params$seed
              )

              df_wenorm  <- data.frame(my_df_wn[[3]][,c("datetime", model_params$response_variable, paste(model_params$response_variable, "_wn", sep =""))])
            },

            "revised" = {
              my_df <- df_new %>%
                missing_treat(method = "rm",
                         response_variable = model_params$response_variable) %>%
                add_date_cols(date_col = datetime, stats = "all")

              aml_AQ <- autoMod(my_df,
                                response_variable = model_params$response_variable,
                                split_by_time = model_params$split_by_time,
                                predictor_variables = model_params$predictor_variables,
                                split_proportion = model_params$split_proportion,
                                seed = model_params$seed,
                                max_models = model_params$max_models,
                                algorithm = model_params$algorithm,
                                criterion = model_params$criterion,
                                max_runtime_secs = model_params$max_runtime_secs
              )

              my_df_wn <- wenorm(my_df,
                                 model = aml_AQ[[1]],
                                 response_variable = model_params$response_variable,
                                 predictor_variables = model_params$predictor_variables,
                                 constant_variables = model_params$constant_variables,
                                 num_iterations = model_params$num_iterations,
                                 seed = model_params$seed,
                                 cpd = model_params$cpd,
                                 window = model_params$window
              )

              df_detrend  <- data.frame(my_df_wn[[3]][,c("datetime", model_params$response_variable, paste(model_params$response_variable, "_wn", sep =""))])
              df_base     <- data.frame(my_df_wn[[4]][,c("datetime", model_params$response_variable, paste(model_params$response_variable, "_wn", sep =""))])
              df_wenorm   <- data.frame(my_df_wn[[5]][,c("datetime", model_params$response_variable, paste(model_params$response_variable, "_wn", sep =""))])
              df_final    <- my_df_wn[[6]]
            }
    )

    aqmod_mod  <- aml_AQ[[1]]
    aqmod_data <- aml_AQ[[3]]
    aqmod_stat <- data.frame(mod_explain(model = aml_AQ[[1]],
                                         data = aml_AQ[[3]],
                                         plots = c("performance"),
                                         stats = c("n","FAC2","FB", "r","RMSE","RMSEs","RMSEu","IOA","IOAr")))
    leaderboard <- aml_AQ[[5]]

    aqmod <- list(
      aqmod_mod = aqmod_mod,
      aqmod_data = aqmod_data,
      aqmod_stat = aqmod_stat,
      leaderboard = leaderboard
    )

    if (model_params$kill_h2o) {
      h2o.shutdown(prompt = FALSE)
    }

  return(list(df_wenorm  = df_wenorm,
              df_base = df_base,
              df_detrend  = df_detrend,
              df_final = df_final,
              aqmod   = aqmod
         ))
}
