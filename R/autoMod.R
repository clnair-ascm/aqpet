#' Automated Modeling Function
#'
#' This function provides an automated workflow for modeling data using H2O's AutoML capabilities.
#'
#' @param df Data frame containing the raw data.
#' @param response_variable Name of the response (dependent) variable. Default is NULL.
#' @param predictor_variables Character vector specifying the predictor (independent) variables. Default is "default".
#' @param excluded_variables Variables to be excluded from the modeling process. Default is NULL.
#' @param max_runtime_secs Maximum runtime in seconds for the AutoML process. Default is NULL.
#' @param max_models Maximum number of models for the AutoML process. Default is 10.
#' @param split_proportion Proportion of data to use for training. Default is 0.8.
#' @param split_by_time Logical indicating whether to split data based on time. Default is FALSE.
#' @param algorithm Algorithm type for best model extraction. Default is "gbm".
#' @param criterion Criterion for best model extraction. Default is "AUTO".
#' @param seed Seed for reproducibility. Default is 1234.
#' @param ... Other arguments passed to the function.
#'
#' @return Returns a list containing the final model, training data, testing data, predictions, and the leaderboard.
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom tidyr drop_na
#' @importFrom rsample initial_time_split initial_split
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.get_best_model h2o.predict h2o.get_leaderboard
#'
#' @author [Yuqing Dai]
#'
#' @examples
#'
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
#' #'
#' #' Randomly replace some values with NA
#' df$A[sample(1:217, 50)] <- NA
#' df$B[sample(1:217, 30)] <- NA
#' df$C[sample(1:217, 20)] <- NA
#' #'
#' df
#' #'
#' aml_test <- autoMod(df, response_variable = "A", predictor_variables = c("E","G"), max_models = 10, split_proportion = 0.8, seed = 123, algorithm = "gbm")
#' aml_test[[5]]
#' #'
#' pred <- h2o.predict(object = aml_test[[1]], newdata = aml_test[[3]])
#' plot(as.data.frame(pred)$predict, as.data.frame(aml_test[[3]])$y)
#'
#' exa <- h2o.explain(aml_test[[1]], aml_test[[3]])
#' exa
#' }
#'
#' @export
#'
autoMod <- function(df,
                    response_variable   = NULL,
                    predictor_variables = "default",
                    excluded_variables  = NULL,
                    max_runtime_secs    = NULL,
                    max_models          = 10,
                    split_proportion    = 0.8,
                    split_by_time       = FALSE,
                    algorithm           = "gbm",
                    criterion           = "AUTO",
                    seed                = 1234,
                    max_mem_size        = "24g",
                    ...) {

  # Ensure df is a data frame
  if (!is.data.frame(df)) {
    stop("Input data must be a data frame.")
  }

  # Process dataframe
  processed_df <- xy_select(df, response_variable = response_variable, predictor_variable = predictor_variables)


  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cluster)

  # Start the H2O cluster (locally)
  h2o::h2o.init(max_mem_size = max_mem_size)

  # Prepare dataframe
  preprocessed_df <- data.frame(processed_df[[1]]) %>% tidyr::drop_na(all_of("y"))
  set.seed(seed)

  # Split data based on time or randomly
  if (split_by_time) {
    data_split <- rsample::initial_time_split(preprocessed_df, prop = split_proportion, strata = y)
  } else {
    data_split <- rsample::initial_split(preprocessed_df, prop = split_proportion, strata = y)
  }

  # Obtain the training and testing data
  training_data <- h2o::as.h2o(training(data_split))
  testing_data  <- h2o::as.h2o(testing(data_split))

  predictor_variables <- setdiff(predictor_variables, excluded_variables)

  # Perform AutoML
  auto_ml <- h2o::h2o.automl(x = predictor_variables,
                             y = "y",
                             training_frame = training_data,
                             max_models = max_models,
                             max_runtime_secs = max_runtime_secs,
                             seed = seed)

  # Select the best model
  final_model <- h2o::h2o.get_best_model(auto_ml, algorithm = algorithm, criterion = criterion)

  # Predict
  predictions <- h2o::h2o.predict(object = final_model, newdata = testing_data)

  # Get leaderboard
  leaderboard_df <- h2o::h2o.get_leaderboard(object = auto_ml, extra_columns = "ALL")
  leaderboard <- as.data.frame(leaderboard_df)

  # Package results
  result_list <- list(final_model, training_data, testing_data, predictions, leaderboard)
  names(result_list) <- c("Model", "TrainingData", "TestingData", "Predictions", "Leaderboard")

  # Clean up
  parallel::stopCluster(cluster)

  return(result_list)
}
