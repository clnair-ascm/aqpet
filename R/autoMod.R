#' Automated Modeling Function
#'
#' This function provides an automated workflow for modeling data using H2O's AutoML capabilities.
#'
#' @param df Data frame containing the raw data.
#' @param response_variable Name of the response (dependent) variable. Default is NULL.
#' @param predictor_variables Character vector specifying the predictor variables. Default is "default".
#' @param excluded_variables Variables to be excluded from the modeling process. Default is NULL.
#' @param max_runtime_secs Maximum runtime in seconds for the AutoML process. Default is NULL.
#' @param max_models Maximum number of models for the AutoML process. Default is 10.
#' @param split_proportion Proportion of data to use for training. Default is 0.8.
#' @param split_by_time Logical indicating whether to split data based on time. Default is FALSE.
#' @param algorithm Algorithm type for best model extraction. Default is "gbm".
#' @param criterion Criterion for best model extraction. Default is "AUTO".
#' @param seed Seed for reproducibility. Default is 1234.
#' @param max_mem_size Maximum memory size allocated to H2O. Default is "24g".
#' @param exclude_algos H2O AutoML algorithms to exclude. Default excludes DeepLearning.
#' @param kill_h2o Logical. If TRUE, shut down H2O at the end. Default is FALSE.
#' @param h2o_ip IP address for H2O connection. Default is "127.0.0.1".
#' @param ... Other arguments passed to the function.
#'
#' @return Returns a list containing the final model, training data, testing data, predictions, leaderboard, and AutoML object.
#'
#' @importFrom parallel detectCores
#' @importFrom rsample initial_time_split initial_split training testing
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.get_best_model h2o.predict h2o.get_leaderboard h2o.shutdown
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
                    exclude_algos       = c("DeepLearning"),
                    kill_h2o            = FALSE,
                    h2o_ip              = "127.0.0.1",
                    ...) {

  # ------------------------------------------------------------------
  # 1. Basic checks
  # ------------------------------------------------------------------

  if (!is.data.frame(df)) {
    stop("Input data must be a data frame.")
  }

  # ------------------------------------------------------------------
  # 2. Process dataframe
  # ------------------------------------------------------------------

  processed_df <- xy_select(
    df,
    response_variable  = response_variable,
    predictor_variable = predictor_variables
  )

  preprocessed_df <- data.frame(processed_df[[1]])

  if (!"y" %in% names(preprocessed_df)) {
    stop("The processed dataframe must contain a response column named 'y'. Please check xy_select().")
  }

  preprocessed_df <- preprocessed_df[!is.na(preprocessed_df$y), , drop = FALSE]

  if (nrow(preprocessed_df) < 10) {
    stop("Too few complete rows after removing missing response values.")
  }

  # Use the predictors actually present in the processed dataframe
  model_predictors <- setdiff(names(preprocessed_df), "y")

  if (!is.null(excluded_variables)) {
    model_predictors <- setdiff(model_predictors, excluded_variables)
  }

  if (length(model_predictors) == 0) {
    stop("No predictor variables remain after processing and exclusions.")
  }

  # ------------------------------------------------------------------
  # 3. H2O setup
  # ------------------------------------------------------------------

  num_cores <- max(1L, parallel::detectCores() - 1L)

  # Avoid proxy interference for local H2O connection
  Sys.setenv(
    no_proxy = "localhost,127.0.0.1",
    NO_PROXY = "localhost,127.0.0.1"
  )

  # Detect job array ID from common HPC systems
  env_vars <- c(
    Sys.getenv("SLURM_ARRAY_TASK_ID", NA),
    Sys.getenv("PBS_ARRAYID", NA),
    Sys.getenv("LSB_JOBINDEX", NA),
    Sys.getenv("SGE_TASK_ID", NA)
  )

  task_id <- suppressWarnings(as.numeric(na.omit(env_vars)[1]))

  if (is.na(task_id)) {
    message("💻 Local or non-array environment detected — using default port 54321")
    base_port <- 54321
  } else {
    message("🏗️ HPC array environment detected — task ID: ", task_id)
    base_port <- 54000 + task_id * 2
  }

  max_retries <- 3
  h2o_conn <- NULL
  port_use <- NULL

  for (try_i in seq_len(max_retries)) {

    port_use <- base_port + (try_i - 1) * 2

    cat("Attempt", try_i, "starting H2O on port", port_use, "\n")

    h2o_conn <- try(
      h2o::h2o.init(
        ip           = h2o_ip,
        port         = port_use,
        max_mem_size = max_mem_size,
        nthreads     = num_cores
      ),
      silent = TRUE
    )

    if (!inherits(h2o_conn, "try-error")) {
      cat("✅ Successfully connected to H2O on port", port_use, "\n")
      break
    }

    cat("⚠️ Failed to connect on port", port_use, "— retrying...\n")
    Sys.sleep(5)
  }

  if (inherits(h2o_conn, "try-error") || is.null(h2o_conn)) {
    stop("❌ Failed to start or connect to H2O after multiple retries.")
  }

  # ------------------------------------------------------------------
  # 4. Split data
  # ------------------------------------------------------------------

  set.seed(seed)

  if (split_by_time) {
    data_split <- rsample::initial_time_split(
      preprocessed_df,
      prop = split_proportion
    )
  } else {
    data_split <- rsample::initial_split(
      preprocessed_df,
      prop = split_proportion,
      strata = y
    )
  }

  training_df <- rsample::training(data_split)
  testing_df  <- rsample::testing(data_split)

  # ------------------------------------------------------------------
  # 5. Convert to H2O frames
  # ------------------------------------------------------------------

  training_data <- h2o::as.h2o(training_df)
  testing_data  <- h2o::as.h2o(testing_df)

  # ------------------------------------------------------------------
  # 6. Run AutoML
  # ------------------------------------------------------------------

  project_name <- paste0(
    "AutoML_",
    ifelse(is.null(response_variable), "response", response_variable),
    "_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    sample.int(1e6, 1)
  )

  auto_ml <- h2o::h2o.automl(
    x                = model_predictors,
    y                = "y",
    training_frame   = training_data,
    max_models       = max_models,
    max_runtime_secs = max_runtime_secs,
    seed             = seed,
    exclude_algos    = exclude_algos,
    project_name     = project_name
  )

  # ------------------------------------------------------------------
  # 7. Select final model
  # ------------------------------------------------------------------

  final_model <- try(
    h2o::h2o.get_best_model(
      auto_ml,
      algorithm = algorithm,
      criterion = criterion
    ),
    silent = TRUE
  )

  if (inherits(final_model, "try-error") || is.null(final_model)) {
    warning(
      "No model found for algorithm = '", algorithm,
      "'. Using the AutoML leader model instead."
    )
    final_model <- auto_ml@leader
  }

  # ------------------------------------------------------------------
  # 8. Predict and get leaderboard
  # ------------------------------------------------------------------

  predictions <- h2o::h2o.predict(
    object  = final_model,
    newdata = testing_data
  )

  leaderboard_df <- h2o::h2o.get_leaderboard(
    object = auto_ml,
    extra_columns = "ALL"
  )

  leaderboard <- as.data.frame(leaderboard_df)

  # ------------------------------------------------------------------
  # 9. Package results
  # ------------------------------------------------------------------

  result_list <- list(
    Model        = final_model,
    TrainingData = training_data,
    TestingData  = testing_data,
    Predictions  = predictions,
    Leaderboard  = leaderboard,
    AutoML       = auto_ml,
    Predictors   = model_predictors,
    H2OPort      = port_use
  )

  # Important:
  # By default, do NOT shut down H2O here because the returned Model,
  # TrainingData, TestingData and Predictions are H2O server-side objects.
  if (isTRUE(kill_h2o)) {
    warning(
      "kill_h2o = TRUE: H2O will be shut down, so returned H2O objects may no longer be usable."
    )
    h2o::h2o.shutdown(prompt = FALSE)
    Sys.sleep(5)
  }

  return(result_list)
}
