#' Set Global Parameters
#' @param data_dir The directory where the data files are stored.
#' @param file_pattern The pattern to match for data files.
#' @param datetime_format The format of the datetime in the data files.
#' @param dependent_variable The dependent variable in the analysis.
#' @param data_timerange The time range for the analysis.
#' @param wenormed Logical, whether the data is weather-normalized.
#' @return A list of Global Control Panel parameters.
setGP <- function(data_dir = ".",
                      file_pattern = "*.csv",
                      datetime_format = "%d/%m/%Y %H:%M",
                      dependent_variable = "no2",
                      data_timerange = c("2018-01-01", "2019-12-31"),
                      wenormed = FALSE) {
  return(list(data_dir = data_dir,
              file_pattern = file_pattern,
              datetime_format = datetime_format,
              dependent_variable = dependent_variable,
              data_timerange = data_timerange,
              wenormed = wenormed))
}

#' Set Weather Normalization Parameters
#' @param response_variable The response variable in the analysis.
#' @param predictor_variables A vector of predictor variables.
#' @param constant_variables A vector of constant variables.
#' @param miss_data_treat The method to treat missing data.
#' @param split_proportion The proportion of data to be used for training.
#' @param split_by_time Logical, whether to split data by time.
#' @param seed The seed for reproducibility.
#' @param max_models The maximum number of models to be trained.
#' @param max_runtime_secs The maximum runtime in seconds.
#' @param wenorm_method The method for weather normalization.
#' @param num_iterations The number of iterations for training.
#' @param algorithm The machine learning algorithm to be used.
#' @param criterion The criterion for model selection.
#' @param write_out Logical, whether to write out the results.
#' @param kill_h2o Logical, whether to shut down the H2O cluster.
#' @param out_dir The output directory.
#' @param cpd Logical, whether to use change point detection.
#' @param window The window size for the analysis.
#' @return A list of Weather Normalized Panel parameters.
setWeNorm <- function(response_variable = "no2",
                              predictor_variables = c("trend", "month", "day", "dow", "doy", "hour", "wd", "ws", "RH", "temp", "sp", "blh"),
                              constant_variables = c("wd", "ws", "RH", "temp", "sp", "blh"),
                              miss_data_treat = "rm",
                              split_proportion = 0.8,
                              split_by_time = FALSE,
                              seed = 123,
                              max_models = 10,
                              max_runtime_secs = 120,
                              wenorm_method = "revised",
                              num_iterations = 300,
                              algorithm = "gbm",
                              criterion = "AUTO",
                              write_out = TRUE,
                              kill_h2o = FALSE,
                              out_dir = "revised_",
                              cpd = TRUE,
                              window = 50) {
  return(list(response_variable = response_variable,
              predictor_variables = predictor_variables,
              constant_variables = constant_variables,
              miss_data_treat = miss_data_treat,
              split_proportion = split_proportion,
              split_by_time = split_by_time,
              seed = seed,
              max_models = max_models,
              max_runtime_secs = max_runtime_secs,
              wenorm_method = wenorm_method,
              num_iterations = num_iterations,
              algorithm = algorithm,
              criterion = criterion,
              write_out = write_out,
              kill_h2o = kill_h2o,
              out_dir = out_dir,
              cpd = cpd,
              window = window))
}

#' Set Synthetic Control Parameters
#' @param treatment_group A vector of treatment group cities.
#' @param start_time The start time for the analysis.
#' @param end_time The end time for the analysis.
#' @param buffer_time The buffer time for the analysis.
#' @param split_proportion The proportion of data to be used for training.
#' @param split_by_time Logical, whether to split data by time.
#' @param seed The seed for reproducibility.
#' @param max_models The maximum number of models to be trained.
#' @param max_runtime_secs The maximum runtime in seconds.
#' @param algorithm The machine learning algorithm to be used.
#' @param criterion The criterion for model selection.
#' @return A list of Synthetic Control Panel parameters.
setSCP <- function(treatment_group = c("Marylebone"),
                          start_time = c("2018-09-01"),
                          end_time = c("2018-09-15"),
                          buffer_time = NULL,
                          split_proportion = 0.8,
                          split_by_time = FALSE,
                          seed = 123,
                          max_models = 7,
                          max_runtime_secs = 120,
                          algorithm = "gbm",
                          criterion = "AUTO") {
  return(list(treatment_group = treatment_group,
              start_time = start_time,
              end_time = end_time,
              buffer_time = buffer_time,
              split_proportion = split_proportion,
              split_by_time = split_by_time,
              seed = seed,
              max_models = max_models,
              max_runtime_secs = max_runtime_secs,
              algorithm = algorithm,
              criterion = criterion))
}
