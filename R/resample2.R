#' @author \Yuqing \Dai
#'
#' Perform Resample
#'
#' The resample2 function applies resample to a given data. It is particularly useful for energy usage data where weather variables are often key resample_variables.
#'
#' @param data A data frame containing the data to be normalized.
#' @param model A trained model used for prediction. Default is NULL.
#' @param response_variable The variable to be predicted by the model. Default is NULL.
#' @param predictor_variables The resample_variables for the model. Default includes wind speed (ws), wind direction (wd), temperature (temp), and relative humidity (rh).
#' @param constant_variables The variables to be excluded as resample_variables for the model. Default excludes datetime and unixtime.
#' @param num_iterations The number of iterations to perform. Default is 1.
#' @param enable_diff If set to TRUE, a new column containing the weather difference is added to the output. Default is TRUE.
#' @param seed Seed for reproducibility. Default is NULL.
#' @param cdp  Logical. If TRUE, smoothed residuals are calculated. Default is FALSE.
#' @param window Numeric. Specifies the window size for smoothing. Default is 10.
#' @return A list containing the final_data (all data frames combined) and summary_data (data summarized by datetime).
#' @export
#' @examples
#' \dontrun{
#' wenorm(df = mydata, response_variable = "target", model = mymodel, num_iterations = 10)
#' }
#'
resample2 <- function(data,
                   resample_data,
                   model = NULL,
                   response_variable = NULL,
                   resample_variables = NULL,
                   constant_variables = NULL,
                   num_iterations = 1,
                   seed = NULL,
                   ...) {
  
  # Check input
  if (!all(c(constant_variables, response_variable) %in% names(data))) {
    stop("All elements of constant_variables and response_variable must be column names in data.")
  }
  
  if (!(is.numeric(num_iterations) & length(num_iterations) == 1 & num_iterations > 0 & floor(num_iterations) == num_iterations)) {
    stop("num_iterations must be a single positive integer.")
  }
  # Set the seed for reproducibility
  set.seed(seed)
  
  data$id <- seq_len(nrow(data))
  constant_variables <- c(constant_variables, "id")
  resample_variables <- resample_variables
  
  # Load parallel processing packages
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package doParallel needed for this function is not installed. Please install it.")
  }
  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cluster)
  
  # Perform operations in parallel
  randomized_dfs <- foreach(i = 1:num_iterations, .packages = "dplyr") %dopar% {
    new_data <- data[, c(constant_variables, response_variable), drop = FALSE]
    # Sample the row indices from resample_data
    sampled_indices <- sample(1:nrow(resample_data), size = nrow(data), replace = TRUE)
    sampled_resample_data <- resample_data[sampled_indices, resample_variables, drop = FALSE]
    combined_data <- cbind(new_data, sampled_resample_data)
    return(combined_data)
  }
  
  # Clean up
  parallel::stopCluster(cluster)
  
  # Combine all data frames into one
  final_data <- dplyr::bind_rows(randomized_dfs) %>%
    as.h2o() %>%
    h2o::h2o.predict(model, .) %>%
    as.data.frame() %>%
    cbind(., bind_rows(randomized_dfs))
  
  summary_data <- final_data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      predict_mean = mean(predict),
      predict_sd = sd(predict)
    )
  
  return(list(data = as.data.frame(final_data),
              summary = as.data.frame(summary_data))
         )
}
