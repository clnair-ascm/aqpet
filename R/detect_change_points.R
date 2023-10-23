#' Detect Change Points in Time Series Data
#'
#' This function detects change points in the time series data using two methods:
#' the strucchange package and the changepoint package with the PELT method.
#'
#' @param data A data frame containing time series data.
#' @param start_time A character or Date object specifying the start time for analysis.
#' @param end_time A character or Date object specifying the end time for analysis.
#'
#' @return Returns plots showing the detected change points using both methods.
#'
#' @importFrom dplyr filter
#' @importFrom strucchange breakpoints
#' @importFrom changepoint cpt.meanvar cpts
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' Create a sample data frame for testing
#' set.seed(123)
#' sample_data <- data.frame(
#'   datetime = seq(as.Date("2023/01/01"), as.Date("2023/06/30"), by = "day"),
#'   no2 = c(rnorm(45, 11, 1),rnorm(45, 10, 1), rnorm(25, 14, 1), rnorm(20, 12, 1), rnorm(46, 10, 1))
#' )
#'
#' Check the dataframe
#' plot(sample_data)
#
#' Call the function with the csv files just created
#' detect_change_points(sample_data, start_time = "2023-01-01", end_time = "2023-06-30")
#' }
#'
#' @export
#'
detect_change_points <- function(data,
                                 start_time,
                                 end_time){
  # Filter data based on start_time and end_time
  data <- sample_data %>% filter(datetime >= start_time & datetime <= end_time)

  # Convert datetime to Date class
  data$datetime <- as.Date(data$datetime)

  # Apply breakpoint analysis using strucchange
  break_points_strucchange <- breakpoints(no2 ~ datetime, data = data)

  # Print breakpoints
  cat("Breakpoints detected by strucchange:\n")
  print(break_points_strucchange)

  # Apply PELT method using changepoint package
  break_points_changepoint <- cpt.meanvar(data$no2, method = "PELT")

  # Print changepoints
  cat("Change points detected by PELT method:\n")
  print(cpts(break_points_changepoint))

  # Plot the series with breakpoints
  cat("Plots:\n")
  par(mfrow = c(2, 1))  # Set the plot area into a 2x1 array
  plot(break_points_strucchange, main="Breakpoints by strucchange")
  plot(break_points_changepoint, main="Change points by PELT method")
}
