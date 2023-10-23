#' Split Data Based on Specified Time Intervals
#'
#' This function partitions a data frame into three segments (pre-event, event, and post-event)
#' based on the specified start and end times.
#'
#' @param df A data frame that contains a datetime column to be used for splitting.
#' @param start_time A character string or a POSIXct object specifying the beginning of the event period.
#' @param end_time A character string or a POSIXct object specifying the end of the event period.
#'
#' @details
#' The function first conducts input checks to ensure that the provided data frame contains a `datetime` column
#' and that both `start_time` and `end_time` are provided. It then splits the data frame into three segments:
#' \itemize{
#'   \item `pre_event`: Entries with a datetime before the specified `start_time`.
#'   \item `event`: Entries within the inclusive range of `start_time` and `end_time`.
#'   \item `post_event`: Entries with a datetime after the specified `end_time`.
#' }
#' All data frames are returned within a named list.
#'
#' @return A named list containing three data frames (`pre_event`, `event`, `post_event`), each corresponding
#' to the aforementioned segments.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(datetime = as.POSIXct(c("2022-01-01 12:00", "2022-01-02 12:00", "2022-01-03 12:00")),
#'                    value = c(10, 20, 30))
#' result <- split_policy_time2(data, start_time = "2022-01-02", end_time = "2022-01-02")
#' print(result$pre_event)  # Data before 2022-01-02
#' print(result$event)      # Data on 2022-01-02
#' print(result$post_event) # Data after 2022-01-02
#'}
#'
#' @export
#'
split_policy_time2 <- function(df, start_time = NULL, end_time = NULL) {

  # Input checks
  if (!is.data.frame(df)) stop("Input df must be a data.frame.")
  if (!"datetime" %in% names(df)) stop("df must contain 'datetime' column.")
  if (is.null(start_time) | is.null(end_time)) stop("Both start_time and end_time should be provided.")

  # Prepare the data
  df$datetime <- as.POSIXct(df$datetime) # ensure datetime column is in correct format

  # Convert start_times and end_times to POSIXct format
  start_time <- as.POSIXct(start_time)
  end_time   <- as.POSIXct(end_time)

  # Create a list to store split dataframes
  dfs <- list()

  # Extract the pre-event, event, and post-event data
  dfs$pre_event <- df[df$datetime < start_time, ]
  dfs$event <- df[df$datetime >= start_time & df$datetime <= end_time, ]
  dfs$post_event <- df[df$datetime > end_time, ]

  return(dfs)
}
