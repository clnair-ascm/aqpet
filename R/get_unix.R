#' Generate Unix Time Sequence
#'
#' This function generates a sequence of Unix timestamps based on a specified
#' time interval between start and end dates. Optionally, it merges the generated
#' sequence with an existing data frame on the "unixtime" column.
#'
#' @param start_date A character string specifying the start date in the format 'YYYY-MM-DD HH:MM:SS'.
#' @param end_date A character string specifying the end date in the format 'YYYY-MM-DD HH:MM:SS'.
#' @param df An optional data frame to merge with the generated Unix time sequence.
#' @param units A character string specifying the time unit for the interval. Valid units include:
#'   "sec", "min", "hour", "day", "week", "month", "quarter", "season", and "year".
#' @param show_date A logical indicating whether to include a human-readable date column.
#'   Default is FALSE.
#'
#' @return Returns a data frame with a "unixtime" column and, if show_date is TRUE,
#'   an additional "datetime" column. If df is provided, the returned data frame is merged with df.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' get_unix(start_date = "2023-01-01 00:00:00", end_date = "2023-01-02 00:00:00", units = "hour", show_date = TRUE)
#' Define the start and end dates
#' start_date <- "2022-01-01 00:00:00"
#' end_date <- "2022-01-01 01:00:00"
#'
#' Generate Unix time values with 10-minute intervals and display human-readable time format
#' unixtime_data <- get_unix(start_date, end_date, units = "min", show_date = TRUE)
#' print(unixtime_data)
#'
#' Create a sample data frame
#' sample_df <- data.frame(unixtime = c(1640995200, 1640995800, 1640996400),
#'                         value = c(10, 20, 30))
#'
#' Merge the sample data frame with Unix time values
#' merged_data <- get_unix(start_date, end_date, df = sample_df, units = "min", show_date = TRUE)
#' print(merged_data)
#' }
#'
#' @export
#'
get_unix <- function(start_date, end_date, df = NULL, units = "min", show_date = FALSE) {

  # Check if the input dates are character strings
  if (!is.character(start_date) || !is.character(end_date)) {
    stop("start_date and end_date must be character strings in the format 'YYYY-MM-DD HH:MM:SS'.")
  }

  # Define the time interval based on the units
  time_interval <- switch(units,
                          "sec" = 1,
                          "min" = 60,
                          "hour" = 3600,
                          "day" = 3600 * 24,
                          "week" = 3600 * 24 * 7,
                          "month" = 3600 * 24 * 30.44, # avg days per month
                          "quarter" = 3600 * 24 * 30.44 * 3,
                          "season" = 3600 * 24 * 30.44 * 3,
                          "year" = 3600 * 24 * 365.25, # consider leap years
                          stop("Invalid units specified.")
  )

  # Generate the sequence of timestamps
  timestamp_seq <- seq(from = as.POSIXct(start_date, tz = "UTC"),
                       to = as.POSIXct(end_date, tz = "UTC"),
                       by = time_interval)

  # Convert the timestamps to Unix time
  unix_time_seq <- as.numeric(timestamp_seq)

  # Create a data frame with the "unixtime" column
  unixtime_data <- data.frame(unixtime = unix_time_seq)

  # Add human-readable time format column if show_date is set to TRUE
  if (show_date) {
    unixtime_data$datetime <- as.POSIXct(unixtime_data$unixtime, origin = "1970-01-01", tz = "UTC")
  }

  # Merge the "unixtime" data frame with another data frame if provided
  if (!is.null(df)) {
    if ("unixtime" %in% colnames(df)) {
      merged_data <- merge(unixtime_data, df, by = "unixtime", all.x = TRUE)
      return(merged_data)
    } else {
      stop("The dataframe df must contain a column named 'unixtime'.")
    }
  } else {
    return(unixtime_data)
  }
}
