#' Add Date-Based Columns to a Data Frame
#'
#' This function enriches a data frame by adding new columns derived from a specified date column.
#' The new columns can represent various aspects of the date such as year, month, day, hour, etc.
#'
#' @param df A data frame.
#' @param date_col A string specifying the name of the date column in df.
#' @param stats A character vector specifying which date-related statistics to add.
#'   Valid options are: "year", "month", "day", "hour", "doy" (day of year),
#'   "dow" (day of week), "unixtime" (UNIX timestamp), "trend", and "all" (to include all statistics).
#'
#' @return A data frame enhanced with the specified date-related columns.
#'
#' @importFrom dplyr mutate row_number
#' @importFrom lubridate year month day hour yday wday
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' df <- data.frame(date = as.POSIXct(c('2021-01-01 14:00', '2021-01-02 15:00')), value = 1:2)
#' enriched_df <- add_date_cols(df, "date", c("year", "month", "day"))
#' }
#'
#' @export
#'
add_date_cols <- function(df, date_col, stats) {
  # Load the required dplyr and lubridate packages if they're not already loaded
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }

  # Define the valid time-related statistics
  timevars <- c(
    "all", "year", "month", "day", "hour", "doy", "dow", "unixtime", "trend"
  )

  # Check if the provided statistics are valid
  matching <- stats %in% timevars

  if (any(!matching)) {
    # Display an error message if any of the provided statistics are invalid
    stop(cat("Can't find the statistic(s)", stats[!matching], "\n"))
  }

  # Determine which statistics to add based on the input
  if ("all" %in% stats) {
    add_year <- TRUE
    add_month <- TRUE
    add_day <- TRUE
    add_hour <- TRUE
    add_doy <- TRUE
    add_dow <- TRUE
    add_unixtime <- TRUE
    add_trend <- TRUE
  } else {
    add_year <- "year" %in% stats
    add_month <- "month" %in% stats
    add_day <- "day" %in% stats
    add_hour <- "hour" %in% stats
    add_doy <- "doy" %in% stats
    add_dow <- "dow" %in% stats
    add_unixtime <- "unixtime" %in% stats
    add_trend <- "trend" %in% stats
  }

  # Add the specified statistics as new columns in the data frame

  df <- df %>%
    mutate(
      year = if (add_year) lubridate::year({{date_col}}) else NULL,
      month = if (add_month) lubridate::month({{date_col}}) else NULL,
      day = if (add_day) lubridate::day({{date_col}}) else NULL,
      hour = if (add_hour) lubridate::hour({{date_col}}) else NULL,
      doy = if (add_doy) lubridate::yday({{date_col}}) else NULL,
      dow = if (add_dow) lubridate::wday({{date_col}}) else NULL,
      unixtime = if (add_unixtime) as.numeric(as.POSIXct({{date_col}})) else NULL,
      trend = if (add_trend) row_number() else NULL
    )
  return(df)
}
