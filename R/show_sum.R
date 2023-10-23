#' Generate Summary Statistics for a Data Frame
#'
#' This function produces summary statistics for numeric and factor columns in a given data frame.
#' The statistics can be generated for the entire data frame or be grouped by specific time intervals.
#'
#' @param df A data frame containing the data to be summarized.
#' @param date_col A character string specifying the name of the date column in the data frame.
#' Defaults to "datetime".
#' @param interval A character string specifying the time interval for grouping the summary.
#' Possible values are "default", "year", "month", "day", and "hour". If "default",
#' statistics are generated without grouping. Defaults to "default".
#'
#' @details
#' The function first validates the input data frame and parameters. It ensures that the provided
#' date column exists in the data frame and is of an appropriate date-time type.
#' The numeric columns are summarized with min, 1st quartile, median, mean, 3rd quartile, max, and standard deviation.
#' Factor columns are summarized with the number of unique values and the levels.
#'
#' When the interval is specified (other than "default"), the function groups the data by the specified
#' time interval and computes the summary statistics for each group.
#'
#' @return A data frame containing the summary statistics. Each row represents a variable, and the columns
#' include statistics such as min, q1, median, mean, q3, max, and sd for numeric variables,
#' and n_unique and levels for factor variables.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' Create a sample data frame with missing values
#' set.seed(12)
#'  df <- data.frame(
#'  datetime = seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-10 00:00:00"), by = "hour"),
#'  A = sample(c(0:100), size = 217, replace = TRUE),
#'  B = sample(c(0:100), size = 217, replace = TRUE),
#'  C = sample(c(0:100), size = 217, replace = TRUE))
#'
#' Randomly replace some values with NA
#' df$A[sample(1:217, 50)] <- NA
#' df$B[sample(1:217, 30)] <- NA
#' df$C[sample(1:217, 20)] <- NA
#'
#' Use the show_missing function to display the missing data distribution, shadow plot, and scatter plot
#' show_sum(df, interval = "hour")
#'}
#'
#' @export
#'
show_sum <- function(df, date_col = "datetime", interval = "default") {

  # Ensure the input is a data frame
  if (!is.data.frame(df)) {
    stop("Error: The input should be a data frame.")
  }

  # Ensure the date_col exists in the data frame
  if (!date_col %in% colnames(df)) {
    stop("Error: The provided date column does not exist in the data frame.")
  }

  # Ensure the date_col is of Date, POSIXct, or POSIXlt class
  if (!(inherits(df[[date_col]], "Date") | inherits(df[[date_col]], "POSIXct") | inherits(df[[date_col]], "POSIXlt"))) {
    stop("Error: The date column should be of class Date, POSIXct, or POSIXlt.")
  }

  # Check if 'interval' is valid
  valid_intervals <- c("default", "year", "month", "day", "hour")
  if (!(interval %in% valid_intervals)) {
    stop(paste("Error: 'interval' should be one of:", paste(valid_intervals, collapse = ", ")))
  }

  if (interval == "default") {
    summary_data <- df %>%
      summarise(across(where(is.numeric),
                       list(min = ~min(., na.rm = TRUE),
                            q1 = ~quantile(., 0.25, na.rm = TRUE),
                            median = ~median(., na.rm = TRUE),
                            mean = ~mean(., na.rm = TRUE),
                            q3 = ~quantile(., 0.75, na.rm = TRUE),
                            max = ~max(., na.rm = TRUE),
                            sd = ~sd(., na.rm = TRUE)
                       ),
                       .names = "{.col}.{.fn}"
      ),
      across(where(is.factor),
             list(n_unique = ~n_distinct(.),
                  levels = ~toString(unique(.))
             ),
             .names = "{.col}.{.fn}"
      ))
    # Reshape data to get one row per variable
    summary_data <- summary_data %>%
      pivot_longer(cols = everything(), names_to = c("column", "statistic"), names_sep = "\\.", values_to = "value") %>%
      pivot_wider(names_from = statistic, values_from = value)
  } else {
    summary_data <- df %>%
      mutate(year = lubridate::year(!!sym(date_col)),
             month = lubridate::month(!!sym(date_col)),
             day = lubridate::day(!!sym(date_col)),
             hour = lubridate::hour(!!sym(date_col))) %>%
      group_by(!!sym(interval)) %>%
      summarise(across(where(is.numeric),
                       list(min = ~min(., na.rm = TRUE),
                            q1 = ~quantile(., 0.25, na.rm = TRUE),
                            median = ~median(., na.rm = TRUE),
                            mean = ~mean(., na.rm = TRUE),
                            q3 = ~quantile(., 0.75, na.rm = TRUE),
                            max = ~max(., na.rm = TRUE),
                            sd = ~sd(., na.rm = TRUE)
                       ),
                       .names = "{.col}.{.fn}"
      ),
      across(where(is.factor),
             list(n_unique = ~n_distinct(.),
                  levels = ~toString(unique(.))
             ),
             .names = "{.col}.{.fn}"
      ))
    # Reshape data to get one row per variable
    summary_data <- summary_data %>%
      pivot_longer(cols = -!!sym(interval), names_to = c("column", "statistic"), names_sep = "\\.", values_to = "value") %>%
      pivot_wider(names_from = statistic, values_from = value) %>%
      filter(!column %in% c("year", "month", "day", "hour"))  # Exclude rows for year, month, day, hour
  }

  # Return the summary table
  return(summary_data)
}
