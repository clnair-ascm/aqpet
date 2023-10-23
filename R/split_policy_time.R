#' Split Data Frame By Policy Time Periods
#'
#' This function splits a data frame into separate data frames based on
#' given start and end times. Each resulting data frame represents a
#' policy period. Any data that falls outside of the provided policy
#' periods will be returned in a separate data frame labeled "out_of_period".
#'
#' @param df A data frame. It must contain a column named 'datetime'.
#' @param start_times A vector containing the start times of each policy period. Should be in POSIXct format or NULL. If NULL, the function will return the original data frame.
#' @param end_times A vector containing the end times of each policy period. Should be in POSIXct format or NULL. If NULL, the end time for each period will be set to the maximum datetime value in the data.
#' @return A list of data frames, each representing a policy period. The names of the list elements are "df_policy_" followed by the
#'         policy period number. Any data outside the provided policy periods will be returned in a separate data frame labeled "df_out_of_period".
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(datetime = seq(as.POSIXct("2021-01-01"),
#'                                   as.POSIXct("2021-12-31"), by = "day"),
#'                    value = rnorm(365))
#' start_times <- as.POSIXct(c("2021-02-01", "2021-05-01", "2021-09-01"))
#' end_times <- as.POSIXct(c("2021-04-30", "2021-08-31", "2021-12-31"))
#' policy_periods <- split_policy_time(data, start_times, end_times)
#' }
#'
#' @export
#'
split_policy_time <- function(df, start_times = NULL, end_times = NULL) {

  # Input checks
  if (!is.data.frame(df)) stop("Input df must be a data.frame.")
  if (!"datetime" %in% names(df)) stop("df must contain 'datetime' column.")
    if (length(start_times) != length(end_times)) stop("start_times and end_times must have the same length.")

  # Prepare the data
  df$datetime <- as.POSIXct(df$datetime) # ensure datetime column is in correct format

  # If no times are provided, return the original dataframe in a list
  if (is.null(start_times) & is.null(end_times)) {
    warning("Both start_times and end_times are NULL. Returning original df.")
    return(list(df))
  }

  # Convert start_times and end_times to POSIXct format
  start_times <- as.POSIXct(start_times)

  # If end_times is NULL, set it to the maximum datetime value, ignoring NA values
  if (is.null(end_times)) {
    end_times <- max(df$datetime, na.rm = TRUE)
  } else {
    end_times <- as.POSIXct(end_times)
  }

  # Create a list to store split dataframes
  dfs <- list()

  # Loop through each policy period defined by start_times and end_times
  for (i in seq_along(start_times)) {
    # Split the data based on the current policy period
    current_df <- df[df$datetime >= start_times[i] & df$datetime <= end_times[i], ]

    # If the current_df is not empty, save it to the list
    if(nrow(current_df) > 0){
      dfs[[paste0("df_policy_", i)]] <- current_df
    }

    # Exclude the current policy period data from the main dataframe
    df <- df[!(df$datetime >= start_times[i] & df$datetime <= end_times[i]), ]
  }

  # Add the remaining data (outside all policy periods) to the list
  if(nrow(df) > 0){
    dfs[["df_out_of_period"]] <- df
  }

  return(dfs)
}
