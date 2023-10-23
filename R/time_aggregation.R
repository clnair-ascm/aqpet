#' Aggregate Data by Specified Time Resolution
#'
#' This function aggregates the input data frame based on a provided time resolution.
#' It handles special cases where wind speed (`ws`) and wind direction (`wd`)
#' are present in the data by converting them to u and v components and then
#' aggregating based on the desired time resolution.
#'
#' @param df A data.frame: the input data set.
#' @param time_resolution A character string indicating the desired time resolution for aggregation (e.g., "day", "hour"). If NULL, no time aggregation is applied.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' If `ws` and `wd` columns are present, the function first converts these
#' to u and v wind components. It then aggregates the data based on the given
#' time resolution. For columns other than `ws` and `wd`, the function calculates
#' the mean for each time period. For the `ws` and `wd` columns, it calculates
#' the resultant wind speed and direction.
#'
#' If `ws` and `wd` columns are not present, the function simply aggregates all columns
#' based on their mean values for each time period.
#'
#' @return A data frame with aggregated values based on the specified time resolution.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'     datetime = as.POSIXct(seq(1, 100, by = 1), origin = "1970-01-01"),
#'     A = rnorm(100), ws = rnorm(100, mean = 10), wd = runif(100, min = 0, max = 360)
#' )
#'
#' aggregated_data <- time_aggregation(data, time_resolution = "day")
#' head(aggregated_data)
#' }
#'
#' @export
#'
time_aggregation <- function(df, time_resolution = NULL, ...) {

  # If no time resolution is provided, return the original data
  if (is.null(time_resolution)) {
    return(df)
  }

  df$datetime <- as.POSIXct(df$datetime)

  if ("ws" %in% names(df) & "wd" %in% names(df)) {
    # Convert wind direction and speed to u and v components
    df <- df %>%
      mutate(u = ws * cos(pi/180 * wd),
             v = ws * sin(pi/180 * wd)) %>%
      group_by(datetime = floor_date(datetime, time_resolution)) %>%
      summarise(across(-c(datetime, wd, ws, u, v), \(x) mean(x, na.rm = TRUE), .names = "mean_{.col}"),
                u_avg = mean(u, na.rm = TRUE),
                v_avg = mean(v, na.rm = TRUE),
                ws = sqrt(u_avg^2 + v_avg^2),
                wd = (atan2(v_avg, u_avg) * 180/pi + 360) %% 360) %>%
      ungroup() %>%
      select(-u_avg, -v_avg)

    # Rename columns to remove the 'mean_' prefix for columns other than wd and ws
    cols_to_rename <- setdiff(names(df), c("datetime", "ws", "wd"))
    new_names <- gsub("^mean_", "", cols_to_rename)
    names(df)[names(df) %in% cols_to_rename] <- new_names

  } else {
    # Use original mean method
    df <- df %>%
      group_by(datetime = floor_date(datetime, time_resolution)) %>%
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  }

  df <- as.data.frame(df)

  return(df)
}
