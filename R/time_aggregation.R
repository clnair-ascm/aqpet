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

  # Early return if no time resolution is provided
  if (is.null(time_resolution)) {
    return(df)
  }

  # Convert datetime column to POSIXct only if it's not already
  if (!inherits(df$datetime, "POSIXct")) {
    df$datetime <- as.POSIXct(df$datetime)
  }

  # Check for wind speed and direction columns
  has_wind_components <- all(c("ws", "wd") %in% names(df))

  # Calculate u and v components if needed
  if (has_wind_components) {
    df <- df %>%
      mutate(u = ws * cos(pi / 180 * wd),
             v = ws * sin(pi / 180 * wd))
  }

  # Group and summarize data
  df <- df %>%
    group_by(datetime = floor_date(datetime, time_resolution)) %>%
    summarise(across(
      .cols = if (has_wind_components) -c(datetime, wd, ws, u, v) else everything(),
      .fns = \(x) mean(x, na.rm = TRUE),
      .names = if (has_wind_components) "mean_{.col}" else "{.col}"
    ))

  # Additional calculations for wind components
  if (has_wind_components) {
    df <- df %>%
      mutate(ws = sqrt(mean_u^2 + mean_v^2),
             wd = (atan2(mean_v, mean_u) * 180 / pi + 360) %% 360) %>%
      select(-mean_u, -mean_v)
  }

  # Rename columns if needed
  if (has_wind_components) {
    cols_to_rename <- setdiff(names(df), c("datetime", "ws", "wd"))
    new_names <- gsub("^mean_", "", cols_to_rename)
    names(df)[names(df) %in% cols_to_rename] <- new_names
  }

  # Convert back to data frame if it's a tibble
  df <- as.data.frame(df)

  return(df)
}
