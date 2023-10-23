#' Process, Aggregate, and Analyze Data based on User-Defined Parameters
#'
#' This function performs a series of operations on a data frame, which include:
#' 1. Removing specific columns based on a treatment group.
#' 2. Time-based aggregation.
#' 3. Bayesian Change Point (BCP) analysis for each non-datetime column.
#' 4. Reshaping the output to wide format for both posterior probabilities and means.
#'
#' @param df A data.frame: the input data set.
#' @param params A list containing user-defined parameters including:
#'     - `treatment_group`: a vector specifying which columns/groups to remove from the data.
#'     - `start_time` and `end_time`: used to filter the resulting data based on a datetime range.
#' @param time_resolution A character string indicating the desired time resolution for aggregation (e.g., "day", "hour"). If NULL, no time aggregation is applied.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list containing:
#'     - `prob`: Data frame in wide format showing posterior probabilities after BCP.
#'     - `mean`: Data frame in wide format showing posterior means after BCP.
#'     - `summary`: A list of data frames with detailed BCP results for each non-datetime column.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'     datetime = as.POSIXct(seq(1, 100, by = 1), origin = "1970-01-01"),
#'     A = rnorm(100), B = rnorm(100), C = rnorm(100)
#' )
#'
#' parameters <- list(
#'     treatment_group = c("B"),
#'     start_time = as.POSIXct(10, origin = "1970-01-01"),
#'     end_time = as.POSIXct(90, origin = "1970-01-01")
#' )
#'
#' result <- treat_select(data, params = parameters, time_resolution = "day")
#' print(result$prob)
#' }
#'
#' @export
#'
treat_select <- function(df,
                         params,
                         time_resolution = NULL,
                         ...) {
  # Function to handle default x-vars
  handle_default_xvars  <- function() {
    # If treatment_group is NULL or doesn't exist in params or is entirely NA
    if (!"treatment_group" %in% names(params) || is.null(params$treatment_group) || all(is.na(params$treatment_group))) {
      return(df)
    }
    # Clean the treatment_group vector to remove NAs
    clean_treatment_group <- na.omit(params$treatment_group)

    # Construct the pattern string for grep
    pattern <- paste(clean_treatment_group, collapse = "|")
    # Identify the columns to drop
    cols_to_drop <- grep(pattern, colnames(df), value = TRUE)
    # Drop the columns and return the modified data frame
    data <- df %>%
      dplyr::select(-dplyr::all_of(cols_to_drop))
    return(data)
  }

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

  df_select <- handle_default_xvars()
  df_xv <- time_aggregation(df_select, time_resolution = time_resolution)

  # Identify non-datetime columns
  non_datetime_cols <- names(df_xv)[!sapply(df_xv, function(x) inherits(x, "POSIXct"))]

  # Split dataframe into a list based on non-datetime columns
  list_p <- lapply(non_datetime_cols, function(col) {
    return(df_xv[, c("datetime", col)])
  })

  # Set names of the list elements to be the non-datetime columns

  list_d <- lapply(1:length(list_p), function(idx) {
    df_tmp <- list_p[[idx]]
    df_imp <- miss_imp(df = df_tmp, method = "rm", response_variable = non_datetime_cols[idx])

    result <- bcp::bcp(df_imp[, 2])

    # Combine the data frames and then rename the columns
    df_combined <- cbind(df_imp, result$posterior.mean, result$posterior.prob)
    colnames(df_combined)[(ncol(df_combined) - 1):(ncol(df_combined))] <- c("posterior.mean", "posterior.prob")

    return(df_combined)
  })

  names(list_d) <- non_datetime_cols

  # Define the function to filter data frame based on datetime
  filter_by_datetime <- function(df, start_time, end_time = NULL) {
    # If end_time is not provided, use the maximum datetime value
    if (is.null(end_time)) {
      end_time <- max(df$datetime, na.rm = TRUE)
    }

    # Filter the data frame
    df_filtered <- df %>%
      dplyr::filter(datetime >= start_time & datetime <= end_time)

    return(df_filtered)
  }

  # Function to combine and reshape data frames
  combine_and_spread <- function(list_d, value_col) {
    list_d %>%
      lapply(select, datetime, all_of(value_col)) %>%
      bind_rows(.id = 'source') %>%
      spread(key = source, value = all_of(value_col))
  }

  # Apply the function to each data frame in list_d
  list_d_filtered <- lapply(list_d, filter_by_datetime,
                            start_time = params$start_time,
                            end_time = params$end_time)

  # Generate combined and reshaped dataframes
  df_wide_prob <- combine_and_spread(list_d, "posterior.prob")
  df_wide_filtered_prob <- combine_and_spread(list_d_filtered, "posterior.prob")
  df_wide_mean <- combine_and_spread(list_d, "posterior.mean")
  df_wide_filtered_mean <- combine_and_spread(list_d_filtered, "posterior.mean")

  return(
    list(
      prob = df_wide_filtered_prob,
      mean = df_wide_filtered_mean,
      summary = list_d
    )
  )
}
