#' Combine Selected Columns from List of Data Frames
#'
#' This function processes a list of data frames and consolidates them based on the specified columns.
#' The data frames in the list are expected to contain a 'datetime' column, and they are merged by this column.
#'
#' @param df_list A list of data frames to be combined.
#' @param colnames Character vector specifying the column names to extract from each data frame in the list. Default is 'y'.
#' @param time_resolution Optional: A time resolution for aggregating data. Valid values might include "minute", "hour", "day", etc.
#'
#' @return Returns a consolidated data frame.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data1 <- data.frame(datetime = as.POSIXct(c("2021-01-01", "2021-01-02")), y = c(1,2))
#' data2 <- data.frame(datetime = as.POSIXct(c("2021-01-01", "2021-01-02")), y = c(3,4))
#' mylist <- list(df1 = data1, df2 = data2)
#'
#' combined_df <- list2df_xyselect(mylist, colnames = "y")
#' print(combined_df)
#' }
#'
#' @export
#'
list2df_xyselect <- function(df_list, colnames = "y", time_resolution = NULL) {
  # Check if the dataframe list is empty
  if (length(df_list) == 0) stop("The list of dataframes is empty.")

  # Initialize an empty dataframe to hold the merged selected columns
  merged_df <- data.frame()

  for(i in seq_along(df_list)) {
    if (!"datetime" %in% names(df_list[[i]])) stop("No 'datetime' column in dataframe", names(df_list)[i], ".")

    for (colname in colnames) {
      # Check if the selected column exists in the dataframe
      if (!(colname %in% names(df_list[[i]]))) {
        warning(paste("Column", colname, "not found in dataframe", names(df_list)[i], ". Skipping this dataframe."))
        next
      }

      # Extract the 'datetime' and selected column
      temp_df <- df_list[[i]][, c("datetime", colname)]

      # Rename the selected column to the column name plus the name of the dataframe
      names(temp_df)[names(temp_df) == colname] <- paste0(colname, "_", names(df_list)[i])

      # If a time resolution has been specified, aggregate the data accordingly
      if (!is.null(time_resolution)) {
        temp_df$datetime <- as.POSIXct(temp_df$datetime)
        temp_df <- temp_df %>%
          mutate(datetime = floor_date(datetime, time_resolution)) %>%
          group_by(datetime) %>%
          summarise(across(everything(), mean, na.rm = TRUE))
      }

      # Merge the current dataframe with the merged_df by 'datetime'
      merged_df <- if (nrow(merged_df) == 0) {
        temp_df
        } else {
          merge(merged_df, temp_df, by = "datetime", all = TRUE)
        }
    }
  }

  return(merged_df)
}
