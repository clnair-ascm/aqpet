#' Merge a List of Data Frames Based on a Common Column
#'
#' This function takes a list of data frames and merges them based on a common 'datetime' column.
#' For columns specified in `colnames`, if there are duplicate columns due to the merge, the function will
#' prioritize non-NA values over NA values.
#'
#' @param df_list A list of data frames to be merged. Each data frame should contain a 'datetime' column.
#' @param colnames Character vector of column names to be considered during the merge.
#'        For these columns, the function will prioritize non-NA values over NA values.
#'
#' @return A merged data frame.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(datetime = c("2021-01-01", "2021-01-02"), x = c(1, NA), y = c(3, 4))
#' df2 <- data.frame(datetime = c("2021-01-01", "2021-01-02"), x = c(NA, 2), z = c(5, 6))
#' df_list <- list(df1 = df1, df2 = df2)
#'
#' merged_df <- merge_dfs(df_list, colnames = c("x"))
#' print(merged_df)
#' }
#'
#' @export
#'
merge_dfs <- function(df_list, colnames) {

    # Check if df_list is actually a list
  if (!is.list(df_list)) {
    stop("The input 'df_list' must be a list of data frames.")
  }

  # Check if colnames is a character
  if (!is.character(colnames)) {
    stop("'colnames' must be a character vector specifying column names.")
  }

  # Initialize the merged dataframe as the first dataframe in the list
  merged_df <- df_list[[1]][, c("datetime", colnames)]

  # Loop over the rest of the list
  for (i in 2:length(df_list)) {
    # Merge with the next dataframe in the list
    temp_df <- merge(merged_df, df_list[[i]][, c("datetime", colnames)],
                     by = "datetime", all = TRUE, suffixes = c("", paste0("_", i)))

    # Loop over the columns
    for (colnames in colnames) {
      # If 'col_name_i' is not NA, replace 'colnames' with 'col_name_i'
      temp_df[[colnames]] <- ifelse(
        !is.na(temp_df[[paste0(colnames, "_", i)]]),
        temp_df[[paste0(colnames, "_", i)]],
        temp_df[[colnames]]
      )

      # Remove 'col_name_i' column
      temp_df[[paste0(colnames, "_", i)]] <- NULL
    }

    # Update the merged dataframe
    merged_df <- temp_df
  }

  return(merged_df)
}
