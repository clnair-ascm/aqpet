#' Remove Data Frames with Specified Columns All NA
#'
#' This function processes a list of data frames and removes any data frame
#' that contains specified columns with all NA values.
#'
#' @param data_list A list of data frames to process.
#' @param col_names A character vector of column names to check for all NA values.
#'
#' @return Returns a filtered list of data frames.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data1 <- data.frame(a = c(1, 2, NA), b = c(NA, NA, NA))
#' data2 <- data.frame(a = c(3, 4, 5), b = c(NA, 1, 2))
#' mylist <- list(df1 = data1, df2 = data2)
#'
#' filtered_list <- list_na_rm(mylist, col_names = "b")
#' print(filtered_list)
#' }
#'
#' @export
#'
list_na_rm <- function(data_list, col_names) {
  new_list <- list()

  for(i in seq_along(data_list)){
    df <- data_list[[i]]

    # Boolean flag to keep track if the dataframe should be kept
    keep_df <- TRUE

    for(col_name in col_names){
      # If the column exists and is all NA, don't keep the df
      if(col_name %in% names(df) && all(is.na(df[[col_name]]))){
        keep_df <- FALSE
        break
      }
    }

    # If keep_df is still TRUE, append it to the new list
    if(keep_df){
      new_list[[names(data_list)[i]]] <- df
    }
  }

  return(new_list)
}
