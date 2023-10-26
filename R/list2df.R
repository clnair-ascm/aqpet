#' Convert List to Data Frame Based on Selected Column
#'
#' This function processes a list of data frames and combines them into a single data frame.
#' A specified column name is created by appending a suffix to the response variable provided in the parameters.
#'
#' @param df_list A list of data frames to be combined.
#' @param params A list containing parameters for processing. The list should include a 'response_variable' element.
#'
#' @return Returns a consolidated data frame.
#'
#' @author [Yuqing Dai, Chengxu Tong]
#'
#' @examples
#' \dontrun{
#' data1 <- data.frame(a = 1:3, b = 4:6)
#' data2 <- data.frame(a = 7:9, b = 10:12)
#' mylist <- list(df1 = data1, df2 = data2)
#' myparams <- list(response_variable = "a")
#'
#' combined_df <- list2df(mylist, myparams)
#' print(combined_df)
#' }
#'
#' @export
#'
list2df <- function(df_list, params) {

  colname <- paste(params$response_variable, "_wn", sep = "")

  df_selected <- list2df_xyselect(df_list, colnames = colname)

  return(df_selected)
}
