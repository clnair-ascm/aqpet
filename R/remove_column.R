#' Remove Specified Columns from a Data Frame
#'
#' This function takes in a data frame and a vector of column names, and returns the data frame with the specified columns removed.
#'
#' @param df A data frame from which columns will be removed.
#' @param cols_to_remove A character vector specifying the names of the columns to be removed from the data frame.
#'
#' @details
#' The function leverages the `select` function from the `dplyr` package to safely deselect columns from the input data frame.
#' It uses the `all_of` function to handle column names provided in the `cols_to_remove` parameter, ensuring that if any of the
#' specified columns do not exist in the data frame, no error is thrown.
#'
#' @return A data frame with the specified columns removed.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   A = 1:5,
#'   B = 6:10,
#'   C = 11:15
#' )
#'
#' # Remove columns A and C
#' modified_data <- remove_columns(data, c("A", "C"))
#' print(modified_data)
#'}
#'
#' @seealso
#' \code{\link[dplyr]{select}}, \code{\link[dplyr]{all_of}}
#'
#' @export
#'
remove_columns <- function(df, cols_to_remove) {
  df %>%
    select(-all_of(cols_to_remove))
}
