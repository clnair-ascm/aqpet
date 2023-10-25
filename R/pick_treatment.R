#' Split Data Based on Treatment Groups
#'
#' This function subsets a data frame based on specified treatment and control groups,
#' generating separate data frames for each treatment variable with associated control variables.
#'
#' @param df A data frame that contains the data to be split.
#' @param treatment_group A character vector specifying the treatment columns
#'        or patterns to match in the column names.
#' @param control_group A character vector specifying the control columns
#'        or "default" to automatically determine the control columns by excluding
#'        those matching the treatment_group patterns. Default is "default".
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function processes the input data frame and identifies columns that match
#' the specified treatment_group patterns. For each matched treatment column, it
#' creates a new data frame containing that treatment column and the associated
#' control columns.
#'
#' If "default" is passed for the control_group parameter, the function will automatically
#' determine the control columns by excluding the ones that match the treatment_group patterns.
#'
#' @return A named list of data frames. Each data frame corresponds to a treatment column
#'         and its associated control columns. The names of the data frames follow the format "df_<treatment_group>".
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   treat_A = rnorm(100),
#'   treat_B = rnorm(100),
#'   ctrl_X = rnorm(100),
#'   ctrl_Y = rnorm(100),
#'   ctrl_Z = rnorm(100)
#' )
#'
#' result <- pick_treatment(data, treatment_group = c("treat_A", "treat_B"))
#' str(result)
#'}
#'
#' @export
#'
pick_treatment <- function(df,
                           treatment_group,
                           control_group = "default",
                           ...) {

  # Function to handle default x-vars
  default_xvars <- function() {
    # Identify columns in df that match treatment_group patterns
    cols_to_drop <- unlist(lapply(treatment_group, function(tg) {
      grep(tg, colnames(df), value = TRUE)
    }))
    df %>% dplyr::select(-dplyr::all_of(cols_to_drop))
  }

  # Select x-variables based on input
  df_xv <- if ("default" %in% control_group) {
    default_xvars()
  } else {
    df %>% dplyr::select(dplyr::all_of(control_group))
  }

  # Create a separate data frame for each y-variable and store them in a named list
  # Use grep to get columns from df that match treatment_group patterns
  valid_treatment_group <- unlist(lapply(treatment_group, function(tg) {
    grep(tg, colnames(df), value = TRUE)
  }))

  output_list <- lapply(valid_treatment_group, function(y) {
    df_y <- df %>% dplyr::select(dplyr::all_of(y))
    df_new <- cbind(df_y, df_xv)
    colnames(df_new)[1] <- "y"
    df_new
  })

  names(output_list) <- paste0("df_", treatment_group)

  return(output_list)
}
