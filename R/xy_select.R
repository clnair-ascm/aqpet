#' Select Response and Predictor Variables from a Data Frame
#'
#' This function selects the desired response (dependent) variable(s) and
#' predictor (independent) variables from a data frame. The function allows for
#' a default set of predictor variables or user-specified predictors. For each
#' response variable, a separate data frame is constructed with the selected
#' predictors and is stored in a named list.
#'
#' @param df A data.frame that contains the data.
#' @param response_variable A vector of response variable(s) to be selected.
#' @param predictor_variables A vector of predictor variable(s) to be selected
#'        or "default" to select the default set of predictors: `unixtime, ws, wd, rh, temp, sp`.
#'        Default is "default".
#' @param ... Additional arguments (currently not used).
#'
#' @return A named list of data frames. Each data frame corresponds to a
#'        response variable combined with the selected predictor variables.
#'        The name of the data frame is prefixed with "df_" followed by the
#'        response variable name.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(unixtime = 1:5, ws = rnorm(5), wd = rnorm(5),
#'                   rh = rnorm(5), temp = rnorm(5), sp = rnorm(5), A = rnorm(5), B = rnorm(5))
#'
#' result <- xy_select(data, response_variable = c("A", "B"), predictor_variables = "default")
#' print(result$df_A)
#' }
#'
#' @export
#'
xy_select <- function(df,
                      response_variable,
                      predictor_variables = "default",
                      ...) {

  # Function to handle default x-vars
  default_xvars <- function() {
    df %>% dplyr::select(unixtime, ws, wd, rh, temp, sp)
  }

  # Select x-variables based on input
  df_xv <- if ("default" %in% predictor_variables) {
    default_xvars()
  } else {
    df %>% dplyr::select(dplyr::all_of(predictor_variables))
  }

  # Create a separate data frame for each y-variable and store them in a named list
  output_list <- lapply(response_variable, function(y) {
    df_y <- df %>% dplyr::select(dplyr::all_of(y))
    df_new <- cbind(df_y, df_xv)
    colnames(df_new)[1] <- "y"
    df_new
  })

  names(output_list) <- paste0("df_", response_variable)

  # lapply(names(output_list), function(name) assign(name, output_list[[name]], envir = .GlobalEnv))

  return(output_list)
}
