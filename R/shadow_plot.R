#' Function to show missing value
#'
#' shadow_plot
#'
#' This function generates a shadow plot to visualize the missing values in a dataframe.
#' Each row of the plot corresponds to a row in the dataframe, and each column corresponds
#' to a variable (column) in the dataframe. Missing values are colored differently from
#' non-missing values, making it easy to identify patterns of missingness.
#'
#' @param df A dataframe for which to generate the shadow plot.
#' @param column A character vector specifying the names of the columns to include in the
#' plot. If NULL (default), all columns in the dataframe are included.
#' @param date_col A character string specifying the name of the date column in the
#' dataframe. If NULL (default), no date column is used.
#' @param plot_title A character string specifying the title of the plot. Default is
#' "Shadow Plot of Missing Data".
#' @param x_label A character string specifying the label for the x-axis. Default is
#' "Variables".
#' @param y_label A character string specifying the label for the y-axis. Default is
#' "Observations".
#' @param na_color A character string specifying the color for missing values. Default is
#' "red".
#' @param non_na_color A character string specifying the color for non-missing values.
#' Default is "blue".
#' @param na_label A character string specifying the label for missing values. Default is
#' "Missing".
#' @param non_na_label A character string specifying the label for non-missing values.
#' Default is "Not Missing".
#' @return A ggplot object representing the shadow plot.
#'
#' @author [Yuqing Dai]
#'
#' @examples
# df <- data.frame(
#   x = c(1, 2, NA, 4, 5),
#   y = c(NA, 2, 3, 4, 5),
#   z = c(1, 2, 3, NA, NA)
# )
# shadow_plot(df)

#' @note This function requires the ggplot2 and dplyr packages. Make sure these packages
#' are installed before using this function.
#'
#' @export
#'
shadow_plot <- function(df,
                        column = NULL,
                        date_col = NULL,
                        plot_title = "Shadow Plot of Missing Data",
                        x_label = "Variables",
                        y_label = "Observations",
                        na_color = "red",
                        non_na_color = "blue",
                        na_label = "Missing",
                        non_na_label = "Not Missing") {

  # Input validation and error handling
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  if (!is.null(column) && !all(column %in% colnames(df))) {
    stop("One or more specified columns are not found in the dataframe")
  }

  if (!is.null(date_col) && !date_col %in% colnames(df)) {
    stop("Date column is not found in the dataframe")
  } else if (!is.null(date_col)) {
    df[[date_col]] <- as.Date(df[[date_col]])
  }

  # Selecting all columns if column parameter is NULL
  if (is.null(column)) {
    column <- colnames(df)
  }

  # Binary map for missing values
  missing_map <- df %>%
    select(column) %>%
    is.na() %>%
    as.data.frame() %>%
    mutate_all(as.numeric) %>%
    rename_all(~ paste0(.x, "_missing")) %>%
    bind_cols(df %>% select(column))

  # Preparing the data for the plot
  gathered_missing_map <- missing_map %>%
    tidyr::gather(Var1, value, -ends_with("_missing"), factor_key = TRUE) %>%
    mutate(row_number = seq_along(value),
           value = ifelse(is.na(value) == 1, non_na_label, na_label))

  # Creating the plot
  plot <- ggplot(gathered_missing_map, aes(x = Var1, y = row_number, color = value)) +
    geom_jitter(alpha = 0.8, size = 2.5, width = 0.3, height = 0) +
    scale_color_manual(values = c(non_na_color, na_color),
                       labels = c(non_na_label, na_label), name = "Label") +
    coord_flip() +
    labs(x = x_label, y = y_label, title = plot_title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 22, face = "bold", margin = margin(b = 12)),
      axis.title.x = element_text(size = 20,  face = "bold", margin = margin(t = 12)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 12)),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18)
    )

  # Returning the plot
  return(plot)
}
#' #'
#' # Create a sample data frame with missing data
#' set.seed(1)
#' df <- data.frame(x = 1:5, y = sample(c(NA, 0:5), 10, replace = TRUE),
#'                   z = sample(c(NA, 0:5), 10, replace = TRUE), w = sample(c(NA, 0:5), 10, replace = TRUE))
#'
#' #' View the data frame
#' df
#'
#' #' Create a shadow plot of the missing data
#' shadow_plot(df)
