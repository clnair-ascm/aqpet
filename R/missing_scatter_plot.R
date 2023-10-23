#' Scatter Plot Highlighting Missing Data
#'
#' This function creates a scatter plot for two specified columns of a data frame and highlights
#' the data points that have missing values in either or both of the columns. The plot provides
#' a visual representation of the distribution of missing and available data points.
#'
#' @param df A data.frame that contains the data.
#' @param x_col The name of the column to be plotted on the x-axis.
#' @param y_col The name of the column to be plotted on the y-axis.
#' @param title The title of the scatter plot. Default is "Scatter Plot with Missing Data Highlighted".
#' @param color_scheme A named vector indicating the colors for highlighting the existing and missing data points.
#'        Default is `c("Existing data" = "black", "Missing data" = "red")`.
#'
#' @return A ggplot scatter plot.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#'  df <- data.frame(x = c(1, 2, NA, 4, 5),
#'                  y = c(NA, 3, 4, 5, NA))
#'
#'  Create scatter plot with missing data highlighted
#'  missing_scatter_plot(df, "x", "y", "My Scatter Plot", c("blue", "red"))
#' }
#'
#' @export
#'
missing_scatter_plot <- function(df,
                                 x_col,
                                 y_col,
                                 title = "Scatter Plot with Missing Data Highlighted",
                                 color_scheme = c("Existing data" = "black", "Missing data" = "red")) {

  # Input validation and error handling
  if (!is.data.frame(df)) {
    stop("Input should be a data frame.")
  }

  if (!x_col %in% colnames(df)) {
    stop(paste("Column", x_col, "not found in the dataframe."))
  }

  if (!y_col %in% colnames(df)) {
    stop(paste("Column", y_col, "not found in the dataframe."))
  }

  # Create a copy of df to prevent modification of original data
  df_copy <- df

  df_copy <- df_copy %>%
    mutate(missing_x = is.na(!!sym(x_col)),
           missing_y = is.na(!!sym(y_col)),
           missing = missing_x | missing_y)

  get_replacement_value <- function(col) {
    limits <- range(col, na.rm = TRUE)
    replacement <- limits[1] - (limits[2] - limits[1]) * 0.05
    return(replacement)
  }
  # Compute x and y replacements for missing values
  x_replacement <- get_replacement_value(df_copy[[x_col]])
  y_replacement <- get_replacement_value(df_copy[[y_col]])

  df_copy[[x_col]][df_copy$missing_x] <- x_replacement
  df_copy[[y_col]][df_copy$missing_y] <- y_replacement

  # Create the plot
  plot <- ggplot(df_copy, aes_string(x = x_col, y = y_col)) +
    geom_point(aes(color = missing), size = 2.5) +
    scale_color_manual(values = color_scheme,
                       name = "Data Status",
                       guide = guide_legend(title = "Data Status")) +
    labs(title = title,
         x = x_col, y = y_col) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 22, face = "bold", margin = margin(b = 12)),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 12)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 12)),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18)
    )

  return(plot)
}
