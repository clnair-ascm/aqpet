#' Plot Timeseries of Treatment and Control Elements
#'
#' This function visualizes a timeseries of data in the provided dataframe. It supports highlighting certain series,
#' specifying a date range, and various formatting options. Additionally, the resulting plot can either be
#' in ggplot2 format or converted to a Plotly interactive plot.
#'
#' @param df A data frame with a 'datetime' column and one or more columns of numeric data.
#' @param y_variable A character vector specifying which columns to plot. Default is all columns except 'datetime'.
#' @param highlight A character vector of column names that should be highlighted in the plot.
#' @param x_range A vector specifying the start and end datetime to be plotted.
#' @param start_time A datetime indicating when a vertical line should be added, typically representing the start of an event.
#' @param end_time A datetime indicating when a vertical line should be added, typically representing the end of an event.
#' @param buffer_time A datetime indicating when a vertical line should be added, often representing a buffer or lag.
#' @param time_resolution A character string that represents the frequency to aggregate the data. Default is "1 hour".
#' @param plotly A logical. If TRUE, returns an interactive Plotly plot. Otherwise, returns a ggplot. Default is FALSE.
#' @param ylab A label for the y-axis. Default is "Value".
#' @param xlab A label for the x-axis. Default is "Date".
#' @param title The main title for the plot.
#' @param subtitle A subtitle for the plot.
#' @param legend.pos The position of the legend on the plot. Options are "right", "left", "top", "bottom", etc.
#' @param text_size A numeric value to scale the size of text elements in the plot. Default is 1.0.
#' @param line_size A numeric value to scale the size of line elements in the plot. Default is 1.0.
#' @param ylim A numeric vector of length 2 indicating the y limits.
#' @param layer_order A character vector specifying the order in which to layer the plotted series.
#' @param line_colors A named list or vector of colors for the lines.
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function offers a flexible way to visualize timeseries data with options to highlight certain series,
#' add vertical lines to indicate specific events, and customize the appearance of the plot.
#'
#' @return If `plotly` is TRUE, returns a Plotly object. Otherwise, returns a ggplot object.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' data <- data.frame(datetime = as.POSIXct(c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", "2022-01-05")),
#'                    value1 = c(1, 2, 3, 4, 5),
#'                    value2 = c(5, 4, 3, 2, 1))
#' plot_tsvat2(data, y_variable = c("value1", "value2"), highlight = "value2")
#'}
#'
#' @export
#'
plot_tsvat2 <- function(df,
                            y_variable = NULL,
                            highlight = NULL,
                            x_range = NULL,
                            start_time = NULL,
                            end_time = NULL,
                            buffer_time = NULL,
                            time_resolution = "1 hour",
                            plotly = FALSE,
                            ylab = "Value",
                            xlab = "Date",
                            title = "Timeseries of treatment and control elements",
                            subtitle = "",
                            legend.pos = "right",
                            text_size = 1.0,
                            line_size = 1.0,
                            ylim = NULL,
                            layer_order = NULL,
                            line_colors = NULL,
                            ...) {
  # Load required packages
  load_packages <- function() {
    packages <- c("purr", "lubridate", "dplyr")
    sapply(packages, function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    })
  }
  # If y_variable (columns to plot) is not provided, use all columns except datetime
  if(is.null(y_variable)) {
    y_variable <- setdiff(names(df), "datetime")
  }

  # If x_range is NULL, set it to the full range of datetime
  if(is.null(x_range)) {
    x_range <- range(df$datetime, na.rm = TRUE)
  }

  # Convert start_time and end_time to POSIXct if not NULL
  if (!is.null(start_time)) {
    start_time <- as.POSIXct(start_time)
  }

  if (!is.null(end_time)) {
    end_time <- as.POSIXct(end_time)
  }

  if (!is.null(buffer_time)) {
    buffer_time <- as.POSIXct(buffer_time)
  }
  # Convert y_variable columns to numeric and ignore any conversion errors
  df <- df %>%
    mutate(across(all_of(y_variable), ~as.numeric(as.character(.)), .names = "{.col}"))

  # Convert dataframe from wide to long format
  # Filter data based on x_range
  df_long <- df %>%
    pivot_longer(all_of(y_variable), names_to = "variable", values_to = "values") %>%
    dplyr::filter(datetime >= x_range[1], datetime <= x_range[2])

  # Reorder dataframe by layer_order if not NULL
  if (!is.null(layer_order)) {
    df_long <- df_long %>%
      mutate(variable = factor(variable, levels = layer_order)) %>%
      arrange(variable)
  }

  # Aggregate data based on time_resolution
  df_long <- df_long %>%
    group_by(variable, datetime = floor_date(datetime, time_resolution)) %>%
    summarise(values = mean(values, na.rm = TRUE), .groups = "drop")

  # Highlight certain variables if specified
  df_long$highlight <- ifelse(df_long$variable %in% highlight, "Treat", "Control")

  # Ensure that the highlighted lines are plotted on top
  df_long <- df_long %>%
    arrange(desc(highlight))

  # Plot
  p <- ggplot(df_long, aes(x = datetime, y = values, group = variable)) +
    geom_line(aes(linetype = highlight, size = highlight, color = highlight)) +
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab, color = "Variable") +
    scale_color_manual(values = c("Control" = "grey", "Treat" = "red")) +
    scale_linetype_manual(values = c("Control" = "solid", "Treat" = "solid")) +
    scale_size_manual(values = c("Control" = 1, "Treat" = 1)) +
    theme_bw() +
    theme(legend.title = element_text(size = 12 * text_size),
          legend.text = element_text(size = 10 * text_size),
          plot.title = element_text(size = 14 * text_size, face = "bold"),
          plot.subtitle = element_text(size = 12 * text_size),
          axis.text = element_text(size = 10 * text_size),
          axis.title = element_text(size = 10 * text_size),
          legend.position = legend.pos,
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
    guides(size = "none", linetype = "none")

  # Use line_colors if provided
  if (!is.null(line_colors)) {
    p <- p + scale_color_manual(values = line_colors)
  }

  # Check if ylim is provided
  # Apply ylim to the plot
  if(!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }

  if (!is.null(start_time))  p <- p + geom_vline(xintercept = as.numeric(start_time), linetype = "dashed", color = "red2", size = line_size)

  if (!is.null(end_time))    p <- p + geom_vline(xintercept = as.numeric(end_time), linetype = "dashed", color = "red2", size = line_size)

  if (!is.null(buffer_time)) p <- p + geom_vline(xintercept = as.numeric(buffer_time), linetype = "dashed", color = "blue2", size = line_size)

  # If plotly option is enabled, convert ggplot to plotly
  if (plotly) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      install.packages("plotly")
    }
    p <- plotly::ggplotly(p)
  }

  print(p)
  invisible(p)
}
