#' Multi-Resolution Time Series Plot
#'
#' This function plots a time series based on provided data, offering multiple visual and analytical features.
#'
#' @param df A data frame containing the data to be plotted, including a datetime column.
#' @param y_variable Character vector specifying which columns in `df` are to be used as y-variables.
#'        If NULL, all columns except 'datetime' will be used. Default is NULL.
#' @param highlight Character vector specifying which y-variables should be highlighted. Default is NULL.
#' @param x_range A vector of two datetimes specifying the start and end of the x-axis.
#'        Default is the range of the datetime column.
#' @param start_times A vector of start times to indicate special periods in the plot. Default is NULL.
#' @param end_times A vector of end times corresponding to `start_times` to indicate special periods in the plot. Default is NULL.
#' @param time_resolution Character string indicating the granularity for data aggregation. Default is "1 hour".
#' @param se Logical indicating if the standard error bands should be plotted around the smooth line. Default is FALSE.
#' @param plotly Logical indicating if the output should be a plotly plot. Default is FALSE.
#' @param span Numeric controlling the amount of smoothing for loess method. Default is 0.75.
#' @param ylab Character string for y-axis label. Default is "Value".
#' @param xlab Character string for x-axis label. Default is "Datetime".
#' @param title Character string for the main plot title. Default is "Timeseries of treatment and control elements".
#' @param subtitle Character string for the plot subtitle. Default is an empty string.
#' @param legend Position of the legend. Default is "right".
#' @param text_size Numeric controlling the size of text elements in the plot. Default is 1.0.
#' @param line_size Numeric controlling the size of lines in the plot. Default is 1.0.
#' @param color Color for the lines. Default is "gray90".
#' @param ylim Numeric vector of length 2 indicating y-axis limits. Default is NULL.
#' @param line_colors Vector of colors for lines if custom colors are required. Default is NULL.
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function takes in a dataframe with a datetime column and plots it as a time series.
#' Users can specify multiple y-variables, highlight certain variables, indicate special periods with `start_times` and `end_times`,
#' aggregate data at different time resolutions, and add smooth curves with optional standard error bands.
#' The resulting plot can be either a ggplot or a plotly interactive plot.
#'
#' @return A ggplot or plotly object, invisibly.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   datetime = seq(from = as.POSIXct("2020-01-01 00:00"), to = as.POSIXct("2020-01-10 23:00"), by = "hour"),
#'   variable1 = rnorm(240),
#'   variable2 = rnorm(240)
#' )
#'
#' plot_mlscm(df, y_variable = c("variable1", "variable2"), highlight = "variable1", start_times = c("2020-01-03 00:00"), end_times = c("2020-01-07 23:00"))
#' }
#'
#' @export
#'
plot_mlscm <- function(df,
                              y_variable = NULL,
                              highlight = NULL,
                              x_range = range(df$datetime, na.rm = TRUE),
                              start_times = NULL,
                              end_times = NULL,
                              time_resolution = "1 hour",
                              se = FALSE,
                              plotly = F,
                              span = 0.75,
                              ylab = "Value",
                              xlab = "Datetime",
                              title = "Timeseries of treatment and control elements",
                              subtitle = "",
                              legend = "right",
                              text_size = 1.0,
                              line_size = 1.0,
                              color = "gray90",
                              ylim = NULL,
                              line_colors = NULL,
                              ...) {
  #'
  # Check y_variable input
  if(is.null(y_variable)) y_variable <- setdiff(names(df), "datetime") else y_variable <- setdiff(y_variable, "datetime")
  # If x_range is NULL, set it to the full range of datetime
  if(is.null(x_range)) x_range <- range(df$datetime, na.rm = TRUE)

  # Convert start_times and end_times to POSIXct if not NULL
  start_times <- if(!is.null(start_times)) as.POSIXct(start_times) else NULL
  end_times <- if(!is.null(end_times)) as.POSIXct(end_times) else NULL

  # Data preprocessing
  # Convert dataframe from wide to long format
  df <- df %>%
    mutate(across(all_of(!!y_variable), function(x) ifelse(is.na(as.numeric(as.character(x))), NA, as.numeric(as.character(x)))))

  df_long <- df %>%
    pivot_longer(all_of(y_variable), names_to = "variable", values_to = "values") %>%
    dplyr::filter(datetime >= x_range[1], datetime <= x_range[2])  # Filter data based on start and end times

  # Aggregate data based on resolution
  df_long <- df_long %>%
    group_by(variable, datetime = floor_date(datetime, time_resolution)) %>%
    summarise(values = mean(values, na.rm = TRUE), .groups = "drop")

  # Highlight certain variables if specified
  df_long$highlight <- ifelse(df_long$variable %in% highlight, TRUE, FALSE)

  # Plot
  p <- ggplot(df_long, aes(x = datetime, y = values)) +
    geom_line(aes(linetype = highlight, size = highlight), color = color)

  if(!is.null(start_times)){
  # Add a smoothed line for each segment
  for (i in seq_along(start_times)) {
    # Subset the data for the i-th segment
    if (i == 1) {
      df_segment <- df_long[df_long$datetime < start_times[i], ]
      df_segment_after <- df_long[df_long$datetime > end_times[i], ]
    } else if (i == length(start_times)) {
      df_segment <- df_long[df_long$datetime > end_times[i - 1] & df_long$datetime < start_times[i], ]
    } else {
      df_segment <- df_long[df_long$datetime > end_times[i - 1] & df_long$datetime < start_times[i], ]
    }
  }

  if (isTRUE(se)) {
      # Add a smoothed line for the i-th segment
    p <- p + geom_smooth(data = df_segment, aes(x = datetime, y = values), method = "loess", span = span, se = se, color = "dodgerblue4", fill = "dodgerblue1")

    # Subset the data for the i-th period
    df_period <- df_long[df_long$datetime >= start_times[i] & df_long$datetime <= end_times[i], ]

    # Add a smoothed line for the i-th period
    p <- p + geom_smooth(data = df_period, aes(x = datetime, y = values), method = "loess", se = se, span = span, color = "tomato4", fill = "tomato1")

    if (!is.null(df_segment_after)) {
      p <- p + geom_smooth(data = df_segment_after, aes(x = datetime, y = values), method = "loess", se = se, span = span, color = "dodgerblue4", fill = "dodgerblue1")
    }
  }

  p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = line_size)

  p <- p +
    # Add shaded region and vertical lines at start_time and end_time
    annotate("rect", xmin = start_times, xmax = end_times, ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_vline(xintercept = as.numeric(c(start_times, end_times)), linetype = "dashed", color = "red2", size = line_size)
  }

  p <- p +

    # Add labels for the x and y axes and the color legend
    labs(
      title = title,  # Add a main title for the plot
      subtitle = subtitle,
      x = xlab,  # Label for the x-axis
      y = ylab,  # Label for the y-axis
    ) +

    # Manually set line types and sizes
    scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "solid")) +  # Set the line type based on the 'highlight' variable
    scale_size_manual(values = c("FALSE" = 1, "TRUE" = 2)) +  # Set the line size based on the 'highlight' variable

    # Set the theme to 'theme_bw' for a clean, academic aesthetic
    theme_bw() +

    # Customize the legend
    theme(
      legend.title = element_text(size = 12 * text_size),  # Set the size of the legend title
      legend.text = element_text(size = 10 * text_size),  # Set the size of the legend text
      plot.title = element_text(size = 14 * text_size, face = "bold"),  # Set the size and style of the main plot title
      plot.subtitle = element_text(size = 12 * text_size),  # Set the size of the subtitle
      axis.text = element_text(size = 10 * text_size),  # Control size of axis text
      axis.title = element_text(size = 10 * text_size),  # Control size of axis titles (labels)
      legend.position = legend,  # Control the position of the legend
      axis.title.x = element_text(margin = margin(t = 15)),  # Adjust vertical position of x-axis label
      axis.title.y = element_text(margin = margin(r = 5)),    # Adjust vertical position of y-axis label
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    ) +

    # To not display size and line type in the legend
    guides(size = "none", linetype = "none")

  # Use line_colors if provided
  if (!is.null(line_colors)) {
    p <- p + scale_color_manual(values = line_colors)
  }

  if(!is.null(ylim)) {      # Check if ylim is provided
    p <- p + coord_cartesian(ylim = ylim)   # Apply ylim to the plot
  }

  # If plotly option is turned on
  if (plotly) {
    if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
    library(plotly)
    p <- ggplotly(p)
  }

  print(p)

  invisible(p)
}
