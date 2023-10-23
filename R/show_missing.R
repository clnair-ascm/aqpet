#' Visualize Missing Data in a Data Frame
#'
#' This function provides insights into the distribution and patterns of missing data in a
#' data frame. It supports creating histograms, shadow plots, and scatter plots.
#'
#' @param df A data frame for which missing data insights are to be generated.
#' @param column A character vector or "all" specifying which columns to consider.
#'               Default is "all".
#' @param color A color string for the histograms. Default is "darkorchid".
#' @param date A logical indicating if date columns should be added. Default is FALSE.
#' @param type A character string specifying the type of temporal aggregation
#'             (e.g., "year", "month"). Default is "NULL".
#' @param threshold A numeric threshold. Only columns with a missing percentage
#'                  above this threshold will be displayed. Default is 0.
#' @param sort_by A character string indicating the sorting variable for the histogram.
#'                Options are "percentage" or the column name. Default is "percentage".
#' @param show_shadow_plot A logical indicating if a shadow plot should be generated.
#'                         Default is FALSE.
#' @param date_col A character string specifying the date column.
#'                 Relevant if date is TRUE.
#' @param show_scatter_plot A logical indicating if a scatter plot should be generated.
#'                          Default is FALSE.
#' @param x_col A character string specifying the x-axis column for the scatter plot.
#' @param y_col A character string specifying the y-axis column for the scatter plot.
#' @param ... Further arguments passed to other methods.
#'
#' @details
#' The function provides various visualizations and summaries of missing data in a given data frame.
#' Users can request a histogram showing missing data percentages by column or by time (e.g., year, month).
#' Optionally, users can visualize missing data patterns using shadow plots and scatter plots.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `missing_data`: A data frame summarizing missing data by column or time.
#'   \item `missing_plot`: A ggplot object visualizing the missing data.
#'   \item `shadow_plot`: (Optional) A ggplot object visualizing the data's "shadow" or
#'                         absence/presence of data.
#'   \item `scatter_plot`: (Optional) A ggplot object showing a scatter plot of the data with missing values highlighted.
#' }
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' set.seed(12)
#'  df <- data.frame(
#'  datetime = seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-10 00:00:00"), by = "hour"),
#'  A = sample(c(0:100), size = 217, replace = TRUE),
#'  B = sample(c(0:100), size = 217, replace = TRUE),
#'  C = sample(c(0:100), size = 217, replace = TRUE))
#'
#' Randomly replace some values with NA
#'  df$A[sample(1:217, 50)] <- NA
#'  df$B[sample(1:217, 30)] <- NA
#'  df$C[sample(1:217, 20)] <- NA
#'  df <- add_date_cols(df, date_col = datetime, stats = "all")
#' Use the show_missing function to display the missing data distribution, shadow plot, and scatter plot
#' show_missing(df,
#'              column = "all",
#'              type = "NULL",
#'              show_shadow_plot = F,
#'              date_col = datetime,
#'              date = T,
#'              show_scatter_plot = T,
#'              x_col = "A",
#'              y_col = "B")
#'
#' @export
#'
show_missing <- function(df,
                         column = "all",
                         color = "darkorchid",
                         date = FALSE,
                         type = "NULL",
                         threshold = 0,
                         sort_by = "percentage",
                         show_shadow_plot = FALSE,
                         date_col = NULL,
                         show_scatter_plot = FALSE,
                         x_col = NULL,
                         y_col = NULL, ...) {

  # Load required packages
  require(tidyverse)
  require(lubridate)

  if (!is.data.frame(df)) {
    stop("Input should be a data frame.")
  }

  if (date) {
    df <- add_date_cols(df, date_col = datetime, stats = "all")
  }

  valid_type <- c("NULL", "year", "month", "hour", "dow")

  if (!is.null(type) && !type %in% valid_type) {
    stop(paste("Can't find the statistic(s), must be NULL, 'year', 'month', 'hour', or 'dow'.", type[!type %in% valid_type], "\n"))
  }

  # Initialize an empty list to store all outputs
  output_list <- list()

  df4plot <- df

  if (type == "NULL") {
    if (column == "all") {
      column <- names(df)[sapply(df, function(col) is.numeric(col) | is.POSIXt(col))]
    }

    df <- df[, column, drop = FALSE]

    missing_data <- df %>%
      gather("variable", "value") %>%
      mutate(is_missing = is.na(value)) %>%
      group_by(variable, is_missing) %>%
      summarize(n = n()) %>%
      mutate(percentage = round((n / sum(n) * 100), 2)) %>%
      dplyr::filter(is_missing) %>%
      arrange(!!sym(sort_by)) %>%
      filter(percentage >= threshold)

    if (nrow(missing_data) == 0) {
      cat("Congratulations! There is no missing data.")
      return(invisible(NULL))
    }

    missing_plot <- ggplot(missing_data, aes(x = reorder(variable, -percentage), y = percentage)) +
      geom_col(fill = color) +
      ylim(0, 100) +
      coord_flip() +
      labs(x = "Variable", y = "Percentage of Missing Data (%)", title = "Missing Data Distribution") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 22, face = "bold", margin = margin(b = 12)),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 12)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 12)),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
      )

    output_list$missing_data <- missing_data
    output_list$missing_plot <- missing_plot
  } else {
    if (column == "all") {
      column <- names(df)[sapply(df, function(col) is.numeric(col) | is.POSIXt(col))]
      column <- column[column != type]
    }

    missing_data <- df %>%
      group_by(!!sym(type)) %>%
      summarize(across(all_of(column), ~round((sum(100 * is.na(.)) / n()), 2), .names = "missing_perc_{.col}")) %>%
      arrange(!!sym(type))

    all_zero_columns <- colnames(select(missing_data, starts_with("missing_perc")))[colSums(select(missing_data, starts_with("missing_perc"))) == 0]

    if (length(all_zero_columns) > 0) {
      cat("Congratulations! There are no missing values within the following columns:\n")
      cat(str_replace_all(all_zero_columns, "missing_perc_", ""), sep = ", ")
      cat("\n")
    }

    if (all(colSums(select(missing_data, starts_with("missing_perc"))) == 0)) {
      stop(paste("Congrats! All values in the missing_data are zero!"))
    }

    non_zero_columns <- colnames(select(missing_data, starts_with("missing_perc")))[colSums(select(missing_data, starts_with("missing_perc"))) > 0]
    non_zero_columns <- c(type, non_zero_columns)
    missing_data <- missing_data[, non_zero_columns, drop = FALSE]

    plot_data <- missing_data %>%
      pivot_longer(cols = starts_with("missing_perc"),
                   names_to = "variable",
                   values_to = "percentage") %>%
      mutate(variable = str_replace(variable, "missing_perc_", ""))

    plot_data <- plot_data %>%
      group_by(variable) %>%
      dplyr::filter(sum(percentage) > 0) %>%
      dplyr::filter(percentage >= threshold)

    histogram_plot <- ggplot(plot_data, aes_string(x = type, y = "percentage", fill = "variable")) +
      geom_col() +
      ylim(0, 100) +
      labs(
        x = type,
        y = "Percentage of Missing Data (%)",
        title = paste("Missing Data Distribution by", type)
      ) +
      facet_wrap(~variable, scales = "free_y", nrow = 1) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 22, face = "bold", margin = margin(b = 12)),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 12)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 12)),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 18, face = "bold")
      )

    output_list$missing_plot <- histogram_plot
    output_list$missing_data <- missing_data
  }

  if (show_shadow_plot) {
    source("shadow_plot.R")
    output_list$shadow_plot <- shadow_plot(df4plot, "datetime", column = column, x_label = "Variables", y_label = "Observations",
                                           na_color = "red", non_na_color = "blue",
                                           na_label = "Missing Values", non_na_label = "Present Values")
  }

  if (show_scatter_plot) {
    source("missing_scatter_plot.R")
    if (is.null(x_col) || is.null(y_col)) {
      stop("x_col and y_col must be specified for the scatter plot.")
    }
    output_list$scatter_plot <- missing_scatter_plot(df4plot, x_col, y_col, color_scheme = c("black", "red"))
  }
  # Return the output_list containing all missing_data
  return(output_list)
}
