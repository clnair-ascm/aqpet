#' @author \Yuqing \Dai
#'
#' Function to Calculate Common Model Evaluation Statistics
#'
#'The mod_explain function provides a convenient way to generate a variety of diagnostic output and statistics for a model. It uses the h2o library for model fitting and evaluation, and ggplot2 for creating visually appealing charts. The function can generate several different output, including residual analysis, variable importance, SHAP (SHapley Additive exPlanations) summary, SHAP explain row, partial dependence, interactive partial dependence, individual conditional expectation (ICE), learning curve, and two-feature partial dependence output.
#'
#' The arguments for the function are:
#'
#'   model: The model that you want to explain. This should be an object that represents a fitted model, for example, an h2o model.
#'
#' data: The data frame used for generating explanations. This can be your training or test dataset.
#'
#' output: The types of output you want to generate. This is a vector of character strings, where each string represents a different plot type. The default setting includes several types of output.
#'
#' stats: The statistical method used for evaluating the performance of the model. The default is "default".
#'
#' plot_type: The type of plot used for SHAP explain row plot. The default is "barplot".
#'
#' row_index: The row index to be explained by SHAP explain row plot. The default is NULL.
#'
#' column: The feature to be explained by partial dependence plot, interactive partial dependence plot, ICE plot, SHAP partial dependence plot, and two-feature partial dependence plot. The default is NULL.
#'
#' column2: The second feature to be explained by two-feature partial dependence plot. The default is NULL.
#'
#' ...: Additional parameters passed to other functions.
#'
#' The function then iterates over each type of plot specified in the output argument, checks if the relevant plot can be generated given the input parameters, and if so, generates the plot and stores it in a list. The function finally returns this list of generated output.
#'
#' The mod_explain function is quite versatile and can help you understand the predictions of your model in different ways. By looking at these output and statistics, you can get a better sense of which features are most important in your model, how these features affect predictions, and whether the model is under or overfitting the data.
#'
#' Note: Some output require additional parameters like column, column2, and row_index. These need to be provided explicitly when needed.
#'
#' This function also sources another R script (mod_stats.R). Make sure that the mod_stats.R file is available in your working directory, and it contains the mod_stats() function used for generating the performance statistics.
#'
#' @example
# 1. Load libraries
# library(h2o)
# # 2. Initialize H2O cluster
# h2o.init()
# # 3. Load Iris dataset
# data(iris)
# # 4. Convert data to H2OFrame
# iris_h2o <- as.h2o(iris)
#
# # 5. Split the data into train and test
# splits <- h2o.splitFrame(iris_h2o, ratios = 0.7, seed = 1234)
# train <- splits[[1]]
# test <- splits[[2]]
#
# # 6. Specify predictors and response
# predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
# response <- "Species"
#
# # Train the GBM model
# gbm_model <- h2o.gbm(x = predictors,
#                      y = response,
#                      training_frame = train,
#                      ntrees = 50,
#                      max_depth = 5,
#                      min_rows = 2,
#                      learn_rate = 0.2,
#                      seed = 1234)

# Use the function:
# mod_explain(gbm_model, test, output = c("ipdp"), column2 = c("Sepal.Width", "Petal.Length"), row_index = 10)
#'
mod_explain <- function(model, data,
                        output = c("residual_analysis", "varimp", "shap_summary",
                                  "shap_explain_row", "pdp",
                                  "ice", "ipdp", "pdp2",
                                  "learning_curve", "h2o_performance", "performance", "shap_pdp",
                                  "two_feature_pdp"),
                        stats = "default",
                        plot_type = "barplot",
                        row_index = NULL,
                        column = NULL,
                        column2 = NULL,
                        plot_stddev = T,
                        ...) {

  # Check if model and data are provided
  if (missing(model) || missing(data)) {
    stop("Both a model and data must be provided.")
  }

  # Load necessary libraries
  if (!require(h2o)) {
    install.packages("h2o")
  }

  # source("mod_stats.R")
  # Initialize an empty list to hold the output
  output_list <- list()

  # For each selected plot type
  for (plot in output) {
    switch(plot,
           "residual_analysis" = {
             output_list$residual_analysis <- h2o.residual_analysis_plot(model, data)
           },
           "varimp" = {
             varimp_df <- h2o.varimp(model)
             varimp_df$variable <- with(varimp_df, reorder(variable, scaled_importance))
             output_list$varimp <- ggplot(varimp_df, aes(x = variable, y = scaled_importance)) +
               geom_bar(stat = "identity", fill = "steelblue") +
               coord_flip() +
               labs(x = "Variable", y = "Importance") +
               theme_bw(base_size = 24) +
               theme(axis.text = element_text(hjust = 1))
           },
           "shap_summary" = {
             output_list$shap_summary <- h2o.shap_summary_plot(model, data)
           },
           "shap_explain_row" = {
             if (is.null(row_index)) {
               stop("A row index must be provided for SHAP explain row plot.")
             }
             output_list$shap_explain_row <- h2o.shap_explain_row_plot(model, data, row_index = row_index, plot_type = plot_type)
           },
           "pdp" = {
             if (is.null(column)) {
               stop("A column name must be provided for partial dependence plot.")
             }
             output_list$pdp <- h2o.pd_plot(model, data, column = column, row_index = row_index)
           },
           "pdp2" = {
             if (is.null(column)) {
               stop("A column name must be provided for partial dependence plot.")
             }
             output_list$pdp2 <- h2o.partialPlot(model, data, cols = column, plot_stddev = plot_stddev)
           },
           "ipdp" = {
             if (is.null(column)) {
               stop("A column name must be provided for interactive partial dependence plot.")
             }
             require(rgl)
             require(plot3Drgl)
             output_list$ipdp <- h2o.partialPlot(model, data, col_pairs_2dpdp = list(column2), row_index = row_index)
           },
           "ice" = {
             if (is.null(column)) {
               stop("A column name must be provided for individual conditional expectation plot.")
             }
             output_list$ice <- h2o.ice_plot(model, data, column = column)
           },
           "learning_curve" = {
             output_list$learning_curve <- h2o.learning_curve_plot(model)
           },
           "h2o_performance" = {
             output_list$h2o_performance <- h2o.performance(model, data)
           },
           "performance" = {
             eva_df <- cbind(as.data.frame(data), as.data.frame(h2o.predict(model, data)))
             output_list$performance <- mod_stats(eva_df, mod = "predict", obs = "y", stats = stats)
           },
           "shap_pdp" = {
             if (is.null(column)) {
               stop("A column name must be provided for SHAP partial dependence plot.")
             }
             output_list$shap_pdp <- h2o.shap_interaction_plot(model, data, column)
           },
           "two_feature_pdp" = {
             if (is.null(column) && is.null(column2)) {
               stop("Two column names must be provided for two-feature partial dependence plot.")
             }
             output_list$two_feature_pdp <- h2o.partialPlot(model, data, cols = column, col_pairs_2dpdp = list(column2))
           },
           {
             stop(paste("Unsupported plot type:", plot))
           }
    )
  }
  return(output_list)
}
