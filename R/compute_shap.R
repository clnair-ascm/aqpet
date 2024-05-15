##
##
compute_shap <- function(model, data, top_n_features = 20) {
  # Get the number of rows in the data
  n_rows <- nrow(data)
  
  # Initialize an empty list to store SHAP values data frames
  shap_values_list <- vector("list", n_rows)
  
  # Loop through each row of the data to get SHAP values
  for (i in 1:n_rows) {
    # Get SHAP values for the current row
    shap_result <- h2o.shap_explain_row_plot(
      model, data, row_index = i, 
      top_n_features = top_n_features, 
      plot_type = "breakdown", 
      contribution_type = "both"
    )
    
    # Extract the data frame from the result and store it in the list
    shap_values_list[[i]] <- shap_result$data
  }
  
  # Combine all individual SHAP value data frames into one
  combined_df <- bind_rows(shap_values_list) %>%
    mutate(diff = end - start) %>%
    group_by(feature) %>%
    summarise(
      ave_diff = mean(diff)
    ) %>%
    as.data.frame()
  
  # Calculate the average start value where id == 1 across the entire dataset
  avg_start_id <- bind_rows(shap_values_list) %>%
    filter(id == 1) %>%
    summarise(avg_start_id = mean(start, na.rm = TRUE)) %>%
    pull(avg_start_id)
  
  avg_end_id <- bind_rows(shap_values_list) %>%
    filter(id == top_n_features) %>%
    summarise(avg_end_id = mean(end, na.rm = TRUE)) %>%
    pull(avg_end_id)
  
  # Add the average start value where id == 1 as a new column to the summary data frame
  combined_df$avg_start_id <- avg_start_id
  combined_df$avg_end_id <- avg_end_id
  
  # Return a list containing both the individual SHAP value data frames and the combined summary data frame
  results <- list(
    individual_shap_values = shap_values_list,
    combined_summary = combined_df
  )
  
  return(results)
}
