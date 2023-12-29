# New function to process files in a directory
rm_ex <- function(directory_path, column_name, perc = 0.025) {
  
  remove_extreme_values <- function(df, column_name) {
    df <- df[df[[column_name]] > 0, ]
    
    lower_bound <- quantile(df[[column_name]], perc, na.rm = TRUE)
    upper_bound <- quantile(df[[column_name]], 1 - perc, na.rm = TRUE)
    filtered_df <- df[df[[column_name]] > lower_bound & df[[column_name]] < upper_bound, ]
    return(filtered_df)
  }
  # Get the list of all files in the folder
  file_list <- list.files(directory_path, full.names = TRUE)
  
  # Loop through each file
  for(file in file_list) {
    # Read the file
    data_frame <- read_csv(file)
    
    # Apply the remove_extreme_values function
    cleaned_data <- remove_extreme_values(data_frame, column_name)
    
    # Create a new filename for the output
    output_file <- paste0("cleaned_", basename(file))
    
    # Write the cleaned data to a new file
    write_csv(cleaned_data, file.path(directory_path, output_file))
  }
}

# Usage example:
# rm_ex("D:\\wmzero\\5_sidework\\1_ruixin\\part1\\work3\\output\\pm10", "PM10_wn")
