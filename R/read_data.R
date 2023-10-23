#' Read Data From Specified Directory and Process It
#'
#' This function reads multiple data files from a specified directory, processes them,
#' and optionally writes the processed data to a CSV file. The function supports both
#' CSV and XLSX file formats.
#'
#' @param params A named list containing various parameters:
#'   - `data_dir`: The directory containing data files.
#'   - `file_pattern`: A regex pattern to match specific files within the directory.
#'   - `datetime_format`: The format of the datetime column in the files.
#'   - `data_timerange`: A vector of two Date elements specifying the range of dates to filter the data.
#'   - `dependent_variable`: The name of the dependent variable in the dataset.
#'   - `wenormed`: A logical value indicating whether to modify the dependent variable name with a "_wn" suffix.
#'
#' @param write_out A logical. If TRUE, writes the processed data to a file named "df_origin.csv".
#' Defaults to FALSE.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' The function first gets a list of filenames matching the given pattern in the directory.
#' It then reads each file, processes the datetime column, filters rows based on the given date range,
#' adds additional date-related columns, and ensures that the dependent variable is numeric.
#' If the `wenormed` parameter is TRUE, it modifies the dependent variable name by adding a "_wn" suffix.
#' Finally, it optionally writes the processed data to a CSV file if `write_out` is TRUE.
#'
#' @return A list containing:
#'   - `list_df`: A named list of data frames, where each data frame corresponds to a file and
#'     the name is the filename without the extension.
#'   - `df_origin`: A combined data frame derived from `list_df`.
#'
#' @author [Yuqing Dai]
#'
#' @examples
#' \dontrun{
#' params <- list(
#'     data_dir = "path/to/data/",
#'     file_pattern = "*.csv",
#'     datetime_format = "%Y-%m-%d %H:%M:%S",
#'     data_timerange = as.Date(c("2022-01-01", "2022-12-31")),
#'     dependent_variable = "target",
#'     deweathered = FALSE
#' )
#'
#' result <- read_data(params)
#' head(result$df_origin)
#' }
#'
#' @export
#'
read_data <- function(params,
                         write_out = FALSE,
                         ...) {

  # Correct directory separator
  params$data_dir <- gsub("\\\\", "/", params$data_dir)

  # Get list of files matching the pattern
  filenames <- list.files(
    path = params$data_dir,
    pattern = params$file_pattern,
    full.names = TRUE
  )

  require(readxl)

  list_df <- setNames(lapply(filenames, function(x) {
    extension <- tools::file_ext(x)
    if (extension == "csv") {
      df <- read_csv(x)
    } else if (extension == "xlsx") {
      df <- read_excel(x)
    } else {
      stop("Unsupported file type")
    }

    df$datetime <- as.POSIXct(df$datetime, format = params$datetime_format)
    df <- df[as.Date(df$datetime) >= as.Date(params$data_timerange[1]) & as.Date(df$datetime) <= as.Date(params$data_timerange[2]), ]
    df <- df[complete.cases(df$datetime), ]
    df <- add_date_cols(df, date_col = datetime, stats = "all")
    df
  }), tools::file_path_sans_ext(basename(filenames)))

  # If 'wenormed', modify dependent variable name
  if (params$wenormed) {
    params$dependent_variable <- paste0(params$dependent_variable, "_wn")
  }

  # Filter out data frames where dependent variable is entirely NA
  list_df <- list_df[!sapply(list_df, function(df) {
    all(is.na(df[[params$dependent_variable]]))
  })]

  # Ensure dependent variable is numeric
  list_df <- lapply(list_df, function(df) {
    df[[params$dependent_variable]] <- as.numeric(df[[params$dependent_variable]])
    return(df)
  })

  list_df2 <- list2df_xyselect(list_df, colnames = params$dependent_variable)

  # Write output if required
  if (write_out) {
    write.csv(list_df2, "df_origin.csv")
  }

  return(list(list_df = list_df, df_origin = list_df2))
}
