#' Building Models Using WENORM
#'
#' This function processes and builds models using the `wenorm` method based on provided parameters.
#'
#' @param mylist A list of data frames to process.
#' @param params A list of parameters specifying details like predictor variables, output directory, wenorm method, etc.
#' @param start_index The index at which to start processing the list of data frames. Default is 1.
#' @param end_index The index at which to end processing the list of data frames. If NULL, it defaults to the length of the input list.
#' @param ... Other arguments passed to the function.
#'
#' @return Returns a list containing processed data frames (list_df), processed data with XY select (data_wn), baseline data (base_line), and AQMOD data (aqmod).
#'
#' @importFrom purrr compact
#' @importFrom base tryCatch Sys.time
#'
#' @author [Yuqing Dai, Chengxu Tong]
#'
#' @examples
#' \dontrun{
#' # Example usage with dummy data and parameters:
#' results <- buildMod(df_list, params)
#' }
#'
#' @export
#'
buildMod <- function(mylist,
                     params,
                     start_index = 1,
                     end_index = NULL,
                     ...) {

  suppressWarnings({

    df_new  <- list_na_rm(mylist[[1]], col_names = params$predictor_variables)

    start_time <- Sys.time()

    df_list <- setNames(vector("list", length(df_new)), names(df_new))
    df_base <- setNames(vector("list", length(df_new)), names(df_new))
    df_detrend <- setNames(vector("list", length(df_new)), names(df_new))
    df_wenorm <- setNames(vector("list", length(df_new)), names(df_new))
    aqmod  <- setNames(vector("list", length(df_new)), names(df_new))
    df_df   <- data.frame()

    # Set the end_index based on df_new if it is NULL
    if(is.null(end_index)) {
      end_index <- length(df_new)
    }

    for(i in start_index:end_index) {
      tryCatch(
        {
          wenorm <- run_wenorm(df = df_new[[i]], model_params = params)

          if(params$write_out) {
            output_data <- switch(params$wenorm_method,
                                  "aml"     = wenorm$aqmod,
                                  "default" = wenorm$df_wenorm,
                                  "revised" = wenorm$df_final,
                                  wenorm$df_wenorm  # Default case if none of the above match
            )

            params$out_dir <- gsub("\\\\", "/", params$out_dir)

            write.csv(wenorm$df_wenorm, paste0(params$out_dir, names(df_new)[[i]], "_", params$response_variable, "_wn.csv"))
            write.csv(wenorm$df_final, paste0(params$out_dir, names(df_new)[[i]], "_", params$response_variable, "_final.csv"))
          }

          df_list[[i]] <- switch(params$wenorm_method,
                                 "aml"     = wenorm$aqmod,
                                 "default" = wenorm$df_wenorm,
                                 "revised" = wenorm$df_final,
                                 wenorm$df_wenorm  # Default case if none of the above match
          )

          aqmod[[i]] <- wenorm$aqmod
          df_detrend[[i]] <- wenorm$df_detrend
          df_wenorm[[i]] <- wenorm$df_wenorm
          df_base[[i]] <- wenorm$df_base
          elapsed <- Sys.time() - start_time

          print(paste("Elapsed time:", round(elapsed, 2), "seconds"))

        },
        error = function(e) {
          print(paste("An error occurred during iteration", i, ": ", e))
        }
      )
    }
    # Remove all NA from each dataframe in the list
    df_list <- purrr::compact(df_list)

    if(params$wenorm_method == "aml") {
      df_df <- NULL
    } else {
      df_df <- list2df_xyselect(df_list, colnames = paste0(params$response_variable, "_wn"))
    }

  })

  return(list(list_df      = df_list,
              data_final   = df_df,
              base_line    = df_base,
              wenorm_line  = df_wenorm,
              detrend_line = df_detrend,
              aqmod        = aqmod))
}
