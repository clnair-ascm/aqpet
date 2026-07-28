#' Building Models Using WENORM
#'
#' This function processes and builds models using the `wenorm` method based on provided parameters.
#'
#' @param mylist A list of data frames to process.
#' @param params A list of parameters specifying details like predictor variables, output directory, wenorm method, etc.
#'
#'   v0.2.2 recognises three optional resampling fields on `params` (all back-compatible --
#'   leave them off and behaviour is identical to v0.2.1):
#'   \itemize{
#'     \item `params$resample_window` -- integer half-width in days of the day-of-year resampling
#'           window (default 14). Governs the `wenorm_method = "TuanVu"` window; accepted-but-unused
#'           by the other methods, so the knob is available regardless of method.
#'     \item `params$resample_pool` -- where the TuanVu resampler draws replacement weather from:
#'           \itemize{
#'             \item `"model"` (default) -- the modelling data itself (v0.2.1 behaviour).
#'             \item `"input"` -- the full per-site input BEFORE missing-response rows are dropped,
#'                   so one frame can carry a long meteorological record alongside a pollutant column
#'                   that is NA outside the study period.
#'             \item a data frame -- a separate meteorological record (e.g. a multi-year MET pool),
#'                   reused for every site. Must contain `datetime` plus every resampled weather column.
#'           }
#'   }
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
#'
#' # Wider window + a separate multi-year MET pool (Tong et al. 2025 style):
#' params$wenorm_method  <- "TuanVu"
#' params$resample_window <- 14
#' params$resample_pool   <- london_met_2000_2024   # data frame: datetime + weather cols
#' results <- buildMod(df_list, params)
#'
#' # Or carry the long met inside each site's own input (pollutant NA outside study period):
#' params$resample_pool <- "input"
#' results <- buildMod(df_list_with_long_met, params)
#' }
#'
#' @export
#'
buildMod <- function(mylist,
                     params,
                     start_index = 1,
                     end_index = NULL,
                     ...) {

  # --- validate the v0.2.2 resampling knobs ONCE, up front (clear early error
  #     instead of one buried inside the per-site tryCatch loop) ---
  if (!is.null(params$resample_window)) {
    rw <- params$resample_window
    if (!(is.numeric(rw) && length(rw) == 1 && rw > 0 && floor(rw) == rw)) {
      stop("params$resample_window must be a single positive integer (days).")
    }
  }
  if (!is.null(params$resample_pool)) {
    rp <- params$resample_pool
    if (is.data.frame(rp)) {
      resample_variables <- setdiff(params$predictor_variables,
                                    c(params$constant_variables, "datetime"))
      needed <- c("datetime", resample_variables)
      miss   <- setdiff(needed, names(rp))
      if (length(miss)) {
        stop("params$resample_pool data frame is missing required column(s): ",
             paste(miss, collapse = ", "))
      }
    } else if (!(is.character(rp) && length(rp) == 1 && rp %in% c("model", "input"))) {
      stop("params$resample_pool must be 'model', 'input', or a data.frame.")
    }
  }

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

          if (params$write_out) {
            output_data <- switch(
              params$wenorm_method,
              "aml"     = wenorm$aqmod,
              "default" = wenorm$df_wenorm,
              "revised" = wenorm$df_final,
              "TuanVu"  = wenorm$df_wenorm,
              wenorm$df_wenorm   # default fallback
            )

            params$out_dir <- gsub("\\\\", "/", params$out_dir)

            # Only write tabular weather-normalisation outputs
            if (params$wenorm_method != "aml") {
              write.csv(
                output_data,
                paste0(
                  params$out_dir,
                  names(df_new)[[i]], "_",
                  params$response_variable,
                  "_wn.csv"
                )
              )

              # df_final == df_wenorm for "TuanVu" -- skip the duplicate write
              if (params$wenorm_method != "TuanVu") {
                write.csv(
                  wenorm$df_final,
                  paste0(
                    params$out_dir,
                    names(df_new)[[i]], "_",
                    params$response_variable,
                    "_final.csv"
                  )
                )
              }
            }
          }

          df_list[[i]] <- switch(params$wenorm_method,
                                 "aml"     = wenorm$aqmod,
                                 "default" = wenorm$df_wenorm,
                                 "revised" = wenorm$df_final,
                                 "TuanVu"  = wenorm$df_wenorm,
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
