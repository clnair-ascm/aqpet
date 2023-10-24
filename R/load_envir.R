#' Load Environment for Analysis
#'
#' This function installs and loads the necessary packages.
#' It provides the user with an option to select their preferred CRAN mirror for package installations.
#'
#' @param mirror_url A character string specifying the preferred CRAN mirror URL.
#'
#' @author [Yuqing Dai, Chengxu Tong]
#'
#' @examples
#' \dontrun{
#' load_envir(mirror_url = "https://cran.ma.imperial.ac.uk/")
#' }
#'
#' @export
load_envir <- function(mirror_url = "https://cran.ma.imperial.ac.uk/") {

  #' Set the repository option
  options(repos = c(CRAN = mirror_url))
  options(timeout = 600)
  #'
  required_packages <- c(
    "bcp", "bsts",
    "changepoint", "devtools",
    "doParallel", "dplyr",
    "foreach", "h2o",
    "lubridate", "magrittr",
    "openair", "plot3Drgl",
    "purrr", "readxl",
    "rgl", "rsample",
    "tidymodels", "tidyverse",
    "zoo", "agua"
  )

  sapply(required_packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  })
  #' Install package augsynth for ASCM
  devtools::install_github("ebenmichael/augsynth")
  library(augsynth)
}
#'
load_envir()
