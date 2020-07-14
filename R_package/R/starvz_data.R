#' @useDynLib starvz

# For packages that we use a lot of functions:
#' @import dplyr
#' @import ggplot2

# For packages that we use a small set of function a lot of times:
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#' @importFrom pipeR %>>%
#' @importFrom magrittr set_colnames
#' @importFrom rlang .data
#' @importFrom lpSolve lp
#' @importFrom logging loginfo logwarn logerror basicConfig addHandler removeHandler writeToConsole
#' @importFrom tibble rowid_to_column as.tibble enframe
#' @importFrom utils head tail modifyList
#' @importFrom tidyr pivot_longer pivot_wider complete separate gather nest unnest expand replace_na
#' @importFrom readr read_csv cols col_integer col_character col_double col_logical
#' @importFrom data.tree as.Node Prune Set
#' @importFrom stats na.omit complete.cases setNames sd quantile lm resid
#' @importFrom arrow arrow_available read_feather read_parquet write_feather write_parquet codec_is_available ParquetWriterProperties
#' @importFrom stringr str_replace str_replace_all str_to_title
#' @importFrom purrr list_modify map
#' @importFrom grDevices colorRampPalette
#' @importFrom methods is
#' @importFrom patchwork wrap_plots
#' @importFrom gtools mixedorder mixedsort
#' @importFrom RColorBrewer brewer.pal
#' @importFrom zoo na.locf
#' @importFrom car outlierTest

# For functions that we use a small number of times use package::function
utils::globalVariables(c("."))

# For some rare cases of internal global variables
pkg.env <- new.env()

# This fixes some problems on recent versions of tidyverse
# Check: https://github.com/tidyverse/tidyr/issues/751
# Check: https://github.com/tidyverse/tidyr/issues/694
if (exists("unnest_legacy")) {
  unnest <- unnest_legacy
}

# This follows:
# https://www.datamentor.io/r-programming/s3-class/
# https://adv-r.hadley.nz/s3.html#s3-classes
new_starvz_data <- function(data = list()) {
  structure(data, class = "starvz_data")
}

validate_starvz_data <- function(data) {
  data_list <- unclass(data)
  if (!is.list(data_list)) {
    stop(
      "Invalid starvz_data",
      call. = FALSE
    )
  }
  data
}

starvz_data <- function(data = list()) {
  validate_starvz_data(new_starvz_data(data))
}

summary.starvz_data <- function(x) {
  cat("StarVZ data\n")
  cat("Elements:\n")
  cat(paste0("\t", names(x), "\n"))
}

print.starvz_data <- function(x) {
  cat("TODO")
}
