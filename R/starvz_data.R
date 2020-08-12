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

#' Check if all required data is available
#'
#' This is check in order and return FALSE if any fail
#' - If data is not NULL
#' - If data is a StarVZ Class
#' - If data has all tables (given by the names of the list tables)
#' - If each repective table has all columns (given the associated vector)
#' - Execute extra_func on data (that should return true or false)
#' @param data starvz_data with trace data
#' @param tables A named list (names are tables of data) of vectors (elements
#' are columns), if tables is null continue
#' @param extra_func Extra function to be applied on data to do a last check
#' @return Logical, TRUE if data pass all tests
#' @include starvz_data.R
#' @examples
#' #starvz_check_data(data, list("MemoryState" = c("x") ))
#' @export
starvz_check_data <- function(data=NULL, tables=list(), extra_func=NULL){
  caller <- paste0("", deparse(sys.calls()[[sys.nframe()-1]]), ":")
  if(is.null(data)) stop(paste(caller, "data is NULL"), call. = FALSE)
  if(class(data)!="starvz_data") stop(paste(caller, "data is not starvz_data"), call. = FALSE)
  if(!is.null(tables)){
    if(!is.list(tables)){
      stop(paste(caller, "tables is not a list"), call. = FALSE)
    }
    if(!is.null(names(tables))){
      if(!all(names(tables) %in% names(data))){
        stop(paste(caller, "Missing Table:", names(tables)[!names(tables) %in% names(data)]), call. = FALSE)
      }

      for(table in names(tables)){
        cols <- tables[[table]]
        nrows <- data[[table]] %>% nrow()
        if(is.null(nrows) || nrows == 0){
            stop(paste(caller, "Table is empty"), call. = FALSE)
        }
        if(is.vector(cols))
        {
            if(!all(cols %in% names(data[[table]]))){
                stop(paste(caller, "Missing Column:", cols[!cols %in% names(data[[table]])]), call. = FALSE)
            }
        }else{
          # Cols is not a vector, continue?
        }
      }
    }
    if(!is.null(extra_func) && !extra_func(data)){
      stop(paste(caller, "Error on extra_func"), call. = FALSE)
    }
  }
  return(TRUE)
}

#' Small StarVZ data of LU Factorization
#'
#' A small StarVZ data object obtained from Chameleon+StarPU LU Factorization
#'
#' @source Chameleon+StarPU
"starvz_sample_lu"