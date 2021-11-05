#' @useDynLib starvz
# For packages that we use a lot of functions:
#' @import dplyr
#' @import ggplot2
#' @import Rcpp
# For packages that we use a small set of function a lot of times:
#' @importFrom grDevices colorRampPalette
#' @importFrom methods is
#' @importFrom utils head tail modifyList
#' @importFrom stats na.omit complete.cases setNames sd quantile lm resid predict
#' @importFrom magrittr %>% set_colnames
#' @importFrom rlang .data
#' @importFrom tibble rowid_to_column as.tibble enframe add_column
#' @importFrom tidyr pivot_longer pivot_wider complete separate gather nest unnest expand replace_na spread
#' @importFrom readr read_csv cols col_integer col_character col_double col_logical
#' @importFrom stringr str_replace str_replace_all str_to_title
#' @importFrom purrr list_modify map map2
#' @importFrom patchwork wrap_plots
#' @importFrom arrow arrow_available read_feather read_parquet write_feather write_parquet codec_is_available ParquetWriterProperties
#' @importFrom lpSolve lp
#' @importFrom data.tree as.Node Prune Set
#' @importFrom gtools mixedorder mixedsort
#' @importFrom RColorBrewer brewer.pal
#' @importFrom zoo na.locf
NULL

# Ignore .
utils::globalVariables(c("."))

# For some rare cases of internal global variables
pkg.env <- new.env()

# Option for DEBUG
pkg.env$log <- FALSE

#' Active internal debug logs
#'
#' Active internal debug logs
#' @param state Active or not logs
#' @return Nothing
#' @examples
#' starvz_set_log(FALSE)
#' @export
starvz_set_log <- function(state) {
  pkg.env$log <- state
}

starvz_suppressWarnings <- function(exp) {
  if (pkg.env$log) {
    exp
  } else {
    suppressWarnings(exp)
  }
}

starvz_log <- function(msg) {
  if (pkg.env$log) {
    cat(paste0(format(Sys.time(), "%X"), " ", msg, "\n"))
  }
}

starvz_warn <- function(msg) {
  cat(paste0(format(Sys.time(), "%X"), " WARNING: ", msg, "\n"))
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

#' Print starvz_data
#'
#' Print starvz_data
#' @param x A starvz_data
#' @param ... optional
#' @return Nothing
#' @examples
#' print(starvz_sample_lu)
#' @export
print.starvz_data <- function(x, ...) {
  cat("StarVZ data\n")
  cat("Execution Makespan: ")
  cat(statistics_makespan(x))
  cat("\nElements: ")
  cat(paste(names(x), collapse = ", "))
  cat("\n")
  class(x) <- "list"
  print(x)
}

#' Summary starvz_data
#'
#' Summary starvz_data
#' @param object A starvz_data
#' @param ... optional
#' @return Nothing
#' @examples
#' summary(starvz_sample_lu)
#' @export
summary.starvz_data <- function(object, ...) {
  cat("StarVZ data\n")
  cat("Execution Makespan: ")
  cat(statistics_makespan(object))
  cat("\nNumber of Application Tasks: ")
  cat(statistics_total_tasks(object))
  cat("\nTypes of Application Tasks: ")
  cat(statistics_total_tasks_types(object))
  cat("\nIdleness during Application execution: ")
  cat(statistics_total_idleness(object))
  cat(" %\nTotal Number of Resources: ")
  cat(statistics_total_resources(object))
  cat("\nNodes: ")
  cat(statistics_total_nodes(object))
  cat("\nGPU: ")
  cat(statistics_total_gpus(object))
  cat("\nCPU: ")
  cat(statistics_total_cpus(object))
  cat("\n")
}

#' Plot starvz_data
#'
#' Plot starvz_data
#' @param x A starvz_data
#' @param ... optional
#' @return Nothing
#' @examples
#' \donttest{
#' plot(starvz_sample_lu)
#' }
#' @export
plot.starvz_data <- function(x, ...) {
  return(starvz_plot(x))
}


#' Check if all required data is available
#'
#' The following conditions are check in order and return FALSE if any fail
#' - If data is not NULL
#' - If data is a StarVZ Class
#' - If data has all tables (given by the names of the list tables)
#' - If each respective table has all columns (given the associated vector)
#' - Execute extra_func on data (that should return true or false)
#' @param data starvz_data with trace data
#' @param tables A named list (names are tables of data) of vectors (elements
#' are columns), if tables is null continue
#' @param extra_func Extra function to be applied on data to do a last check
#' @return Logical, TRUE if data pass all tests
#' @usage starvz_check_data(data = NULL,
#'              tables = list(), extra_func = NULL)
#' @include starvz_data.R
#' @examples
#' starvz_check_data(starvz_sample_lu,
#'                   tables = list("Comm_state" = c("Node")))
#' @export
starvz_check_data <- function(data = NULL,
                              tables = list(),
                              extra_func = NULL) {
  caller <- "starvz_check_data:"
  if (sys.nframe() > 1) {
    caller <- paste0("", deparse(sys.calls()[[sys.nframe() - 1]]), ":")
  }
  if (is.null(data)) stop(paste(caller, "data is NULL"), call. = FALSE)
  if (class(data) != "starvz_data") stop(paste(caller, "data is not starvz_data"), call. = FALSE)
  if (!is.null(tables)) {
    if (!is.list(tables)) {
      stop(paste(caller, "tables is not a list"), call. = FALSE)
    }
    if (!is.null(names(tables))) {
      if (!all(names(tables) %in% names(data))) {
        stop(paste(caller, "Missing Table:", names(tables)[!names(tables) %in% names(data)]), call. = FALSE)
      }

      for (table in names(tables)) {
        cols <- tables[[table]]
        nrows <- data[[table]] %>% nrow()
        if (is.null(nrows) || nrows == 0) {
          stop(paste(caller, "Table is empty"), call. = FALSE)
        }
        if (is.vector(cols)) {
          if (!all(cols %in% names(data[[table]]))) {
            stop(paste(caller, "Missing Column:", cols[!cols %in% names(data[[table]])]), call. = FALSE)
          }
        } else {
          # Cols is not a vector, continue?
        }
      }
    }
    if (!is.null(extra_func) && !extra_func(data)) {
      stop(paste(caller, "Error on extra_func"), call. = FALSE)
    }
  }
  return(TRUE)
}

#' Small StarVZ data of LU Factorization
#'
#' A small StarVZ data object obtained from Chameleon+StarPU LU Factorization
#' Generated by:\preformatted{
#' library(starvz)
#' pre_phase1 <- starvz_phase1(system.file("extdata", "lu_trace",
#'                                         package = "starvz"),
#'                             lu_colors,
#'                             state_filter=2,
#'                             whichApplication="lu")
#' starvz_sample_lu <- starvz_read(system.file("extdata",
#'                                             "lu_trace",
#'                                             package = "starvz"),
#'                                 system.file("extdata",
#'                                             "config.yaml",
#'                                             package = "starvz"),
#'                                 selective=FALSE)
#' usethis::use_data(starvz_sample_lu)
#' }
#'
#' @source starvz_phase1 and starvz_read
"starvz_sample_lu"
