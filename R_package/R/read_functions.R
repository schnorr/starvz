#' @include starvz_data.R

starvz_read_some_feather <- function(directory = ".", tables = c("application")) {
  l1 <- list(Origin = directory)
  l2 <- lapply(tables, function(table) {
    table_file <- file.path(directory, paste0(table, ".feather"))
    if (file.exists(table_file)) {
      read_parquet(table_file)
    } else {
      loginfo(paste("The file:", table_file, " does not exist on that directory. Ignore."))
      NULL
    }
  })
  names(l2) <- tables %>% str_to_title()
  c(l1, l2)
}

starvz_read_feather <- function(directory = ".") {
  filenames <- list.files(
    path = directory,
    pattern = "*.feather",
    full.names = FALSE,
    recursive = FALSE
  ) %>%
    str_replace_all(".feather", "")

  starvz_read_some_feather(directory = directory, tables = filenames)
}

# retrocompatibility
the_fast_reader_function <- starvz_read_feather

starvz_read_some_parquet <- function(directory = ".", tables = c("application")) {
  if (!codec_is_available("gzip")) {
    logwarn("Arrow Gzip is not available, try using arrow::install_arrow()")
  }
  l1 <- list(Origin = directory)
  l2 <- lapply(tables, function(table) {
    table_file <- file.path(directory, paste0(table, ".parquet"))
    if (file.exists(table_file)) {
      read_parquet(table_file)
    } else {
      loginfo(paste("The file:", table_file, " does not exist on that directory. Ignore."))
      NULL
    }
  })
  names(l2) <- tables %>% str_to_title()
  c(l1, l2)
}

starvz_read_parquet <- function(directory = ".") {
  filenames <- list.files(
    path = directory,
    pattern = "*.parquet",
    full.names = FALSE,
    recursive = FALSE
  ) %>%
    str_replace_all(".parquet", "")

  starvz_read_some_parquet(directory = directory, tables = filenames)
}

#' Read selective starvz trace files
#'
#' Read the directory of trace files (feather or parquet) and
#' the configfuration file, and return a starvz_data class with only
#' data that will be used in active plots in cofiguration file
#'
#' @param directory Directory path of trace files
#' @param config_file Path for configuration yaml file
#' @return The starvz_data with all tables
#' @examples
#' starvz_selective_read("folder_to_parquet_files/")
#' starvz_selective_read(directory = "folder_to_parquet_files/", config_file = "path_to_config.yaml")
#' starvz_selective_read() # Read current directory
#' @export
starvz_selective_read <- function(directory = ".", config_file = NULL) {
  if (is.null(config_file)) {
    config_file <- file.path(directory, "config.yaml")
  }
  data <- list()
  data$config <- starvz_read_config(config_file)

  # Always try to load Version, Colors and Y
  tables_to_load <- c("version", "colors", "y")

  if (data$config$st$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$st$tasks$active) {
    tables_to_load <- c(tables_to_load, "gaps", "starpu", "application", "link")
  }

  if (data$config$st$cpb || data$config$st$cpb_mpi$active) {
    tables_to_load <- c(tables_to_load, "dag", "starpu", "application", "link")
  }

  if (data$config$pmtool$bounds$active) {
    tables_to_load <- c(tables_to_load, "application", "pmtool")
  }

  if (data$config$atree$active) {
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if (data$config$utiltreenode$active) {
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if (data$config$utiltreedepth$active) {
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if (data$config$activenodes$active) {
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if (data$config$computingnodes$active) {
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if (data$config$kiteration$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$summary_nodes$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$pmtool$state$active) {
    tables_to_load <- c(tables_to_load, "application", "pmtool_states")
  }

  if (data$config$pmtool$kiteration$active) {
    tables_to_load <- c(tables_to_load, "application", "pmtool_states")
  }

  if (data$config$memory$state$active) {
    tables_to_load <- c(tables_to_load, "application", "tasks", "task_handles", "data_handles", "memory_state", "events_memory", "link")
  }

  if (data$config$memory$transfers$active) {
    tables_to_load <- c(tables_to_load, "application", "tasks", "task_handles", "data_handles", "memory_state", "events_memory", "link")
  }

  if (data$config$starpu$active) {
    tables_to_load <- c(tables_to_load, "starpu")
  }

  if (data$config$ready$active) {
    tables_to_load <- c(tables_to_load, "variable")
  }

  if (data$config$usedmemory$active) {
    tables_to_load <- c(tables_to_load, "variable")
  }

  if (data$config$submitted$active) {
    tables_to_load <- c(tables_to_load, "variable")
  }

  if (data$config$lackready$active) {
    tables_to_load <- c(tables_to_load, "starpu", "variable")
  }

  if (data$config$imbalance$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$power_imbalance$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$hete_imbalance$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$utilheatmap$active) {
    tables_to_load <- c(tables_to_load, "application")
  }

  if (data$config$mpibandwidth$active) {
    tables_to_load <- c(tables_to_load, "variable")
  }

  if (data$config$mpiconcurrent$active) {
    tables_to_load <- c(tables_to_load, "link")
  }

  if (data$config$mpiconcurrentout$active) {
    tables_to_load <- c(tables_to_load, "link")
  }

  if (data$config$mpistate$active) {
    tables_to_load <- c(tables_to_load, "comm_state")
  }

  if (data$config$gflops$active) {
    tables_to_load <- c(tables_to_load, "variable")
  }

  if (data$config$gpubandwidth$active) {
    tables_to_load <- c(tables_to_load, "variable")
  }

  tables_to_load <- tables_to_load %>% unique()
  loginfo(paste("Read:", paste(tables_to_load, collapse = " ")))
  starvz_read_some(directory = directory, tables = tables_to_load, config_file)
}

starvz_read_some <- function(directory = ".", tables = c("application"), config_file = NULL) {
  check_arrow()
  # Check if there is arrow gz files
  filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = FALSE)
  if (length(filenames) > 0) {
    loginfo("Detected parquet files")
    data <- starvz_read_some_parquet(directory, tables = tables)
  } else {
    data <- starvz_read_some_feather(directory, tables = tables)
  }

  if (is.null(config_file)) {
    config_file <- file.path(directory, "config.yaml")
  }
  data$config <- starvz_read_config(config_file)

  final_data <- starvz_data(data)

  return(final_data)
}

#' Read starvz trace files
#'
#' Read the directory of trace files (feather or parquet) and
#' the configfuration file, and return a starvz_data class used
#' in starvz functions
#'
#' @param directory Directory path of trace files
#' @param config_file Path for configuration yaml file
#' @return The starvz_data with all tables
#' @examples
#' starvz_read("folder_to_parquet_files/")
#' starvz_read(directory = "folder_to_parquet_files/", config_file = "path_to_config.yaml")
#' starvz_read() # Read current directory
#' @export
starvz_read <- function(directory = ".", config_file = NULL) {
  check_arrow()
  # Check if there is arrow gz files
  filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = FALSE)
  if (length(filenames) > 0) {
    loginfo("Detected parquet files")
    data <- starvz_read_parquet(directory)
  } else {
    data <- starvz_read_feather(directory)
  }

  if (is.null(config_file)) {
    config_file <- file.path(directory, "config.yaml")
  }
  data$config <- starvz_read_config(config_file)

  final_data <- starvz_data(data)

  return(final_data)
}
