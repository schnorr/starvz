#' @include starvz_data.R

starvz_read_some_feather <- function(directory = ".", tables = c("application")) {
  l1 <- list(Origin = directory)
  l2 <- lapply(tables, function(table) {
    table_file <- file.path(directory, paste0(table, ".feather"))
    if (file.exists(table_file)) {
      arrow::read_parquet(table_file)
    } else {
      starvz_log(paste("The file:", table_file, " does not exist on that directory. Ignore."))
      NULL
    }
  })
  names(l2) <- tables %>% str_to_title()
  c(l1, l2)
}

starvz_read_feather <- function(directory = ".") {
  check_arrow()
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
  check_arrow()
  if (!arrow::codec_is_available("gzip")) {
    starvz_warn("R package arrow does not have 'gzip' codec, try using arrow::install_arrow()")
    return(list())
  } else {
    l1 <- list(Origin = directory)
    l2 <- lapply(tables, function(table) {
      table_file <- file.path(directory, paste0(table, ".parquet"))
      if (dir.exists(table_file)) {
        arrow::open_dataset(table_file)
      }else if (file.exists(table_file)) {
        arrow::read_parquet(table_file)
      } else {
        starvz_log(paste("The file:", table_file, " does not exist on that directory. Ignore."))
        NULL
      }
    })
    names(l2) <- tables %>% str_to_title()
    c(l1, l2)
  }
}

starvz_read_parquet <- function(directory = ".") {
  check_arrow()
  filenames <- list.files(
    path = directory,
    pattern = "*.parquet",
    full.names = FALSE,
    recursive = FALSE
  ) %>%
    str_replace_all(".parquet", "")

  starvz_read_some_parquet(directory = directory, tables = filenames)
}

starvz_read_some <- function(directory = ".", tables = c("application"), config_file = NULL) {
  check_arrow()
  # Check if there is arrow gz files
  filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = FALSE)
  if (length(filenames) > 0) {
    starvz_log("Detected parquet files")
    data <- starvz_read_some_parquet(directory, tables = tables)
  } else {
    data <- starvz_read_some_feather(directory, tables = tables)
  }

  # If no config_file is specified, try to fallback on the config.yaml inside directory
  # If it is still not available, it will use a default one
  if (!is.null(config_file)) {
    data$config <- starvz_read_config(config_file)
  } else {
    data$config <- starvz_read_config(file.path(directory, "config.yaml"), warn = FALSE)
  }

  final_data <- starvz_data(data)

  return(final_data)
}

#' Read starvz trace files
#'
#' Read the directory of trace files (feather or parquet) \cr
#' and the configuration file, and return a starvz_data
#' class used in starvz functions
#'
#' @param directory Directory path of trace files
#' @param config_file Path for configuration yaml file
#' @param selective if True, only read data needed for creating panels activated
#' in config
#' @return The starvz_data with all tables
#' @usage starvz_read(directory = ".",
#'                config_file = NULL, selective = TRUE)
#' @examplesIf requireNamespace("arrow", quietly = TRUE)
#' starvz_read("folder_to_parquet_files/")
#' starvz_read(
#'   directory = "folder_to_parquet_files/",
#'   config_file = "path_to_config.yaml"
#' )
#' starvz_read() # Read current directory
#' @export
starvz_read <- function(directory = ".",
                        config_file = NULL,
                        selective = TRUE) {
  check_arrow()

  # If no config_file is specified, try to fallback on the config.yaml inside directory
  # If it is still not available, it will use a default one
  if (!is.null(config_file)) {
    config <- starvz_read_config(config_file)
  } else {
    config <- starvz_read_config(file.path(directory, "config.yaml"), warn = FALSE)
  }

  if (selective) {
    # Always try to load Version, Colors and Y
    tables_to_load <- c("version", "colors", "y")

    if (config$st$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$st$tasks$active) {
      tables_to_load <- c(tables_to_load, "gaps", "starpu", "application", "link", "last")
    }

    if (config$st$cpb || config$st$cpb_mpi$active) {
      tables_to_load <- c(tables_to_load, "dag", "starpu", "application", "link")
    }

    if (config$pmtool$bounds$active) {
      tables_to_load <- c(tables_to_load, "application", "pmtool")
    }

    if (config$atree$active) {
      tables_to_load <- c(tables_to_load, "application", "atree")
    }

    if (config$utiltreenode$active) {
      tables_to_load <- c(tables_to_load, "application", "atree")
    }

    if (config$utiltreedepth$active) {
      tables_to_load <- c(tables_to_load, "application", "atree")
    }

    if (config$activenodes$active) {
      tables_to_load <- c(tables_to_load, "application", "atree")
    }

    if (config$computingnodes$active) {
      tables_to_load <- c(tables_to_load, "application", "atree")
    }

    if (config$kiteration$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$summary_nodes$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$pmtool$state$active) {
      tables_to_load <- c(tables_to_load, "application", "pmtool_states")
    }

    if (config$pmtool$kiteration$active) {
      tables_to_load <- c(tables_to_load, "application", "pmtool_states")
    }

    if (config$memory$state$active) {
      tables_to_load <- c(tables_to_load, "application", "tasks", "task_handles", "data_handles", "memory_state", "events_memory", "link")
    }

    if (config$memory$transfers$active) {
      tables_to_load <- c(tables_to_load, "application", "tasks", "task_handles", "data_handles", "memory_state", "events_memory", "link")
    }

    if (config$starpu$active) {
      tables_to_load <- c(tables_to_load, "starpu")
    }

    if (config$node_events$active) {
      tables_to_load <- c(tables_to_load, "events")
    }

    if (config$ready$active) {
      tables_to_load <- c(tables_to_load, "variable")
    }

    if (config$usedmemory$active) {
      tables_to_load <- c(tables_to_load, "variable")
    }

    if (config$submitted$active) {
      tables_to_load <- c(tables_to_load, "variable")
    }

    if (config$lackready$active) {
      tables_to_load <- c(tables_to_load, "starpu", "variable")
    }

    if (config$imbalance$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$power_imbalance$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$hete_imbalance$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$utilheatmap$active) {
      tables_to_load <- c(tables_to_load, "application")
    }

    if (config$mpibandwidth$active) {
      tables_to_load <- c(tables_to_load, "variable")
    }

    if (config$mpiconcurrent$active) {
      tables_to_load <- c(tables_to_load, "link")
    }

    if (config$mpiconcurrentout$active) {
      tables_to_load <- c(tables_to_load, "link")
    }

    if (config$mpistate$active) {
      tables_to_load <- c(tables_to_load, "comm_state")
    }

    if (config$gflops$active) {
      tables_to_load <- c(tables_to_load, "variable")
    }

    if (config$gpubandwidth$active) {
      tables_to_load <- c(tables_to_load, "variable")
    }

    tables_to_load <- tables_to_load %>% unique()
    starvz_log(paste("Read:", paste(tables_to_load, collapse = " ")))
    data <- starvz_read_some(directory = directory, tables = tables_to_load, config_file)
  } else {
    # Check if there is arrow gz files
    filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = FALSE)
    if (length(filenames) > 0) {
      starvz_log("Detected parquet files")
      data <- starvz_read_parquet(directory)
    } else {
      data <- starvz_read_feather(directory)
    }
  }

  data$config <- config

  final_data <- starvz_data(data)

  return(final_data)
}
