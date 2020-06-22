starvz_read_some_feather <- function (directory = ".", tables = c("application"))
{
  l1 <- list(Origin = directory);
  l2 <- lapply(tables, function(table) {
      table_file <- file.path(directory, paste0(table, ".feather"))
      if (file.exists(table_file)){
          read_parquet(table_file)
      }else{
          loginfo(paste("The file:", table_file, " does not exist on that directory. Ignore."));
          NULL;
      }
  });
  names(l2) <- tables %>% str_to_title();
  c(l1, l2);
}

starvz_read_feather <- function (directory = ".")
{
    filenames <- list.files(path = directory,
                            pattern = "*.feather",
                            full.names = FALSE,
                            recursive = FALSE) %>%
                  str_replace_all(".feather", "")

    starvz_read_some_feather(directory=directory, tables=filenames)
}

# retrocompatibility
the_fast_reader_function <- starvz_read_feather

starvz_read_some_parquet <- function (directory = ".", tables = c("application")){
  if(!codec_is_available("gzip")){
    logwarn("Arrow Gzip is not available, try using arrow::install_arrow()")
  }
  l1 <- list(Origin = directory);
  l2 <- lapply(tables, function(table) {
      table_file <- file.path(directory, paste0(table, ".parquet"))
      if (file.exists(table_file)){
          read_parquet(table_file)
      }else{
          loginfo(paste("The file:", table_file, " does not exist on that directory. Ignore."));
          NULL;
      }
  });
  names(l2) <- tables %>% str_to_title();
  c(l1, l2);
}

starvz_read_parquet <- function (directory = ".")
{
    filenames <- list.files(path = directory,
                            pattern = "*.parquet",
                            full.names = FALSE,
                            recursive = FALSE) %>%
                  str_replace_all(".parquet", "")

    starvz_read_some_parquet(directory=directory, tables=filenames)
}

# This will use Pajer to select what files to read
starvz_selective_read <- function(directory = ".")
{
  # Always try to load Version, Colors and Y
  tables_to_load <- c("version", "colors", "y")

  if(pjr(pajer$st$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if (pjr(pajer$st$tasks$active)){
    tables_to_load <- c(tables_to_load, "gaps", "starpu", "application", "link")
  }

  if (pjr(pajer$st$cpb) || pjr(pajer$st$cpb_mpi$active)){
    tables_to_load <- c(tables_to_load, "dag", "starpu", "application", "link")
  }

  if (pjr(pajer$pmtool$bounds$active)){
    tables_to_load <- c(tables_to_load, "application", "pmtool")
  }

  if(pjr(pajer$atree$active)){
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if(pjr(pajer$utiltreenode$active)){
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if(pjr(pajer$utiltreedepth$active)){
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if(pjr(pajer$activenodes$active)){
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if(pjr(pajer$computingnodes$active)){
    tables_to_load <- c(tables_to_load, "application", "atree")
  }

  if(pjr(pajer$kiteration$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if(pjr(pajer$summary_nodes$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if(pjr(pajer$pmtool$state$active)){
    tables_to_load <- c(tables_to_load, "application", "pmtool_states")
  }

  if(pjr(pajer$pmtool$kiteration$active)){
    tables_to_load <- c(tables_to_load, "application", "pmtool_states")
  }

  if(pjr(pajer$memory$state$active)){
    tables_to_load <- c(tables_to_load, "application", "tasks", "task_handles", "data_handles", "memory_state", "link")
  }

  if(pjr(pajer$memory$transfers$active)){
    tables_to_load <- c(tables_to_load, "application", "tasks", "task_handles", "data_handles", "memory_state", "link")
  }

  if(pjr(pajer$starpu$active)){
    tables_to_load <- c(tables_to_load, "starpu")
  }

  if(pjr(pajer$ready$active)){
    tables_to_load <- c(tables_to_load, "variable")
  }

  if(pjr(pajer$usedmemory$active)){
    tables_to_load <- c(tables_to_load, "variable")
  }

  if(pjr(pajer$submitted$active)){
    tables_to_load <- c(tables_to_load, "variable")
  }

  if(pjr(pajer$lackready$active)){
    tables_to_load <- c(tables_to_load, "starpu", "variable")
  }

  if(pjr(pajer$imbalance$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if(pjr(pajer$power_imbalance$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if(pjr(pajer$hete_imbalance$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if(pjr(pajer$utilheatmap$active)){
    tables_to_load <- c(tables_to_load, "application")
  }

  if(pjr(pajer$mpibandwidth$active)){
    tables_to_load <- c(tables_to_load, "variable")
  }

  if(pjr(pajer$mpiconcurrent$active)){
    tables_to_load <- c(tables_to_load, "link")
  }

  if(pjr(pajer$mpiconcurrentout$active)){
    tables_to_load <- c(tables_to_load, "link")
  }

  if(pjr(pajer$mpistate$active)){
    tables_to_load <- c(tables_to_load, "comm_state")
  }

  if(pjr(pajer$gflops$active)){
    tables_to_load <- c(tables_to_load, "variable")
  }

  if(pjr(pajer$gpubandwidth$active)){
    tables_to_load <- c(tables_to_load, "variable")
  }

  tables_to_load <- tables_to_load %>% unique()
  loginfo(paste("Read:", paste(tables_to_load, collapse=" ")))
  starvz_read_some(directory = directory, tables = tables_to_load)
}

starvz_read_some <- function(directory = ".", tables = c("application"))
{
    check_arrow();
    # Check if there is arrow gz files
    filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = FALSE);
    if(length(filenames)>0){
        loginfo("Detected parquet files")
        starvz_read_some_parquet(directory, tables = tables)
    }else{
        starvz_read_some_feather(directory, tables = tables)
    }
}

starvz_read <- function(directory = ".")
{
    check_arrow();
    # Check if there is arrow gz files
    filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = FALSE);
    if(length(filenames)>0){
        loginfo("Detected parquet files")
        starvz_read_parquet(directory)
    }else{
        starvz_read_feather(directory)
    }
}
