starvz_write_feather <- function(data, directory="."){
  check_arrow();
  invisible(data %>% purrr::list_modify("Origin" = NULL) %>% names %>%
  lapply(function(x) {
      filename <- paste0(directory, "/", tolower(x), ".feather");
      loginfo(filename);
      if (!is.null(data[[x]])){
          if(is.data.frame(data[[x]])){
              write_feather(data[[x]], filename);
          } else {
              loginfo(paste(filename, "must be a data frame."));
          }
      } else {
          loginfo(paste("Data for", filename, "has not been feathered because is empty."));
      }
  }))
}

starvz_write_parquet <- function(data, directory="."){
  check_arrow();
  if(!codec_is_available("gzip")){
    logwarn("Arrow Gzip is not available, try using arrow::install_arrow()")
  }
  invisible(data %>% purrr::list_modify("Origin" = NULL) %>% names %>%
  lapply(function(x) {
      filename <- paste0(directory, "/", tolower(x), ".parquet");
      loginfo(filename);
      if (!is.null(data[[x]])){
          if(is.data.frame(data[[x]])){
              properties = ParquetWriterProperties$create(data[[x]], compression = "gzip")
              write_parquet(data[[x]], filename, properties=properties)
          } else {
              loginfo(paste(filename, "must be a data frame."));
          }
      } else {
          loginfo(paste("Data for", filename, "has not been feathered because is empty."));
      }
  }))
}

convert_feather_parquet <- function(directory = "."){
    data <- starvz_read_feather(directory)
    starvz_write_parquet(data, directory)
}
