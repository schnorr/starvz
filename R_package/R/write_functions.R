starvz_write_feather <- function(data){
  check_arrow();
  invisible(data %>% purrr::list_modify("Origin" = NULL) %>% names %>%
  lapply(function(x) {
      filename <- paste0(tolower(x), ".feather"); loginfo(filename);
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

starvz_write_parquet <- function(data){
  check_arrow();
  invisible(data %>% purrr::list_modify("Origin" = NULL) %>% names %>%
  lapply(function(x) {
      filename <- paste0(tolower(x), ".parquet"); loginfo(filename);
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
