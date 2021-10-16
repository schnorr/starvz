#' @include starvz_data.R

starvz_write_feather <- function(data, directory = ".") {
  check_arrow()
  invisible(data %>% list_modify("Origin" = NULL) %>% names() %>%
    lapply(function(x) {
      filename <- paste0(directory, "/", tolower(x), ".feather")
      starvz_log(filename)
      if (!is.null(data[[x]])) {
        if (is.data.frame(data[[x]])) {
          write_feather(data[[x]], filename)
        } else {
          starvz_log(paste(filename, "must be a data frame."))
        }
      } else {
        starvz_log(paste("Data for", filename, "has not been feathered because is empty."))
      }
    }))
}

starvz_write_parquet <- function(data, directory = ".") {
  check_arrow()
  if(is.null(data)){
    return(NULL)
  }
  if (!codec_is_available("gzip")) {
    starvz_warn("R package arrow does not have 'gzip' codec, try using arrow::install_arrow()")
  }else{
    invisible(data %>% list_modify("Origin" = NULL) %>% names() %>%
      lapply(function(x) {
        filename <- paste0(directory, "/", tolower(x), ".parquet")
        starvz_log(filename)
        if (!is.null(data[[x]])) {
          if (is.data.frame(data[[x]])) {
            write_parquet(data[[x]], filename, compression = "gzip")
          } else {
            starvz_log(paste(filename, "must be a data frame."))
          }
        } else {
          starvz_log(paste("Data for", filename, "has not been feathered because is empty."))
        }
      }))
  }
}

convert_feather_parquet <- function(directory = ".") {
  data <- starvz_read_feather(directory)
  starvz_write_parquet(data, directory)
}
