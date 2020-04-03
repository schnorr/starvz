
starvz_write_feather <- function(data){
  invisible(data %>% purrr::list_modify("Origin" = NULL) %>% names %>%
  lapply(function(x) {
      filename <- paste0("pre.", tolower(x), ".feather"); loginfo(filename);
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
