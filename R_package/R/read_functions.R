starvz_read_some_feather <- function (directory = ".", files = c("state.feather"))
{
    l1 <- list(Origin = directory);
    l2 <- lapply(files, function(filename) {
        if (file.exists(filename)){
            read_feather(filename);
        }else{
            loginfo(paste("The file:", filename, " does not exist on that directory. Ignore."));
            NULL;
        }
    });
    names(l2) <- files %>% basename() %>% str_replace_all("pre.|.feather", "") %>% str_to_title();
    c(l1, l2);
}

starvz_read_feather <- function (directory = ".")
{
    filenames <- list.files(path = directory, pattern = "*.feather", full.names = TRUE, recursive = TRUE);
    starvz_read_some_feather(directory=directory, files = filenames)
}
# retrocompatibility
the_fast_reader_function <- starvz_read_feather

starvz_read_some_parquet <- function (directory = ".", files = c("state.parquet")){
  if(!codec_is_available("gzip")){
    logwarn("Arrow Gzip is not available, try using arrow::install_arrow()")
  }
  l1 <- list(Origin = directory);
  l2 <- lapply(files, function(filename) {
      if (file.exists(filename)){
          read_parquet(filename)
      }else{
          loginfo(paste("The file:", filename, " does not exist on that directory. Ignore."));
          NULL;
      }
  });
  names(l2) <- files %>% basename() %>% str_replace_all("pre.|.parquet", "") %>% str_to_title();
  c(l1, l2);
}

starvz_read_parquet <- function (directory = ".")
{
    filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = TRUE);
    starvz_read_some_parquet(directory=directory, files=filenames)
}

starvz_read <- function(directory = ".")
{
    check_arrow();
    # Check if there is arrow gz files
    filenames <- list.files(path = directory, pattern = "*.parquet", full.names = TRUE, recursive = TRUE);
    if(length(filenames)>0){
        loginfo("Detected parquet files")
        starvz_read_parquet(directory)
    }else{
        starvz_read_feather(directory)
    }
}
