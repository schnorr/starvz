the_fast_reader_function <- function (directory = ".")
{

    filenames <- list.files(path = directory, pattern = "pre.*.feather", full.names = TRUE, recursive = TRUE);
    
    l1 <- list(Origin = directory);
    l2 <- lapply(filenames, function(filename) {
        if (file.exists(filename)){
            read_feather(filename);
        }else{
            loginfo(paste("The file", x, "does not exist on that directory. Ignore."));
            NULL;
        }
    });
    names(l2) <- filenames %>% basename() %>% str_replace_all("pre.|.feather", "") %>% str_to_title();
    c(l1, l2);
}
