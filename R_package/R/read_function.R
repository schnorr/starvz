the_fast_reader_function <- function (directory = ".")
{
    names <- c("State", "Variable", "Link", "DAG", "Y", "ATree", "Gaps", "pmtool",
               "pmtool_states", "data_handles", "tasks", "task_handles", "Events");

    filenames <- gsub("^", "pre.", gsub("$", ".feather", tolower(names)));

    l1 <- list(Origin = directory);
    l2 <- lapply(filenames, function(x) {
        filename = paste0(directory, "/", x);
        if (file.exists(filename)){
            read_feather(filename);
        }else{
            loginfo(paste("The file", x, "does not exist on that directory. Ignore."));
            NULL;
        }
    });
    names(l2) <- names;
    c(l1, l2);
}
