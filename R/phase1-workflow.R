#!/usr/bin/Rscript

suppressMessages(library(starvz))

##############################
# Usage                      #
##############################
usage <- function ()
{
    stop("Usage: ", basename(commandArgs()[4]), " <directory> [application](optional)\n   where <directory> contains CSV files of the workflow;\n   where [application](optional) is either cholesky or qrmumps.", call.=FALSE)
}

# Get the arguments to this script
args = commandArgs(trailingOnly=TRUE)

if (length(args) < 1) {
    usage();
}else if (length(args) < 2) {
   input.application = "";
}else{
   input.application = args[[2]];
}

input.directory = args[[1]];

# This fixes some problems on recent versions of tidyverse
# Check: https://github.com/tidyverse/tidyr/issues/751
# Check: https://github.com/tidyverse/tidyr/issues/694
if(exists("unnest_legacy")){
  unnest <- unnest_legacy
}

if ( is.null(input.directory) ){
    usage();
}
if (input.application == "cholesky"){
    states.fun = cholesky_colors;
    states.filter = 2;
}else if (input.application == "qrmumps") {
    states.fun = qrmumps_colors;
    states.filter = 1;
}else if (input.application == "cholesky_pastix") {
    states.fun = cholesky_pastix_colors;
    states.filter = 1;
}else if (input.application == "cfd") {
    states.fun = cfd_colors;
    states.filter = 1;
}else if (input.application == "lu") {
    states.fun = lu_colors;
    states.filter = 2;
}else if (input.application == "") {
    states.fun = cfd_colors;
    states.filter = 0;
}

setwd(input.directory);

data <- the_reader_function (directory = input.directory,
                             app_states_fun = states.fun,
                             state_filter = states.filter,
                             whichApplication = input.application);

loginfo("Let's start to write the pre-processed files as feather data");

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

loginfo("Pre-process finished correctly.");
