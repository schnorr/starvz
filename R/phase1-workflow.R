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

# State
filename <- "pre.state.feather";
loginfo(filename);
if (!is.null(data$State)){
    write_feather(data$State, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# Variable
filename <- "pre.variable.feather";
loginfo(filename);
if (!is.null(data$Variable)){
    write_feather(data$Variable, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# Link
filename <- "pre.link.feather";
loginfo(filename);
if (!is.null(data$Link)){
    write_feather(data$Link, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# DAG
filename <- "pre.dag.feather";
loginfo(filename);
if (!is.null(data$DAG)){
    write_feather(data$DAG, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# Y
filename <- "pre.y.feather";
loginfo(filename);
if (!is.null(data$Y)){
    write_feather(data$Y, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# ATree
filename <- "pre.atree.feather";
loginfo(filename);
if (!is.null(data$ATree)){
    write_feather(data$ATree, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# Gaps
filename <- "pre.gaps.feather";
loginfo(filename);
if (!is.null(data$Gaps)){
    write_feather(data$Gaps, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# PMtool
filename <- "pre.pmtool.feather";
loginfo(filename);
if (!is.null(data$pmtool)){
    write_feather(data$pmtool, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

filename <- "pre.pmtool_states.feather";
loginfo(filename);
if (!is.null(data$pmtool_states)){
    write_feather(data$pmtool_states, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# Data Rec
filename <- "pre.data_handles.feather";
loginfo(filename);
if (!is.null(data$data_handles)){
    write_feather(data$data_handles, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

# Tasks Rec
filename <- "pre.tasks.feather";
loginfo(filename);
if (!is.null(data$tasks)){
    write_feather(data$tasks, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

filename <- "pre.task_handles.feather";
loginfo(filename);
if (!is.null(data$task_handles)){
    write_feather(data$task_handles, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

filename <- "pre.events.feather";
loginfo(filename);
if (!is.null(data$Events)){
    write_feather(data$Events, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

filename <- "pre.papi.feather";
loginfo(filename);
if (!is.null(data$papi)){
    write_feather(data$papi, filename);
}else{
    loginfo(paste("Data for", filename, "has not been feathered because is empty."));
}

loginfo("Pre-process finished correctly.");
