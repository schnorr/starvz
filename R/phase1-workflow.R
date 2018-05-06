#!/usr/bin/Rscript

# see:
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script (Suppressingfire answer)
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
other.name <- paste(sep="/", script.basename, "phase1.R")
print(paste("Sourcing",other.name,"from",script.name))

source(other.name)

#
# This script gets a directory with CSV files from StarPU PajeNGR framework
# and process them using the_reader_function. As output, it generates
# ready-to-plot feather files.
#

##############################
# Usage                      #
##############################
usage <- function ()
{
    stop("Usage: pre-workflow.R <directory> <application>\n   where <directory> contains CSV files of the workflow;\n   where <application> is either cholesky or qrmumps.", call.=FALSE)
}

# Get the arguments to this script
args = commandArgs(trailingOnly=TRUE)

if (length(args) < 2) {
    usage();
}

input.directory = args[[1]];
input.application = args[[2]];

if (is.null(input.directory) ||
    (input.application != "cholesky" && input.application != "qrmumps" && input.application != "cfd")){
    usage();
}

if (input.application == "cholesky"){
    states.fun = cholesky_colors;
    states.filter.strict = FALSE;
}else if (input.application == "qrmumps") {
    states.fun = qrmumps_colors;
    states.filter.strict = FALSE;
}else if (input.application == "cholesky_pastix") {
    states.fun = cholesky_pastix_colors;
    states.filter.strict = FALSE;
}else if (input.application == "cfd") {
    states.fun = cfd_colors;
    states.filter.strict = FALSE;
}


setwd(input.directory);

data <- the_reader_function (directory = input.directory,
                             app_states_fun = states.fun,
                             strict_state_filter = states.filter.strict,
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

loginfo("Pre-process finished correctly.");
