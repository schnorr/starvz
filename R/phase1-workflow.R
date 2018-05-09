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

basicConfig();
logForOrg <- function(record) { paste(record$levelname, record$logger, record$msg, sep=':') }
addHandler(writeToConsole, formatter=logForOrg);
removeHandler('basic.stdout');

phase1_plan <- drake_plan(
    rawDfw = read_state_csv(input.application, states.fun, states.filter.strict, input.directory),
    zero = read_zero(rawDfw),
    normalizedDfw = normalize_dfw(rawDfw, zero, input.application, states.fun, outlier_definition),
    highlightedDfw = hl_y_coordinates(normalizedDfw, input.directory),
    dfa = atree_load(input.directory),
    dfap = build_dfap(dfa),
    dfw = join_dfw_dfap(highlightedDfw, dfap),
    dfv = read_vars_set_new_zero(input.directory, zero),
    dfl = read_links(input.directory, zero),
    dfdag = read_dag(input.directory, dfw, dfl),
    dfhie = hl_y_paje_tree(input.directory),
    dpmtb = pmtools_bounds_csv_parser(input.directory),
    dpmts = pmtools_states_csv_parser(input.directory, input.application, dfhie, dfw),
    ddh = data_handles_csv_parser(input.directory),
    dtasks = tasks_csv_parser(input.directory),
    data = aggregate_data(directory, dfw, dfv, dfl, dfdag, dfhie, dfa, dpmtb, dpmts, ddh, dtasks),
    gaps = calculate_gaps(data),
    success = save_feathers(data, gaps)
);

plan_config <- drake_config(phase1_plan);

loginfo("Pre-process finished correctly.");
