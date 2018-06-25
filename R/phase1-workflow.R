#!/usr/bin/Rscript

# see:
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script (Suppressingfire answer)
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)

other.name <- paste(sep='/', script.basename, 'deps.R');
print(paste('Sourcing',other.name,'from',script.name));
source(other.name);

other.name <- paste(sep="/", script.basename, "phase1.R");
print(paste("Sourcing",other.name,"from",script.name));
source(other.name);

other.name <- paste(sep='/', script.basename, 'phase1-io.R');
print(paste('Sourcing',other.name,'from',script.name));
source(other.name);

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

targets <- c('dfw', 'dfe', 'dfa', 'dfv', 'dfl', 'dfdag', 'dpmtb', 'dpmts', 'ddh', 'task_handles', 'tasks');
data <- lapply(targets, resolve_io_function);
names(data) <- targets

# Data manipulation
dfw <- manipulate_state_csv(input.application, states.fun, states.filter.strict, data$dfw);
zero <- manipulate_zero(dfw);
dfw <- normalize_dfw(dfw, zero, input.application, states.fun, outlier_definition);
dfhie <- hl_y_paje_tree(data$dfe);
highlightedDfw <- hl_y_coordinates(dfw, dfhie);
dfa <- manipulate_atree(data$dfa);
dfap <- build_dfap(dfa);
dfw <- join_dfw_dfap(dfw, dfap);
dfw <- manipulate_vars_set_new_zero(data$dfv, zero);
dfl <- manipulate_links(data$dfl, zero);
dfdag <- manipulate_dag(data$dfdag, dfw, dfl);
dpmtb <- manipulate_pmtools_bounds(data$dpmtb);
dpmts <- manipulate_pmtools_states(data$dpmts, input.application, dfhie, dfw);
ddh <- manipulate_data_handles(data$ddh);
dtasks <- manipulate_tasks(data$tasks, data$task_handles);
gaps <- calculate_gaps(input.application, dfw, dfdag, dfl);
data <- aggregate_data(input.application, dfw, dfv, dfl, dfdag, dfhie, dfa, dpmtb, dpmts, ddh, dtasks);

# # Saving data
# save_feathers(data, gaps);

# loginfo("Pre-process finished correctly.");
