#!/usr/bin/Rscript

# see:
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script (Suppressingfire answer)
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)

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

phase1_plan <- drake_plan(
    dfw = read_state(input.directory),
    dfe = read_entities(input.directory),
    dfa = read_atree(input.directory),
    dfv = read_variables(input.directory),
    dfl = read_links_io(input.directory),
    dfdag = read_dag_io(input.directory),
    dpmtb = read_pmtools_bounds(input.directory),
    dpmts = read_pmtools_states(input.directory),
    ddh = read_data_handles(input.directory),
    task_handles = read_tasks_handles(input.directory),
    tasks = read_tasks(input.directory)
);

plan_config <- drake_config(phase1_plan);
vis_drake_graph(plan_config);
clean(plan_config);
jobs <- max_useful_jobs(plan_config);
loadd(dfw, dfe, dfa, dfv, dfl, dfdag, dpmtb, dpmts, ddh, task_handles, tasks);
make(phase1_plan, j = jobs);

# Data manipulation
dfw <- manipulate_state_csv(input.application, states.fun, states.filter.strict, dfw);
zero <- manipulate_zero(dfw);
dfw <- normalize_dfw(dfw, zero, input.application, states.fun, outlier_definition);
dfhie <- hl_y_paje_tree(dfe);
highlightedDfw <- hl_y_coordinates(dfw, dfhie);
dfa <- manipulate_atree(dfa);
dfap <- build_dfap(dfa);
dfw <- join_dfw_dfap(dfw, dfap);
dfw <- manipulate_vars_set_new_zero(dfv, zero);
dfl <- manipulate_links(dfl, zero);
dfdag <- manipulate_dag(dfdag, dfw, dfl);
dpmtb <- manipulate_pmtools_bounds(dpmtb);
dpmts <- manipulate_pmtools_states(dpmts, input.application, dfhie, dfw);
ddh <- manipulate_data_handles(ddh);
dtasks <- manipulate_tasks(tasks, task_handles);
gaps <- calculate_gaps(input.application, dfw, dfdag, dfl);
data <- aggregate_data(input.application, dfw, dfv, dfl, dfdag, dfhie, dfa, dpmtb, dpmts, ddh, dtasks);

# Saving data
save_feathers(data, gaps);

loginfo("Pre-process finished correctly.");
