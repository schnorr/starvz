#!/usr/bin/Rscript

# see:
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script (Suppressingfire answer)
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
other.name.1 <- paste(sep="/", script.basename, "phase1.R")
other.name.2 <- paste(sep="/", script.basename, "phase2.R")
print(paste("Sourcing",other.name.1,"from",script.name))
print(paste("Sourcing",other.name.2,"from",script.name))
source(other.name.1)
source(other.name.2)

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
    stop("Usage: phase2-workflow.R <directory> <config>", call.=FALSE)
}

directory = args[[1]];
pajer.config = args[[2]];
name = "output.png";

data <- the_fast_reader_function(directory);

print("Data has been read");

pajer <- config::get(file = pajer.config, config="exp7");

png(name, width = 1000, height = 900);
print(grid.arrange(the_master_function(data)));
dev.off();
