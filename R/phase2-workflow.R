#!/usr/bin/Rscript

library(starvz)

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
    stop("Usage: phase2-workflow.R <directory> <config>", call.=FALSE)
}

directory = args[[1]];
pajer.config = args[[2]];

name = "output.png";
if (length(args)==3) {
	name = args[[3]];
}

data <- the_fast_reader_function(directory);

print("Data has been read");

pajer <- config::get(file = pajer.config, config="exp7");

png(name, width = 1000, height = 1500);
print(grid.arrange(the_master_function(data)));
end <- dev.off();
