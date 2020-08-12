#!/usr/bin/Rscript
options(crayon.enabled = FALSE)

# Options to help debug
# https://stackoverflow.com/questions/1975110/printing-stack-trace-and-continuing-after-error-occurs-in-r
options(keep.source = TRUE, error = quote({
  dump.frames()  # writes to last.dump
  n <- length(last.dump)
  if (n > 0) {
    calls <- names(last.dump)
    cat("Environment:\n", file = stderr())
    cat(paste0("  ", seq_len(n), ": ", calls), sep = "\n", file = stderr())
    cat("\n", file = stderr())
  }

  if (!interactive()) q()
}))

library(starvz)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
    stop("Usage: phase2-workflow.R <directory> <config>", call.=FALSE)
}

directory = args[[1]];
config_file = args[[2]];

name = paste0(directory, "/starvz.png");
if (length(args)==3) {
	name = args[[3]];
}

cat("StarVZ - Phase 2 - Start\n", file = stdout())

data <- starvz_read(directory, config_file);

starvz_plot(data, name, save=TRUE)

cat("End of StarVZ - Phase 2\n", file = stdout())
