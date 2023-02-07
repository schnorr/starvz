#!/usr/bin/env Rscript
library(starvz)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: phase2-workflow.R <directory> <config>", call. = FALSE)
}

directory <- args[[1]]
config_file <- args[[2]]
name <- paste0(directory, "/starvz.pdf")
if (length(args) == 3) {
  name <- args[[3]]
}

cat("StarVZ - Phase 2 - Start\n", file = stdout())
starvz_set_log(TRUE)
data <- starvz_read(directory, config_file)
p <- starvz_plot(data, name, save = TRUE)

cat("End of StarVZ - Phase 2\n", file = stdout())
