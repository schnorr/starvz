#!/usr/bin/env Rscript
# Call function memsnaps from start to end with informed steps

library(starvz)
library(magrittr)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: phase2-memsnaps.R <directory> <step>", call. = FALSE)
}

directory <- args[[1]]
step <- as.integer(args[[2]])
path <- paste0(directory, "/snap_")

data <- starvz_read(directory, selective = FALSE)
end <- data$Application %>%
  .$End %>%
  max()
cat("Generating snaps at:")
cat(directory)
cat("\n")
multiple_snaps(data, 0, end, step, path, scale = 8, width = 4, height = 3)
