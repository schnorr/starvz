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

suppressMessages(library(starvz))

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

pajer <- config::get(file = pajer.config);

png(name, width = 1000, height = 1500);
print(grid.arrange(the_master_function(data)));
end <- dev.off();
