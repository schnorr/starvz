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
check_arrow();
directory = args[[1]];
pajer.config = args[[2]];

name = paste0(directory, "/starvz.png");
if (length(args)==3) {
	name = args[[3]];
}

cat("StarVZ - Phase 2 - Start\n", file = stdout())

pajer <- config::get(file = pajer.config);

data <- starvz_selective_read(directory);

if(pjr(pajer$guided_plot)){
  r <- starvz_guided_plot(data, name)
}else{
  ggsave(name, plot=the_master_function(data), width = 10, height = 18, units = "in", dpi=120)
}

cat("End of StarVZ - Phase 2\n", file = stdout())
