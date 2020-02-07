#!/usr/bin/Rscript

suppressMessages(library(starvz))

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
    stop("Usage: 2d_map.R <directory> <save_file>", call.=FALSE)
}

directory = args[[1]];
name = args[[2]];

print("Map2D Reading...");

data <- the_fast_reader_function(directory);

print("Data has been read");

x <- data$Data_handle %>% filter(HomeNode==0) %>%
                     separate(Coordinates, c("Y", "X")) %>%
                     mutate(X=as.numeric(X), Y=as.numeric(Y)) %>%
                     ggplot(aes(x=X, y=Y, fill=factor(MPIRank))) +
                     geom_tile()

ggsave(name, x, width=10, height=10)
