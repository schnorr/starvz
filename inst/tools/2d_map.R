#!/usr/bin/env Rscript
# Print the MPI Owner distribution considering a 2D structure of data

library(starvz)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: 2d_map.R <directory> <save_file>", call. = FALSE)
}

directory <- args[[1]]
name <- args[[2]]
data <- starvz_read(directory)
data$Data_handle %>%
  .$MPIOwner %>%
  unique() %>%
  length() -> n_nodes

x <- data$Data_handle %>%
  select(MPIOwner, Coordinates) %>%
  unique() %>%
  separate(Coordinates, c("Y", "X")) %>%
  mutate(X = as.numeric(X), Y = as.numeric(Y)) %>%
  ggplot(aes(x = X, y = Y, fill = MPIOwner)) +
  geom_tile(alpha = 0.8) +
  geom_text(aes(label = factor(MPIOwner)), size = 2) +
  scale_fill_viridis(name = "Node", breaks = seq(0, n_nodes)) +
  scale_y_reverse(expand = c(0.01, 0.01)) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 15)) +
  xlab("Column") +
  ylab("Line")

ggsave(name, x, width = 10, height = 12, units = "in", dpi = 200)
