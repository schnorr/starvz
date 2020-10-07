#' @include starvz_data.R

default_theme <- function(base_size = 22, expand = 0.05, legend_title = FALSE, skip_x = FALSE) {
  ret <- list()

  ret[[length(ret) + 1]] <- theme_bw(base_size = base_size)
  ret[[length(ret) + 1]] <- theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.spacing = unit(1, "mm"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.box.spacing = unit(0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0)
  )
  if (!legend_title) {
    ret[[length(ret) + 1]] <- theme(legend.title = element_blank())
  }

  ret[[length(ret) + 1]] <- xlab("Time [ms]")
  if (!skip_x) {
    ret[[length(ret) + 1]] <- scale_x_continuous(
      expand = c(expand, 0),
      labels = function(x) format(x, big.mark = "", scientific = FALSE)
    )
  }

  return(ret)
}
