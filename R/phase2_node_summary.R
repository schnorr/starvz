
#' Create a bar plot with node information
#'
#' Bar plot with makespan and abe per node
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_node_summary(data = starvz_sample_lu)
#' @export
panel_node_summary <- function(data, legend = data$config$summary_nodes$legend,
                               base_size = data$config$base_size,
                               expand_x = data$config$expand,
                               x_start = data$config$limits$start,
                               x_end = data$config$limits$end) {
  ## Check for non-valid arguments
  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(base_size) || !is.numeric(base_size)) {
    base_size <- 22
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  hl_per_node_ABE(data$Application) -> Abes
  data$Application %>%
    group_by(.data$Node) %>%
    arrange(-.data$End) %>%
    slice(1) %>%
    select("Node", "End") %>%
    ungroup() %>%
    mutate(Node = as.integer(as.character(.data$Node))) -> Makespans

  Makespans %>%
    mutate(Metric = "Makespan") %>%
    rename(Value = "End") -> makes
  makes$Value <- makes$Value - Abes$Result
  Abes %>%
    select("Node", "Result") %>%
    mutate(Node = as.integer(as.character(.data$Node))) %>%
    mutate(Metric = "Abe") %>%
    rename(Value = "Result") %>%
    bind_rows(makes) %>%
    mutate(Metric = factor(.data$Metric, levels = c("Makespan", "Abe"))) -> all_data
  Nodes <- all_data %>%
    mutate(Node = as.integer(.data$Node)) %>%
    .$Node %>%
    max()
  all_data %>%
    mutate(Node = as.integer(.data$Node)) %>%
    ggplot(aes(y = .data$Node, x = .data$Value, fill = .data$Metric)) +
    default_theme(base_size, expand_x) +
    scale_y_reverse(
      breaks = function(x) {
        unique(
          floor(pretty(seq(0, (max(x) + 1) * 1.1)))
        )
      },
      expand = c(expand_x, 0)
    ) +
    geom_col(width = 0.8, orientation = "y") -> splot
  if (legend) {
    splot <- splot + theme(legend.position = "none")
  }

  splot <- splot +
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  return(splot)
}
