#' @include starvz_data.R

node_summary <- function(data) {
  hl_per_node_ABE(data$Application) -> Abes
  data$Application %>%
    group_by(.data$Node) %>%
    arrange(-.data$End) %>%
    slice(1) %>%
    select(.data$Node, .data$End) %>%
    ungroup() %>%
    mutate(Node = as.integer(as.character(.data$Node))) -> Makespans

  Makespans %>%
    mutate(Metric = "Makespan") %>%
    rename(Value = .data$End) -> makes
  makes$Value <- makes$Value - Abes$Result
  Abes %>%
    select(.data$Node, .data$Result) %>%
    mutate(Node = as.integer(as.character(.data$Node))) %>%
    mutate(Metric = "Abe") %>%
    rename(Value = .data$Result) %>%
    bind_rows(makes) %>%
    mutate(Metric = factor(.data$Metric, levels = c("Makespan", "Abe"))) -> all_data
  Nodes <- all_data %>%
    mutate(Node = as.integer(.data$Node)) %>%
    .$Node %>%
    max()
  all_data %>%
    mutate(Node = as.integer(.data$Node)) %>%
    ggplot(aes(y = .data$Node, x = .data$Value, fill = .data$Metric)) +
    default_theme(data$config$base_size, data$config$expand) +
    scale_y_reverse(
      breaks = function(x) {
        unique(
          floor(pretty(seq(0, (max(x) + 1) * 1.1)))
        )
      },
      expand = c(data$config$expand, 0)
    ) +
    geom_col(width = 0.8, orientation = "y") -> splot
  return(splot)
}
