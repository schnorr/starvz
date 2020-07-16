#' @include starvz_data.R

k_chart <- function(data = NULL, middle_lines = NULL, per_node = FALSE, colors = NULL) {
  dfw <- data$Application
  if (is.null(dfw)) stop("dfw provided to k_chart is NULL")
  if (is.null(colors)) stop("colors provided to k_chart is NULL")

  # Prepare for colors
  colors %>%
    select(.data$Value, .data$Color) %>%
    unique() %>%
    .$Color -> appColors
  appColors %>% setNames(colors %>% select(.data$Value, .data$Color) %>% unique() %>% .$Value) -> appColors

  # Prepare for borders
  if (per_node) {
    dfw %>% group_by(.data$Node, .data$Iteration) -> temp1
  } else {
    dfw %>% group_by(.data$Iteration) -> temp1
  }
  dfborders <- temp1 %>%
    summarize(Start = min(.data$Start), End = max(.data$End)) %>%
    mutate(IterationB = lead(.data$Iteration), StartB = lead(.data$Start)) %>%
    mutate(IterationE = lead(.data$Iteration), EndB = lead(.data$End)) %>%
    na.omit()

  # Prepare for middle
  lapply(middle_lines, function(percentage) {
    dfw %>%
      select(.data$Node, .data$Iteration, .data$Start, .data$End) -> temp1
    if (per_node) {
      temp1 %>% group_by(.data$Node, .data$Iteration) -> temp1
    } else {
      temp1 %>% group_by(.data$Iteration) -> temp1
    }
    temp1 %>%
      mutate(Number.Tasks = n()) %>%
      arrange(.data$Start) %>%
      slice(unique(as.integer(.data$Number.Tasks * percentage))) %>%
      ungroup() %>%
      mutate(Middle = .data$Start + (.data$End - .data$Start) / 2) -> temp1
    if (per_node) {
      temp1 %>% group_by(.data$Node) -> temp1
    }
    temp1 %>%
      arrange(.data$Iteration) %>%
      mutate(Middle.Next = lead(.data$Middle)) %>%
      mutate(IterationB = lead(.data$Iteration)) %>%
      mutate(Percentage = percentage) %>%
      ungroup() %>%
      na.omit()
  }) %>% bind_rows() -> dfmiddle

  # Height of each bar
  height <- 0.8

  goijk <- dfw %>% ggplot() +
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual(values = appColors) +
    theme_bw(base_size = 12) +
    xlab("Time [ms]") +
    ylab("Iteration") +
    default_theme(data$config$base_size, data$config$expand) +
    # Keep the alpha = 1 even if we use an alpha below
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    scale_y_reverse(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    # The start border
    geom_curve(data = dfborders, aes(
      x = .data$Start, xend = .data$StartB,
      y = .data$Iteration + height - height / 2, yend = .data$IterationB + height - height / 2
    ), curvature = 0.1, angle = 20) +
    # The end border
    geom_curve(data = dfborders, aes(
      x = .data$End, xend = .data$EndB,
      y = .data$Iteration - height / 2, yend = .data$IterationB - height / 2
    ), curvature = -0.1, angle = 20) +
    # The state
    geom_rect(aes(
      fill = .data$Value,
      xmin = .data$Start,
      xmax = .data$End,
      ymin = .data$Iteration - height / 2,
      ymax = .data$Iteration + height / 2
    ), alpha = .5)
  if (!is.null(middle_lines)) {
    goijk <- goijk +
      # The median line
      geom_curve(data = dfmiddle, aes(x = .data$Middle, xend = .data$Middle.Next, y = .data$Iteration - height / 2, yend = .data$terationB - height / 2), curvature = -0.1, angle = 20, color = "black")
  }
  return(goijk)
}
