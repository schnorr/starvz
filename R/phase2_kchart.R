
#' Create a special chart for applications with iterations
#'
#' Plot iteraionts Y over Time X
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param middle_lines plot a middle line
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param per_node Create node facets
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_kiteration(data = starvz_sample_lu)
#' @export
panel_kiteration <- function(data = NULL,
                             middle_lines = data$config$kiteration$middlelines,
                             base_size = data$config$base_size,
                             expand_x = data$config$expand,
                             legend = data$config$ready$legend,
                             x_start = data$config$limits$start,
                             x_end = data$config$limits$end,
                             per_node = data$config$kiteration$pernode) {
  starvz_check_data(data, tables = list("Application" = c("Iteration")))

  dfw <- data$Application

  if (length(middle_lines) == 0) {
    middle_lines <- NULL
  }

  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  # Prepare for colors
  data$Colors %>%
    select(.data$Value, .data$Color) %>%
    unique() %>%
    .$Color -> appColors
  appColors %>% setNames(data$Colors %>% select(.data$Value, .data$Color) %>% unique() %>% .$Value) -> appColors

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

  if (per_node) {
    goijk <- goijk + facet_wrap(~Node, ncol = 1)
  }

  goijk <- goijk +
    coord_cartesian(
      xlim = c(x_start, x_end)
    )

  if (!legend) {
    goijk <- goijk + theme(legend.position = "none")
  } else {
    goijk <- goijk + theme(legend.spacing.x = unit(0.2, "cm"))
  }

  return(goijk)
}
