#' Shows nodes events
#'
#' Plot a Gantt chart for all nodes where program events are states
#' An example of event is the fxt_flush
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
#' panel_node_events(data = starvz_sample_lu)
#' @export
panel_node_events <- function(data = NULL,
                              legend = data$config$node_events$legend,
                              base_size = data$config$base_size,
                              expand_x = data$config$expand,
                              x_start = data$config$limits$start,
                              x_end = data$config$limits$end) {
  starvz_check_data(data, tables = list("Events" = c("Value")))

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

  data$Events %>% filter(.data$Type == "program event type") -> program_events

  program_events %>%
    group_by(.data$Container) %>%
    filter(.data$Value == "fxt_start_flush" | .data$Value == "fxt_stop_flush") %>%
    mutate(Last_Value = lag(.data$Value), Last_Start = lag(.data$Start)) %>%
    filter(.data$Value == "fxt_stop_flush") %>%
    mutate(Name = "fxt_flush") -> matched_fxt_flush_events

  # Just to Make sure
  matched_fxt_flush_events %>%
    filter(.data$Value == .data$Last_Value) %>%
    nrow() -> is_wrong
  if (is_wrong > 0) {
    starvz_warn("Something wrong matching fxt flush events")
  }

  matched_fxt_flush_events %>%
    select(.$Container) %>%
    distinct() %>%
    arrange(.data$Container) %>%
    mutate(Id = as.numeric(.data$Container) + 0.5) -> cont_all_names

  cont_all_names %>% .$Container -> cont_names
  cont_all_names %>% .$Id -> cont_breaks

  matched_fxt_flush_events %>%
    mutate(
      y = as.numeric(.data$Container) + 0.1,
      ymax = .data$y + 0.9
    ) %>%
    ggplot(aes(
      ymin = .data$y, ymax = .data$ymax,
      xmin = .data$Start, xmax = .data$Last_Start,
      fill = .data$Name
    )) +
    default_theme(base_size, expand_x) +
    geom_rect() +
    scale_y_continuous(breaks = cont_breaks, labels = cont_names, expand = c(expand_x, 0)) +
    ylab("Events") +
    coord_cartesian(
      xlim = c(x_start, x_end)
    ) -> panel

  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  } else {
    panel <- panel + theme(legend.position = "top")
  }

  return(panel)
}
