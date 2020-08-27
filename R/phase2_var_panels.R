#' Create a line chart panel with ready tasks submission
#'
#' Use the Variable traces to create a line chart panel with ready tasks
#' submission per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_ready(data = starvz_sample_lu)
#' @export
panel_ready <- function(data, legend = data$config$ready$legend,
                        base_size = data$config$base_size,
                        expand_x = data$config$expand,
                        x_start = data$config$limits$start,
                        x_end = data$config$limits$end,
                        y_start = 0,
                        y_end = data$config$ready$limit,
                        step = data$config$ready$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  panel <- data$Variable %>%
    filter(grepl("sched", .data$ResourceId), grepl("Ready", .data$Type)) %>%
    var_integration_segment_chart(
      step = agg_step,
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}


#' Create a line chart panel with submitted tasks submission
#'
#' Use the Variable traces to create a line chart panel with submitted tasks
#' submission per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_submitted(data = starvz_sample_lu)
#' @export
panel_submitted <- function(data, legend = data$config$submitted$legend,
                            base_size = data$config$base_size,
                            expand_x = data$config$expand,
                            x_start = data$config$limits$start,
                            x_end = data$config$limits$end,
                            y_start = 0,
                            y_end = data$config$submitted$limit,
                            step = data$config$submitted$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  panel <- data$Variable %>%
    filter(grepl("sched", .data$ResourceId), grepl("Submitted", .data$Type)) %>%
    var_integration_segment_chart(
      step = agg_step,
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}


#' Create a line chart panel with used memory
#'
#' Use the Variable traces to create a line chart panel with used memory
#' per resource, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_usedmemory(data = starvz_sample_lu)
#' @export
panel_usedmemory <- function(data, legend = data$config$usedmemory$legend,
                             base_size = data$config$base_size,
                             expand_x = data$config$expand,
                             x_start = data$config$limits$start,
                             x_end = data$config$limits$end,
                             y_start = 0,
                             y_end = data$config$usedmemory$limit,
                             step = data$config$usedmemory$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  panel <- data$Variable %>%
    filter(grepl("Used", .data$Type)) %>%
    var_integration_segment_chart(
      step = agg_step,
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}


#' Create a line chart panel with GFlops
#'
#' Use the Variable traces to create a line chart panel with GFlops
#' per resource, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_gflops(data=starvz_sample_lu)
#' }
#' @export
panel_gflops <- function(data, legend = data$config$gflops$legend,
                         base_size = data$config$base_size,
                         expand_x = data$config$expand,
                         x_start = data$config$limits$start,
                         x_end = data$config$limits$end,
                         y_start = 0,
                         y_end = data$config$gflops$limit,
                         step = data$config$gflops$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  panel <- data$Variable %>%
    filter(.data$Type == "GFlops") %>%
    var_integration_segment_chart(
      step = agg_step,
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}


#' Create a line chart panel with GPU bandwidth
#'
#' Use the Variable traces to create a line chart panel with GPU bandwidth
#' per resource, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @param total show total bandwidth of the execution per resource
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_gpubandwidth(data=starvz_sample_lu)
#' @export
panel_gpubandwidth <- function(data, legend = data$config$gpubandwidth$legend,
                               base_size = data$config$base_size,
                               expand_x = data$config$expand,
                               x_start = data$config$limits$start,
                               x_end = data$config$limits$end,
                               y_start = 0,
                               y_end = data$config$gpubandwidth$limit,
                               step = data$config$gpubandwidth$step,
                               total = data$config$gpubandwidth$total) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  bandwidth_data <- data$Variable %>%
    filter(grepl("MEMMANAGER", .data$ResourceId), grepl("Out", .data$Type)) %>%
    # Remove the MANAGER0, which is CPU-only
    # TODO: After the logical OR there is a support for single-node StarPU traces
    filter(.data$Resource != "MEMMANAGER0" | .data$Node != "MEMMANAGER0")

  panel <- bandwidth_data %>%
    var_integration_segment_chart(
      step = agg_step,
      ylabel = "GPU\n(MB/s)",
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )

  if (total) {
    ms <- bandwidth_data %>%
      group_by(.data$Type, .data$Node, .data$ResourceType, .data$Start, .data$End, .data$Duration) %>%
      summarize(Value = sum(.data$Value), N = n()) %>%
      rename(ResourceId = .data$Node)
    y_size <- layer_scales(panel)$y$range$range[2]
    panel <- panel + ms %>% var_chart_text(tstart = x_start, tend = x_end, y_end = y_size)
  }
  return(panel)
}


#' Create a line chart panel with MPI concurrent
#'
#' Use the Variable traces to create a line chart panel with MPI concurrent
#' per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_mpiconcurrent(data=starvz_sample_lu)
#' @export
panel_mpiconcurrent <- function(data, legend = data$config$mpiconcurrent$legend,
                                base_size = data$config$base_size,
                                expand_x = data$config$expand,
                                x_start = data$config$limits$start,
                                x_end = data$config$limits$end,
                                y_start = 0,
                                y_end = data$config$mpiconcurrent$limit,
                                step = data$config$mpiconcurrent$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  if ((data$Link %>% filter(grepl("mpicom", .data$Key)) %>% nrow()) == 0) {
    starvz_warn("There aren't any information on MPI, ignoring it.")
    return(geom_blank())
  }

  panel <- concurrent_mpi(data) %>%
    var_integration_segment_chart(
      step = agg_step,
      ylabel = "Concurrent\nMPI Tasks Send",
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )

  return(panel)
}



#' Create a line chart panel with MPI concurrent out
#'
#' Use the Variable traces to create a line chart panel with MPI concurrent out
#' per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_mpiconcurrentout(data=starvz_sample_lu)
#' @export
panel_mpiconcurrentout <- function(data, legend = data$config$mpiconcurrentout$legend,
                                   base_size = data$config$base_size,
                                   expand_x = data$config$expand,
                                   x_start = data$config$limits$start,
                                   x_end = data$config$limits$end,
                                   y_start = 0,
                                   y_end = data$config$mpiconcurrentout$limit,
                                   step = data$config$mpiconcurrentout$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  if ((data$Link %>% filter(grepl("mpicom", .data$Key)) %>% nrow()) == 0) {
    starvz_warn("There aren't any information on MPI, ignoring it.")
    return(geom_blank())
  }

  panel <- concurrent_mpi(data, out = TRUE) %>%
    var_integration_segment_chart(
      step = agg_step,
      ylabel = "Concurrent\nMPI Tasks Recv",
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )

  return(panel)
}


#' Create a line chart panel with MPI bandwidth
#'
#' Use the Variable traces to create a line chart panel with MPI bandwidth
#' per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_mpibandwidth(data=starvz_sample_lu)
#' @export
panel_mpibandwidth <- function(data, legend = data$config$mpibandwidth$legend,
                               base_size = data$config$base_size,
                               expand_x = data$config$expand,
                               x_start = data$config$limits$start,
                               x_end = data$config$limits$end,
                               y_start = 0,
                               y_end = data$config$mpibandwidth$limit,
                               step = data$config$mpibandwidth$step) {

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

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- 100
    } else {
      agg_step <- data$config$global_agg_step
    }
  } else {
    agg_step <- step
  }

  if ((data$Link %>% filter(grepl("mpicom", .data$Key)) %>% nrow()) == 0) {
    starvz_warn("There aren't any information on MPI, ignoring it.")
    return(geom_blank())
  }

  mpi_out <- data$Variable %>% filter(grepl("mpict", .data$ResourceId), grepl("Out", .data$Type))

  panel <- mpi_out %>%
    var_integration_segment_chart(
      step = agg_step,
      ylabel = "MPI\n(MB/s)",
      base_size = base_size,
      expand = expand_x,
      legend = legend
    ) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )

  return(panel)
}
