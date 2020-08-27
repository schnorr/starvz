
#' Create a space time visualization of pmtool application as a Gantt chart
#'
#' Use the PMTOOL Application trace data to plot the task computations by ResourceId
#' over the execution time.
#'
#' @param data starvz_data with trace data
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param legend enable/disable legends
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_pmtool_st(data = starvz_sample_lu)
#' @export
panel_pmtool_st <- function(data = NULL,
                            legend = data$config$pmtool$state$legend,
                            base_size = data$config$base_size,
                            expand_x = data$config$expand,
                            x_start = data$config$limits$start,
                            x_end = data$config$limits$end) {
  starvz_check_data(data, tables = list("Pmtool_states" = c("Value", "sched")))

  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  # Get traces
  dfw <- data$Pmtool_states

  starvz_log("Entry of state_pmtool_chart")

  # Filter
  dfwapp <- dfw %>%
    filter(.data$sched == data$config$pmtool$state$sched)

  # Obtain time interval
  tstart <- dfwapp %>%
    .$Start %>%
    min()
  tend <- dfwapp %>%
    .$End %>%
    max()

  # Plot
  gow <- ggplot() +
    default_theme(base_size, expand_x)

  # Add states and outliers if requested
  gow <- gow + geom_pmtool_states(data)

  # add makespan
  if (data$config$pmtool$makespan) gow <- gow + geom_makespan_pmtool(data)

  gow <- gow + coord_cartesian(
    xlim = c(x_start, x_end)
  )

  if (!legend) {
    gow <- gow + theme(legend.position = "none")
  } else {
    gow <- gow + theme(legend.position = "top")
  }


  starvz_log("Exit of state_pmtool_chart")
  return(gow)
}

#' Create a special chart for applications with iterations with PMtool data
#'
#' Plot iteraionts Y over Time X of PMtool data
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_pmtool_kiteration(data = starvz_sample_lu)
#' @export
panel_pmtool_kiteration <- function(data = NULL,
                                    legend = data$config$pmtool$kiteration$legend,
                                    x_start = data$config$limits$start,
                                    x_end = data$config$limits$end) {
  starvz_check_data(data, tables = list("Pmtool_states" = c("Value", "sched", "Iteration")))

  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  dfw <- data$Pmtool_states %>%
    filter(.data$sched == data$config$pmtool$state$sched)

  # Prepare for colors
  data$Colors %>%
    select(.data$Value, .data$Color) %>%
    unique() %>%
    .$Color -> appColors
  appColors %>% setNames(data$Colors %>% select(.data$Value, .data$Color) %>% unique() %>% .$Value) -> appColors

  # Prepare for borders
  dfborders <- dfw %>%
    group_by(.data$Iteration) %>%
    summarize(Start = min(.data$Start), End = max(.data$End)) %>%
    mutate(IterationB = lead(.data$Iteration), StartB = lead(.data$Start)) %>%
    mutate(IterationE = lead(.data$Iteration), EndB = lead(.data$End)) %>%
    na.omit()

  # Height of each bar
  height <- 0.8

  dfw %>% ggplot() +
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual(values = appColors) +
    theme_bw(base_size = 12) +
    xlab("Time [ms]") +
    ylab("PMTool\nIteration") +
    default_theme(data$config$base_size, data$config$expand) +
    # Keep the alpha = 1 even if we use an alpha below
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    scale_y_reverse() +
    # The start border
    geom_curve(data = dfborders, aes(
      x = .data$Start, xend = .data$StartB,
      y = .data$Iteration + height, yend = .data$IterationB + height
    ), curvature = 0.1, angle = 20) +
    # The end border
    geom_curve(data = dfborders, aes(x = .data$End, xend = .data$EndB, y = .data$Iteration, yend = .data$IterationB), curvature = -0.1, angle = 20) +
    # The state
    geom_rect(aes(
      fill = .data$Value,
      xmin = .data$Start,
      xmax = .data$End,
      ymin = .data$Iteration,
      ymax = .data$Iteration + height
    ), alpha = .5) -> goijk

  goijk <- goijk +
    coord_cartesian(
      xlim = c(x_start, x_end)
    )

  if (!legend) {
    goijk <- goijk + theme(legend.position = "none")
  } else {
    goijk <- goijk + theme(legend.position = "top")
  }
  return(goijk)
}



geom_pmtool_states <- function(data = NULL) {
  if (is.null(data)) stop("data is NULL when given to geom_pmtool_states")


  dfw <- data$Pmtool_states %>% filter(.data$sched == data$config$pmtool$state$sched)


  starvz_log("Starting geom_pmtool_states")

  ret <- list()

  # Color mapping
  ret[[length(ret) + 1]] <- scale_fill_manual(values = extract_colors(dfw, data$Colors))

  # Y axis breaks and their labels
  gg <- data$Application
  yconfm <- yconf(gg, data$config$st$labels)
  ret[[length(ret) + 1]] <- scale_y_continuous(breaks = yconfm$Position + (yconfm$Height / 3), labels = yconfm$ResourceId, expand = c(data$config$expand, 0))
  # Y label
  ret[[length(ret) + 1]] <- ylab("Pmtool Workers")

  # Add states
  ret[[length(ret) + 1]] <-
    geom_rect(data = dfw, aes(
      fill = .data$Value,
      xmin = .data$Start,
      xmax = .data$End,
      ymin = .data$Position,
      ymax = .data$Position + .data$Height - 0.4
    ), alpha = 0.5, color = ifelse(data$config$st$rect_outline, "black", NA))


  starvz_log("Finishing geom_pmtool_states")

  return(ret)
}


geom_pmtool_bounds <- function(data = NULL) {
  if (is.null(data$Pmtool)) {
    starvz_warn("Pmtool bounds config is active but the data is NULL")
    return(NULL)
  }

  dftemp <- data$Application %>%
    filter(grepl("CPU|CUDA", .data$ResourceId)) %>%
    select(.data$Node, .data$Resource, .data$ResourceType, .data$Duration, .data$Value, .data$Position, .data$Height)
  # Y position
  minPos <- dftemp %>%
    pull(.data$Position) %>%
    min()
  minHeight <- dftemp %>%
    pull(.data$Height) %>%
    min()
  maxPos <- dftemp %>%
    pull(.data$Position) %>%
    max() + minHeight / 1.25

  # Filter
  dfwapp <- data$Application

  # Obtain time interval
  tstart <- dfwapp %>%
    .$Start %>%
    min()

  data$Pmtool %>%
    mutate(
      MinPosition = minPos,
      MaxPosition = maxPos
    ) -> df.pmtool

  bsize <- data$config$base_size / 5

  bound <- df.pmtool %>%
    filter(!is.na(.data$Time)) %>%
    mutate(Label = data$config$pmtool$bounds$label) %>%
    mutate(Label = ifelse(.data$Label, paste0(.data$Alg, ": ", round(.data$Time, 0)),
      round(.data$Time, 0)
    )) %>%
    unique() %>%
    filter(.data$Alg %in% data$config$pmtool$bounds$alg)

  ret <- list(
    geom_segment(
      data = bound %>% filter(.data$Bound == FALSE),
      aes(
        x = .data$Time + tstart,
        xend = .data$Time + tstart,
        y = .data$MinPosition,
        yend = .data$MaxPosition
      ), size = 5, alpha = .7, color = "lightgrey"
    ),
    geom_text(
      data = bound %>% filter(.data$Bound == FALSE),
      aes(
        x = .data$Time + tstart, y = .data$MinPosition + (.data$MaxPosition - .data$MinPosition) / 2,
        label = .data$Label
      ), angle = 90, color = "black", size = bsize, fontface = "italic"
    )
  )

  ret[[length(ret) + 1]] <- list(
    geom_segment(
      data = bound %>% filter(.data$Bound == TRUE),
      aes(x = .data$Time + tstart, xend = .data$Time + tstart, y = .data$MinPosition, yend = .data$MaxPosition), size = 5, alpha = .7, color = "gainsboro"
    ),
    geom_text(data = bound %>% filter(.data$Bound == TRUE), aes(x = .data$Time + tstart, y = .data$MinPosition + (.data$MaxPosition - .data$MinPosition) / 2, label = .data$Label), angle = 90, color = "black", size = bsize)
  )

  return(ret)
}

geom_makespan_pmtool <- function(data = NULL) {
  if (is.null(data)) stop("data provided for geom_makespan_pmtool is NULL")
  dfw <- data$Pmtool_states

  bsize <- data$config$base_size

  tend <- dfw %>%
    filter(.data$sched == data$config$pmtool$state$sched) %>%
    pull(.data$End) %>%
    max()
  starvz_log(paste("makespan pm tool is", tend))
  height <- dfw %>%
    select(.data$Position) %>%
    na.omit() %>%
    pull(.data$Position) %>%
    max()
  starvz_log(paste("max height for makespan is", height))
  ret <- geom_text(data = data.frame(), x = tend, y = height * .5, aes(label = round(tend, 0)), angle = 90, size = bsize / 4)
  return(ret)
}
