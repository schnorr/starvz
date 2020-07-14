#' @include starvz_data.R

state_pmtool_chart <- function(data = NULL) {
  if (is.null(data)) stop("data provided to state_chart is NULL")

  # Get traces
  dfw <- data$Pmtool_states

  loginfo("Entry of state_pmtool_chart")

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
    default_theme(data$config$base_size, data$config$expand)

  # Add states and outliers if requested
  gow <- gow + geom_pmtool_states(data)

  # add makespan
  if (data$config$pmtool$makespan) gow <- gow + geom_makespan_pmtool(data)

  loginfo("Exit of state_pmtool_chart")
  return(gow)
}



k_chart_pmtool <- function(data = NULL, colors = NULL) {
  if (is.null(data$Pmtool_states)) stop("dfw provided to pm k_chart is NULL")

  dfw <- data$Pmtool_states %>%
    filter(.data$sched == data$config$pmtool$state$sched)

  # Prepare for colors
  colors %>%
    select(.data$Value, .data$Color) %>%
    unique() %>%
    .$Color -> appColors
  appColors %>% setNames(colors %>% select(.data$Value, .data$Color) %>% unique() %>% .$Value) -> appColors

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
  return(goijk)
}



geom_pmtool_states <- function(data = NULL) {
  if (is.null(data)) stop("data is NULL when given to geom_pmtool_states")


  dfw <- data$Pmtool_states %>% filter(.data$sched == data$config$pmtool$state$sched)


  loginfo("Starting geom_pmtool_states")

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


  loginfo("Finishing geom_pmtool_states")

  return(ret)
}


geom_pmtool_bounds <- function(data = NULL) {
  if (is.null(data)) stop("data is NULL when given to geom_pmtool_bounds")

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
  loginfo(paste("makespan pm tool is", tend))
  height <- dfw %>%
    select(.data$Position) %>%
    na.omit() %>%
    pull(.data$Position) %>%
    max()
  loginfo(paste("max height for makespan is", height))
  ret <- geom_text(data = data.frame(), x = tend, y = height * .5, aes(label = round(tend, 0)), angle = 90, size = bsize / 4)
  return(ret)
}
