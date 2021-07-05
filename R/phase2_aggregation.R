#' @include starvz_data.R
NULL

#' Create a space-time visualization with dynamic aggregation.
#'
#' Use any state trace data to plot the task computations by ResourceId
#' over the execution time with Gantt Chart. This function dynamically aggregate
#' states with a dynamic/automatic time-step.
#'
#' @param data starvz_data with trace data
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param expand_x expand size for scale_x_continuous padding
#' @param expand_y expand size for scale_y_continuous padding
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_st_agg_dynamic(data = starvz_sample_lu)
#' }
#' @export
panel_st_agg_dynamic <- function(data = NULL,
                                 x_start = data$config$limits$start,
                                 x_end = data$config$limits$end,
                                 expand_x = data$config$expand,
                                 expand_y = data$config$st$expand) {
  if (is.null(data)) {
    return(NULL)
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }

  if (is.null(expand_y) || !is.numeric(expand_y)) {
    expand_y<- 0.01
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  starvz_log("Vinicius Entry Agg")

  # Parameters
  with.outliers <- data$config$st$outliers
  with.states <- data$config$st$aggregation$states
  # The longer outlier
  longer.outlier <- data$Application %>%
    filter(.data$Outlier == TRUE) %>%
    pull(.data$Duration) %>%
    max()
  starvz_log(paste("Longer outlier for min_time_pure definition is", longer.outlier))
  with.min_time_pure <- config_value(data$config$st$aggregation$step, longer.outlier)

  # Considering only application states
  dfw <- data$Application

  # Obtain time interval
  tstart <- dfw %>%
    .$Start %>%
    min()
  tend <- dfw %>%
    .$End %>%
    max()

  starvz_log("Vinicius Plotting Agg")

  # Plot
  gow <- ggplot() +
    geom_aggregated_states(
      data = data,
      Show.Outliers = with.outliers,
      states = with.states,
      min_time_pure = with.min_time_pure,
      base_size = data$config$base_size,
      labels = data$config$st$labels,
      expand_value_x = expand_x,
      expand_value_y = expand_y
    ) +
    xlab("Time [ms]")

  # Y Label
  gow <- gow + ylab("Application Workers")

  # The per-node ABE
  if (data$config$st$abe$active) gow <- gow + geom_abe(data)

  # add makespan
  if (data$config$st$makespan) gow <- gow + geom_makespan(data$Application, bsize = data$config$base_size)

  # add idleness
  if (data$config$st$idleness) gow <- gow + geom_idleness(data)

  # add Global CPB
  if (data$config$st$cpb) gow <- gow + geom_cpb(data)

  # check if task dependencies should be added
  if (data$config$st$tasks$active) {
    tasklist <- data$config$st$tasks$list
    levels <- data$config$st$tasks$levels

    tasksel <- gaps_backward_deps(
      data = data,
      tasks = tasklist,
      levels = levels
    )

    gow <- gow + geom_path_highlight(tasksel)
  } else {
    starvz_log("DO NOT add dependencies")
  }

  starvz_log("ViniciusExit Agg")

  gow <- gow +
    coord_cartesian(xlim = c(x_start, x_end), ylim = c(0, NA))

  return(gow)
}

#' Create a space-time visualization with static aggregation.
#'
#' Use any state trace data to plot the task computations by ResourceId
#' over the execution time with Gantt Chart. This function aggregate
#' states with a static/user-defined time-step.
#'
#' @param data starvz_data with trace data
#' @param runtime if this is runtime data
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param expand_x expand size for scale_x_continuous padding
#' @param expand_y expand size for scale_y_continuous padding
#' @param outliers print outliers on top
#' @param step time-step
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_st_agg_static(data = starvz_sample_lu)
#' }
#' @export
panel_st_agg_static <- function(data = NULL, runtime = FALSE,
                                x_start = data$config$limits$start,
                                x_end = data$config$limits$end,
                                expand_x = data$config$expand,
                                expand_y = data$config$st$expand,
                                outliers = data$config$st$outliers,
                                step = data$config$st$aggregation$step) {
  if (is.null(data)) {
    return(NULL)
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

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  if (runtime) {
    dfw_agg <- st_time_aggregation(data$StarPU, colors = data$Colors, step = agg_step)
  } else {
    dfw_agg <- st_time_aggregation(data$Application, colors = data$Colors, step = agg_step)
  }

  starvz_log("Entry Agg")

  # Considering only application or StarPU data
  if (runtime == FALSE) {
    dfw <- data$Application
  } else {
    dfw <- data$Starpu
  }
  # Obtain time interval
  tstart <- dfw %>%
    .$Start %>%
    min()
  tend <- dfw %>%
    .$End %>%
    max()

  # Calculate resources idleness
  total_time <- tend - tstart

  # yconf
  yconfm <- yconf(dfw, data$config$st$labels, data$Y)

  starvz_log("Plotting Agg")

  # Plot
  gow <- dfw_agg %>% ggplot() +
    default_theme(base_size = data$config$base_size, expand = expand_x) +
    # coord_cartesian(xlim=c(tstart, tend)) +
    xlab("Time [ms]") +
    scale_fill_manual(values = extract_colors(dfw, data$Colors)) +
    scale_y_continuous(
      breaks = yconfm$Position,
      labels = yconfm$ResourceId,
      expand = c(expand_y, 0)
    ) +
    # Print time-aggregated data
    geom_rect(aes(
      fill = .data$Task,
      xmin = .data$Start,
      xmax = .data$End,
      ymin = .data$Position + .data$TaskPosition,
      ymax = .data$Position + (.data$TaskPosition + .data$TaskHeight)
    ), alpha = .5)

  if (!runtime && outliers) {
    # Print outliers on top
    gow <- gow + geom_rect(
      data = (dfw %>% filter(.data$Outlier == TRUE)),
      aes(
        fill = .data$Value,
        xmin = .data$Start,
        xmax = .data$End,
        ymin = .data$Position,
        ymax = .data$Position + .data$Height - 0.4
      ), alpha = 1
    )
  }

  if (!runtime) {
    # Y Label
    gow <- gow + ylab("Application Workers")

    # The per-node ABE
    if (data$config$st$abe$active) gow <- gow + geom_abe(data)

    # add makespan
    if (data$config$st$makespan) gow <- gow + geom_makespan(data$Application, bsize = data$config$base_size)

    # add idleness
    if (data$config$st$idleness) gow <- gow + geom_idleness(data)

    # add Global CPB
    if (data$config$st$cpb) gow <- gow + geom_cpb(data)

    # check if task dependencies should be added
    if (data$config$st$tasks$active) {
      tasklist <- data$config$st$tasks$list
      levels <- data$config$st$tasks$levels

      tasksel <- gaps_backward_deps(
        data = data,
        tasks = tasklist,
        levels = levels
      )

      gow <- gow + geom_path_highlight(tasksel)
    } else {
      starvz_log("DO NOT add dependencies")
    }
  } else {
    # Y Label
    gow <- gow + ylab("StarPU Workers")

    # add some color palette for StarPU States
    gow <- gow + scale_fill_manual(values = starpu_colors())
  }

  gow <- gow +
    coord_cartesian(xlim = c(x_start, x_end), ylim = c(0, NA))


  starvz_log("Exit Agg")
  return(gow)
}

time_aggregation_prep <- function(dfw = NULL) {
  if (is.null(dfw)) {
    return(NULL)
  }

  dfw_initial <- dfw %>%
    rename(Task = .data$Value) %>%
    group_by(.data$ResourceId, .data$Task) %>%
    mutate(Value = 1) %>%
    select(
      -.data$Duration,
      -.data$Size, -.data$Depth, -.data$Params, -.data$JobId,
      -.data$Footprint, -.data$Tag,
      -.data$GFlop, -.data$X, -.data$Y, -.data$Iteration, -.data$Subiteration,
      -.data$Resource, -.data$Outlier, -.data$Height,
      -.data$Position
    )

  # Define the first zero
  dfw_zero_1 <- dfw_initial %>%
    slice(1) %>%
    mutate(StartN = 0, EndN = .data$Start, Value = 0)

  # Define other zeroes
  dfw_zero_N <- dfw_initial %>% mutate(StartN = .data$End, EndN = lead(.data$Start), Value = 0)

  # Row bind them
  dfw_agg_prep <- dfw_zero_1 %>%
    bind_rows(dfw_zero_N) %>%
    mutate(Start = .data$StartN, End = .data$EndN) %>%
    select(-.data$StartN, -.data$EndN) %>%
    bind_rows(dfw_initial) %>%
    ungroup()

  # Set max end time for NA cases
  dfw_agg_prep <- dfw_agg_prep %>%
    filter(!complete.cases(.)) %>%
    mutate(End = max(dfw$End)) %>%
    bind_rows(dfw_agg_prep %>% filter(complete.cases(.))) %>%
    mutate(Duration = .data$End - .data$Start) %>%
    arrange(.data$ResourceId, .data$Task, .data$Start)

  return(dfw_agg_prep)
}

time_aggregation_do <- function(dfw_agg_prep = NULL, step = NA) {
  if (is.null(dfw_agg_prep)) {
    return(NULL)
  }
  if (is.na(step)) {
    return(NULL)
  }

  dfw_agg_prep %>%
    do(remyTimeIntegrationPrep(., myStep = step)) %>%
    mutate(Start = .data$Slice, End = lead(.data$Slice), Duration = .data$End - .data$Start) %>%
    ungroup() %>%
    na.omit()
}

time_aggregation_post <- function(dfw = NULL, dfw_agg = NULL, colors = NULL) {
  if (is.null(dfw)) {
    return(NULL)
  }
  if (is.null(dfw_agg)) {
    return(NULL)
  }

  df_ResourceId <- dfw %>%
    select(
      .data$ResourceId,
      .data$Node, .data$Resource, .data$ResourceType,
      .data$Position, .data$Height
    ) %>%
    unique()
  df_Task <- colors %>%
    select(.data$Value, .data$Color) %>%
    unique()

  dfwagg_full <- dfw_agg %>%
    left_join(df_ResourceId, by = "ResourceId") %>%
    left_join(df_Task, by = c("Task" = "Value"))

  return(dfwagg_full)
}

node_spatial_aggregation <- function(dfw) {
  if (is.null(dfw)) {
    return(NULL)
  }
  dfw %>%
    group_by(.data$Node, .data$ResourceType) %>%
    mutate(N = n_distinct(.data$ResourceId)) %>%
    ungroup() %>%
    group_by(
      .data$Node, .data$Slice, .data$Task, .data$ResourceType,
      .data$Duration, .data$N
    ) %>%
    summarize(Activity = sum(.data$Value)) %>%
    mutate(Activity = .data$Activity / .data$N) %>%
    ungroup()
}


st_time_aggregation <- function(dfw = NULL, colors = NULL, StarPU.View = FALSE, step = 100) {
  if (is.null(dfw)) {
    return(NULL)
  }

  dfw_agg_prep <- time_aggregation_prep(dfw %>% select(-.data$Node, -.data$ResourceType))
  dfw_agg <- time_aggregation_do(dfw_agg_prep %>%
    group_by(.data$ResourceId, .data$Task), step)
  dfwagg_full <- time_aggregation_post(dfw, dfw_agg, colors)

  # Preparation for visualization (imitate geom_area)
  dfwagg_full %>%
    arrange(.data$ResourceId, .data$Task, .data$Slice, .data$Start) %>%
    group_by(.data$ResourceId, .data$Slice) %>%
    mutate(
      TaskHeight = (.data$Height * 0.9 * .data$Value),
      TaskPosition = cumsum(.data$TaskHeight) - .data$TaskHeight
    ) %>%
    ungroup() -> dfwagg_full_view

  return(dfwagg_full_view)
}



# Vinicius agg

# This function computes chunks by ResourceId, tasks in the same chunk will be aggregated. This function is used by aggregate_trace function.
# Input: Start,End,Duration,Value,JobId columns from a common data table with ResourceId, Start, End, Duration, Value, JobId columns
#        excludeIds - list of JobIds to do not aggregate
#        min_time_pure - states longer than this value should be kept pure.
#        states - list of states to be aggregated
# Output: a list of Chunks with the same size of the input columns (Start,End,Duration,Value,JobId)
compute_chunk <- function(Start, End, Duration, Value, JobId, excludeIds = "", states, min_time_pure) {
  chunk <- rep(0, length(Duration))
  if (states %in% c("all", "All")) {
    v <- Duration > min_time_pure # pure states
  } else {
    v <- Duration > min_time_pure | !(Value %in% c(unlist(strsplit(states, ",")))) | (JobId %in% c(unlist(strsplit(excludeIds, ",")))) # pure states: duration > threshold OR is not present in the list of states for aggregation OR is present in excludeIds list
  }
  v2 <- c(FALSE, Start[-1] > End[0:(length(v) - 1)] + min_time_pure) # "idle" states (actually white spaces once normal idle states are already present in the trace )
  chunk[v | c(FALSE, v[0:(length(v) - 1)]) | v2] <- 1
  cumsum(chunk)
}

# This function creates an aggregated version of the trace. Using the computed chunks, it calculates the Start/End values of each chunk. It also calculates a proportion (Activity) that represents the time spent by each different state.
# Input: df_native - a common data table with ResourceId, Start, End, Duration, Value, JobId columns
#        states - list of states to be aggregated
#        excludeIds - list of JobIds to do not aggregate
#        min_time_pure - states longer than this value should be kept pure.
# Output: a modified data table with new columns indicating the Chunk, the Number of tasks in this chunk (by state), Activity representing the proportion of time spent in this state, Aggregated that shows if a state is pure or not.
aggregate_trace <- function(df_native, states, excludeIds, min_time_pure) {
  df_native <- df_native %>%
    group_by(.data$ResourceId) %>%
    mutate(Chunk = compute_chunk(.data$Start, .data$End, .data$Duration, .data$Value, .data$JobId, excludeIds, states, min_time_pure))
  df_native2 <- df_native %>%
    group_by(.data$ResourceId, .data$Chunk) %>%
    mutate(Start = head(.data$Start, 1), End = tail(.data$End, 1))
  df_aggregate <- df_native2 %>%
    group_by(.data$ResourceId, .data$Chunk, .data$Value) %>%
    summarize(Duration = sum(.data$Duration), Start = head(.data$Start, 1), End = head(.data$End, 1), Number = n()) # n() is used to get the number of lines (former length(ResourceId))
  df_aggregate <- df_aggregate %>%
    group_by(.data$ResourceId, .data$Chunk) %>%
    mutate(Activity = .data$Duration / (.data$End - .data$Start))
  df_aggregate <- df_aggregate %>%
    group_by(.data$ResourceId, .data$Chunk) %>%
    mutate(aggregated = ifelse((abs(.data$Activity - 1) <= 0.00001) & .data$Number == 1, FALSE, TRUE))
  df_aggregate %>% ungroup()
}

geom_aggregated_states <- function(data = NULL, Show.Outliers = FALSE, min_time_pure = 1000, states = NA, base_size = 22, labels = "1", expand_value_x = 0.05, expand_value_y = 0.01) {
  if (is.null(data)) stop("data is NULL when given to geom_aggregated_states")
  if (is.na(states)) stop("states is NA when given to geom_aggregated_states")

  starvz_log("Starting geom_aggregated_states")

  # Define the exclude ids based on the Show.Outliers parameter
  if (Show.Outliers) {
    data$Application %>%
      filter(.data$Outlier == TRUE) %>%
      pull(.data$JobId) -> excludeIds
  } else {
    excludeIds <- c("")
  }

  # Prepare Y coordinates for left_join
  data$Y %>%
    rename(ResourceId = .data$Parent) %>%
    separate(.data$ResourceId, into = c("Node", "Resource"), remove = FALSE, extra = "drop", fill = "right") %>%
    mutate(Node = as.factor(.data$Node)) %>%
    mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource))) -> ydf

  # Do the aggregation
  data$Application %>%
    select(.data$ResourceId, .data$Start, .data$End, .data$Duration, .data$Value, .data$JobId) %>%
    aggregate_trace(states, excludeIds, min_time_pure) %>%
    left_join(
      ydf %>% select(.data$ResourceId, .data$Resource, .data$Node, .data$ResourceType, .data$Height, .data$Position),
      by = c("ResourceId" = "ResourceId")
    ) -> dfw

  # The list of geoms
  ret <- list()

  # Add the default theme
  ret[[length(ret) + 1]] <- default_theme(base_size, expand_value_x)

  # Y axis breaks and their labels
  yconfm <- yconf(dfw, labels, data$Y)
  ret[[length(ret) + 1]] <- scale_y_continuous(
    breaks = yconfm$Position + (yconfm$Height / 3), labels = yconfm$ResourceId,
    expand = c(expand_value_y, 0)
  )

  ret[[length(ret) + 1]] <- geom_rect(data = dfw, aes(
    fill = .data$Value,
    xmin = .data$Start,
    xmax = .data$End,
    ymin = .data$Position,
    ymax = .data$Position + (.data$Height - 0.4) * .data$Activity,
    alpha = .data$aggregated
  ))
  ret[[length(ret) + 1]] <- guides(alpha = FALSE)
  ret[[length(ret) + 1]] <- scale_alpha_discrete(range = c(1, .6))
  ret[[length(ret) + 1]] <- scale_fill_manual(values = extract_colors(dfw, data$Colors))

  starvz_log("Finishing geom_aggregated_states")

  return(ret)
}
