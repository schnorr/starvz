#' @include starvz_data.R
NULL

#' Create a line chart with homogeneous imbalance metrics.
#'
#' This function creates a line chart with imbalance metrics. The function
#' applies the metrics on fixed time-steps defined by the user. The metrics
#' consider that the resources are homogeneous.
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
#' panel_imbalance(data = starvz_sample_lu)
#' }
#' @export
panel_imbalance <- function(data, legend = data$config$imbalance$legend,
                            base_size = data$config$base_size,
                            expand_x = data$config$expand,
                            x_start = data$config$limits$start,
                            x_end = data$config$limits$end,
                            y_start = 0,
                            y_end = data$config$imbalance$limit,
                            step = data$config$imbalance$step) {
  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- as.double(100)
    } else {
      agg_step <- as.double(data$config$global_agg_step)
    }
  } else {
    agg_step <- as.double(step)
  }

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

  data_app <- data$Application
  utilization_per_step(data_app, agg_step) %>%
    ungroup() %>%
    group_by(.data$Step) %>%
    summarize(
      "IP" = metric_imbalance_percentage(.data$UtilizationTime),
      "IT" = metric_imbalance_time(.data$UtilizationTime, agg_step),
      "STD" = metric_imbalance_std(.data$UtilizationTime, agg_step),
      "AVG" = metric_imbalance_norm(.data$UtilizationTime, agg_step)
    ) %>%
    pivot_longer(-.data$Step, names_to = "metric", values_to = "value") %>%
    mutate(Time = .data$Step * agg_step + agg_step / 2) -> to_plot

  to_plot %>% var_imbalance_plot("Imb Metric", agg_step, base_size, expand_x) -> panel

  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  } else {
    panel <- panel + theme(legend.position = "top")
  }
  panel <- panel +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}

#' Create a line chart with heterogeneous imbalance metrics.
#'
#' This function creates a line chart with imbalance metrics. The function
#' applies the metrics on fixed time-steps defined by the user. The metrics
#' consider that the resources are heterogeneous and defined by a constant
#' power factor. For the effects of this function, one task is select for
#' computing the relative power between resources.
#'
#' @inheritParams panel_imbalance
#' @param task Task used to computer relative resource power.
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_power_imbalance(data = starvz_sample_lu)
#' }
#' @export
panel_power_imbalance <- function(data, legend = data$config$power_imbalance$legend,
                                  base_size = data$config$base_size,
                                  expand_x = data$config$expand,
                                  x_start = data$config$limits$start,
                                  x_end = data$config$limits$end,
                                  y_start = 0,
                                  y_end = data$config$power_imbalance$limit,
                                  step = data$config$power_imbalance$step,
                                  task = data$config$power_imbalance$task) {
  # Compute POWER
  task <- data$config$power_imbalance$task
  if (is.null(task)) {
    starvz_warn("Task is not available for imbalance power")
    return(NULL)
  }

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
      agg_step <- as.double(100)
    } else {
      agg_step <- as.double(data$config$global_agg_step)
    }
  } else {
    agg_step <- as.double(step)
  }


  data$Application %>%
    filter(.data$Value == task) %>%
    group_by(.data$Node, .data$ResourceId) %>%
    summarize(power = 1 / mean(.data$Duration), .groups = "drop") %>%
    .$power -> power

  if (length(power) !=
    data$Application %>%
      select(.data$Node, .data$ResourceId) %>%
      unique() %>%
      nrow()) {
    starvz_warn("Power could not be computed for all resource in imbalance power")
    return(geom_blank())
  }

  utilization_per_step(data$Application, agg_step) %>%
    ungroup() %>%
    group_by(.data$Step) %>%
    summarize(
      "IP" = metric_power_imbalance_percentage(.data$Utilization, power),
      "IT" = metric_power_imbalance_time(.data$Utilization, power),
      "STD" = metric_power_imbalance_std(.data$Utilization),
      "AVG" = metric_power_imbalance_norm(.data$Utilization, power)
    ) %>%
    pivot_longer(-.data$Step, names_to = "metric", values_to = "value") %>%
    mutate(Time = .data$Step * agg_step + agg_step / 2) -> to_plot

  to_plot %>% var_imbalance_plot("Imb Metric\nPower", agg_step, base_size, expand_x) -> panel

  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  } else {
    panel <- panel + theme(legend.position = "top")
  }
  panel <- panel +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}

#' Create a line chart with heterogeneous resources and tasks imbalance metrics
#'
#' This function creates a line chart with imbalance metrics. The function
#' applies the metrics on fixed time-steps defined by the user. The metrics
#' consider that the resources are heterogeneous, and each task has a different
#' performance per resource.
#'
#' @inheritParams panel_imbalance
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_hete_imbalance(data = starvz_sample_lu)
#' }
#' @export
panel_hete_imbalance <- function(data, legend = data$config$hete_imbalance$legend,
                                 base_size = data$config$base_size,
                                 expand_x = data$config$expand,
                                 x_start = data$config$limits$start,
                                 x_end = data$config$limits$end,
                                 y_start = 0,
                                 y_end = data$config$hete_imbalance$limit,
                                 step = data$config$hete_imbalance$step) {
  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- as.double(100)
    } else {
      agg_step <- as.double(data$config$global_agg_step)
    }
  } else {
    agg_step <- as.double(step)
  }

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

  data_app <- data$Application
  utilization_per_step_double_hetero(agg_step, data_app) %>%
    group_by(.data$Step) %>%
    summarize( # "PI"=metric_percent_imbalance(UtilizationTime, step),
      "IP" = metric_abe_imbalance_percentage(.data$Utilization, .data$ABE, .data$nmABE, agg_step),
      "IT" = metric_abe_imbalance_time(.data$Utilization, .data$ABE, agg_step),
      "STD" = metric_abe_imbalance_std(.data$Utilization),
      "AVG" = metric_abe_imbalance_norm(.data$ABE, agg_step)
    ) %>%
    pivot_longer(-.data$Step, names_to = "metric", values_to = "value") %>%
    mutate(Time = .data$Step * agg_step + agg_step / 2) -> to_plot

  to_plot %>% var_imbalance_plot("Imb Metric\nHete", agg_step, base_size, expand_x) -> panel

  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  } else {
    panel <- panel + theme(legend.position = "top")
  }
  panel <- panel +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}

#' Create a heatmap of resource utilization
#'
#' Similar to the other resource oriented plots but shows the utilization
#' per time step
#'
#' @inheritParams panel_imbalance
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_utilheatmap(data = starvz_sample_lu)
#' }
#' @export
panel_utilheatmap <- function(data, legend = data$config$utilheatmap$legend,
                              base_size = data$config$base_size,
                              expand_x = data$config$expand,
                              x_start = data$config$limits$start,
                              x_end = data$config$limits$end,
                              y_start = 0,
                              y_end = NA,
                              step = data$config$utilheatmap$step) {
  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      agg_step <- as.double(100)
    } else {
      agg_step <- as.double(data$config$global_agg_step)
    }
  } else {
    agg_step <- as.double(step)
  }
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

  utilization_per_step(data$Application, agg_step) %>%
    ungroup() %>%
    left_join(data$Y, by = c("ResourceId" = "Parent")) -> to_plot

  to_plot %>%
    select(.data$Position) %>%
    unique() %>%
    arrange(.data$Position) %>%
    .$Position -> lvl
  to_plot %>%
    mutate(Height = 1, Position = factor(.data$Position, levels = lvl)) %>%
    mutate(Position = as.integer(.data$Position)) -> to_plot

  yconfv <- yconf(to_plot %>% ungroup(), data$config$utilheatmap$labels, data$Y)

  to_plot %>%
    mutate(Time = .data$Step * agg_step + agg_step / 2) %>%
    ggplot(aes(y = .data$Position, x = .data$Time, fill = .data$Utilization)) +
    geom_raster() +
    default_theme(base_size, expand_x, legend_title = TRUE) +
    scale_y_continuous(breaks = yconfv$Position, labels = yconfv$ResourceId, expand = c(expand_x, 0)) +
    labs(y = "Utilization", x = "Time") +
    scale_fill_gradient2(
      name = "Load [%]", midpoint = 0.5, low = "blue", mid = "white",
      high = "red", limits = c(0, 1)
    ) +
    guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5)) -> panel

  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  } else {
    panel <- panel + theme(legend.position = "top")
  }
  panel <- panel +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )
  return(panel)
}

metric_percent_imbalance <- function(times, max = NULL) {
  max_util <- max(times)
  mean_util <- mean(times)
  pi <- ((max_util / mean_util) - 1)
  if (!is.null(max)) {
    pi <- pi / ((max / mean_util))
  }
  return(pi)
}

metric_imbalance_percentage <- function(times) {
  max_util <- max(times)
  mean_util <- mean(times)
  n <- length(times)
  ip <- ((max_util - mean_util) / max_util) * (n / (n - 1))
  return(ip)
}

metric_imbalance_time <- function(times, max = NULL) {
  max_util <- max(times)
  mean_util <- mean(times)
  n <- length(times)
  it <- max_util - mean_util
  if (!is.null(max)) {
    it <- it / max
  }
  return(it)
}

metric_imbalance_std <- function(times, max = NULL) {
  std <- sd(times)
  if (!is.null(max)) {
    std <- std / max
  }
  return(std)
}

metric_imbalance_norm <- function(times, max = NULL) {
  norm <- mean(abs(times))
  if (!is.null(max)) {
    norm <- norm / max
  }
  return(norm)
}

metric_power_percent_imbalance <- function(times, max = NULL) {
  max_util <- max(times)
  mean_util <- mean(times)
  pi <- ((max_util / mean_util) - 1)
  if (!is.null(max)) {
    pi <- pi / ((max / mean_util))
  }
  return(pi)
}

metric_power_imbalance_percentage <- function(utilization, power) {
  max_util <- max(utilization)
  tp <- sum(power)
  max_r <- which(utilization == max(utilization))
  norm <- sum(abs(utilization * power))
  norm <- norm / sum(power)
  it <- max_util - norm
  c1 <- it / max_util
  c2 <- tp / (tp - power[max_r])
  ip <- c1 * c2
  return(ip)
}

metric_power_imbalance_time <- function(Utilization, power) {
  max_util <- max(Utilization)

  norm <- sum(abs(Utilization * power))
  norm <- norm / sum(power)
  it <- max_util - norm
  return(it)
}

metric_power_imbalance_std <- function(utilization) {
  std <- sd(utilization)
  return(std)
}

metric_power_imbalance_norm <- function(utilization, power) {
  norm <- sum(abs(utilization * power))
  norm <- norm / sum(power)
  return(norm)
}

metric_abe_imbalance_percentage <- function(utilization, ABE, nmABE, step) {
  max_util <- max(utilization)
  norm <- ABE / step
  it <- max_util - norm
  c1 <- it / max_util
  c2 <- nmABE / ABE
  ip <- c1 * c2
  ip <- max(0, min(1, ip))
  return(ip)
}

metric_abe_imbalance_time <- function(Utilization, ABE, step) {
  max_util <- max(Utilization)

  norm <- ABE / step

  it <- max_util - norm
  it <- max(0, min(1, it))
  return(it)
}

metric_abe_imbalance_std <- function(utilization) {
  std <- sd(utilization)
  return(std)
}

metric_abe_imbalance_norm <- function(ABE, step) {
  norm <- ABE / step
  return(norm)
}

utilization_per_step <- function(data_app, step) {
  min_time <- min(data_app$Start)
  max_time <- max(data_app$End)

  data_app %>%
    filter(.data$Start >= 0) %>%
    select(.data$JobId, .data$Duration, .data$Node, .data$ResourceId, .data$ResourceType, .data$Start, .data$End) %>%
    mutate(
      SStep = as.integer(floor(.data$Start / step)),
      EStep = as.integer(floor(.data$End / step)),
      UtilFirst = ifelse(.data$SStep != .data$EStep, step - .data$Start %% step, .data$Duration),
      UtilLast = .data$End %% step
    ) %>%
    mutate(FullUtil = mapply(function(x, y) seq(x, y, by = 1), .data$SStep, .data$EStep)) %>%
    unnest(cols = c(.data$FullUtil)) %>%
    mutate(Util = case_when(
      (.data$FullUtil == .data$SStep) ~ .data$UtilFirst,
      (.data$FullUtil == .data$EStep) ~ .data$UtilLast,
      TRUE ~ step
    )) %>%
    rename(Step = .data$FullUtil) %>%
    select(-.data$SStep, -.data$EStep, -.data$UtilFirst, -.data$UtilLast) %>%
    group_by(.data$ResourceId, .data$Node, .data$ResourceType, .data$Step) %>%
    summarize(Utilization = sum(.data$Util) / step, .groups = "drop") %>%
    complete(.data$ResourceId, Step = 0:(max_time / step), fill = list(Utilization = 0)) %>%
    mutate(UtilizationTime = .data$Utilization * step)
}


var_imbalance_plot <- function(data, name, step, base_size, expand) {
  col <- brewer.pal(n = 5, name = "Set1")
  data %>%
    select(.data$Step) %>%
    unique() %>%
    mutate(Step = .data$Step * step) -> steps
  data %>% ggplot(aes(x = .data$Time, y = .data$value, colour = .data$metric)) +
    default_theme(base_size, expand) +
    geom_point(size = 1) +
    # geom_vline(data=steps, aes(xintercept=Step), alpha=0.2) +
    geom_line() +
    theme(panel.grid.major.y = element_line(color = "grey80")) +
    scale_color_manual(limits = c("PI", "IP", "IT", "AVG", "STD"), values = col) +
    labs(y = name, x = "Step") +
    ylim(0, 1)
}

# Consider heterogenery tasks and resources
# TODO: Last step is bogus
utilization_per_step_double_hetero <- function(step, df) {
  max_time <- max(df$End)

  df %>%
    filter(.data$Start > 0) %>%
    select(
      .data$JobId, .data$Value, .data$Duration, .data$Node,
      .data$ResourceId, .data$ResourceType, .data$Start, .data$End
    ) %>%
    mutate(
      SStep = as.integer(floor(.data$Start / step)),
      EStep = as.integer(floor(.data$End / step)),
      UtilFirst = ifelse(.data$SStep != .data$EStep, step - .data$Start %% step, .data$Duration),
      UtilLast = .data$End %% step
    ) %>%
    mutate(FullUtil = mapply(function(x, y) seq(x, y, by = 1), .data$SStep, .data$EStep)) %>%
    unnest(cols = c(.data$FullUtil)) %>%
    mutate(Util = case_when(
      (.data$FullUtil == .data$SStep) ~ .data$UtilFirst,
      (.data$FullUtil == .data$EStep) ~ .data$UtilLast,
      TRUE ~ step
    )) %>%
    rename(Step = .data$FullUtil) %>%
    select(-.data$SStep, -.data$EStep, -.data$UtilFirst, -.data$UtilLast) -> temp

  temp %>%
    group_by(.data$ResourceId, .data$Node, .data$ResourceType, .data$Step) %>%
    mutate(PTask = .data$Util / .data$Duration) %>%
    ungroup() %>%
    group_by(.data$Node, .data$Value, .data$Step) %>%
    summarize(NTasks = sum(.data$PTask), .groups = "drop") -> tasks_per_slice

  temp %>%
    group_by(.data$ResourceId, .data$Node, .data$ResourceType, .data$Step) %>%
    summarize(Utilization = sum(.data$Util) / step, .groups = "drop") %>%
    complete(.data$ResourceId, Step = 0:(max_time / step), fill = list(Utilization = 0)) %>%
    mutate(UtilizationTime = .data$Utilization * step) -> util

  util %>%
    group_by(.data$Step) %>%
    arrange(-.data$Utilization) %>%
    slice(1) %>%
    select(.data$Step, .data$ResourceType) -> max_res

  tasks_per_slice %>% rename(freq = .data$NTasks) -> ts
  df %>%
    select(.data$ResourceType, .data$ResourceId) %>%
    distinct() %>%
    group_by(.data$ResourceType) %>%
    mutate(n = n()) %>%
    select(.data$ResourceType, n) %>%
    distinct() -> n_resources

  temp %>%
    select(.data$Value, .data$ResourceType, .data$Duration, .data$Step) %>%
    ungroup() %>%
    rename(codelet = .data$Value) %>%
    group_by(.data$ResourceType, .data$codelet, .data$Step) %>%
    mutate(Outlier = ifelse(.data$Duration > outlier_definition(.data$Duration), TRUE, FALSE)) %>%
    filter(!.data$Outlier) %>%
    summarize(mean = mean(.data$Duration), .groups = "drop") %>%
    left_join(n_resources, by = c("ResourceType")) -> ri

  ts %>%
    select(.data$Step) %>%
    unique() %>%
    rowwise() %>%
    mutate(ABE = starpu_apply_abe_per_slice(.data$Step, ri, ts)) %>%
    mutate(nmABE = starpu_apply_abe_per_slice(.data$Step, ri, ts, max_res)) %>%
    mutate(
      ABE = ifelse(.data$ABE > step, step, .data$ABE),
      nmABE = ifelse(.data$nmABE > step, step, .data$nmABE)
    ) %>%
    ungroup() -> ABE_steps

  util %>% left_join(ABE_steps, by = c("Step")) -> ret
  return(ret)
}
