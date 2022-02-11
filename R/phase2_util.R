#' @include starvz_data.R

check_arrow <- function() {
  if (!arrow_available()) {
    starvz_warn("R package arrow was not property installed, use: install_arrow()")
  }
}

extract_colors <- function(dfw = NULL, colors = NULL) {
  if (is.null(dfw)) {
    return(NULL)
  }
  if (is.null(colors)) {
    return(NULL)
  }

  dfw <- dfw %>% ungroup()

  dfw %>%
    select(.data$Value) %>%
    unique() %>%
    left_join(colors, by = c("Value")) %>%
    .$Color %>%
    setNames(dfw %>% select(.data$Value) %>% unique() %>% .$Value)
}

yconf <- function(dfw = NULL, option = "ALL", Y = NULL, show_mpi = TRUE) {
  if (is.null(dfw)) {
    return(NULL)
  }

  dfw %>% mutate(Node = as.integer(as.character(.data$Node))) -> dfw
  dfw %>%
    mutate(ResourceId = factor(.data$ResourceId)) %>%
    mutate(ResourceId = factor(.data$ResourceId,
      levels = mixedsort(levels(.data$ResourceId))
    )) -> dfw
  if (option == "1CPU_per_NODE") { # First
    # One CPU per node
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      ungroup()
  } else if (option == "1GPU_per_NODE") { # Last
    # One GPU per node
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(n()) %>%
      ungroup()
  } else if (option == "NODES_only") { # First
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      mutate(ResourceId = .data$Node) %>%
      ungroup()
    show_mpi <- FALSE
  } else if (option == "NODES_1_in_10") { # First
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      mutate(ResourceId = .data$Node) %>%
      ungroup() %>%
      mutate(Node = as.integer(as.character(.data$Node))) %>%
      arrange(.data$Node) %>%
      slice(seq(1, n(), 10))
  } else if (option == "ALL") {
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node, .data$ResourceType) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      ungroup()
  } else { # First and Last ("FIRST_LAST") or anything else
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node, .data$ResourceType) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(c(1, n())) %>%
      ungroup()
  }
  if (!is.null(Y) & show_mpi == TRUE) {
    y_conf <- y_conf %>%
      mutate(
        ResourceId = as.character(.data$ResourceId),
        ResourceType = as.character(.data$ResourceType)
      )
    Y %>%
      filter(.data$Type == "Communication Thread State") %>%
      mutate(ResourceId = .data$Parent) %>%
      separate(.data$Parent, c("Node", "ResourceType"), extra = "drop", fill = "right") %>%
      mutate(
        Node = as.integer(.data$Node),
        ResourceId = as.character(.data$ResourceId)
      ) %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      bind_rows(y_conf) %>%
      mutate(
        ResourceId = as.factor(.data$ResourceId),
        ResourceType = as.factor(.data$ResourceType)
      ) -> y_conf
  }
  return(y_conf)
}

userYLimit <- function(obj, configuration, xlimits) {
  if (!is.null(configuration)) {
    # if there is an user vertical scale defined, use it
    tpScale <- list(
      coord_cartesian(
        xlim = xlimits,
        ylim = c(0, configuration)
      )
    )
    obj <- obj + tpScale
  }
  return(obj)
}

outlier_definition <- function(x) {
  (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}

#' Create the title of StarVZ plot
#'
#' Use the directory of traces name to create a plot title
#'
#' @param data starvz_data with trace data
#' @param title title text, if NULL it will fallback to data$Origin then to "Null Title"
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_title(data = starvz_sample_lu)
#' @export
panel_title <- function(data, title = data$config$title$text) {
  if (is.null(title)) {
    if (is.null(data$Origin)) {
      title <- "Null Title"
    } else {
      title <- data$Origin
    }
  }
  ggplot() +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = .5, y = .5, label = title, size = rel(8))
}

#' Create the diagnostig plot for the regression model
#'
#' Use the starvz Application data to observe how the regression model used
#' in the task anomaly classification fits the data.
#'
#' @param data starvz_data with trace data
#' @param freeScales free X,Y scales for each task and resource type combination
#' @param model_type Choose the regression model type to use
#' @return A ggplot object
#' @include starvz_data.R
#' @usage panel_model_gflops(data,
#'          freeScales = TRUE, model_type = "LOG_LOG")
#' @examples
#' \dontrun{
#' panel_model_gflops(data = starvz_sample_sample)
#' }
#' @export
panel_model_gflops <- function(
                           data,
                           freeScales = TRUE,
                           model_type = "LOG_LOG") {

  # create the base ggplot object that is enhanced according to the model_type
  model_panel <- data$Application %>%
    filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
    filter(.data$GFlop > 0) %>%
    ggplot(aes(x = .data$GFlop, y = .data$Duration)) +
    theme_bw(base_size = data$config$base_size) +
    labs(y = "Duration (ms)", x = "GFlops") +
    scale_color_manual(values = c("black", "orange")) +
    theme(
      legend.position = "top",
      strip.text.x = element_text(size = rel(1)),
      axis.text.x = element_text(angle = 45, vjust = 0.5)
    ) +
    labs(color = "Anomaly")

  # define de log-log LR model to later use
  model_LR_log <- function(df) {
    lm(log(Duration) ~ log(GFlop), data = df)
  }


  # Linear Regression model_type with Duration ~ GFlop
  if (model_type == "LR") {
    model_panel <- model_panel +
      geom_point(alpha = .5) +
      geom_smooth(method = "lm", formula = "y ~ x", color = "green", fill = "blue") +
      ggtitle("Using LR model: Duration ~ GFlop")
    # log-log transformed linear regression
  } else if (model_type == "LOG_LOG") {
    task_model <- model_LR_log <- function(df) {
      lm(log(Duration) ~ log(GFlop), data = df)
    }
    # Step 1: apply the model to each task, considering the ResourceType
    model_fit <- data$Application %>%
      filter(grepl("qrt", .data$Value)) %>%
      # cannot have zero gflops
      filter(.data$GFlop > 0) %>%
      unique() %>%
      group_by(.data$ResourceType, .data$Value) %>%
      nest() %>%
      mutate(model = map(.data$data, task_model)) %>%
      mutate(Prediction = map(.data$model, function(model) {
        data_predict <- suppressWarnings(predict(model, interval = "prediction", level = 0.95))
        data_predict %>%
          tibble(fit_LR = exp(.[, 1]), lwr_LR = exp(.[, 2]), upr_LR = exp(.[, 3])) %>%
          select(.data$fit_LR, .data$upr_LR, .data$lwr_LR)
      })) %>%
      unnest(c(.data$data, .data$Prediction)) %>%
      ungroup() %>%
      select(.data$fit_LR, .data$lwr_LR, .data$upr_LR, .data$JobId)

    # fit log models over data
    model_data <- data$Application %>%
      filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
      filter(.data$GFlop > 0) %>%
      left_join(model_fit, by = ("JobId")) %>%
      unique() %>%
      group_by(.data$ResourceType, .data$Value) %>%
      nest() %>%
      mutate(model_log = map(.data$data, model_LR_log)) %>%
      ungroup() %>%
      select(-.data$model_log) %>%
      unnest(cols = c(.data$data)) %>%
      arrange(.data$GFlop)

    model_panel <- model_data %>%
      ggplot(aes(x = .data$GFlop, y = .data$Duration, group = .data$Value)) +
      theme_bw(base_size = data$config$base_size) +
      scale_color_manual(values = c("black", "orange")) +
      theme(
        legend.position = "top",
        strip.text.x = element_text(size = rel(1)),
        axis.text.x = element_text(angle = 45, vjust = 0.5)
      ) +
      # Outlier_LR_LOG is renamed as the default Outlier
      geom_point(aes(color = .data$Outlier), alpha = .5) +
      geom_line(aes(x = .data$GFlop, y = .data$fit_LR), color = "green", size = .8) +
      geom_line(aes(x = .data$GFlop, y = .data$lwr_LR), color = "red", size = .8) +
      geom_line(aes(x = .data$GFlop, y = .data$upr_LR), color = "red", size = .8) +
      labs(y = "Duration (ms)", x = "GFlops", color = "Anomaly") +
      ggtitle("Using LOG~LOG transformed LR model: log(Duration) ~ log(GFlop)")

    # finite mixture of LR log~log models
  } else if (model_type == "FLEXMIX") {
    # fit log models over raw data
    model_data <- data$Application %>%
      filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
      filter(.data$GFlop > 0) %>%
      filter(!is.na(.data$Cluster)) %>%
      unique() %>%
      group_by(.data$ResourceType, .data$Value, .data$Cluster) %>%
      nest() %>%
      mutate(model_log = map(.data$data, model_LR_log)) %>%
      ungroup() %>%
      mutate(predictValue = map(.data$model_log, function(x) {
        exp(predict(x))
      })) %>%
      select(-.data$model_log) %>%
      unnest(cols = c(.data$data, .data$predictValue)) %>%
      arrange(.data$predictValue)

    model_panel <- model_data %>%
      ggplot(aes(
        x = .data$GFlop, y = .data$Duration, group = .data$Cluster, shape = as.factor(.data$Cluster),
        color = as.factor(.data$Outlier_FLEXMIX)
      )) +
      theme_bw(base_size = data$config$base_size) +
      scale_color_manual(values = c("black", "orange")) +
      scale_shape_manual(values = c(15, 19)) +
      theme(
        legend.position = "top",
        strip.text.x = element_text(size = rel(1)),
        axis.text.x = element_text(angle = 45, vjust = 0.5)
      ) +
      geom_point(alpha = .5) +
      # adjust log model to the normal data
      geom_line(aes(x = .data$GFlop, y = .data$fit), color = "green", size = .8) +
      geom_line(aes(x = .data$GFlop, y = .data$lwr, linetype = as.factor(.data$Cluster)), color = "red", size = .8) +
      geom_line(aes(x = .data$GFlop, y = .data$upr, linetype = as.factor(.data$Cluster)), color = "red", size = .8) +
      labs(y = "Duration (ms)", x = "GFlops", color = "Anomaly", linetype = "Cluster", shape = "Cluster") +
      ggtitle("Using multiple LOG models (flexmix): log(Duration) ~ log(GFlop)")
    # Weighted linear regression 1/GFlop
  } else if (model_type == "WLR") {
    gflops <- data$Application %>%
      filter(grepl("qrt", .data$Value)) %>%
      filter(.data$GFlop > 0) %>%
      .$GFlop

    model_panel <- model_panel +
      geom_point(aes(color = .data$Outlier_WLR), alpha = .5) +
      geom_smooth(
        method = "lm", formula = "y ~ x", color = "green", fill = "blue",
        mapping = aes(weight = 1 / gflops)
      ) +
      ggtitle("Using WLR model: Duration ~ GFlop, weight=1/GFlop")
  } else if (model_type == "NLR") {
    model_panel <- model_panel +
      geom_point(aes(color = .data$Outlier_NLR), alpha = .5) +
      geom_smooth(method = "lm", formula = "y ~ I(x**(2/3))", color = "green", fill = "blue") +
      ggtitle("Using NLR model: Duration ~ GFlop**2/3")
    # plot all models together for comparison purposes
  } else {
    model_panel <- model_panel +
      geom_point(alpha = .5) +
      ggtitle(" You should specify a valid model_type ['LR', 'LOG_LOG', 'NLR', 'FLEXMIX', 'WLR'] ")
  }

  # Controls the scales by using facet grid or facet wrap
  if (freeScales) {
    model_panel <- model_panel + facet_wrap(.data$ResourceType ~ .data$Value, scales = "free", ncol = 4)
  } else {
    model_panel <- model_panel + facet_grid(.data$ResourceType ~ .data$Value, scales = "free")
  }

  return(model_panel)
}

#' Plot resource utilization using tasks as color
#'
#' Use data Application to create a panel of the total resource utilization
#' that helps to observe the time related resource utilization by task
#'
#' @param data starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param legend enable/disable plot legends
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_resource_usage_task(data = starvz_sample_lu)
#' }
#' @export
panel_resource_usage_task <- function(data = NULL,
                                      step = NULL,
                                      legend = FALSE,
                                      x_start = data$config$limits$start,
                                      x_end = data$config$limits$end) {
  starvz_check_data(data, tables = list("Application" = c("Value")))

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      step <- 100
    } else {
      step <- data$config$global_agg_step
    }
  }

  df1 <- get_resource_utilization(Application = data$Application, step = step)

  # join data frames
  df2 <- df1 %>%
    ungroup() %>%
    group_by(.data$Slice) %>%
    unique() %>%
    droplevels()

  # must expand data frame to make geom_area work properly
  df_plot <- df2 %>%
    filter(!is.na(.data$Task)) %>% ungroup() %>%
    expand(.data$Slice, .data$Task) %>%
    left_join(df2 %>% filter(.data$Value != 0), by = c("Task", "Slice")) %>%
    mutate(Value1 = ifelse(is.na(.data$Value1), 0, .data$Value1))

  df_plot <- df_plot %>%
    ungroup() %>%
    # expand all time slices with the possible colors (for geom_ribbon)
    expand(.data$Slice, .data$Task) %>%
    left_join(df_plot, by = c("Slice", "Task")) %>%
    group_by(.data$Task) %>%
    # mutate(Value1 = ifelse(is.na(.data$Value1), 0, .data$Value1)) %>%
    mutate(Value1 = na.locf(.data$Value1)) %>%
    arrange(.data$Slice, .data$Task) %>%
    group_by(.data$Slice) %>%
    filter(!is.na(.data$Task)) %>%
    # define Ymin and Ymax for geom ribbon
    mutate(Usage = sum(.data$Value1)) %>%
    mutate(Ymin = lag(.data$Value1), Ymin = ifelse(is.na(.data$Ymin), 0, .data$Ymin)) %>%
    mutate(Ymin = cumsum(.data$Ymin), Ymax = .data$Ymin + .data$Value1) %>%
    # remove Ending nodes at the middle of a Slice to keep their res. utilization
    ungroup() %>%
    mutate(Slc = .data$Slice %% step) %>%
    filter(.data$Slc == 0 | .data$Slice == max(.data$Slice))

  # plot data
  df_plot %>%
    ggplot() +
    geom_ribbon(aes(ymin = .data$Ymin, ymax = .data$Ymax, x = .data$Slice, fill = as.factor(.data$Task))) +
    default_theme(data$config$base_size, data$config$expand) +
    scale_fill_manual(values = extract_colors(data$Application, data$Colors)) +
    ylab("Usage %\nTask") +
    ylim(0, 100) -> panel

  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  }

  return(panel)
}

# Calculate the computational resource utilization by task
get_resource_utilization <- function(Application = NULL, step = 100) {
  # Arrange data
  df_filter <- Application %>%
    select(.data$Start, .data$End, .data$Value, .data$JobId) %>%
    unique() %>%
    rename(Task = .data$Value) %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- Application %>%
    filter(grepl("CPU", .data$ResourceType) | grepl("CUDA", .data$ResourceType)) %>%
    select(.data$ResourceId) %>%
    unique() %>%
    nrow()

  # Compute the parallelism
  data_node_parallelism <- df_filter %>%
    gather(.data$Start, .data$End, key = "Event", value = "Time") %>%
    arrange(.data$Time) %>%
    # group by task type
    group_by(.data$Task) %>%
    mutate(Value = ifelse(.data$Event == "Start", 1, -1)) %>%
    mutate(parallelism = cumsum(.data$Value)) %>%
    ungroup()

  # Do the time aggregation of resource utilization
  data_node_plot <- data_node_parallelism %>%
    select(.data$Task, .data$Time, .data$parallelism) %>%
    arrange(.data$Time) %>%
    mutate(End = lead(.data$Time)) %>%
    mutate(Duration = .data$End - .data$Time) %>%
    rename(Start = .data$Time, Value = .data$parallelism) %>%
    na.omit() %>%
    group_by(.data$Task) %>%
    do(remyTimeIntegrationPrepNoDivision(., myStep = step)) %>%
    # This give us the total worker usage grouped by Task in a time slice
    mutate(Value1 = .data$Value / (step * NWorkers) * 100) %>%
    ungroup()

  return(data_node_plot)
}

#' Plot the total computed GFlops difference over time given two traces
#'
#' Use starvz_data Application and the GFlop columns to create a plot that shows
#' the total computed GFlop difference over time using geom_line. The blue color
#' represent the faster execution and the red the slower one.
#'
#' @param data1 starvz_data with trace data
#' @param data2 starvz_data with trace data
#' @param legend enable/disable plot legends
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param add_end_line add smaller end time vertical line
#' @return A ggplot object
#' @examples
#' \dontrun{
#' panel_gflops_computed_difference(data1, data2)
#' }
#' @export
panel_gflops_computed_difference <- function(data1 = NULL,
                                             data2 = NULL,
                                             legend = FALSE,
                                             x_start = NULL,
                                             x_end = NULL,
                                             add_end_line = TRUE) {
  starvz_check_data(data1, tables = list("Application" = c("GFlop")))
  starvz_check_data(data2, tables = list("Application" = c("GFlop")))

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  # check wich one is the slower execution
  end1 <- data1$Application %>%
    pull(.data$End) %>%
    max()
  end2 <- data2$Application %>%
    pull(.data$End) %>%
    max()
  if (end1 >= end2) {
    slower <- data1
    faster <- data2
    end_cut <- end2
    x_end <- end1
  } else {
    faster <- data1
    slower <- data2
    end_cut <- end1
    x_end <- end2
  }

  # get the total computation over time
  # (QRT tasks only work for qrmumps, add new ones for other applications)
  data_gflop <- faster$Application %>%
    filter(grepl("qrt", .data$Value)) %>%
    bind_rows(slower$Application %>%
      filter(grepl("qrt", .data$Value)) %>%
      mutate(GFlop = -.data$GFlop)) %>%
    arrange(.data$End)

  # calculate the actual difference in the total computed GFlops
  data_diff_line <- data_gflop %>%
    mutate(GFlopDiff = cumsum(.data$GFlop)) %>%
    select(.data$Start, .data$End, .data$GFlopDiff) %>%
    mutate(signal = ifelse(.data$GFlopDiff >= 0, "Faster", "Slower"))

  lineplot <- data_diff_line %>%
    mutate(groups = 1) %>%
    ggplot(aes(x = .data$End, y = .data$GFlopDiff, color = .data$signal)) +
    geom_line(aes(group = .data$groups)) +
    scale_color_manual(
      breaks = c("Faster", "Slower"),
      values = c("blue", "red")
    ) +
    labs(y = paste0("GFlops\n difference")) +
    default_theme(data1$config$base_size, data1$config$expand) +
    theme(legend.position = "none")
  # geom_hline(aes(yintercept=c(0)) , linetype="dashed")

  # add legend
  if (legend) {
    lineplot <- lineplot +
      theme(legend.position = "top")
  }

  # adjust x start and end
  tXScale <- list(
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  )

  lineplot <- lineplot + tXScale

  # add the end line for faster execution
  if (add_end_line) {
    lineplot <- lineplot + geom_vline(aes(xintercept = c(end_cut)))
  }

  return(lineplot)
}

statistics_makespan <- function(data) {
  if (is.null(data$Application)) {
    return(NA)
  }
  data$Application %>%
    select(.data$End) %>%
    pull(.data$End) %>%
    na.omit() %>%
    max() -> makespan
  return(makespan)
}

statistics_total_tasks <- function(data) {
  if (is.null(data$Application)) {
    return(NA)
  }
  data$Application %>%
    nrow() -> total_tasks
  return(total_tasks)
}

statistics_total_tasks_types <- function(data) {
  if (is.null(data$Application)) {
    return(NA)
  }
  data$Application %>%
    select(.data$Value) %>%
    distinct() %>%
    nrow() -> total_tasks_types
  return(total_tasks_types)
}

statistics_total_nodes <- function(data) {
  if (is.null(data$Application)) {
    return(NA)
  }
  data$Application %>%
    select(.data$Node) %>%
    distinct() %>%
    nrow() -> total_nodes
  return(total_nodes)
}

statistics_total_resources <- function(data) {
  if (is.null(data$Starpu)) {
    return(NA)
  }
  data$Starpu %>%
    select(.data$ResourceId) %>%
    distinct() %>%
    nrow() -> total_resources
  return(total_resources)
}

statistics_total_gpus <- function(data) {
  if (is.null(data$Starpu)) {
    return(NA)
  }
  data$Starpu %>%
    filter(.data$ResourceType == "CUDA") %>%
    select(.data$ResourceId) %>%
    distinct() %>%
    nrow() -> total_gpus
  return(total_gpus)
}

statistics_total_cpus <- function(data) {
  if (is.null(data$Starpu)) {
    return(NA)
  }
  data$Starpu %>%
    filter(.data$ResourceType == "CPU") %>%
    select(.data$ResourceId) %>%
    distinct() %>%
    nrow() -> total_cpus
  return(total_cpus)
}

statistics_total_idleness <- function(data) {
  if (is.null(data$Application)) {
    return(NA)
  }
  if (is.null(data$Starpu)) {
    return(NA)
  }
  data$Application %>%
    summarize(active = sum(.data$Duration)) %>%
    .$active -> total_time_active

  total_resources <- statistics_total_resources(data)

  makespan <- statistics_makespan(data)

  percent_active <- total_time_active / (total_resources * makespan)

  return(100.0 - percent_active)
}

last <- function(data, path){
	get_last_path(data$Last, path) -> deps
	ret <- NULL

  data$Link %>%
  filter(.data$Type == "MPI communication") %>%
  rename(JobId = .data$Key) %>%
  rename(ResourceId = .data$Dest) %>%
  select(.data$JobId, .data$Start, .data$End, .data$ResourceId) %>%
  separate(.data$ResourceId, into = c("Node", "Resource"), remove = FALSE, extra = "drop", fill = "right") %>%
  left_join((data$Y %>% select(-.data$Type) %>% mutate(Parent = as.character(.data$Parent))), by = c("ResourceId" = "Parent")) %>%
  select(.data$JobId, .data$Start, .data$End, .data$Position, .data$Height) %>%
  bind_rows(data$Application %>% select(.data$JobId, .data$Start, .data$End, .data$Position, .data$Height)) -> all_states

	data$Last %>% rename(Dependent=.data$Last) %>% select(.data$JobId, .data$Dependent) %>% inner_join(all_states, by=c("JobId"="JobId")) -> app_dep
	for(i in seq(1, length(deps))){
	   app_dep %>% filter(.data$JobId %in% deps[[i]]) %>%
	   mutate(Path = path[i]) %>% arrange(.data$End) %>% bind_rows(ret) -> ret
	}

	return(ret)
}
