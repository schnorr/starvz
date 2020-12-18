#' @include starvz_data.R

check_arrow <- function() {
  if (!arrow_available()) {
    starvz_warn("Arrow was not property installed, use: install_arrow()")
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
#' @examples
#' \dontrun{
#' panel_model_gflops(data = starvz_sample_data)
#' }
#' @export
panel_model_gflops <- function(data, freeScales = TRUE, model_type="LOG_LOG") {
  
  # create the base ggplot object that is enhanced according to the model_type
  model_panel <- data$Application %>%
    filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
    filter(.data$GFlop > 0) %>%
    ggplot(aes(x = .data$GFlop, y = .data$Duration, color = .data$Outlier)) +
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
  model_LR_log <- function(df) { lm(log(Duration) ~ log(GFlop), data = df) }
    

  # Linear Regression model_type with Duration ~ GFlop 
  if(model_type == "LR"){
    model_panel <- model_panel + 
      geom_point(alpha = .5) +
      geom_smooth(method = "lm", formula = "y ~ x", color = "green", fill = "blue") +
      ggtitle("Using LR model: Duration ~ GFlop")
  # log-log transformed linear regression
  } else if(model_type == "LOG_LOG") {
    # fit log models over data
    model_data <- data$Application %>%
      filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
      filter(.data$GFlop > 0) %>%
      unique() %>%
      group_by(.data$ResourceType, .data$Value) %>%
      nest() %>%
      mutate(model_log = map(.data$data, model_LR_log)) %>%
      ungroup() %>%
      select(-.data$model_log) %>%
      unnest(cols = c(.data$data)) %>%
      arrange(.data$GFlop)
  
    model_panel <- model_data %>%
      ggplot(aes(x = .data$GFlop, y = .data$Duration, color = .data$Outlier)) +
      theme_bw(base_size = data$config$base_size) +
      scale_color_manual(values = c("black", "orange")) +
      theme(
        legend.position = "top",
        strip.text.x = element_text(size = rel(1)),
        axis.text.x = element_text(angle = 45, vjust = 0.5)
      ) +
      geom_point(alpha = .5) +
      geom_line(aes(x=.data$GFlop, y=.data$fit), color="green", size=.8) +
      geom_line(aes(x=.data$GFlop, y=.data$lwr), color="red", size=.8) +
      geom_line(aes(x=.data$GFlop, y=.data$upr), color="red", size=.8) +
      labs(y = "Duration (ms)", x = "GFlops", color = "Anomaly") +
      ggtitle("Using LOG~LOG transformed LR model: log(Duration) ~ log(GFlop)")

  # finite mixture of LR log~log models
  } else if(model_type == "FLEXMIX") {
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
      mutate(predictValue = map(.data$model_log, function(x){ exp(predict(x)) } )) %>%
      select(-.data$model_log) %>%
      unnest(cols=c(.data$data, .data$predictValue)) %>%
      arrange(.data$predictValue)

    model_panel <- model_data %>%
      ggplot(aes(x = .data$GFlop, y = .data$Duration, group=.data$Cluster, shape=as.factor(.data$Cluster), 
        color = as.factor(.data$Outlier))) +
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
      geom_line(aes(x=.data$GFlop, y=.data$fit), color="green", size=.8) +
      geom_line(aes(x=.data$GFlop, y=.data$lwr, linetype=as.factor(.data$Cluster)), color="red", size=.8) +
      geom_line(aes(x=.data$GFlop, y=.data$upr, linetype=as.factor(.data$Cluster)), color="red", size=.8) +
      labs(y = "Duration (ms)", x = "GFlops", color="Anomaly", linetype="Cluster") +
      ggtitle("Using multiple LOG models (flexmix): log(Duration) ~ log(GFlop)")
  # Weighted linear regression 1/GFlop
  } else if(model_type == "WLR") {

    gflops <- data$Application %>% 
      filter(grepl("qrt", .data$Value)) %>% 
      filter(.data$GFlop > 0) %>% .$GFlop
  
    model_panel <- model_panel +
      geom_point(alpha = .5) +
      geom_smooth(method = "lm", formula="y ~ x", color = "green", fill= "blue",
          mapping = aes(weight = 1/gflops)
        ) +
        ggtitle("Using WLR model: Duration ~ GFlop, weight=1/GFlop")
  # plot all models together for comparison purposes
  } else if( model_type == "ALL") {

    # fit log models over data, need to use exp(predict)
    model_data <- data$Application %>%
      filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
      filter(.data$GFlop > 0) %>%
      unique() %>%
      group_by(.data$ResourceType, .data$Value) %>%
      nest() %>%
      mutate(model_log = map(.data$data, model_LR_log)) %>%
      ungroup() %>%
      mutate(predictValue = map(.data$model_log, function(x){ exp(predict(x)) } )) %>%
      select(-.data$model_log) %>%
      unnest(cols = c(.data$data, .data$predictValue)) %>%
      # now group by cluster to consider flexmix models
      group_by(.data$ResourceType, .data$Value, .data$Cluster) %>%
      nest() %>%
      mutate(model_log = map(.data$data, model_LR_log)) %>%
      ungroup() %>%
      mutate(predictValue_flexmix = map(.data$model_log, function(x){ exp(predict(x)) } )) %>%
      select(-.data$model_log) %>%
      unnest(cols = c(.data$data, .data$predictValue_flexmix)) %>%
      arrange(.data$GFlop)
  
    # for WLR comparison
    gflops <- data$Application %>% 
      filter(grepl("qrt", .data$Value)) %>% 
      arrange(.data$GFlop) %>%
      filter(.data$GFlop > 0) %>% .$GFlop
  
    model_panel <- model_data %>%
      ggplot(aes(x = .data$GFlop, y = .data$Duration, color = .data$Outlier, shape = as.factor(.data$Cluster))) +
      theme_bw(base_size = data$config$base_size) +
      labs(y = "Duration (ms)", x = "GFlops") +
      scale_color_manual(values = c("black", "orange")) +
      theme(
        legend.position = "top",
        strip.text.x = element_text(size = rel(1)),
        axis.text.x = element_text(angle = 45, vjust = 0.5)
      ) +
      geom_point(alpha = .5) +
      geom_smooth(method = "lm", formula="y ~ x", color = "red", linetype="dashed", se = FALSE) +
      geom_smooth(method = "lm", formula = "y ~ I(x^(2/3))", color = "#eb34de", linetype="dashed",  se=FALSE) +
      geom_smooth(method = "lm", formula="y ~ x", color = "#34ebeb", linetype="dashed", fill= "blue",
          mapping = aes(weight = 1/gflops)
        ) +
      geom_line(aes(x = .data$GFlop, y = .data$predictValue), color = "green") +
      geom_line(aes(x = .data$GFlop, y = .data$predictValue_flexmix, linetype=as.factor(.data$Cluster)), color="orange") +
      # adjust log model to the normal data
      labs(y = "Duration (ms)", x = "GFlops", subtitle="LR (red) | NLR 2/3 (pink) | WLR (cyan) | log-log(green) | flexmix(orange)") +
      labs(color="Anomaly", linetype="Cluster", shape="Cluster") +
      ggtitle("Plotting all models: LR, NLR(GFlop^2/3), WLR, LR(log~log), FLEXMIX(log~log)")
  } else {
    model_panel <- model_panel + 
      geom_point(alpha = .5) +
      ggtitle(" Yoou should specify a valid model_type ['LR', 'LOG_LOG', 'FLEXMIX', 'WLR', 'ALL'] ")
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
#' \dontrun{
#' panel_resource_usage_task(data = starvz_sample_data)
#' }
#' @export
panel_resource_usage_task <- function(data = NULL,
                                      step = NULL,
                                      legend = FALSE,
                                      x_start = data$config$limits$start,
                                      x_end = data$config$limits$end) 
{
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
    group_by(.data$Slice) %>% unique()

  # must expand data frame to make geom_area work properly
  df_plot <- df2 %>%
    filter(!is.na(.data$Task)) %>%
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
  
    if(!legend) {
     panel <- panel + theme(legend.position = "none")
    }
  
  return(panel)
}

# Calculate the computational resource utilization by task
get_resource_utilization <- function( Application = NULL, step = 100) 
{
  # Arrange data
  df_filter <- Application %>%
    select(.data$ANode, .data$Start, .data$End, .data$Value, .data$JobId) %>%
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

  # Do the time aggregation of resource utilization by ANode
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
