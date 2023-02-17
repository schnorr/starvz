#' @include starvz_data.R
total_tasks_completed_node_het <- function(time, df_app) {
  nodes <- df_app %>%
    .$Node %>%
    unique()

  ratio <- df_app %>%
    group_by(.data$Node, .data$ResourceId, .data$Value) %>%
    summarize(mean = mean(.data$Duration), .groups="drop") %>%
    group_by(.data$Node, .data$Value) %>%
    summarize(time = 1 / sum(1 / mean), .groups="drop")

  total_tasks <- df_app %>%
    group_by(.data$Node, .data$Value) %>%
    summarize(total = n(), .groups="drop") %>%
    left_join(ratio, by = c("Node", "Value")) %>%
    mutate(total = .data$total * .data$time) %>%
    group_by(.data$Node) %>%
    summarize(total = sum(.data$total), .groups="drop") %>%
    ungroup() %>%
    arrange(.data$Node) %>%
    .$total

  processed_tasks <- df_app %>%
    filter(.data$End <= time) %>%
    group_by(.data$Node, .data$Value) %>%
    summarize(total = n(), .groups="drop") %>%
    left_join(ratio, by = c("Node", "Value")) %>%
    mutate(total = .data$total * .data$time) %>%
    group_by(.data$Node) %>%
    summarize(total = sum(.data$total), .groups="drop") %>%
    ungroup() %>%
    complete(Node = nodes, fill = list(total = 0)) %>%
    arrange(.data$Node) %>%
    .$total 

  return(processed_tasks / total_tasks)
}

# Total tasks completed (0.0-1.0) globaly considering heterogeneity
total_tasks_completed_global_het <- function(time, df_app) {
 total_tasks <- df_app %>%
    group_by(.data$Node, .data$ResourceId, .data$Value) %>%
    summarize(total = n(), mean = mean(.data$Duration), .groups="drop") %>%
    group_by(.data$Value) %>%
    summarize(total = sum(.data$total), 
              time = 1 / sum(1 / .data$mean), .groups="drop") %>%
    mutate(total = .data$total * .data$time) %>%
    ungroup() %>%
    summarize(total = sum(.data$total), .groups="drop") %>%
    .$total

  processed_tasks <- df_app %>%
    group_by(.data$Node, .data$ResourceId, .data$Value) %>%
    filter(.data$End <= time) %>%
    summarize(total = n(), mean = mean(.data$Duration), .groups="drop") %>%
    group_by(.data$Value) %>%
    summarize(total = sum(.data$total), 
              time = 1 / sum(1 / mean), .groups="drop") %>%
    mutate(total = .data$total * .data$time) %>%
    ungroup() %>%
    summarize(total = sum(.data$total), .groups="drop") %>%
    .$total

  return(processed_tasks / total_tasks)
}

# Helper function to prepare abe functions parameters
abe_of_step <- function(step, selection, steps) {
  start_step <- steps[step]
  end_step <- steps[step + 1]

  selection <- selection %>% droplevels()

  n_resources <- selection %>%
    select("ResourceId", "ResourceType") %>%
    distinct() %>%
    group_by(.data$ResourceType) %>%
    summarize(n = n())

  samp <- selection %>%
    group_by(.data$Value, .data$ResourceType) %>%
    summarize(mean = mean(.data$Duration), .groups = "drop") %>%
    left_join(n_resources, by = c("ResourceType")) %>%
    mutate(mean_n = .data$mean / .data$n) %>%
    rename(codelet = .data$Value) %>%
    mutate(Node = 0) %>%
    arrange(.data$codelet)

  freq <- selection %>%
    filter(.data$End > start_step, .data$Start < end_step) %>%
    mutate(effective = (pmin(.data$End, end_step) - 
                        pmax(.data$Start, start_step)) /
                       (.data$End - .data$Start)) %>%
    group_by(.data$Value) %>%
    summarize(freq = sum(.data$effective)) %>%
    rename(codelet = "Value") %>%
    full_join(samp %>% select("codelet"),
              by = c("codelet"), multiple = "all") %>%
    distinct() %>%
    mutate(Node = 0, freq = if_else(is.na(freq), 0, freq)) %>%
    arrange(.data$codelet) %>%
    rename(Value = "codelet")

  r <- starpu_freq_abe(freq, samp)
  return(r)
}

#' Create the progress panel
#'
#' The progress panel show a progress metric per node, clustering nodes with
#' a similar metric
#' @param df_app starvz_data Application data
#' @param nsteps integer Number of times steps
#' @param cluster_option numeric In case of "Mode Density", the bandwidth
#' @param func_progress_node function progress funcion per node that return [0-1]
#' @param func_progress_global function progress function globaly that return [0-1]
#' @param cluster_func string "Mode Density" or "GMM"
#' @param show_abe boolean Add abe to plots
#' @param plot_node_lines boolean Add to return list the progress metric non-clustered
#' @param plot_cluster_info boolean Add to return list the plot of cluster information
#' @param abe_label_pos_y numeric ajust ABE label in y
#' @param abe_label_pos_x numeric ajust ABE label in x
#' @return List, steps - numeric list of steps, step - numeric the number of steps,
#' original_metrics - ggplot, if plot_node_lines is true, plot_den - ggplot, if plot_cluster_info is true,
#' joined_data - tibble, cluster data computed, cluster_metrics - ggplot the progress cluster visualization
#' @usage panel_progress(df_app, nsteps, cluster_option,
#'                         func_progress_node = NULL,
#'                         func_progress_global = NULL,
#'                         cluster_func = "Mode Density",
#'                         show_abe = TRUE,
#'                         plot_node_lines = FALSE,
#'                         plot_cluster_info = FALSE,
#'                         abe_label_pos_y = 0.05,
#'                         abe_label_pos_x = 1.02)
#' @include starvz_data.R
#' @examples
#' panel_progress(starvz_sample_lu$Application, 20, 0.01, show_abe = FALSE)
#' @export
panel_progress <- function(df_app, nsteps, cluster_option,
                           func_progress_node = NULL,
                           func_progress_global = NULL,
                           cluster_func = "Mode Density",
                           show_abe = TRUE,
                           plot_node_lines = FALSE,
                           plot_cluster_info = FALSE,
                           abe_label_pos_y = 0.05,
                           abe_label_pos_x = 1.02) {
  # Default arguments
  if (is.null(func_progress_node)) {
    func_progress_node <- total_tasks_completed_node_het
  }
  if (is.null(func_progress_global)) {
    func_progress_global <- total_tasks_completed_global_het
  }
  if (is.null(df_app)) {
    starvz_warn("df_app is NULL")
    return(NULL)
  }
  # Compute end, steps, and intervals
  df_app %>%
    .$End %>%
    max() -> end
  steps <- seq(0, end, end / nsteps)

  ret <- list()

  ret$steps <- steps
  ret$step <- end / nsteps

  lapply(steps, func_progress_node, df_app) -> r
  df_app %>%
    arrange(.data$Node) %>%
    .$Node %>%
    unique() -> nodes
  do.call(cbind, r) -> matrix_metric
  dim(matrix_metric)[2] -> intervals

  colnames(matrix_metric) <- seq(1, intervals)

  matrix_metric %>%
    as.data.frame() %>%
    mutate(Node = nodes) -> metric

  # Compute the ABE metric if needed
  abe_visu <- NULL
  if (show_abe) {
    steps_lp <- lapply(seq(1, length(steps) - 1), abe_of_step, df_app, steps) %>%
    unlist()

    abe_of_step(1, df_app, c(0, steps[length(steps)])) -> global_abev

    global_p_ideal <- lapply(steps, func_progress_global, df_app) %>%
      unlist()

    global_abe <- data.frame(x = c(global_abev))

    abe_bond_data <- data.frame(y = c(0, global_p_ideal[-1]), abe = c(0, steps_lp)) %>%
      mutate(x = cumsum(.data$abe))
    abe_visu <- list(
      geom_point(
        data = abe_bond_data, aes(x = .data$x, y = .data$y),
        color = "black", shape = 8
      ),
      geom_line(
        data = abe_bond_data, aes(x = .data$x, y = .data$y),
        color = "black", linetype = "dotted"
      ),
      geom_vline(
        data = global_abe, aes(xintercept = .data$x),
        color = "black", linetype = "dotted"
      )
    )
  }

  # Compute the original metric lines for each node
  original_lines <- metric %>%
    pivot_longer(!"Node", names_to = "step", values_to = "values") %>%
    mutate(step = steps[as.numeric(.data$step)], Node = as.integer(.data$Node)) %>%
    group_by(.data$step)

  # Compute the original plot (metric per node)
  if(plot_node_lines){
    original_lines %>%
      ggplot() +
      geom_line(aes(x = .data$step, y = .data$values,
       color = .data$Node, group = .data$Node)) +
      geom_point(aes(x = .data$step, y = .data$values, 
      color = .data$Node, group = .data$Node)) +
      abe_visu +
      #   scale_y_reverse() +
      theme_bw(base_size = 20) +
      ylab("Metric Value [0, 1]") +
      theme(legend.position = "top") +
      guides(color = guide_colourbar(barwidth = 20, barheight = 0.5)) +
      xlab("Time [ms]") -> original_metrics

    if (requireNamespace("viridis", quietly = TRUE)) {
      original_metrics <- original_metrics + viridis::scale_color_viridis()
    } else {
      starvz_warn("In panel_progress: We suggest package viridis for a high number of nodes")
    }

    ret$original_metrics <- original_metrics
  }

  # This is the list of densities in case needed
  list_den <- list()

  # Function to compute groups using density
  groups_density <- function(data, bw) {
    if (bw == "SJ") {
      if (length(unique(data)) > 1) {
        bw <- stats::bw.SJ(data)
      } else {
        bw <- 1
      }
    }

    stats::density(data,
      bw = bw, adjust = 1,
      kernel = c(
        "gaussian", "epanechnikov", "rectangular",
        "triangular", "biweight",
        "cosine", "optcosine"
      ),
      weights = NULL, 
      give.Rkern = FALSE, subdensity = FALSE
    ) -> den
    den$y <- round(den$y, digits = 12)
    size <- length(den$y)
    left <- c(FALSE, den$y[1:(size - 2)] >= den$y[2:(size - 1)], FALSE)
    rigth <- c(FALSE, den$y[2:(size - 1)] <= den$y[3:(size)], FALSE)
    which(left == TRUE & rigth == TRUE & c(1, diff(den$y)) != 0) -> mins

    data_min <- data.frame(x = den$x[mins + 1], y = den$y[mins + 1])

    cut(data, c(-0.1, data_min$x, 1), labels = FALSE) %>%
      as.factor() %>%
      as.numeric() -> r

    list_den <<- append(list_den, list(list(den = den, mins = data_min$x, r = r)))

    return(r)
  }

  # Function to compute groups using GMM
  groups_gmm <- function(metrics) {
    if (length(unique(metrics)) == 1) {
      return(rep(0, length(metrics)))
    }
    mod <- mclust::Mclust(metrics, modelNames = "V")
    return(mod$classification - 1)
  }

  # Compute the groups
  if (cluster_func == "Mode Density") {
    lapply(metric %>% select(-"Node"), groups_density, cluster_option) -> groups

    den_graph <- function(step) {
      data.frame(mins = list_den[[step]]$mins) -> v

      v <- v %>%
        arrange(.data$mins) %>%
        mutate(tmin = lag(.data$mins, default = 0)) %>%
        mutate(tmin = .data$mins - .data$tmin) %>%
        filter(.data$tmin > 0.005)

      plot <- data.frame(
        x = list_den[[step]]$den$x,
        y = list_den[[step]]$den$y
      ) %>% ggplot() +
        geom_line(aes(x = .data$x, y = .data$y)) +
        geom_vline(data = v, aes(xintercept = .data$mins), color = "blue") +
        ggtitle(paste0("Step ", step)) +
        theme_bw()
      return(plot)
    }
    if(plot_cluster_info){
      plot_den <- den_graph(1)
      for (i in seq(2, length(steps))) {
        plot_den <- plot_den + den_graph(i)
      }
      plot_den + patchwork::plot_layout(ncol = 5)

      ret$plot_den <- plot_den
    }
  } else if (cluster_func == "GMM") {
    if (requireNamespace("mclust", quietly = TRUE)) {
      groups <- lapply(metric %>% select(-"Node"), groups_gmm)
    }else{
      starvz_warn("In panel_progress: GMM require package mclust")
    }
  } else {
    starvz_warn("Invalid cluster option")
    return(NULL)
  }

  # Join all groups
  do.call(cbind, groups) -> tibble_groups

  dim(tibble_groups)[2] -> intervals

  colnames(tibble_groups) <- seq(1, intervals)

  # Tidy data
  final_groups <- tibble_groups %>%
    as.data.frame() %>%
    mutate(Node = nodes) %>%
    pivot_longer(!"Node", names_to = "Interval",
     values_to = "Groups")

 final_metrics <- metric %>% pivot_longer(!"Node", names_to = "Interval",
   values_to = "Metric")

  # Get metrics per group
  joined_data <- final_groups %>%
    inner_join(final_metrics, by = c("Node", "Interval")) %>%
    mutate(Interval = as.integer(.data$Interval))

  # Compute where there are divisions in groups
  centroids <- joined_data %>%
    group_by(.data$Interval, .data$Groups) %>%
    summarize(mean = mean(.data$Metric), .groups="drop") %>%
    mutate(TInterval = steps[.data$Interval])

  # Compute the grouped lines
  lines <- joined_data %>%
    group_by(.data$Node) %>%
    arrange(.data$Interval) %>%
    mutate(LastGroup = lag(.data$Groups)) %>%
    ungroup() %>%
    select("Interval", "Groups", "LastGroup") %>%
    group_by(.data$Interval, .data$Groups, .data$LastGroup) %>%
    summarize(n = n(), .groups="drop") %>%
    filter(!is.na(.data$LastGroup)) %>%
    mutate(LastInterval = .data$Interval - 1) %>%
    mutate(
      TLastInterval = steps[.data$LastInterval],
      TInterval = steps[.data$Interval]
    )

  # Gen abe plot elements
  abe_s_visu <- NULL
  if (show_abe) {
    abe_s_visu <- list(
      geom_point(
        data = abe_bond_data, aes(x = .data$x, y = .data$y),
        color = "black", shape = 8
      ),
      geom_line(
        data = abe_bond_data, aes(x = .data$x, y = .data$y),
        color = "black", linetype = "dotted"
      ),
      geom_vline(
        data = global_abe, aes(xintercept = .data$x),
        color = "black", linetype = "dotted"
      ),
      geom_text(
        data = global_abe, aes(
          x = .data$x * abe_label_pos_x,
          y = abe_label_pos_y
        ),
        label = "ABE", angle = 90, size = 6
      )
    )
  }

  plot_lines <- lines %>%
    inner_join(centroids, by = c(
      "LastInterval" = "Interval",
      "LastGroup" = "Groups"
    )) %>%
    rename(LastMean = "mean") %>%
    inner_join(centroids, by = c(
      "Interval",
      "Groups"
    ))


  sum_nodes <- function(x) {
    x <- as.numeric(x)
    gr <- cumsum(c(TRUE, diff(x) != 1))
    y <- unname(tapply(x, gr, FUN = function(.x) {
      paste(unique(range(.x)), collapse = "-")
    }))
    return(y)
  }

  total_nodes <- joined_data %>%
   .$Node %>% 
   levels() %>% 
   length()

  last <- plot_lines %>%
    select("Interval", "Groups", "n") %>%
    ungroup() %>%
    add_row(Interval = 1, Groups = 1.0, n = total_nodes) %>%
    arrange(.data$Interval)

  # Compute disjoint lines
  extra <- plot_lines %>%
    left_join(last, by = c("LastInterval" = "Interval", "LastGroup" = "Groups"), multiple = "all") %>%
    filter(.data$n.x < .data$n.y) %>%
    mutate(Pos_x = (.data$TInterval - .data$TLastInterval) / 2 + .data$TLastInterval) %>%
    select(-"TLastInterval", -"TInterval.x", -"TInterval.y", -"TInterval") %>%
    mutate(Pos_y = (.data$mean - .data$LastMean) / 2 + .data$LastMean) %>%
    select("Interval", "LastInterval", "LastGroup", "Groups", "Pos_x", "Pos_y") %>%
    distinct()

  # If there are divisions we need to show the group where less nodes departed
  extra_plot <- NULL
  if (nrow(extra) > 0) {
    extra %>%
      left_join(joined_data %>% select(-"Metric"),
        by = c("Interval", "Groups"), multiple = "all"
      ) %>%
      rename(NodeA = "Node") %>%
      group_by(.data$Interval, .data$Groups, .data$Pos_x, .data$Pos_y) %>%
      nest(NodesA = "NodeA") %>%
      left_join(joined_data %>% select(-"Metric"),
        by = c("LastInterval" = "Interval", "LastGroup" = "Groups"), multiple = "all"
      ) %>%
      nest(NodesB = "Node") %>%
      rowwise() %>%
      mutate(Nodes = list(intersect(unlist(.data$NodesA), unlist(.data$NodesB)))) %>%
      mutate(size = length(unlist(.data$Nodes))) %>%
      group_by(.data$Interval, .data$LastGroup) %>%
      arrange(.data$size) %>%
      slice(1:(n() - 1)) %>%
      mutate(STR = toString(sum_nodes(unlist(.data$Nodes)))) %>%
      ungroup() %>%
      select("Pos_x", "Pos_y", "STR") -> extra
    centroids %>%
      ungroup() %>%
      select("TInterval", "mean") %>%
      rename(Pos_x = "TInterval", Pos_y = "mean") %>%
      mutate(STR = "") -> all_points
    extra <- bind_rows(extra, all_points)
    
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      extra_plot <- list(ggrepel::geom_label_repel(
        data = extra,
        aes(x = .data$Pos_x, y = .data$Pos_y, label = .data$STR),
        min.segment.length = 1, size = 6, alpha = 0.9,
        box.padding = 1,
        segment.linetype = "twodash",
        force = 100,
        segment.size = 0.4
      ))
    } else {
      starvz_warn("In panel_progress: We suggest package ggrepel for a high number of nodes")
      extra_plot <- list(geom_label(
        data = extra %>% filter(.data$STR != ""),
        aes(x = .data$Pos_x, y = .data$Pos_y, label = .data$STR),
        size = 6, alpha = 0.9
      ))
    }
  } else {
    extra <- NULL
  }

  # The main plot
  plot_lines %>%
    ggplot() +
    geom_line(
      data = original_lines,
      aes(x = .data$step, y = .data$values, group = .data$Node), color = "gray", alpha = 0.5, linewidth = 0.5
    ) +
    geom_segment(aes(
      x = .data$TLastInterval, xend = .data$TInterval,
      y = .data$LastMean, yend = .data$mean, size = .data$n
    ), alpha = 0.8) +
    scale_size_continuous(range = c(0.5, 1.5)) +
    geom_point(data = centroids, aes(x = .data$TInterval, y = .data$mean, color = as.factor(.data$Groups)), size = 2) +
    scale_color_brewer(palette = "Set1", name = "Group") +
    guides(size = guide_legend(title = "Number of nodes")) +
    abe_s_visu +
    theme_bw(base_size = 22) +
    ylab("Metric Value [0, 1]") +
    theme(legend.position = "top") +
    extra_plot +
    theme(legend.title = element_text(size = 18)) +
    xlab("Time [ms]") -> cluster_metrics

  ret$joined_data <- joined_data

  ret$cluster_metrics <- cluster_metrics
  return(ret)
}