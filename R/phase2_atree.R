
#' Create the elimination tree structure plot along time
#'
#' Use Atree and Application data to create the elimination tree
#' structure plot in a ggplot object and return it
#'
#' @param data starvz_data with trace data
#' @param end_arrow behavior of the end arrow [ParentEnd, ComputationEnd]
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \dontrun{
#' panel_atree_structure(starvz_sample_lu)
#' }
#' @export
panel_atree_structure <- function(data = NULL, end_arrow = "ParentEnd") {
  if (is.null(data)) stop("input data for geom_atree_plot is NULL")

  makespan <- data$Application %>%
    .$End %>%
    max()

  dtree <- tibble()

  # make child end point to parent end
  if (tolower(end_arrow) == "parentend") {
    dtree <- data$Atree %>%
      # Get Start time of the first task belonging to each ANode
      left_join(
        data$Application %>%
          select("ANode", "Start", "End") %>%
          group_by(.data$ANode) %>%
          summarize(
            Start = min(.data$Start),
            End = max(.data$End)
          ),
        by = "ANode"
      ) %>%
      # Get graphical properties of Parent for each ANode
      left_join(
        x = .,
        y = .,
        by = c("Parent" = "ANode"), suffix = c("", ".Parent")
      ) %>%
      select(-"Parent.Parent") %>%
      # Keep only non pruned nodes for tree structure
      filter(.data$NodeType != "Pruned") %>%
      # Calculate coordinates for lines connecting child with parent
      # Values used in the Start line
      mutate(
        Edge.X = .data$Start,
        Edge.Y = .data$Position + .data$Height / 2,
        Edge.Xend = .data$Start.Parent,
        Edge.Yend = .data$Position.Parent + .data$Height.Parent / 2
      ) %>%
      # Values used in the End line
      mutate(
        Edge.End.X = .data$End,
        Edge.End.Y = .data$Position + .data$Height / 2,
        Edge.End.Xend = .data$End.Parent,
        Edge.End.Yend = .data$Position.Parent + .data$Height.Parent / 2
      )

    # else child nodes point to the parent Y in the last computational task time X among the children
  } else {
    dtree <- data$Atree %>%
      # Get Start time of the first task belonging to each ANode (keep Start as it is, but consider two end points clean and comptation)
      left_join(
        data$Application %>%
          mutate(EndType = case_when(
            grepl("clean", .data$Value) ~ "CleanEnd",
            TRUE ~ "ComputationEnd"
          )) %>%
          select("ANode", "Start", "End", "EndType") %>%
          group_by(.data$ANode, .data$EndType) %>%
          mutate(End = max(.data$End)) %>%
          group_by(.data$ANode) %>%
          mutate(Start = min(.data$Start)),
        by = "ANode"
      ) %>%
      # Keep only non pruned nodes for tree structure
      filter(.data$NodeType != "Pruned") %>%
      ungroup() %>%
      unique() %>%
      spread(.data$EndType, .data$End) %>%
      # Calculate coordinates for lines connecting child with parent
      # Get graphical properties of Parent for each ANode
      left_join(
        x = .,
        y = .,
        by = c("Parent" = "ANode"), suffix = c("", ".Parent")
      ) %>%
      select(-"Parent.Parent") %>%
      group_by(.data$Parent) %>%
      mutate(ComputationEnd.Parent = max(.data$ComputationEnd)) %>%
      ungroup() %>%
      # Calculate coordinates for lines connecting child with parent
      # Values used in the Start line (keep the same)
      mutate(
        Edge.X = .data$Start,
        Edge.Y = .data$Position + .data$Height / 2,
        Edge.Xend = .data$Start.Parent,
        Edge.Yend = .data$Position.Parent + .data$Height.Parent / 2
      ) %>%
      # Values used in the End line
      mutate(
        End = .data$CleanEnd, # last moment for node lifetime
        Edge.End.X = .data$ComputationEnd,
        Edge.End.Y = .data$Position + .data$Height / 2,
        Edge.End.Xend = .data$ComputationEnd.Parent,
        Edge.End.Yend = .data$Position.Parent + .data$Height.Parent / 2
      )
  }


  # data frame for the tree plot structure
  dstruct <- dtree %>%
    # Remove nodes without parent
    filter(!is.na(.data$Parent)) %>%
    # The root has no tasks associated with, remove it.
    mutate(Parent = as.integer(.data$Parent)) %>%
    filter(.data$Parent != max(.data$Parent))

  # data frame for node "lifetime" geom_segment
  dline <- dtree %>%
    select("ANode", "Start", "End", "Position", "Height", "Parent") %>%
    filter(!is.na(.data$Parent), !is.na(.data$Start))

  atreeplot <- ggplot() +
    # Lines connecting child with parent (Start)
    geom_segment(
      data = dstruct,
      aes(
        x = .data$Edge.X,
        y = .data$Edge.Y,
        xend = .data$Edge.Xend,
        yend = .data$Edge.Yend
      ), arrow = arrow(length = unit(0.03, "npc")), color = "#1B9E77"
    ) +
    geom_point(
      data = dstruct,
      aes(
        x = .data$Edge.X,
        y = .data$Edge.Y
      ), color = "#1B9E77"
    ) +
    # Lines connecting child with parent (End)
    geom_segment(
      data = dstruct,
      arrow = arrow(length = unit(0.03, "npc")),
      aes(
        x = .data$Edge.End.X,
        y = .data$Edge.End.Y,
        xend = .data$Edge.End.Xend,
        yend = .data$Edge.End.Yend
      ), color = "#D95F02"
    )

  # End free memory point (use for end_arrow="ComputationEnd" only)
  if (tolower(end_arrow) != "parentend") {
    atreeplot <- atreeplot +
      geom_point(
        data = dline,
        aes(
          y = .data$Position + .data$Height / 2,
          x = .data$End,
        ), color = "black"
      )
  }

  atreeplot <- atreeplot +
    geom_point(
      data = dstruct,
      aes(
        x = .data$Edge.End.X,
        y = .data$Edge.End.Y
      ), color = "#D95F02"
    ) +
    # Horizontal lines
    geom_segment(
      data = dline,
      aes(
        y = .data$Position + .data$Height / 2,
        yend = .data$Position + .data$Height / 2,
        x = .data$Start,
        xend = .data$End
      ), color = "lightblue"
    )

  return(atreeplot)
}

#' Create the elimination tree plot with some options in the config file
#'
#' Use starvz_data to create a representation of the elimination tree structure
#' considering initialization, communication, and computational tasks. These
#' representations can be controlled in the configuration file.
#'
#' @param data starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param legend enable/disable panel legend
#' @param zoom enable/disable vertical zoom in the tree structure
#' @param computation enable/disable computations representations in the tree
#' @param pruned enable/disable pruned computations representations in the tree
#' @param initialization enable/disable initialization tasks representation
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param communication enable/disable communication tasks representation
#' @param anomalies enable/disable anomalies tasks representation
#' @param performance_metric which metric to represent ["time", "gflops"]
#' @param level draw a dashed line to divide the tree at the level h
#' @param end_arrow behavior of the end arrow [ParentEnd, ComputationEnd]
#' @return A ggplot object
#' @examples
#' \dontrun{
#' panel_atree(starvz_sample_lu, step = 10)
#' panel_atree(starvz_sample_lu,
#'   step = 20,
#'   communication = FALSE, initialization = FALSE
#' )
#' }
#' @export
panel_atree <- function(data = NULL, step = data$config$atree$step, legend = data$config$atree$legend, zoom = FALSE,
                        computation = data$config$atree$computation$active,
                        pruned = data$config$atree$computation$pruned$active,
                        initialization = data$config$atree$initialization$active,
                        x_start = data$config$limits$start,
                        x_end = data$config$limits$end,
                        communication = data$config$atree$communication$active,
                        anomalies = data$config$atree$anomalies$active,
                        performance_metric = "time",
                        level = 0,
                        end_arrow = "ParentEnd") {
  starvz_check_data(data, tables = list("Atree" = c("ANode")))

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      step <- 100
    } else {
      step <- data$config$global_agg_step
    }
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  data_utilization_node <- resource_utilization_tree_node(data$Application, data$Atree,
    step = step, group_pruned = TRUE, performance_metric = performance_metric
  ) %>%
    filter(.data$Value1 != 0) %>%
    select("ANode", "Slice", "Value1") %>%
    ungroup() %>%
    rename(NodeUsage = "Value1") %>%
    left_join(data$Atree, by = "ANode")

  # Resize for not pruned nodes, first and last computational tasks
  data_tree_utilization_not_pruned <- data_utilization_node %>%
    inner_join(
      data$Application %>%
        filter(grepl("qrt", .data$Value)) %>%
        select("ANode", "End", "Start") %>%
        group_by(.data$ANode) %>%
        mutate(Start = min(.data$Start), End = max(.data$End)) %>%
        select("ANode", "Start", "End") %>%
        unique(),
      by = "ANode"
    ) %>%
    mutate(Start = ifelse(.data$Start >= .data$Slice, .data$Start, .data$Slice)) %>%
    mutate(End = ifelse(.data$Slice + step >= .data$End, .data$End, .data$Slice + step))

  # 1. Add the atree structure representation first
  atreeplot <- panel_atree_structure(data, end_arrow = end_arrow) +
    default_theme(data$config$base_size, data$config$expand) +
    ylab("Elimination Tree\n[Submission Order]")

  # Add the computation, initialization, and communication over the tree structure plot
  # 2. Add computations
  # 2.1 Add the not pruned computation representation
  if (computation) {
    atreeplot <- atreeplot +
      atree_geom_rect_gradient(data_tree_utilization_not_pruned, yminOffset = 0, ymaxOffset = 1)
  }
  # 2.2 Add the pruned computation representation
  # TODO can't use pruned with gflops performance metric, fix this
  if (pruned) {
    # Manipulate data for the tree resource utilization plots
    # Resize for pruned nodes Start and End of the aggregated pruned nodes
    data_tree_utilization_pruned <- data_utilization_node %>%
      inner_join(
        data$Application %>%
          filter(grepl("do_subtree", .data$Value)) %>%
          select("ANode", "End", "Start") %>%
          group_by(.data$ANode) %>%
          mutate(Start = min(.data$Start), End = max(.data$End)) %>%
          unique(),
        by = "ANode"
      ) %>%
      group_by(.data$Parent, .data$Position) %>%
      mutate(Start = min(.data$Start), End = max(.data$End)) %>%
      mutate(Start = ifelse(.data$Start >= .data$Slice, .data$Start, .data$Slice)) %>%
      mutate(End = ifelse(.data$Slice + step >= .data$End, .data$End, .data$Slice + step))

    atreeplot <- atreeplot +
      atree_geom_rect_gradient(data_tree_utilization_pruned, yminOffset = 0.25, ymaxOffset = 0.75)
  }
  if (tolower(performance_metric) == "time") {
    # 2.3 Add the color to represent resource utilization
    atreeplot <- atreeplot +
      scale_fill_gradient2(
        name = paste0("Computational Load %"),
        limits = c(0, 100), breaks = c(0, 50, 100),
        midpoint = 50, low = "blue", mid = "yellow", high = "red"
      ) +
      theme(legend.title = element_text(size = rel(0.8)))
  } else {
    minGFlop <- round(min(data_utilization_node$NodeUsage), 1)
    maxGFlop <- round(max(data_utilization_node$NodeUsage), 1)
    midGFlop <- round((maxGFlop + minGFlop) / 2, 1)

    atreeplot <- atreeplot +
      scale_fill_gradient2(
        name = "GFlops Throughput", limits = c(minGFlop, maxGFlop), breaks = c(minGFlop, midGFlop, maxGFlop),
        midpoint = midGFlop, low = "blue", mid = "yellow", high = "red"
      ) +
      theme(legend.title = element_text(size = rel(0.8)))
  }

  # 3. Add initialization tasks representation
  if (initialization) {
    # filter initialization tasks
    dfw_init <- data$Application %>%
      filter(grepl("init_", .data$Value)) %>%
      unique() %>%
      select(-"Position", -"Height") %>%
      left_join(data$Atree, by = "ANode") %>%
      filter(!is.na(Position))

    atreeplot <- atreeplot +
      atree_geom_rect(dfw_init, "#4DAF4A", yminOffset = 0, ymaxOffset = 1, alpha = 0.7)
  }

  # 4. Add "communication" (block_copy | block_extadd) tasks in the tree nodes
  if (communication) {
    # filter communication tasks
    dfw_comm <- data$Application %>%
      filter(grepl("block_copy", .data$Value) | grepl("block_extadd", .data$Value)) %>%
      unique() %>%
      select(-"Position", -"Height") %>%
      left_join(data$Atree, by = "ANode") %>%
      filter(!is.na(.data$Position))

    atreeplot <- atreeplot +
      atree_geom_rect(dfw_comm, "#000000", yminOffset = 0.25, ymaxOffset = 0.75, alpha = 0.7)
  }

  # 5. add division in the tree by its levels
  if (level > 1) {
    cut_positions <- data$Atree %>%
      filter(.data$Depth == level & .data$Intermediary) %>%
      arrange(-.data$Position) %>%
      pull(.data$Position)
    cut_positions <- cut_positions[1:length(cut_positions)] + 1

    atreeplot <- atreeplot +
      geom_hline(aes(yintercept = cut_positions, linetype = "dashed"), color = "black") +
      scale_linetype_identity(name = "", breaks = c("dashed"), labels = paste0("Cut at level ", level), guide = "legend")
  }

  # 6. Vertical zoom
  if (zoom) {
    z.start <- data$config$atree$zoom$start
    z.end <- data$config$atree$zoom$end
    max.y.coordinate <- (data$Atree %>% pull(.data$Position) %>% max()) +
      (data$Atree %>% pull(.data$Height) %>% max())
    tzScale <- list(
      coord_cartesian(
        xlim = c(x_start, x_end),
        ylim = c(
          z.start / 100 * max.y.coordinate,
          z.end / 100 * max.y.coordinate
        )
      )
    )
    atreeplot <- atreeplot + tzScale
  } else {
    tzScale <- list(
      coord_cartesian(
        xlim = c(x_start, x_end)
      )
    )
    atreeplot <- atreeplot + tzScale
  }

  # 7. Add representation for anomalies in the tree
  if (anomalies) {
    atreeplot <- atreeplot +
      atree_geom_anomalies(data)
  }

  # 8. Add legend to the plot
  if (legend) {
    atreeplot <- atreeplot +
      guides(shape = FALSE, alpha = FALSE, color = FALSE) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.box.just = "left",
        legend.direction = "horizontal",
        legend.text = element_text(size = rel(0.8), colour = "black")
      )
  } else {
    atreeplot <- atreeplot + theme(legend.position = "none")
  }
  return(atreeplot)
}

#' Create the resource utilization by tree node plot
#'
#' Use starvz_data Application and Atree to create a plot that shows the
#' total resource utilization, painted by tree node using geom_ribbon. The
#' colors are reused between nodes, not tied to a specific tree node.
#'
#' @param data starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @examples
#' \dontrun{
#' panel_utiltreenode(data = starvz_sample_lu, step = 100)
#' }
#' @export
panel_utiltreenode <- function(data = NULL,
                               step = data$config$utiltreenode$step,
                               x_start = data$config$limits$start,
                               x_end = data$config$limits$end) {
  starvz_check_data(data, tables = list("Atree" = c("ANode")))

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      step <- 100
    } else {
      step <- data$config$global_agg_step
    }
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  df1 <- resource_utilization_tree_node(data$Application, data$Atree, step = step, group_pruned = FALSE)

  event_data <- df1 %>%
    filter(.data$Value != 0) %>%
    group_by(.data$ANode) %>%
    summarize(Start = min(.data$Slice), End = max(.data$Slice) + step) %>%
    gather(.data$Start, .data$End, key = "Event", value = "Time") %>%
    arrange(.data$Time, .data$Event)

  # Set node colors
  pkg.env$active_colors <- c()
  pkg.env$df_colors <- tibble(ANode = character(), Event = character(), color = integer())
  apply(event_data, 1, define_colors) -> colors
  colors <- tibble(Color = colors)
  event_data <- event_data %>%
    cbind(colors) %>%
    as_tibble() %>%
    select("ANode", "Color") %>%
    unique()

  # join data frames
  df2 <- df1 %>%
    ungroup() %>%
    left_join(event_data, by = "ANode") %>%
    group_by(.data$Slice)

  # must expand data frame to make geom_area work properly
  df_plot <- df2 %>%
    filter(!is.na(.data$Color)) %>%
    select(-"ANode") %>%
    expand(.data$Slice, .data$Color) %>%
    left_join(df2 %>% filter(.data$Value != 0), by = c("Slice", "Color")) %>%
    mutate(Value1 = ifelse(is.na(.data$Value1), 0, .data$Value1))

  df_plot <- df_plot %>%
    # expand all time slices with the possible colors (for geom_ribbon)
    expand(.data$Slice, Color = 0:(df_plot$Color %>% max())) %>%
    left_join(df_plot, by = c("Slice", "Color")) %>%
    group_by(.data$Color) %>%
    arrange(.data$Color) %>%
    mutate(Value1 = na.locf(.data$Value1)) %>%
    group_by(.data$Slice) %>%
    arrange(.data$Slice, -.data$Color) %>%
    # define Ymin and Ymax for geom ribbon
    mutate(Usage = sum(.data$Value1)) %>%
    mutate(Ymin = lag(.data$Value1), Ymin = ifelse(is.na(.data$Ymin), 0, .data$Ymin)) %>%
    mutate(Ymin = cumsum(.data$Ymin), Ymax = .data$Ymin + .data$Value1) %>%
    # remove Ending nodes at the middle of a Slice to keep their res. utilization
    ungroup() %>%
    mutate(Slc = .data$Slice %% step) %>%
    filter(.data$Slc == 0 | .data$Slice == max(.data$Slice))

  # decide the size of the pallet, if it will include more colors than just green
  ncolors <- df_plot %>%
    ungroup() %>%
    select("Color") %>%
    max(.$Color)
  if (ncolors <= 10) {
    palette <- brewer.pal(9, "Greens")[3:9]
  } else if (ncolors <= 20) {
    palette <- c(
      brewer.pal(9, "Reds")[3:9], brewer.pal(9, "Greens")[3:9]
    )
  } else {
    palette <- c(
      brewer.pal(9, "Oranges")[3:9], brewer.pal(9, "Blues")[3:9],
      brewer.pal(9, "Reds")[3:9], brewer.pal(9, "Greens")[3:9]
    )
  }

  fill_palette <- rev(colorRampPalette(palette)(ncolors + 1))

  df_plot %>%
    ggplot() +
    geom_ribbon(aes(ymin = .data$Ymin, ymax = .data$Ymax, x = .data$Slice, fill = as.factor(.data$Color))) +
    scale_fill_manual(values = fill_palette) +
    default_theme(data$config$base_size, data$config$expand) +
    theme(legend.position = "none") +
    ylab("Usage %\nANode") +
    ylim(0, 100) -> panel

  tzScale <- list(
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  )
  panel <- panel + tzScale
  return(panel)
}

#' Create the resource utilization by tree depth plot
#'
#' Use starvz_data Application and Atree to create a plot that shows the
#' total resource utilization, painted by tree depth level using geom_ribbon
#'
#' @param data starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param legend enable/disable plot legends
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @examples
#' \dontrun{
#' panel_utiltreedepth(starvz_sample_lu, step = 100, legend = TRUE)
#' }
#' @export
panel_utiltreedepth <- function(data,
                                step = data$config$utiltreenode$step,
                                x_start = data$config$limits$start,
                                x_end = data$config$limits$end,
                                legend = data$config$utiltreedepth$legend) {
  starvz_check_data(data, tables = list("Atree" = c("ANode")))

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

  # Prepare data
  depth_plot_data <- resource_utilization_tree_depth(data$Application, data$Atree, step)

  maxDepth <- depth_plot_data %>%
    .$Depth %>%
    max()
  depthPalette <- rev(colorRampPalette(brewer.pal(9, "YlOrRd"))(maxDepth))

  panel <- depth_plot_data %>%
    ggplot() +
    geom_ribbon(aes(ymin = .data$Ymin, ymax = .data$Ymax, x = .data$Slice, fill = as.factor(.data$Depth))) +
    default_theme(data$config$base_size, data$config$expand) +
    theme(legend.position = "top") +
    scale_fill_manual(values = depthPalette) +
    ylab("Usage %\nDepth") +
    ylim(0, 100)

  # configure legend
  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  }

  tzScale <- list(
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  )
  panel <- panel + tzScale

  return(panel)
}

nodememuse_check <- function(data) {
  data$Application %>%
    filter(grepl("front", .data$Value) & .data$GFlop != 0) %>%
    nrow() -> nr
  return(!(nr == 0))
}

#' Create the node memory usage plot
#'
#' Use starvz_data to create a line plot of the memory usage in MB of
#' active nodes along the application execution time
#'
#' @param data starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param aggregation enable/disable time aggregation for the plot
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param legend enable/disable plot legends
#' @examples
#' \dontrun{
#' panel_nodememuse(starvz_sample_lu, step = 100)
#' }
#' @export
panel_nodememuse <- function(data = NULL,
                             step = data$config$activenodes$aggregation$step,
                             aggregation = data$config$activenodes$aggregation$active,
                             x_start = data$config$limits$start,
                             x_end = data$config$limits$end,
                             legend = data$config$activenodes$nodememuse$legend) {
  starvz_check_data(data,
    tables = list(
      "Atree" = c("ANode"),
      "Application" = c("Value", "GFlop")
    ),
    extra_func = nodememuse_check
  )

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      step <- 100
    } else {
      step <- data$config$global_agg_step
    }
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  panel <- geom_blank()

  # Prepare data
  data_mem_utilization <- data$Application %>%
    filter(grepl("front", .data$Value) | grepl("do_subtree", .data$Value)) %>%
    rename(Task = "Value") %>%
    # we wrote the memory usage in the GFlops field...
    mutate(UsedMemMB = ifelse(grepl("clean", .data$Task), -.data$GFlop * 1024, .data$GFlop * 1024)) %>%
    arrange(.data$Start) %>%
    mutate(Value = cumsum(.data$UsedMemMB)) %>%
    mutate(Type = NA)

  if (aggregation) {
    panel <- var_integration_chart(data_mem_utilization, ylabel = NA, step = step, facetting = FALSE, base_size = data$config$base_size, expand = data$config$expand)
  } else {
    # TODO: try to reuse some code from above
    # mem use without time aggregation
    df_mem <- data$Application %>%
      filter(grepl("front", .data$Value) | grepl("do_sub", .data$Value)) %>%
      select("Start", "Value", "GFlop", "ANode", "Node") %>%
      arrange(.data$Start) %>%
      mutate(GFlop = ifelse(.data$Value == "clean_front", -.data$GFlop, .data$GFlop)) %>%
      mutate(MemMB = .data$GFlop * 1024) %>%
      mutate(UsedMemMB = cumsum(.data$MemMB)) %>%
      mutate(Time = .data$Start * 0.9999) %>%
      gather(.data$Start, .data$Time, key = "Start", value = "Time") %>%
      select(-"Start") %>%
      arrange(.data$Time) %>%
      mutate(UsedMemMB = lag(.data$UsedMemMB))

    panel <- df_mem %>%
      ggplot(aes(x = .data$Time, y = .data$UsedMemMB, color = .data$Node)) +
      geom_line() +
      default_theme(data$config$base_size, data$config$expand)
  }

  panel <- panel +
    ylab("Used\nMB") +
    scale_color_brewer(palette = "Dark2")

  # configure legend
  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  }

  tzScale <- list(
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  )
  panel <- panel + tzScale

  return(panel)
}

#' Create the active nodes in memory plot
#'
#' Use starvz_data to create a line plot of the number of active nodes per type
#' along the application execution time
#'
#' @param data starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param aggregation enable/disable time aggregation for the plot
#' @param legend enable/disable plot legends
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @examples
#' \dontrun{
#' panel_activenodes(data = starvz_sample_lu, step = 100)
#' }
#' @export
panel_activenodes <- function(data = NULL,
                              step = data$config$activenodes$aggregation$step,
                              aggregation = data$config$activenodes$aggregation$active,
                              x_start = data$config$limits$start,
                              x_end = data$config$limits$end,
                              legend = data$config$activenodes$legend) {
  starvz_check_data(data, tables = list("Atree" = c("ANode")))

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data$config$global_agg_step)) {
      step <- 100
    } else {
      step <- data$config$global_agg_step
    }
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  panel <- geom_blank()

  # Prepare data
  data_active_nodes <- data$Application %>%
    filter(grepl("front", .data$Value) | grepl("subtree", .data$Value)) %>%
    select("ResourceId", "Start", "End", "Value", "ANode") %>%
    mutate(
      node_count = case_when(
        .data$Value == "do_subtree" ~ 1,
        .data$Value == "init_front" ~ 1,
        .data$Value == "clean_front" ~ -1,
        TRUE ~ 0
      )
    ) %>%
    # get the node types from Atree
    left_join(
      data$Atree %>% select("ANode", "NodeType"),
      by = "ANode"
    ) %>%
    arrange(.data$Start) %>%
    group_by(.data$NodeType) %>%
    rename(Task = "Value") %>%
    mutate(Value = cumsum(.data$node_count)) %>%
    filter(!is.na(.data$NodeType))

  # use time aggregation or not
  if (aggregation) {
    data_active_nodes <- data_active_nodes %>%
      # need for the var integration
      mutate(Type = NA) %>%
      mutate(ResourceType = .data$NodeType, Node = .data$NodeType) %>%
      ungroup()

    panel <- var_integration_chart(data_active_nodes, ylabel = NA, step = step, facetting = FALSE, base_size = data$config$base_size, expand = data$config$expand)
  } else {
    panel <- data_active_nodes %>%
      ggplot(aes(x = .data$Start, y = .data$Value, color = .data$NodeType)) +
      default_theme(data$config$base_size, data$config$expand) +
      geom_line()
  }

  panel <- panel +
    ylab("Active\nNodes") +
    scale_colour_brewer(palette = "Dark2")

  # configure legend
  if (!legend) {
    panel <- panel + theme(legend.position = "none")
  }

  tzScale <- list(
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  )
  panel <- panel + tzScale

  return(panel)
}

# Calculate the computational resource utilization by tree node
#' Create the node memory usage plot
#'
#' Use starvz_data to create a line plot of the memory usage in MB of
#' active nodes along the application execution time
#'
#' @param Application starvz application data
#' @param Atree starvz elimination tree data
#' @param step size in milliseconds for the time aggregation step
#' @param group_pruned aggregate computations of the same parent pruned nodes
#' @param performance_metric Performance metric to save in Value1 [Time, GFlops]
resource_utilization_tree_node <- function(Application = NULL,
                                           Atree = NULL,
                                           step = 100,
                                           group_pruned = FALSE,
                                           performance_metric = "Time") {
  # Prepare and filter data
  df_filter <- Application %>%
    filter(
      grepl("qrt", .data$Value) | grepl("subtree", .data$Value)
    ) %>%
    select("ANode", "Start", "End", "JobId", "GFlop") %>%
    unique() %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- Application %>%
    select("ResourceId") %>%
    unique() %>%
    nrow()

  # Group pruned nodes with same Parent to aggregate their computations
  if (isTRUE(group_pruned)) {
    # When we change ANode, we also need to update it in Atree, but this will modify other plots as well
    groupPruned <- Atree %>%
      mutate(NewANode = case_when(.data$NodeType == "Pruned" ~ paste0(.data$Parent, "Pruned"), TRUE ~ .data$ANode)) %>%
      select("ANode", "NodeType", "NewANode")

    df_filter <- df_filter %>%
      left_join(
        groupPruned %>%
          select("ANode", "NodeType", "NewANode"),
        by = "ANode"
      ) %>%
      mutate(originalAnode = .data$ANode, ANode = .data$NewANode)
  }

  if (tolower(performance_metric) == "time") {
    # Compute the node parallelism
    data_node_parallelism <- df_filter %>%
      gather(.data$Start, .data$End, key = "Event", value = "Time") %>%
      arrange(.data$Time) %>%
      group_by(.data$ANode) %>%
      mutate(Value = ifelse(.data$Event == "Start", 1, -1)) %>%
      mutate(nodeParallelism = cumsum(.data$Value)) %>%
      ungroup()

    # Do the time aggregation of resource utilization by ANode
    data_node_plot <- data_node_parallelism %>%
      select("ANode", "Time", "nodeParallelism") %>%
      arrange(.data$Time) %>%
      mutate(End = lead(.data$Time)) %>%
      mutate(Duration = .data$End - .data$Time) %>%
      rename(Start = "Time", Value = "nodeParallelism") %>%
      na.omit() %>%
      group_by(.data$ANode) %>%
      do(remyTimeIntegrationPrepNoDivision(., myStep = step)) %>%
      # This give us the total worker usage grouped by ANode in a time slice
      mutate(Value1 = .data$Value / (step * NWorkers) * 100) %>%
      ungroup()
  } else if (tolower(performance_metric) == "gflops") {
    # get the slices and the task data
    data_node_plot <- getSlices(Application, step = step) %>%
      tibble(Slice = .) %>%
      group_by(.data$Slice) %>%
      mutate(nest(Application %>%
        filter(grepl("qrt", .data$Value) | grepl("subtree", .data$Value)) %>%
        select("Start", "End", "ANode", "Duration", "GFlop"), data = everything())) %>%
      # filter task by slice and calculate GFlops contribution to that slice
      mutate(SliceData = map2(.data$data, .data$Slice, function(x, y) {
        SliceStart <- .data$Slice
        SliceEnd <- .data$Slice + step
        x %>%
          filter((.data$End >= SliceStart & .data$End <= SliceEnd) |
            (.data$Start >= SliceStart & .data$Start <= SliceEnd) |
            (.data$Start <= SliceStart & .data$End >= SliceEnd)) %>%
          mutate(GFlopMultiplier = case_when(
            # 1 - task is completely inside the slice:                 | s---e  |
            .data$Start >= SliceStart & .data$End <= SliceEnd ~ 1,
            # 2 - task start before the slice and ends inside it:  s---|----e   |
            .data$Start <= SliceStart & .data$End <= SliceEnd ~ 1 - (SliceStart - .data$Start) / .data$Duration,
            # 3 - task start in the slice and ends after it:           |  s-----|--e
            .data$Start >= SliceStart & .data$End >= SliceEnd ~ 1 - (.data$End - SliceEnd) / .data$Duration,
            # 4 - task starts before the slice and ends afeter it: s---|--------|--e
            .data$Start <= SliceStart & .data$End >= SliceEnd ~ 1 - ((.data$End - SliceEnd) + (SliceStart - .data$Start)) / .data$Duration
          )) %>%
          mutate(SliceGFlop = .data$GFlop * .data$GFlopMultiplier) %>%
          group_by(.data$ANode) %>%
          summarize(GFlop = sum(.data$GFlop), SliceGFlop = sum(.data$SliceGFlop), .groups = "keep")
      })) %>%
      select(-"data") %>%
      unnest(cols = c("SliceData")) %>%
      mutate(Value1 = .data$SliceGFlop) %>%
      ungroup()
  }

  # Restore original values of ANode for the Pruned nodes
  if (isTRUE(group_pruned)) {
    data_node_plot <- data_node_plot %>%
      left_join(df_filter %>% select("ANode", "originalAnode"),
        by = "ANode"
      ) %>%
      mutate(ANode = .data$originalAnode) %>%
      select(-"originalAnode")
  }

  return(data_node_plot)
}

# Calculate the computational resource utilization by tree depth
resource_utilization_tree_depth <- function(Application = NULL, Atree = NULL, step = 100) {
  # Prepare and filter data
  df_filter <- Application %>%
    filter(grepl("qrt", .data$Value) | grepl("do_subtree", .data$Value)) %>%
    select("ANode", "Start", "End", "JobId") %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- Application %>%
    filter(grepl("CPU", .data$ResourceType) | grepl("CUDA", .data$ResourceType)) %>%
    select("ResourceId") %>%
    unique() %>%
    nrow()

  # Compute the node parallelism
  df_node_parallelism <- df_filter %>%
    select("ANode", "Start", "JobId") %>%
    mutate(Event = "Start") %>%
    rename(Time = "Start") %>%
    bind_rows(df_filter %>%
      select("ANode", "End", "JobId") %>%
      mutate(Event = "End") %>%
      rename(Time = "End")) %>%
    arrange(.data$Time) %>%
    group_by(.data$ANode) %>%
    mutate(Value = ifelse(.data$Event == "Start", 1, -1)) %>%
    mutate(nodeParallelism = cumsum(.data$Value)) %>%
    ungroup()

  # Integrate resource utilization by ANode
  df_node_plot <- df_node_parallelism %>%
    select("ANode", "Time", "nodeParallelism") %>%
    arrange(.data$Time) %>%
    mutate(End = lead(.data$Time)) %>%
    mutate(Duration = .data$End - .data$Time) %>%
    rename(Start = "Time", Value = "nodeParallelism") %>%
    na.omit() %>%
    group_by(.data$ANode) %>%
    do(remyTimeIntegrationPrepNoDivision(., myStep = step)) %>%
    # This give us the total worker usage grouped by ANode in a time slice
    mutate(Value1 = (.data$Value / (step * NWorkers)) * 100) %>%
    # usage by slice
    group_by(.data$Slice) %>%
    arrange(.data$Slice) %>%
    mutate(Usage = sum(.data$Value1)) %>%
    # usage by depth
    left_join(Atree %>% select("ANode", "Depth"), by = "ANode") %>%
    group_by(.data$Slice, .data$Depth) %>%
    mutate(Value1 = sum(.data$Value1)) %>%
    ungroup() %>%
    select(-"ANode", -"Value") %>%
    unique()

  # expand all time slices with the possible colors (for geom_ribbon)
  data_depth_plot <- df_node_plot %>%
    expand(.data$Slice, Depth = 1:(df_node_plot$Depth %>% max())) %>%
    left_join(df_node_plot, by = c("Slice", "Depth")) %>%
    group_by(.data$Depth) %>%
    arrange(.data$Depth) %>%
    mutate(Value1 = na.locf(.data$Value1)) %>%
    arrange(.data$Slice, .data$Depth) %>%
    group_by(.data$Slice) %>%
    mutate(Ymin = lag(.data$Value1), Ymin = ifelse(is.na(.data$Ymin), 0, .data$Ymin)) %>%
    mutate(Ymin = cumsum(.data$Ymin), Ymax = .data$Ymin + .data$Value1) %>%
    mutate(Slc = .data$Slice %% step) %>%
    ungroup() %>%
    filter(.data$Slc == 0 | .data$Slice == max(.data$Slice))

  return(data_depth_plot)
}

# Add anomalies representation in the tree structure
atree_geom_anomalies <- function(data) {
  anomalies_points <- data$Application %>%
    select(-"Position") %>%
    left_join(
      data$Atree %>%
        select(
          "ANode",
          "Position"
        ),
      by = "ANode"
    ) %>%
    filter(.data$Outlier) %>%
    mutate(Height = 1)

  list(
    geom_point(
      data = anomalies_points,
      aes(
        x = .data$Start,
        y = .data$Position + .data$Height / 2,
        color = .data$Value,
      ),
      size = 0.5,
      shape = 1
    ),
    scale_color_manual(values = c(
      "geqrt" = "#FF7F00", "gemqrt" = "#377EB8", "tpqrt" = "#F781BF", "tpmqrt" = "#A65628",
      "lapack_geqrt" = "#FF7F00", "lapack_gemqrt" = "#377EB8", "lapack_tpqrt" = "#F781BF", "lapack_tpmqrt" = "#A65628"
    )),
    scale_shape_manual(values = c("19"))
  )
}


# Define a geom rect receiving the data and defining the fill color as the resource usage
atree_geom_rect_gradient <- function(data, yminOffset, ymaxOffset) {
  list(
    geom_rect(
      data = data,
      aes(
        fill = .data$NodeUsage,
        xmin = .data$Start,
        xmax = .data$End,
        ymin = .data$Position + yminOffset,
        ymax = .data$Position + ymaxOffset,
      )
    )
  )
}

# Define a geom rect receiving the data, color, and height of the geom_rect
atree_geom_rect <- function(data, color, yminOffset, ymaxOffset, alpha) {
  list(
    geom_rect(
      data = data,
      aes(
        xmin = .data$Start,
        xmax = .data$End,
        ymin = .data$Position + yminOffset,
        ymax = .data$Position + ymaxOffset
      ),
      fill = color,
      alpha = alpha
    )
  )
}

# remyTimeIntegrationPrep without dividing the Value column by the time slice
remyTimeIntegrationPrepNoDivision <- function(dfv = NULL, myStep = 100) {
  if (is.null(dfv)) {
    return(NULL)
  }
  if (nrow(dfv) == 0) {
    return(NULL)
  }
  mySlices <- getSlices(dfv, step = myStep)
  tibble(Slice = mySlices, Value = c(remyTimeIntegration(dfv, slices = mySlices), 0))
}

# Set of functions to define the color of the nodes in the resource utilization plot
get_min_color <- function(node) {
  min_color <- 0
  while (1) {
    if (min_color %in% pkg.env$active_colors) {
      min_color <- min_color + 1
    } else {
      pkg.env$df_colors <- pkg.env$df_colors %>%
        rbind(tibble(ANode = node, Event = "Start", color = min_color))
      pkg.env$active_colors <- c(pkg.env$active_colors, min_color)
      return(min_color)
    }
  }
}

find_node_color <- function(node) {
  color <- pkg.env$df_colors %>%
    filter(.data$ANode == node & .data$Event == "Start") %>%
    select("color") %>%
    .$color

  return(color)
}

set_color_available <- function(node) {
  color <- find_node_color(node)

  pkg.env$df_colors <- pkg.env$df_colors %>%
    rbind(tibble(ANode = node, Event = "End", color = color))
  pkg.env$active_colors <- pkg.env$active_colors[pkg.env$active_colors != as.integer(color)]

  return(color)
}

define_colors <- function(data) {
  ANode <- data[1]
  Event <- data[2]

  if (Event == "Start") {
    get_min_color(ANode)
  } else {
    set_color_available(ANode)
  }
}

get_tree_utilization <- function(data, step, performance_metric) {
  data_utilization_node <- resource_utilization_tree_node(data$Application,
    data$Atree,
    step = step, group_pruned = TRUE, performance_metric = performance_metric
  ) %>%
    unique() %>%
    filter(.data$Value1 != 0) %>%
    select("ANode", "Slice", "Value1") %>%
    ungroup() %>%
    rename(NodeUsage = "Value1") %>%
    left_join(data$Atree, by = "ANode")

  # Resize for not pruned nodes, first and last computational tasks
  # data_tree_utilization <- data_gflops_node  %>%
  data_tree_utilization <- data_utilization_node %>%
    inner_join(
      data$Application %>%
        filter(grepl("qrt", .data$Value) | grepl("do_sub", .data$Value)) %>%
        select("ANode", "End", "Start") %>%
        group_by(.data$ANode) %>%
        mutate(Start = min(.data$Start), End = max(.data$End)) %>%
        select("ANode", "Start", "End") %>%
        unique(),
      by = "ANode"
    ) %>%
    mutate(Start = ifelse(.data$Start >= .data$Slice, .data$Start, .data$Slice)) %>%
    mutate(End = ifelse(.data$Slice + step >= .data$End, .data$End, .data$Slice + step))

  return(data_tree_utilization)
}

get_tree_flops <- function(data, step) {
  data_utilization_node <- resource_utilization_tree_node(data$Application, data$Atree, step = step, group_pruned = TRUE) %>%
    unique() %>%
    filter(.data$Value != 0) %>%
    select("ANode", "Slice", "Value1") %>%
    ungroup() %>%
    rename(NodeUsage = "Value1") %>%
    left_join(data$Atree, by = "ANode")

  # Resize for not pruned nodes, first and last computational tasks
  data_tree_utilization <- data_utilization_node %>%
    inner_join(
      data$Application %>%
        filter(grepl("qrt", .data$Value) | grepl("do_sub", .data$Value)) %>%
        select("ANode", "End", "Start") %>%
        group_by(.data$ANode) %>%
        mutate(Start = min(.data$Start), End = max(.data$End)) %>%
        select("ANode", "Start", "End") %>%
        unique(),
      by = "ANode"
    ) %>%
    mutate(Start = ifelse(.data$Start >= .data$Slice, .data$Start, .data$Slice)) %>%
    mutate(End = ifelse(.data$Slice + step >= .data$End, .data$End, .data$Slice + step))

  return(data_tree_utilization)
}

#' Combine two atree plots to compare two different executions
#'
#' Use starvz_data Application and Atree to create a plot that shows the
#' total resource utilization, painted by tree node using geom_ribbon. The
#' colors are reused between nodes, not tied to a specific tree node.
#'
#' @param data1 starvz_data with trace data
#' @param data2 starvz_data with trace data
#' @param step size in milliseconds for the time aggregation step
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param performance_metric which metric to represent ["time", "gflops"]
#' @param add_diff_line add the computed gflops difference line
#' @param add_end_line add smaller end time vertical line
#' @return A ggplot object
#' @examples
#' \dontrun{
#' panel_compare_tree(data1, data2, step = 100)
#' }
#' @export
panel_compare_tree <- function(data1 = NULL,
                               data2 = NULL,
                               step = data1$config$utiltreenode$step,
                               x_start = data1$config$limits$start,
                               x_end = data1$config$limits$end,
                               performance_metric = "Time",
                               add_diff_line = FALSE,
                               add_end_line = FALSE) {
  starvz_check_data(data1, tables = list("Atree" = c("ANode")))
  starvz_check_data(data2, tables = list("Atree" = c("ANode")))

  if (is.null(step) || !is.numeric(step)) {
    if (is.null(data1$config$global_agg_step)) {
      step <- 100
    } else {
      step <- data1$config$global_agg_step
    }
  }

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

  data_tree_utilization1 <- get_tree_utilization(faster, step, performance_metric)
  data_tree_utilization2 <- get_tree_utilization(slower, step, performance_metric)

  # Calculate the diff
  data_diff <- data_tree_utilization1 %>%
    mutate(Execution = "NodeUsage.faster") %>%
    bind_rows(data_tree_utilization2 %>%
      mutate(Execution = "NodeUsage.slower")) %>%
    select(-"Start", -"End") %>%
    spread(.data$Execution, .data$NodeUsage) %>%
    mutate(
      NodeUsage.faster = ifelse(is.na(.data$NodeUsage.faster), 0, .data$NodeUsage.faster),
      NodeUsage.slower = ifelse(is.na(.data$NodeUsage.slower), 0, .data$NodeUsage.slower)
    ) %>%
    # check when we have ocncurrent or exclusive execution
    mutate(boxColor = case_when(
      .data$NodeUsage.faster != 0 & .data$NodeUsage.slower != 0 ~ "Concurrent execution",
      .data$NodeUsage.faster != 0 & .data$NodeUsage.slower == 0 ~ "Exclusive faster",
      .data$NodeUsage.faster == 0 & .data$NodeUsage.slower != 0 ~ "Exclusive slower"
    )) %>%
    # ifelse(.data$NodeUsage.faster == 0 | .data$NodeUsage.slower == 0, "Exclusive execution", "Concurrent execution")) %>%
    mutate(NodeUsage = .data$NodeUsage.faster - .data$NodeUsage.slower)

  # add cumulative performance metric line
  if (add_diff_line) {
    lineplot <- panel_gflops_diff(
      data_tree_utilization1, data_tree_utilization2,
      slower$config$base_size, slower$config$expand, performance_metric
    )
  }

  atreeplot <- panel_atree_structure(slower) +
    default_theme(slower$config$base_size, slower$config$expand) +
    ylab("Elimination Tree\n[Submission Order]")

  atreeplot <- atreeplot +
    geom_rect(
      data = data_diff,
      aes(
        fill = .data$NodeUsage,
        xmin = .data$Slice,
        xmax = .data$Slice + step,
        ymin = .data$Position,
        ymax = .data$Position + .data$Height,
        color = .data$boxColor,
        linetype = .data$boxColor
      ),
      # color = "#807d7c",
      size = 0.5
    ) +
    scale_linetype_manual(
      breaks = c("Concurrent execution", "Exclusive faster", "Exclusive slower"),
      values = c("blank", "solid", "solid")
    ) +
    scale_color_manual(
      breaks = c("Concurrent execution", "Exclusive faster", "Exclusive slower"),
      values = c("black", "blue", "red")
    ) +
    guides(color = FALSE, linetype = FALSE)

  myPalette <- colorRampPalette(c(rev(brewer.pal(9, "Reds")[2:9]), brewer.pal(9, "Greens")[2:9]))

  if (tolower(performance_metric) == "time") {
    palette_colors <- scale_fill_gradientn(colours = myPalette(20), limits = c(-100, 100))
    atreeplot <- atreeplot + palette_colors
  } else if (tolower(performance_metric) == "gflops") {
    minGFlop <- round(min(data_diff$NodeUsage), 1)
    maxGFlop <- round(max(data_diff$NodeUsage), 1)
    atreeplot <- atreeplot + scale_fill_gradient2(
      name = paste0("GFlops difference per time slice (", step, "ms)"),
      low = "red", mid = "grey", high = "blue",
      limits = c(minGFlop, maxGFlop),
      midpoint = 0,
      breaks = c(minGFlop, 0, maxGFlop)
    ) +
      theme(legend.title = element_text(size = rel(0.8))) +
      guides(fill = guide_colorbar(barwidth = 15, barheight = 1, nbin = 30, title.position = "right", title.vjust = 1), linetype = "none")
    # palette_colors <- scale_fill_gradientn(colours = myPalette(20))
  }

  tzScale <- list(
    coord_cartesian(
      xlim = c(x_start, x_end)
    )
  )
  atreeplot <- atreeplot + tzScale

  # add vetical computation ending line for faster case
  if (add_end_line) {
    atreeplot <- atreeplot + geom_vline(aes(xintercept = c(end_cut)))

    if (add_diff_line) {
      lineplot <- lineplot + geom_vline(aes(xintercept = c(end_cut)))
      lineplot <- lineplot + tzScale
    }
  }
  # add the line to the plot
  if (add_diff_line) {
    atreeplot <- (atreeplot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())) / lineplot + patchwork::plot_layout(heights = c(1, 0.25))
  }

  return(atreeplot)
}

# Represent the total computed GFlops difference over time
panel_gflops_diff <- function(data_tree_utilization1, data_tree_utilization2, base_size, expand, performance_metric) {
  data_diff_line <- data_tree_utilization1 %>%
    mutate(Execution = "NodeUsage.x") %>%
    bind_rows(data_tree_utilization2 %>%
      mutate(Execution = "NodeUsage.y")) %>%
    spread(.data$Execution, .data$NodeUsage) %>%
    mutate(
      NodeUsage.x = ifelse(is.na(.data$NodeUsage.x), 0, .data$NodeUsage.x),
      NodeUsage.y = ifelse(is.na(.data$NodeUsage.y), 0, .data$NodeUsage.y)
    ) %>%
    group_by(.data$Slice, .groups = "keep") %>%
    summarize(NodeUsage.x = sum(.data$NodeUsage.x), NodeUsage.y = sum(.data$NodeUsage.y)) %>%
    ungroup() %>%
    mutate(Usage.x = cumsum(.data$NodeUsage.x), Usage.y = cumsum(.data$NodeUsage.y)) %>%
    mutate(diff = .data$Usage.x - .data$Usage.y) %>%
    mutate(signal = ifelse(.data$diff >= 0, "positive", "negative"))

  lineplot <- data_diff_line %>%
    arrange(.data$signal) %>%
    ggplot(aes(x = .data$Slice, y = .data$diff, color = .data$signal)) +
    geom_line(aes(group = .data$.groups)) +
    scale_color_manual(
      breaks = c("positive", "negative"),
      values = c("blue", "red")
    ) +
    labs(y = paste0(performance_metric, "\n difference")) +
    default_theme(base_size, expand) +
    theme(legend.position = "none")

  return(lineplot)
}
