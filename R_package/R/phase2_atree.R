#' @include starvz_data.R

geom_atree <- function(data = NULL, Offset = 1.02, Flip = TRUE) {
  if (is.null(data)) stop("input data for geom_atree is NULL")

  makespan <- data$Application %>%
    .$End %>%
    max()

  ffactor <- ifelse(Flip, +1, -1)
  dfactor <- makespan * 0.04
  doffset <- makespan * Offset

  dtree <- data$Atree %>%
    # Get Start time of the first task belonging to each ANode
    left_join(data$Application %>%
      filter(.data$Value != "block_copy") %>%
      select(.data$ANode, .data$Start, .data$End) %>%
      group_by(.data$ANode) %>%
      summarize(
        Start = min(.data$Start),
        End = max(.data$End)
      ),
    by = "ANode"
    ) %>%
    # Get graphical properties of each parent for each row
    left_join(
      x = .,
      y = .,
      by = c("Parent" = "ANode"), suffix = c(".Node", ".Parent")
    ) %>%
    rename(
      Height = .data$Height.Node,
      Position = .data$Position.Node,
      Depth = .data$Depth.Node,
      Intermediary = .data$Intermediary.Node,
      Start = .data$Start.Node,
      End = .data$End.Node
    ) %>%
    select(-.data$Parent.Parent) %>%
    # Keep only intermediary nodes
    filter(.data$Intermediary == TRUE) %>%
    # Calculate coordinates for lines connecting child with parent
    mutate(
      Edge.X = .data$Start,
      Edge.Y = .data$Position + .data$Height / 2,
      Edge.Xend = .data$Start.Parent,
      Edge.Yend = .data$Position.Parent + .data$Height.Parent / 2
    ) %>%
    mutate(
      Edge.End.X = .data$End,
      Edge.End.Y = .data$Position + .data$Height / 2,
      Edge.End.Xend = .data$End.Parent,
      Edge.End.Yend = .data$Position.Parent + .data$Height.Parent / 2
    )

  # data frame for the tree plot structure
  dstruct <- dtree %>%
    # Remove that without parent
    filter(!is.na(.data$Parent)) %>%
    # The root has no tasks associated with, remove it.
    mutate(Parent = as.integer(.data$Parent)) %>%
    filter(.data$Parent != max(.data$Parent))

  # data frame for node "lifetime" geom_segment
  dline <- dtree %>%
    select(.data$ANode, .data$Start, .data$End, .data$Position, .data$Height, .data$Parent) %>%
    filter(!is.na(.data$Parent), !is.na(.data$Start))

  ret <-
    list(
      # Lines connecting child with parent (Start)
      geom_segment(
        data = dstruct,
        arrow = arrow(length = unit(0.03, "npc")),
        aes(
          x = .data$Edge.X,
          y = .data$Edge.Y,
          xend = .data$Edge.Xend,
          yend = .data$Edge.Yend
        ),
        color = "blue"
      ),
      geom_point(
        data = dstruct,
        aes(
          x = .data$Edge.X,
          y = .data$Edge.Y
        ), color = "blue"
      ),
      # Lines connecting child with parent (End)
      geom_segment(
        data = dstruct,
        arrow = arrow(length = unit(0.03, "npc")),
        aes(
          x = .data$Edge.End.X,
          y = .data$Edge.End.Y,
          xend = .data$Edge.End.Xend,
          yend = .data$Edge.End.Yend
        ),
        color = "red"
      ),
      geom_point(
        data = dstruct,
        aes(
          x = .data$Edge.End.X,
          y = .data$Edge.End.Y
        ), color = "red"
      ),
      # Fix time coordinates
      coord_cartesian(xlim = c(0, makespan)),
      # Horizontal lines
      geom_segment(data = dline, aes(
        y = .data$Position + .data$Height / 2,
        yend = .data$Position + .data$Height / 2, x = .data$Start, xend = .data$End
      ), color = "lightblue")
    )
  return(ret)
}

atree_temporal_chart <- function(data = NULL, step = 100, globalEndTime = NULL) {
  if (is.null(data)) stop("a NULL data has been provided to atree_temporal_chart")

  loginfo("Entry of atree_temporal_chart")

  df_node_plot <- resource_utilization_tree_node(data = data, step = step)

  # Calculate NodeUsage, this represent the "most active" node at the time slice
  df_node_plot %>%
    filter(.data$Value != 0) %>%
    select(.data$ANode, .data$Slice, .data$Value1, .data$Usage) %>%
    ungroup() %>%
    rename(NodeUsage = .data$Value1) %>%
    left_join(data$Atree, by = "ANode") %>%
    # resize for node computation Start
    inner_join(data$Application %>%
      filter(grepl("init_", .data$Value)) %>%
      select(.data$ANode, .data$End) %>% group_by(.data$ANode) %>%
      filter(.data$End == max(.data$End)),
    by = "ANode"
    ) %>%
    mutate(Start = ifelse(.data$End >= .data$Slice, .data$End, .data$Slice)) %>%
    # resize for node computation End
    select(-.data$End) %>%
    inner_join(data$Application %>%
      select(.data$ANode, .data$End) %>% group_by(.data$ANode) %>%
      filter(.data$End == max(.data$End)),
    by = "ANode"
    ) %>%
    mutate(End = ifelse(.data$Slice + step >= .data$End, .data$End, .data$Slice + step)) -> df_node_plot_filtered

  # filter initialization tasks
  dfw_init <- data$Application %>%
    filter(grepl("init_", .data$Value)) %>%
    unique() %>%
    select(-.data$Position, -.data$Height) %>%
    left_join(data$Atree, by = "ANode")

  # Prepare for colors
  atreeplot <- data$Application %>%
    # Considering only Intermediary nodes
    filter(.data$Intermediary) %>%
    filter(grepl("qrt", .data$Value) | grepl("subtree", .data$Value)) %>%
    unique() %>%
    # Remove all tasks that do not have ANode
    filter(!is.na(.data$Height.ANode)) %>%
    ggplot() +
    default_theme(data$config$base_size, data$config$expand) +
    ylab("Elimination\nTree [nodes]") +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    # Add the atree structure representation
    geom_atree(data, Offset = 1.05, Flip = TRUE)

  if (data$config$atree$utilization) {
    atreeplot <- atreeplot +
      geom_rect(
        data = df_node_plot_filtered,
        aes(
          fill = .data$NodeUsage,
          xmin = .data$Start,
          xmax = .data$End,
          ymin = .data$Position,
          ymax = .data$Position + .data$Height
        )
      ) +
      scale_fill_gradient2(name = "Computational Load", limits = c(0, 100), midpoint = 50, low = "blue", mid = "yellow", high = "red") +
      geom_rect(
        data = dfw_init,
        aes(
          xmin = .data$Start,
          xmax = .data$End,
          ymin = .data$Position,
          ymax = .data$Position + .data$Height
        ),
        fill = "#4DAF4A"
      )
  }

  # plot "communication" block_copy tasks in the tree nodes
  if (data$config$atree$communication$active) {
    # filter communication tasks
    dfw_comm <- dfw %>%
      filter(grepl("block_", .data$Value)) %>%
      unique() %>%
      select(-.data$Position, -.data$Height) %>%
      left_join(data$Atree, by = "ANode")

    atreeplot <- atreeplot +
      geom_rect(
        data = dfw_comm,
        aes(
          xmin = .data$Start,
          xmax = .data$End,
          ymin = .data$Position + 0.25 * .data$Height,
          ymax = .data$Position + 0.75 * .data$Height
        ),
        fill = "#000000", alpha = .3
      )
  }

  loginfo("Exit of atree_temporal_chart")
  return(atreeplot)
}

active_nodes_chart <- function(data = NULL) {
  if (is.null(data)) stop("a NULL data has been provided to active_nodes_chart")
  loginfo("Entry of active_nodes_chart")
  activenodesplot <- geom_blank()

  if (data$config$activenodes$aggregation$active) {
    # use time aggregation
    dfv <- data$Application %>%
      rename(Task = .data$Value) %>%
      filter(grepl("front", .data$Task) | grepl("subtree", .data$Task)) %>%
      mutate(node_count = case_when(
        .data$Task == "do_subtree" ~ 1,
        .data$Task == "init_front" ~ 1,
        .data$Task == "clean_front" ~ -1,
        TRUE ~ 0
      )) %>%
      mutate(nodeType = case_when(grepl("subtree", .data$Task) ~ "Pruned")) %>%
      mutate(Type = NA)

    # get the nodes that where pruned
    seq_tree <- dfv %>%
      filter(grepl("subtree", .data$Task)) %>%
      .$ANode

    dfv <- dfv %>%
      select(-.data$nodeType) %>%
      left_join(dfv %>% select(.data$ANode, .data$nodeType), by = "ANode") %>%
      mutate(nodeType = ifelse(.data$ANode %in% seq_tree, "Pruned", "Not Pruned")) %>%
      arrange(.data$Start) %>%
      group_by(.data$nodeType) %>%
      mutate(Value = 0) %>%
      mutate(Value = cumsum(.data$node_count)) %>%
      mutate(ResourceType = .data$nodeType, Node = .data$nodeType)

    step <- data$config$activenodes$aggregation$step
    activenodesplot <- var_integration_chart(dfv, ylabel = NA, step = step, facetting = FALSE, base_size = data$config$base_size, expand = data$config$expand)
  } else {
    # do not use time aggregation
    df_active <- data$Application %>%
      filter(grepl("front", .data$Value) | grepl("subtree", .data$Value)) %>%
      select(.data$ResourceId, .data$Start, .data$End, .data$Value, .data$ANode) %>%
      mutate(
        node_count = case_when(
          .data$Value == "do_subtree" ~ "1",
          .data$Value == "init_front" ~ "1",
          .data$Value == "clean_front" ~ "-1",
          TRUE ~ "<NA>"
        ),
        node_count = as.integer(.data$node_count)
      )

    # get all nodes that roots of sequential subtrees
    df_active %>%
      filter(.data$Value == "do_subtree") %>%
      mutate(nodeType = TRUE) %>%
      select(.data$ANode, .data$nodeType) -> seq_tree

    df_active %>%
      filter_at(vars(.data$ANode), any_vars(.data %in% seq_tree$ANode)) %>%
      mutate(nodeType = "Pruned") -> seq_nodes

    df_active %>%
      filter_at(vars(.data$ANode), any_vars(!.data %in% seq_tree$ANode)) %>%
      mutate(nodeType = "Not Pruned") -> front_nodes

    # let's merge them
    df_all <- front_nodes %>% bind_rows(seq_nodes)
    df_all %>%
      arrange(.data$Start) %>%
      group_by(.data$nodeType) %>%
      mutate(active = 0) %>%
      mutate(active = cumsum(.data$node_count)) -> df_all

    activenodesplot <- df_all %>%
      ggplot(aes(x = .data$Start, y = .data$active, color = .data$nodeType)) +
      default_theme(data$config$base_size, data$config$expand) +
      geom_line()
  }

  activenodesplot <- activenodesplot +
    theme(legend.position = "top") +
    ylab("Active\nNodes") +
    scale_colour_brewer(palette = "Dark2")

  loginfo("Exit of active_nodes_chart")
  return(activenodesplot)
}

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

resource_utilization_tree_node <- function(data = NULL, step = 100) {
  loginfo("Entry of resource_utilization_tree_node")
  # Prepare and filter data
  df_filter <- data$Application %>%
    filter(
      grepl("qrt", .data$Value) | grepl("subtree", .data$Value)
    ) %>%
    select(.data$ANode, .data$Start, .data$End, .data$JobId) %>%
    unique() %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- data$Application %>%
    filter(grepl("CPU", .data$ResourceType) | grepl("CUDA", .data$ResourceType)) %>%
    select(.data$ResourceId) %>%
    unique() %>%
    nrow()

  # Compute the node parallelism
  df_node_parallelism <- df_filter %>%
    select(.data$ANode, .data$Start, .data$JobId) %>%
    mutate(Event = "Start") %>%
    rename(Time = .data$Start) %>%
    bind_rows(df_filter %>%
      select(.data$ANode, .data$End, .data$JobId) %>%
      mutate(Event = "End") %>%
      rename(Time = .data$End)) %>%
    arrange(.data$Time) %>%
    group_by(.data$ANode) %>%
    mutate(Value = ifelse(.data$Event == "Start", 1, -1)) %>%
    mutate(nodeParallelism = cumsum(.data$Value)) %>%
    ungroup()

  # Integrate resource utilization by ANode
  df_node_plot <- df_node_parallelism %>%
    select(.data$ANode, .data$Time, .data$nodeParallelism) %>%
    arrange(.data$Time) %>%
    mutate(End = lead(.data$Time)) %>%
    mutate(Duration = .data$End - .data$Time) %>%
    rename(Start = .data$Time, Value = .data$nodeParallelism) %>%
    na.omit() %>%
    group_by(.data$ANode) %>%
    do(remyTimeIntegrationPrepNoDivision(., myStep = step)) %>%
    # This give us the total worker usage grouped by ANode in a time slice
    mutate(Value1 = .data$Value / (step * NWorkers) * 100) %>%
    ungroup() %>%
    group_by(.data$Slice) %>%
    arrange(.data$Slice) %>%
    mutate(Usage = sum(.data$Value1))

  loginfo("Exit of resource_utilization_tree_node")
  return(df_node_plot)
}

resource_utilization_tree_node_plot <- function(data = NULL, step = 100) {
  df1 <- resource_utilization_tree_node(data = data, step = step)

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
    select(.data$ANode, .data$Color) %>%
    unique()

  # join data frames
  df2 <- df1 %>%
    ungroup() %>%
    left_join(event_data, by = "ANode") %>%
    group_by(.data$Slice)

  # must expand data frame to make geom_area work properly
  df_plot <- df2 %>%
    filter(!is.na(.data$Color)) %>%
    select(-.data$ANode) %>%
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
    select(.data$Color) %>%
    max(.$Color)
  if (ncolors <= 10) {
    palette <- brewer.pal(9, "Greens")[3:9]
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
    coord_cartesian(ylim = c(0, 100)) +
    ylim(0, 100)
}

resource_utilization_tree_depth <- function(data = NULL, base_size = 22, expand = 0.05) {
  maxDepth <- data %>%
    .$Depth %>%
    max()
  depthPalette <- rev(colorRampPalette(brewer.pal(9, "YlOrRd"))(maxDepth))

  data %>%
    ggplot() +
    geom_ribbon(aes(ymin = .data$Ymin, ymax = .data$Ymax, x = .data$Slice, fill = as.factor(.data$Depth))) +
    default_theme(base_size, expand) +
    theme(legend.position = "top") +
    scale_fill_manual(values = depthPalette) +
    ylab("Usage %\nDepth") +
    coord_cartesian(ylim = c(0, 100)) +
    ylim(0, 100)
}

resource_utilization_tree_depth_plot <- function(data = NULL, step = 100) {
  loginfo("Entry of resource_utilization_tree_depth_plot")
  # Prepare and filter data
  df_filter <- data$Application %>%
    filter(grepl("qrt", .data$Value) | grepl("do_sub", .data$Value)) %>%
    select(.data$ANode, .data$Start, .data$End, .data$JobId) %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- data$Application %>%
    filter(grepl("CPU", .data$ResourceType) | grepl("CUDA", .data$ResourceType)) %>%
    select(.data$ResourceId) %>%
    unique() %>%
    nrow()

  # Compute the node parallelism
  df_node_parallelism <- df_filter %>%
    select(.data$ANode, .data$Start, .data$JobId) %>%
    mutate(Event = "Start") %>%
    rename(Time = .data$Start) %>%
    bind_rows(df_filter %>%
      select(.data$ANode, .data$End, .data$JobId) %>%
      mutate(Event = "End") %>%
      rename(Time = .data$End)) %>%
    arrange(.data$Time) %>%
    group_by(.data$ANode) %>%
    mutate(Value = ifelse(.data$Event == "Start", 1, -1)) %>%
    mutate(nodeParallelism = cumsum(.data$Value)) %>%
    ungroup()

  # Integrate resource utilization by ANode
  df_node_plot <- df_node_parallelism %>%
    select(.data$ANode, .data$Time, .data$nodeParallelism) %>%
    arrange(.data$Time) %>%
    mutate(End = lead(.data$Time)) %>%
    mutate(Duration = .data$End - .data$Time) %>%
    rename(Start = .data$Time, Value = .data$nodeParallelism) %>%
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
    left_join(data$Atree %>% select(.data$ANode, .data$Depth), by = "ANode") %>%
    group_by(.data$Slice, .data$Depth) %>%
    mutate(Value1 = sum(.data$Value1)) %>%
    ungroup() %>%
    select(-.data$ANode, -.data$Value) %>%
    unique()

  # expand all time slices with the possible colors (for geom_ribbon)
  df_depth_plot <- df_node_plot %>%
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

  loginfo("Exit of resource_utilization_tree_depth_plot")
  resource_utilization_tree_depth(df_depth_plot, base_size = data$config$base_size, expand = data$config$expand)
}

nodes_memory_usage_plot <- function(data = NULL) {
  loginfo("Entry of nodes_memory_usage_plot")
  if (is.null(data)) stop("a NULL data has been provided to nodes_memory_usage_plot")
  node_mem_use <- geom_blank()

  if (data$config$activenodes$nodememuse$aggregation$active) {
    # mem use with time aggregation
    dfv <- data$Application %>%
      filter(grepl("front", .data$Value) | grepl("do_sub", .data$Value)) %>%
      rename(Task = .data$Value) %>%
      # GFlop here holds the memory allocated/freed by a task
      mutate(MemMB = ifelse(grepl("clean", .data$Task), -.data$GFlop * 1024, .data$GFlop * 1024)) %>%
      arrange(.data$Start) %>%
      mutate(Value = cumsum(.data$MemMB)) %>%
      group_by(.data$Node) %>%
      select(
        -.data$Duration,
        -.data$Size, -.data$Depth, -.data$Params, -.data$JobId, -.data$Footprint, -.data$Tag,
        -.data$GFlop, -.data$X, -.data$Y, -.data$Iteration, -.data$Subiteration,
        -.data$Resource, -.data$Outlier, -.data$Height,
        -.data$Position
      ) %>%
      mutate(Type = NA)

    step <- data$config$activenodes$nodememuse$aggregation$step
    node_mem_use <- var_integration_chart(dfv, ylabel = NA, step = step, facetting = FALSE, base_size = data$config$base_size, expand = data$config$expand)
  } else {
    # mem use without time aggregation
    df_mem <- data$Application %>%
      filter(grepl("front", .data$Value) | grepl("do_sub", .data$Value)) %>%
      select(.data$Start, .data$Value, .data$GFlop, .data$ANode, .data$Node) %>%
      arrange(.data$Start) %>%
      mutate(GFlop = ifelse(.data$Value == "clean_front", -.data$GFlop, .data$GFlop)) %>%
      mutate(MemMB = .data$GFlop * 1024) %>%
      mutate(UsedMemMB = cumsum(.data$MemMB)) %>%
      mutate(Time = .data$Start * 0.9999) %>%
      gather(.data$Start, .data$Time, key = "Start", value = "Time") %>%
      select(-.data$Start) %>%
      arrange(.data$Time) %>%
      mutate(UsedMemMB = lag(.data$UsedMemMB))

    node_mem_use <- df_mem %>%
      ggplot(aes(x = .data$Time, y = .data$UsedMemMB, color = .data$Node)) +
      geom_line() +
      default_theme(data$config$base_size, data$config$expand)

  }
  node_mem_use <- node_mem_use +
    theme(legend.position = "none") +
    ylab("Used\nMB") +
    scale_color_brewer(palette = "Dark2")

  loginfo("Exit of nodes_memory_usage_plot")
  return(node_mem_use)
}

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
    select(.data$color) %>%
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
