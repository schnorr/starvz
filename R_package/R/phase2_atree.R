#' @include starvz_data.R

#' Create the elimination tree structure plot along time
#'
#' Use Atree and Application data to create the elimination tree strucutre plot in a ggplot object and return it
#'
#' @param Application starvz_data Application trace data
#' @param Atree starvz_data Atree elimination tree trace data
#' @return A ggplot object
#' @examples
#' geom_atree_plot(data$Application, data$Atree)
#' @export
geom_atree_plot <- function(Application = NULL, Atree = NULL) {
  if (is.null(Atree)) stop("input data for geom_atree_plot is NULL")

  makespan <- Application %>%
    .$End %>%
    max()

  dtree <- Atree %>%
    # Get Start time of the first task belonging to each ANode
    left_join(Application %>%
      select(.data$ANode, .data$Start, .data$End) %>%
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
    select(-.data$Parent.Parent) %>%
    # Keep only non pruned nodes for tree structure
    filter(.data$NodeType != "Pruned") %>%
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
    # Remove nodes without parent
    filter(!is.na(.data$Parent)) %>%
    # The root has no tasks associated with, remove it.
    mutate(Parent = as.integer(.data$Parent)) %>%
    filter(.data$Parent != max(.data$Parent))

  # data frame for node "lifetime" geom_segment
  dline <- dtree %>%
    select(.data$ANode, .data$Start, .data$End, .data$Position, .data$Height, .data$Parent) %>%
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
    ) +
    geom_point(
      data = dstruct,
      aes(
        x = .data$Edge.End.X,
        y = .data$Edge.End.Y
      ), color = "#D95F02"
    ) +
    # Fix time coordinates
    coord_cartesian(xlim = c(0, makespan)) +
    # Horizontal lines
    geom_segment(data = dline, 
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
#' considering the initialization, communication and computational tasks. These
#' representations can be controlled in the configuration file. 
#'
#' @param Application starvz_data Application trace data
#' @param Atree starvz_data Atree elimination tree trace data
#' @param step size in milliseconds for the time aggregation step 
#' @return A ggplot object
#' @examples
#' atree_temporal_plot(data$Application, data$Atree, step=100)
#' @export
atree_temporal_plot <- function(Application = NULL, Atree = NULL, step = 100) {
  if (is.null(Application) | is.null(Atree)) stop("a NULL data has been provided to atree_temporal_plot")

  loginfo("Entry of atree_temporal_plot")

  data_utilization_node <- resource_utilization_tree_node(Application, Atree, step = step, group_pruned = TRUE)

  # Manipulate data for the tree resource utilization plots
  data_tree_utilization_plot <- data_utilization_node %>%
    filter(.data$Value != 0) %>%
    select(.data$ANode, .data$Slice, .data$Value1) %>%
    ungroup() %>%
    rename(NodeUsage = .data$Value1) %>%
    left_join(Atree, by = "ANode") %>%
    # Resize for node computation Start
    inner_join(Application %>%
      filter(grepl("init_", .data$Value) | grepl("do_subtree", .data$Value)) %>%
      select(.data$ANode, .data$End) %>% 
      group_by(.data$ANode) %>%
      filter(.data$End == max(.data$End)),
    by = "ANode"
    ) %>%
    mutate(Start = ifelse(.data$End >= .data$Slice, .data$End, .data$Slice)) %>%
    # resize for node computation End
    select(-.data$End) %>%
    inner_join(Application %>%
      select(.data$ANode, .data$End) %>% 
      group_by(.data$ANode) %>%
      filter(.data$End == max(.data$End)),
    by = "ANode"
    ) %>%
    mutate(End = ifelse(.data$Slice + step >= .data$End, .data$End, .data$Slice + step))

  # 0. Add the atree structure representation first
  atreeplot <- geom_atree_plot(Application, Atree) +
    default_theme(data$config$base_size, data$config$expand) +
    ylab("Elimination\nTree [nodes]") +
    scale_y_continuous(breaks = NULL, labels = NULL)

  # Add the computation, initialization, and communication over the tree structure plot
  # 1. Add computations  
  if (data$config$atree$computation$active) {
    atreeplot <- atreeplot +
      atree_geom_rect_gradient(data_tree_utilization_plot %>% filter(.data$NodeType != "Pruned"), yminOffset=0, ymaxOffset=1)

    # Add the pruned node computation representations
    if(data$config$atree$computation$pruned$active) {
      atreeplot <- atreeplot +
        atree_geom_rect_gradient(data_tree_utilization_plot %>% filter(.data$NodeType == "Pruned"), yminOffset=0.25, ymaxOffset=0.75)
    }

    # Add the color to represent computation
    atreeplot <- atreeplot +
      scale_fill_gradient2(name = "Computational Load", limits = c(0, 100), midpoint = 50, low = "blue", mid = "yellow", high = "red")
  }

  # 2. Add initialization tasks representation
  if (data$config$atree$initialization$active) {
    # filter initialization tasks
    dfw_init <- Application %>%
      filter(grepl("init_", .data$Value)) %>%
      unique() %>%
      select(-.data$Position, -.data$Height) %>%
      left_join(Atree, by = "ANode")

    atreeplot <- atreeplot +
      atree_geom_rect(dfw_init, "#4DAF4A", yminOffset=0, ymaxOffset=1, alpha=0.7)
  }

  # 3. Add "communication" (block_copy | block_extadd) tasks in the tree nodes
  if (data$config$atree$communication$active) {
    # filter communication tasks
    dfw_comm <- Application %>%
      filter(grepl("block_", .data$Value) | grepl("block_extadd", .data$Value)) %>%
      unique() %>%
      select(-.data$Position, -.data$Height) %>%
      left_join(Atree, by = "ANode")

    atreeplot <- atreeplot +
      atree_geom_rect(dfw_comm, "#000000", yminOffset=0.25, ymaxOffset=0.75, alpha=0.7)
  }

  loginfo("Exit of atree_temporal_plot")
  return(atreeplot)
}

#' Create the resource utilization by tree node plot
#'
#' Use starvz_data Application and Atree to create a plot that shows the
#' total resource utilization, painted by tree node using geom_ribbon. The
#' colors are reused between nodes, not tied to as specific tree node.
#'
#' @param Application starvz_data Application trace data
#' @param Atree starvz_data Atree elimination tree trace data
#' @param step size in milliseconds for the time aggregation step 
#' @return A ggplot object
#' @examples
#' resource_utilization_tree_node_plot(data$Application, data$Atree, step=100)
#' @export
resource_utilization_tree_node_plot <- function(Application = NULL, Atree = NULL, step = 100) {
  df1 <- resource_utilization_tree_node(Application, Atree, step = step, group_pruned=FALSE)

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
  } else if(ncolors <= 20) {
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
    coord_cartesian(ylim = c(0, 100)) +
    ylim(0, 100)
}

#' Create the resource utilization by tree depth plot
#'
#' Use starvz_data Application and Atree to create a plot that shows the
#' total resource utilization, painted by tree depth level using geom_ribbon
#'
#' @param Application starvz_data Application trace data
#' @param Atree starvz_data Atree elimination tree trace data
#' @param step size in milliseconds for the time aggregation step 
#' @return A ggplot object
#' @examples
#' resource_utilization_tree_depth_plot(data$Application, data$Atree, step=100)
#' @export
resource_utilization_tree_depth_plot <- function(Application = NULL, Atree = NULL, step = 100) {
  #Prepare data
  depth_plot_data <- resource_utilization_tree_depth(Application, Atree, step)

  maxDepth <- depth_plot_data %>%
    .$Depth %>%
    max()
  depthPalette <- rev(colorRampPalette(brewer.pal(9, "YlOrRd"))(maxDepth))

  depth_plot_data %>%
    ggplot() +
    geom_ribbon(aes(ymin = .data$Ymin, ymax = .data$Ymax, x = .data$Slice, fill = as.factor(.data$Depth))) +
    default_theme(data$config$base_size, data$config$expand) +
    theme(legend.position = "top") +
    scale_fill_manual(values = depthPalette) +
    ylab("Usage %\nDepth") +
    coord_cartesian(ylim = c(0, 100)) +
    ylim(0, 100)
}

# Calculate the computational resource utilization by tree node
resource_utilization_tree_node <- function(Application = NULL, Atree = NULL, step = 100, group_pruned=FALSE) {
  loginfo("Entry of resource_utilization_tree_node")
  # Prepare and filter data
  df_filter <- Application %>%
    filter(
      grepl("qrt", .data$Value) | grepl("subtree", .data$Value)
    ) %>%
    select(.data$ANode, .data$Start, .data$End, .data$JobId) %>%
    unique() %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- Application %>%
    filter(grepl("CPU", .data$ResourceType) | grepl("CUDA", .data$ResourceType)) %>%
    select(.data$ResourceId) %>%
    unique() %>%
    nrow()

  # Group pruned nodes with same Parent to aggregate their computations
  if(isTRUE(group_pruned)) {
    # When we change ANode, we also need to update it in Atree, but this will modify other plots as well
    groupPruned <- Atree %>%
      mutate(NewANode = case_when(.data$NodeType == "Pruned" ~ paste0(.data$Parent, "Pruned"), TRUE ~ .data$ANode)) %>%
      select(.data$ANode, .data$NodeType, .data$NewANode)

    df_filter <- df_filter %>%
      left_join(groupPruned %>% 
        select(.data$ANode, .data$NodeType, .data$NewANode),
        by="ANode"
      ) %>%
      mutate(originalAnode = .data$ANode, ANode = .data$NewANode)
  }

  # Compute the node parallelism
  data_node_parallelism <- df_filter %>%
    gather(.data$Start, .data$End, key="Event", value="Time") %>%
    arrange(.data$Time) %>%
    group_by(.data$ANode) %>%
    mutate(Value = ifelse(.data$Event == "Start", 1, -1)) %>%
    mutate(nodeParallelism = cumsum(.data$Value)) %>%
    ungroup()

  # Do the time aggregation of resource utilization by ANode
  data_node_plot <- data_node_parallelism %>%
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
    ungroup()

  # Restore original values of ANode for the Pruned nodes
  if(isTRUE(group_pruned)) {
    data_node_plot <- data_node_plot %>%
      left_join(df_filter %>% select(.data$ANode, .data$originalAnode),
        by="ANode"
      ) %>%
      mutate(ANode = .data$originalAnode) %>%
      select(-.data$originalAnode)
  }

  loginfo("Exit of resource_utilization_tree_node")
  return(data_node_plot)
}

# Calculate the computational resource utilization by tree depth
resource_utilization_tree_depth <- function(Application = NULL, Atree = NULL, step = 100) {
  loginfo("Entry of resource_utilization_tree_depth_plot")
  # Prepare and filter data
  df_filter <- Application %>%
    filter(grepl("qrt", .data$Value) | grepl("do_subtree", .data$Value)) %>%
    select(.data$ANode, .data$Start, .data$End, .data$JobId) %>%
    arrange(.data$Start)

  # Get number of workers for resource utilization
  NWorkers <- Application %>%
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
    left_join(Atree %>% select(.data$ANode, .data$Depth), by = "ANode") %>%
    group_by(.data$Slice, .data$Depth) %>%
    mutate(Value1 = sum(.data$Value1)) %>%
    ungroup() %>%
    select(-.data$ANode, -.data$Value) %>%
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
        ymax = .data$Position + ymaxOffset
      )
    )
  )
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

  }
  node_mem_use <- node_mem_use +
    theme(legend.position = "none") +
    ylab("Used\nMB") +
    scale_color_brewer(palette = "Dark2")

  loginfo("Exit of nodes_memory_usage_plot")
  return(node_mem_use)
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
