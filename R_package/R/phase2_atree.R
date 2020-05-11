geom_atree <- function (data=NULL, Offset=1.02, Flip = TRUE)
{
    if(is.null(data)) stop("input data for geom_atree is NULL");

    makespan = data$Application %>% .$End %>% max;

    ffactor <- ifelse(Flip, +1, -1);
    dfactor <- makespan * 0.04;
    doffset <- makespan * Offset;

    dtree <- data$Atree %>%
        # Get Start time of the first task belonging to each ANode
        left_join(data$Application %>%
                  filter(Type == "Worker State", Value != "block_copy") %>%
                  select(ANode, Start, End) %>%
                  group_by(ANode) %>%
                  summarize(Start = min(Start),
                            End = max(End)),
                  by="ANode") %>%
        # Get graphical properties of each parent for each row
        left_join(x = .,
                  y = .,
                  by=c("Parent" = "ANode"), suffix=c(".Node", ".Parent")) %>%
        rename(Height = Height.Node,
               Position = Position.Node,
               Depth = Depth.Node,
               Intermediary = Intermediary.Node,
               Start = Start.Node,
               End = End.Node) %>%
        select(-Parent.Parent) %>%
        # Keep only intermediary nodes
        filter(Intermediary == TRUE) %>%
        # Calculate coordinates for Labels
        mutate(Label.X = Start,
               Label.Y = Position + Height/2) %>%
        # Calculate coordinates for horizontal line
        mutate(Line.Y = Position + Height/2,
            Line.X.start = Start,
            Line.X.end = End) %>%
        # Calculate coordinates for lines connecting child with parent
        mutate(Edge.X = Start,
               Edge.Y = Position + Height/2,
               Edge.Xend = Start.Parent,
               Edge.Yend = Position.Parent + Height.Parent/2) %>%
        mutate(Edge.End.X = End,
               Edge.End.Y = Position + Height/2,
               Edge.End.Xend = End.Parent,
               Edge.End.Yend = Position.Parent + Height.Parent/2) %>%
        mutate(Middle.X = Start + (End - Start)/2,
               Middle.Y = Position + Height/2,
               Middle.End.X = Start.Parent + (End.Parent - Start.Parent)/2,
               Middle.End.Y = Position.Parent + Height.Parent/2);

    # data frame for the tree plot structure
    dstruct <- dtree %>%
      # Remove that without parent
      filter(!is.na(Parent)) %>%
      # The root has no tasks associated with, remove it.
      mutate(Parent = as.integer(Parent)) %>%
      filter(Parent != max(Parent));

    # data frame for node "lifetime" geom_segment
    dline <- dtree %>%
      select(ANode, Start, End, Position, Height, Parent) %>%
      filter(!is.na(Parent), !is.na(Start));

    ret <-
        list(
            # Lines connecting child with parent (Start)
            geom_segment(data=dstruct,
                         arrow=arrow(length = unit(0.03, "npc")),
                         aes(x=Edge.X,
                             y=Edge.Y,
                             xend=Edge.Xend,
                             yend=Edge.Yend),
                         color="blue"),
            geom_point(data=dstruct,
                       aes(x=Edge.X,
                           y=Edge.Y), color="blue"),
            #geom_point(data=d,
            #           aes(x=Edge.Xend,
            #               y=Edge.Yend), color="blue"),
            # Lines connecting child with parent (End)
            geom_segment(data=dstruct,
                         arrow=arrow(length = unit(0.03, "npc")),
                         aes(x=Edge.End.X,
                             y=Edge.End.Y,
                             xend=Edge.End.Xend,
                             yend=Edge.End.Yend),
                         color="red"),
            geom_point(data=dstruct,
                       aes(x=Edge.End.X,
                           y=Edge.End.Y), color="red"),
            #geom_point(data=d,
            #           aes(x=Edge.End.Xend,
            #               y=Edge.End.Yend), color="red"),
            # Fix time coordinates
            coord_cartesian(xlim=c(0, makespan)),
            # Horizontal lines
            geom_segment(data=dline, aes(y = Position + Height/2, yend = Position + Height/2, x = Start, xend = End), color="lightblue")
        );
    return(ret);
}

atree_temporal_chart <- function(data = NULL, step = 100, globalEndTime = NULL)
{
    if (is.null(data)) stop("a NULL data has been provided to atree_temporal_chart");

    loginfo("Entry of atree_temporal_chart");

    dfw <- data$Application;
    dfa <- data$Atree;
    
    df_node_plot <- resource_utilization_tree_node(data=data, step=step);

    # Calculate NodeUsage, this represent the "most active" node at the time slice
    df_node_plot %>% 
      filter(Value != 0) %>%
      select(ANode, Slice, Value1, Usage) %>%
      ungroup() %>%
      # calculate usage in percentage by node given the total Usage
      mutate(NodeUsage = 100 * (Value1/Usage)) %>%
      left_join(data$Atree, by="ANode") %>%
      inner_join(data$Application %>%
                  filter(grepl("init_", Value)) %>%
                  select(ANode, End) %>%
                  group_by(ANode) %>%
                  filter(End == max(End)),
                by="ANode") %>%
      mutate(Start = ifelse(End >= Slice, End, Slice)) -> df_node_plot_filtered  

    # filter initialization tasks 
    dfw_init <- dfw %>%
      filter(grepl("init_", Value)) %>%
      unique() %>%
      select(-Position, -Height) %>%
      left_join(data$Atree, by="ANode");

    # Prepare for colors
    atreeplot <- dfw %>%
      # Considering only application data and Worker State
      filter(Intermediary) %>%
      filter(grepl("lapack_", Value) | grepl("subtree", Value)) %>%
      unique() %>%
      # Remove all tasks that do not have ANode
      filter(!is.na(Height.ANode)) %>%
      # Plot
      ggplot() +
      default_theme() +
      ylab("Elimination\nTree [nodes]") +
      scale_y_continuous(breaks=NULL, labels=NULL) +
      # Add the atree representation on top
      geom_atree(data, Offset = 1.05, Flip = TRUE)

    if (pjr_value(pajer$atree$utilization, TRUE)){
        atreeplot <- atreeplot +
            geom_rect(data=df_node_plot_filtered,
                      aes(fill=NodeUsage,
                          xmin=Start,
                          xmax=Slice+step,
                          ymin=Position,
                          ymax=Position+Height)) +
            #scale_fill_viridis(option="plasma") +
            #scale_fill_gradient(low="lightsalmon", high="red1") +
            scale_fill_gradient2(name="Computational Load", limits=c(0,100), midpoint=50, low="blue", mid="yellow", high="red") +
            geom_rect(data=dfw_init,
                      aes(xmin=Start,
                          xmax=End,
                          ymin=Position,
                          ymax=Position+Height),
                      fill="#4DAF4A");
    }
    loginfo("Exit of atree_temporal_chart");
    return(atreeplot);
}

active_nodes_chart <- function(data = NULL)
{
    if (is.null(data)) stop("a NULL data has been provided to active_nodes_chart");

    loginfo("Entry of active_nodes_chart");

    data %>%
      filter(grepl("front", Value) | grepl("subtree", Value)) %>%
      select(ResourceId, Start, End, Value, ANode) %>%
      mutate(node_count = case_when(Value == "do_subtree" ~ "1",
                                    Value == "init_front" ~ "1",
                                    Value == "clean_front" ~ "-1",
                                    TRUE ~ "<NA>"),
             node_count = as.integer(node_count)) -> df_active

    # get all nodes that roots of sequential subtrees
    df_active %>%
      filter(Value == "do_subtree") %>%
      mutate(nodeType = TRUE) %>%
      select(ANode, nodeType) -> seq_tree

    df_active %>%
      filter_at(vars(ANode), any_vars(. %in% seq_tree$ANode)) %>%
      mutate(nodeType = "sequential")  -> seq_nodes

    df_active %>%
      filter_at(vars(ANode), any_vars(!. %in% seq_tree$ANode)) %>%
      mutate(nodeType = "parallel") -> front_nodes

    # let's merge them
    df_all <- front_nodes %>% bind_rows(seq_nodes)
    df_all %>%
      arrange(Start) %>%
      group_by(nodeType) %>%
      mutate(active = 0) %>%
      mutate(active = cumsum(node_count)) -> df_all

    activenodesplot <- df_all %>%
      ggplot(aes(x=Start, y=active, color=nodeType)) +
      default_theme() +
      geom_line() +
      theme(legend.position="top") +
      ylab("Active\nNodes") +
      scale_colour_brewer(palette = "Dark2");

    loginfo("Exit of active_nodes_chart");

    return(activenodesplot);
}

## time integration for cpu active atree nodes
atree_time_aggregation_prep <- function(dfw = NULL)
{
    if (is.null(dfw)) return(NULL);

    dfw_initial <- dfw %>%
        rename(Task = Value) %>%
        # This is the only difference
        group_by (ANode) %>%
        arrange(Start) %>%
        mutate(Value = 1) %>%
        select(-Duration, -Color, -Nature, -Type,
               -Size, -Depth, -Params, -JobId, -Footprint, -Tag,
               -GFlop, -X, -Y, -Iteration, -Subiteration,
               -Node, -Resource, -ResourceType, -Outlier, -Height,
               -Position);

    # Define the first zero
    dfw_zero_1 <- dfw_initial %>% slice(1) %>% mutate(StartN = 0, EndN = Start, Value = 0);

    # Define other zeroes
    dfw_zero_N <- dfw_initial %>% mutate(StartN = End, EndN = lead(Start), Value = 0);

    # Row bind them
    dfw_agg_prep <- dfw_zero_1 %>%
        bind_rows(dfw_zero_N) %>%
        mutate(Start = StartN, End = EndN) %>%
        select(-StartN, -EndN) %>%
        bind_rows(dfw_initial) %>%
        ungroup() %>%
        arrange(Start);

    # Set max end time for NA cases
    dfw_agg_prep <- dfw_agg_prep %>%
        filter(!complete.cases(.)) %>%
        mutate(End = max(dfw$End)) %>%
        bind_rows(dfw_agg_prep %>% filter(complete.cases(.))) %>%
        mutate (Duration = End - Start) %>%
        arrange(ResourceId, Task, Start);

    return(dfw_agg_prep);
}

atree_time_aggregation_do <- function(dfw_agg_prep = NULL, step = NA)
{
    if (is.null(dfw_agg_prep)) return(NULL);
    if (is.na(step)) return(NULL);

    dfw_agg_prep %>%
        group_by(ANode) %>%
        do(remyTimeIntegrationPrep(., myStep = step)) %>%
        mutate(Start = Slice, End = lead(Slice), Duration = End-Start) %>%
        ungroup() %>%
        na.omit()
}

atree_time_aggregation <- function(dfw = NULL, step = 100)
{
    if (is.null(dfw)) return(NULL);

    dfw <- dfw %>% filter(Type == "Worker State");

    dfw_agg_prep <- atree_time_aggregation_prep(dfw);
    dfw_agg <- atree_time_aggregation_do (dfw_agg_prep, step);

    dfw_agg %>%
        group_by(Slice) %>%
        filter(Value != 0) %>%
        summarize(Quantity = n(), Activity = sum(Value))
} 

computing_nodes_chart <- function(data=NULL, step = 100)
{
  loginfo("Entry of computing_nodes_chart");
  atree_time_aggregation(dfw=data$Application, step=step) %>%
    ggplot(aes(x=Slice, y=Quantity)) +
    geom_line() +
    default_theme() +
    ylab("Tree\nParallelism") +
    scale_colour_brewer(palette = "Dark2");
}

# remyTimeIntegrationPrep without dividing the Value column by the time slice
remyTimeIntegrationPrepNoDivision <- function(dfv = NULL, myStep = 100)
{
    if (is.null(dfv)) return(NULL);
    if (nrow(dfv) == 0) return(NULL);
    mySlices <- getSlices(dfv, step = myStep);
    tibble(Slice = mySlices, Value = c(remyTimeIntegration(dfv, slices=mySlices), 0));
}

resource_utilization_tree_node <- function(data = NULL, step = 100)
{
  loginfo("Entry of resource_utilization_tree_node");
  # Prepare and filter data
  data$Application %>%
    filter(Type == "Worker State",
           grepl("lapack", Value) | grepl("subtree", Value)) %>%
    select(ANode, Start, End, JobId) %>%
    unique() %>%
    arrange(Start) -> df_filter
  
  # Get number of workers for resource utilization
  NWorkers <- data$Application %>%
      filter(grepl("CPU", ResourceType) | grepl("CUDA", ResourceType)) %>%
      select(ResourceId) %>% unique() %>% nrow()

  # Compute the node parallelism
  df_filter %>%
    select(ANode, Start, JobId) %>%
    mutate(Event = "Start") %>%
    rename(Time = Start) %>%
    bind_rows(df_filter %>%
                select(ANode, End, JobId) %>%
                mutate(Event = "End") %>%
                rename(Time = End)
              ) %>%
    arrange(Time) %>%
    group_by(ANode) %>%
    mutate(Value = ifelse(Event == "Start", 1, -1)) %>%
    mutate(nodeParallelism = cumsum(Value)) %>%
    ungroup() -> df_node_parallelism

  # Integrate resource utilization by ANode
  df_node_parallelism %>%
    select(ANode, Time, nodeParallelism) %>%
    arrange(Time) %>%
    mutate(End = lead(Time)) %>%
    mutate(Duration = End-Time) %>%
    rename(Start = Time, Value = nodeParallelism) %>%
    na.omit() %>%
    group_by(ANode) %>%
    do(remyTimeIntegrationPrepNoDivision(., myStep = step)) %>%
    # This give us the total worker usage grouped by ANode in a time slice
    mutate(Value1 = Value/(step*NWorkers)*100) %>%
    ungroup() %>%
    group_by(Slice) %>%
    arrange(Slice) %>%
    mutate(Usage = sum(Value1)) -> df_node_plot

  loginfo("Exit of resource_utilization_tree_node");
  return(df_node_plot)
}

resource_utilization_tree_node_plot <- function(data = NULL, step = 100)
{
  df1 <- resource_utilization_tree_node(data=data, step=step)

  event_data <- df1 %>%
    filter(Value != 0) %>%
    group_by(ANode) %>%
    summarize(Start=min(Slice), End=max(Slice)+step) %>%
    arrange(Start, End) %>%
    gather(Start, End, key="Event", value="Time") %>%
    arrange(Time, Event);

  # Set node colors
  active_colors <<- c()
  df_colors <<- tibble(ANode=character(), Event=character(), color=integer())
  apply(event_data, 1, define_colors) -> colors
  colors = tibble(Color=colors)
  event_data <- event_data %>% 
    cbind(colors) %>%
    as_tibble() %>%
    select(ANode, Color) %>%
    unique();

  # join data frames
  df2 <- df1 %>%
    ungroup() %>%
    left_join(event_data, by="ANode") %>%
    group_by(Slice);
  
  # must expand data frame to make geom_area work properly
  df2 %>%
    filter(!is.na(Color)) %>%
    select(-ANode) %>%
    expand(Slice, Color) %>%
    left_join(df2 %>% filter(Value != 0), by=c("Slice", "Color")) %>%
    mutate(Value1 = ifelse(is.na(Value1), 0, Value1)) -> df_plot

  ncolors <- df_plot %>% ungroup() %>% select(Color) %>% max(.$Color)
  if (ncolors <= 10) {
    palette = brewer.pal(9, "Greens")[3:9];
  } else {
    palette = c(brewer.pal(9, "Oranges")[3:9], brewer.pal(9, "Blues")[3:9],
                brewer.pal(9, "Reds")[3:9], brewer.pal(9, "Greens")[3:9])
  }

  fill_palette <- rev(colorRampPalette(palette)(ncolors+1))

  df_plot %>%
    ggplot() +
    geom_area(aes(x=Slice, y=Value1, fill=as.factor(Color)), stat = "identity", position = "stack") +
    scale_fill_manual(values=fill_palette) +
    default_theme() +
    theme(legend.position = "none") +
    ylab("Usage %\nANode") +
    coord_cartesian(ylim=c(0,100)) + ylim(0,100)
}

resource_utilization_tree_depth <- function(data = NULL)
{

  maxDepth <- data %>% ungroup() %>% select(Depth) %>% unique() %>% max(.$Depth)
  depthPalette <- rev(colorRampPalette(brewer.pal(9, "YlOrRd"))(maxDepth));

  data %>%
    filter(Depth != 0) %>%
    ungroup() %>%
    arrange(Depth) %>%
    ggplot() +
    geom_area(aes(x=Slice, y=Value2, fill=as.factor(Depth))) +
    default_theme() +
    theme(legend.position = "top") +
    scale_fill_manual(values = depthPalette) +
    ylab("Usage %\nDepth") +
    coord_cartesian(ylim=c(0,100)) + ylim(0,100)
}

resource_utilization_tree_depth_plot <- function(data = NULL, step = 100)
{
  loginfo("Entry of resource_utilization_tree_depth_plot");
  # Prepare and filter data
  df_filter <- data$Application %>%
    filter(Type == "Worker State",
           grepl("lapack", Value) | grepl("subtree", Value)) %>%
    select(ANode, Start, End, JobId) %>%
    arrange(Start)

  # Get number of workers for resource utilization
  NWorkers <- data$Application %>%
      filter(grepl("CPU", ResourceType) | grepl("CUDA", ResourceType)) %>%
      select(ResourceId) %>% unique() %>% nrow()

  # Compute the node parallelism
  df_node_parallelism <- df_filter %>%
    select(ANode, Start, JobId) %>%
    mutate(Event = "Start") %>%
    rename(Time = Start) %>%
    bind_rows(df_filter %>%
              select(ANode, End, JobId) %>%
              mutate(Event = "End") %>%
              rename(Time = End)
              ) %>%
    arrange(Time) %>%
    group_by(ANode) %>%
    mutate(Value = ifelse(Event == "Start", 1, -1)) %>%
    mutate(nodeParallelism = cumsum(Value)) %>%
    ungroup()

  # Integrate resource utilization by ANode
  df_node_plot <- df_node_parallelism %>%
    select(ANode, Time, nodeParallelism) %>%
    arrange(Time) %>%
    mutate(End = lead(Time)) %>%
    mutate(Duration = End-Time) %>%
    rename(Start = Time, Value = nodeParallelism) %>%
    na.omit() %>%
    group_by(ANode) %>%
    do(remyTimeIntegrationPrepNoDivision(., myStep = step)) %>%
    # This give us the total worker usage grouped by ANode in a time slice
    mutate(Value1 = (Value / (step*NWorkers)) * 100) %>%
    ungroup() %>%
    group_by(Slice) %>%
    arrange(Slice) %>%
    mutate(Usage = sum(Value1))

  # Compute for Depth
  df_depth_plot <- df_node_plot %>%
    left_join(data$Atree, by="ANode") %>%
    select(-Value, -Height, -Position, -Intermediary) %>%
    filter(!is.na(Depth)) %>%
    ungroup() %>%
    group_by(Slice, Depth) %>%
    mutate(Value2 = sum(Value1)) %>%
    select(Slice, Depth, Value2) %>%
    unique()

  loginfo("Exit of resource_utilization_tree_depth_plot");
  resource_utilization_tree_depth(df_depth_plot)
}
nodes_memory_usage_plot <- function(data = NULL)
{
  loginfo("Entry of nodes_memory_usage_plot");
  if (is.null(data)) stop("a NULL data has been provided to nodes_memory_usage_plot");

  df_mem <- data$Application %>%
    filter(grepl("front", Value) | grepl("do_sub", Value)) %>%
    select(Start, Value, GFlop, ANode, Node) %>%
    arrange(Start) %>%
    mutate(GFlop = ifelse(Value=="clean_front", -GFlop, GFlop)) %>%
    mutate(MemMB = GFlop*1024) %>%
    mutate(UsedMemMB = cumsum(MemMB)) %>%
    mutate(Time = Start*0.9999) %>%
    gather(Start, Time, key="Start", value="Time") %>% 
    select(-Start) %>%
    arrange(Time) %>%
    mutate(UsedMemMB = lag(UsedMemMB))

  node_mem_use <- df_mem %>%
    ggplot(aes(x=Time, y=UsedMemMB, color=Node)) +
    geom_line() + 
    default_theme() +
    theme(legend.position = "none") +
    ylab("Used\nMB") +
    scale_color_brewer(palette="Dark2");

  if (pjr_value(pajer$activenodes$nodememuse$mempeak, FALSE)) {
    mem_peak = max(df_mem$UsedMemMB);
    node_mem_use <- node_mem_use + 
      geom_text(aes(x=0, y=mem_peak), hjust=-.5, vjust=1.1 , color="red", label=paste0("Memory peak = ", round(mem_peak, digits=2), "MB")) +
      geom_hline(yintercept = mem_peak, color="red", linetype = "dashed");
  }

  loginfo("Exit of nodes_memory_usage_plot");
  return(node_mem_use);
}

get_min_color <- function(node) {
  min_color = 0
  while(1) {
    if(min_color %in% active_colors) {
      min_color = min_color+1
    } else {
      df_colors <<- df_colors %>%
        rbind(tibble(ANode=node, Event="Start", color=min_color))
      active_colors <<- c(active_colors, min_color)
      return(min_color)
    }
  }
}
 
find_node_color <- function(node) {
  color <- df_colors %>%
    filter(ANode == node & Event == "Start") %>%
    select(color) %>%
    .$color

  return(color)
}

set_color_available <- function(node) {
  color <- find_node_color(node);
  
  df_colors <<- df_colors %>%
    rbind(tibble(ANode=node, Event="End", color=color)) 
  active_colors <<- active_colors[active_colors != as.integer(color)]
 
 return(color)
}

define_colors <- function(data) {
    ANode <- data[1]
    Event  <- data[2]

    if(Event == "Start") {
      get_min_color(ANode)
    } else {
      set_color_available(ANode)
    }
}
