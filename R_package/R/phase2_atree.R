geom_atree <- function (data=NULL, Offset=1.02, Flip = TRUE)
{
    if(is.null(data)) stop("input data for geom_atree is NULL");

    makespan = data$Application %>% .$End %>% max;

    ffactor <- ifelse(Flip, +1, -1);
    dfactor <- makespan * 0.04;
    doffset <- makespan * Offset;

    d <- data$Atree %>%
        # Get Start time of the first task belonging to each ANode
        left_join(data$Application %>%
                  filter(Type == "Worker State") %>%
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

    ret <-
        list(
            # Lines connecting child with parent (Start)
            geom_segment(data=d,
                         aes(x=Edge.X,
                             yend=Edge.Yend,
                             xend=Edge.Xend,
                             y=Edge.Y),
                         color="blue"),
            geom_point(data=d,
                       aes(x=Edge.X,
                           y=Edge.Y), color="blue"),
            geom_point(data=d,
                       aes(x=Edge.Xend,
                           y=Edge.Yend), color="blue"),
            # Lines connecting child with parent (End)
            geom_segment(data=d,
                         aes(x=Edge.End.X,
                             yend=Edge.End.Yend,
                             xend=Edge.End.Xend,
                             y=Edge.End.Y),
                         color="red"),
            geom_point(data=d,
                       aes(x=Edge.End.X,
                           y=Edge.End.Y), color="red"),
            geom_point(data=d,
                       aes(x=Edge.End.Xend,
                           y=Edge.End.Yend), color="red"),
            # Fix time coordinates
            coord_cartesian(xlim=c(0, makespan)),
            # Horizontal lines
            geom_segment(data=d, aes(y = Position, yend = Position, x = Start, xend = End), color="lightblue")
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
      left_join(data$Atree, by="ANode") -> df_node_plot_filtered
    
    # filter initialization tasks 
    dfw_init_block <- dfw %>%
      filter(Type == "Worker State", Application) %>%
      filter(grepl("init_", Value)) %>%
      unique() %>%
      select(-Position, -Height) %>%
      left_join(data$Atree, by="ANode")

    # Prepare for colors
    atreeplot <- dfw %>%
      # Considering only application data and Worker State
      filter(Type == "Worker State", Application, Intermediary) %>%
      filter(grepl("lapack_", Value) | grepl("subtree", Value)) %>%
      unique() %>%
      # Remove all tasks that do not have ANode
      filter(!is.na(Height.ANode)) %>%
      # Plot
      ggplot() +
      default_theme() +
      ylab("Task\nLocation") +
      scale_y_continuous(breaks=NULL, labels=NULL) +
      # Add the atree representation on top
      geom_atree(data, Offset = 1.05, Flip = TRUE) +
      geom_rect(data=df_node_plot_filtered,
                aes(fill=NodeUsage,
                xmin=Slice,
                xmax=Slice+step,
                ymin=Position,
                ymax=Position+Height)) +
      scale_fill_viridis(option="plasma") +
      geom_rect(data=dfw_init_block,
                  aes(xmin=Start,
                  xmax=End,
                  ymin=Position,
                  ymax=Position+Height),
                  fill="green");
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
    ylab("Computing\nNodes") +
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
    filter(Type == "Worker State", Intermediary,
           grepl("lapack", Value) | grepl("subtree", Value)) %>%
    select(ANode, Start, End, JobId) %>%
    unique() %>%
    arrange(Start) -> df_filter
  
  loginfo("Get number of workers for resource utilization");
  # Get number of workers for resource utilization
  NWorkers <- data$Application %>%
      filter(grepl("CPU", ResourceType) | grepl("CUDA", ResourceType)) %>%
      select(ResourceId) %>% unique() %>% nrow()

  loginfo("Compute the node parallelism");
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

  loginfo("Integrate resource utilization by ANode");
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
    resource_utilization_tree_node(data=data, step=step) %>%
      ggplot(aes(x=Slice, y=Value1, fill=as.factor(ANode))) +
      geom_area() +
      default_theme() +
      theme(legend.position = "none") +
      ylab("Usage %\nANode") +
}

resource_utilization_tree_depth <- function(data = NULL)
{
  data %>%
    filter(Depth != 0) %>%
    ungroup() %>%
    arrange(Depth) %>%
    ggplot() +
    geom_area(aes(x=Slice, y=Value2, fill=as.factor(Depth))) +
   # scale_fill_manual(values = heat) +
    default_theme() +
    theme(legend.position = "none") +
    #xlab("Time [ms]") +
    ylab("Usage %\nDepth") +
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
