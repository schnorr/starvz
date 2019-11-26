
geom_atree <- function (data=NULL, Offset=1.02, Flip = TRUE)
{
    if(is.null(data)) stop("input data for geom_atree is NULL");

    makespan = data$State %>% filter(Application == TRUE) %>% .$End %>% max;

    ffactor <- ifelse(Flip, +1, -1);
    dfactor <- makespan * 0.04;
    doffset <- makespan * Offset;

    data <- data$ATree %>%
        # Get graphical properties of each parent for each row
        left_join(data$ATree, by=c("Parent" = "ANode"), suffix=c(".Node", ".Parent")) %>%
        rename(Height = Height.Node, Position = Position.Node, Depth = Depth.Node, Intermediary = Intermediary.Node) %>%
        select(-Parent.Parent) %>%
        # Keep only intermediary nodes
        filter(Intermediary == TRUE) %>%
        # Calculate coordinates for Labels
        mutate(Label.X = doffset - (Depth * dfactor) * ffactor,
               Label.Y = Position + Height/2) %>%
        # Calculate coordinates for lines connecting child with parent
        mutate(Edge.X = doffset - ((Depth-0.3) * dfactor) * ffactor,
               Edge.Y = Position + Height/2,
               Edge.Xend = doffset - ((Depth.Parent+0.3) * dfactor) * ffactor,
               Edge.Yend = Position.Parent + Height.Parent/2);

    ret <-
        list(
            # Horizontal lines
#            geom_segment(data=(data %>% mutate(XOrigin = max(Depth)+0.6)), aes(yend=Position, x=Depth+0.5, xend=XOrigin, y=Position), color="lightgray"),
            # Lines connecting child with parent
            geom_segment(data=data, aes(x=Edge.X, yend=Edge.Yend, xend=Edge.Xend, y=Edge.Y), color="gray"),
            # The Label
            geom_text(data=data, size=3, aes(y=Label.Y, label=ANode, x=Label.X)),
            # Fix time coordinates
            #coord_cartesian(xlim=c(0, makespan))
        );
    return(ret);
}
atree_temporal_chart <- function(data = NULL, globalEndTime = NULL)
{
    if (is.null(data)) stop("a NULL data has been provided to atree_temporal_chart");

    loginfo("Entry of atree_temporal_chart");

    dfw <- data$State;
    dfa <- data$ATree;
    # Prepare for colors
    namedcolors <- extract_colors(dfw);
    atreeplot <- dfw %>%
        filter(Application == TRUE) %>%
        # Remove all tasks that do not have ANode
        filter(!is.na(Height.ANode)) %>%
        # Plot
        ggplot() +
        default_theme() +
        ylab("Task Location") +
        scale_y_continuous(breaks=NULL, labels=NULL) +
        scale_fill_manual(values = namedcolors) +
        geom_rect(aes(fill=as.factor(Value),
                      xmin=Start,
                      xmax=End,
                      ymin=Position.ANode,
                      ymax=Position.ANode+Height.ANode), alpha=.5) +
        # Add the atree representation on top
        geom_atree(data, Offset = 1.05, Flip = TRUE);

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
      ylab("Active Nodes") +
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

    dfw <- dfw %>% filter(Application) %>% filter(Type == "Worker State")

    dfw_agg_prep <- atree_time_aggregation_prep (dfw);
    dfw_agg <- atree_time_aggregation_do (dfw_agg_prep, step);

    dfw_agg %>%
        group_by(Slice) %>%
        filter(Value != 0) %>%
        summarize(Quantity = n(), Activity = sum(Value))
}
