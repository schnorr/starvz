
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
            coord_cartesian(xlim=c(0, makespan))
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
