
state_pmtool_chart <- function (data = NULL)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    # Get traces
    dfw <- data$pmtool_states;

    loginfo("Entry of state_pmtool_chart");

    # Filter
    dfwapp = dfw %>%
        filter(sched == pajer$pmtool$state$sched);

    # Obtain time interval
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_pmtool_states(data);


    #if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

    #if (pjr(pajer$pmtool$bounds$active)) gow = gow + geom_pmtools_bounds(data);

    # add makespan
    if (pjr(pajer$st$makespan)) gow = gow + geom_makespan_pmtool(data);

    # add idleness
    #if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

    # add Global CPB
    #if (pjr(pajer$st$cpb) || pjr(pajer$st$cpb_mpi$active)) gow = gow + geom_cpb(data);

    loginfo("Exit of state_pmtool_chart");
    return(gow);
}



k_chart_pmtool <- function (dfw = NULL)
{
    if (is.null(dfw)) stop("dfw provided to k_chart is NULL");

    dfw <- dfw %>%
        filter(sched == pajer$pmtool$state$sched);

    # Prepare for colors
    dfw %>% select(Value, Color) %>% unique %>% .$Color -> choleskyColors
    choleskyColors %>% setNames(dfw %>% select(Value, Color) %>% unique %>% .$Value) -> choleskyColors;

    # Prepare for borders
    dfborders <- dfw %>%
        group_by(Iteration) %>%
        summarize(Start = min(Start), End=max(End)) %>%
        mutate(IterationB = lead(Iteration), StartB = lead(Start)) %>%
        mutate(IterationE = lead(Iteration), EndB = lead(End)) %>%
        na.omit;

    # Height of each bar
    height = 0.8;

    dfw %>% ggplot() +
        guides(fill = guide_legend(nrow = 1)) +
        scale_fill_manual(values = choleskyColors) +
        theme_bw(base_size=12) +
        xlab("Time [ms]") +
        ylab("PMTool\nIteration") +
        default_theme() +
        # Keep the alpha = 1 even if we use an alpha below
        guides(fill = guide_legend(override.aes = list(alpha=1))) +
        scale_y_reverse() +
        # The start border
        geom_curve(data=dfborders, aes(x=Start, xend=StartB, y=Iteration+height, yend=IterationB+height), curvature=0.1, angle=20) +
        # The end border
        geom_curve(data=dfborders, aes(x=End, xend=EndB, y=Iteration, yend=IterationB), curvature=-0.1, angle=20) +
        # The state
        geom_rect(aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Iteration,
                      ymax=Iteration+height), alpha=.5) -> goijk;
    return(goijk);
}



geom_pmtool_states <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_pmtool_states");


    dfw <- data$pmtool_states %>% filter(sched == pajer$pmtool$state$sched);


    loginfo("Starting geom_pmtool_states");

    ret <- list();

    # Color mapping
    ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    gg <- data$State %>% filter(Application == TRUE);
    yconfm <- yconf(gg);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab("Pmtool Workers");

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+Height-0.4), alpha=0.5);


    loginfo("Finishing geom_pmtool_states");

    return(ret);
}


geom_pmtools_bounds <- function(data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_pmtools_bounds");

    dftemp <- data$State %>%
        filter(Application == TRUE) %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Position, Height);
    # Y position
    minPos = dftemp %>% pull(Position) %>% min;
    minHeight = dftemp %>% pull(Height) %>% min;
    maxPos = dftemp %>% pull(Position) %>% max + minHeight/2;

    data$pmtool %>%
      mutate(MinPosition = minPos,
            MaxPosition = maxPos) -> df.pmtool;

    bsize = pjr_value(pajer$base_size, 22)/5;

    for(i in 1:nrow(df.pmtool)) {
      bound <- df.pmtool[i,];
      if (!is.null(bound)){
          ret <- list(
              geom_segment(data=bound, aes(x = Time+tstart, xend=Time+tstart, y = MinPosition, yend=MaxPosition), size=5, alpha=.7, color="gray"),
              geom_text (data=bound, aes(x = Time+tstart, y = MinPosition+(MaxPosition-MinPosition)/2, label=paste0(ifelse(pjr_value(pajer$pmtool$bounds$label, TRUE), paste0(Alg, ": "), ""), round(Time, 0))), angle=90, color="black", size=bsize)
          );
          return(ret);
      }
    }
    return(list());
}

geom_makespan_pmtool <- function(data = NULL)
{
    if(is.null(data)) stop("data provided for geom_makespan_pmtool is NULL");
    dfw <- data$pmtool_states;

    bsize = pjr_value(pajer$base_size, 22);

    tend = dfw %>% filter(sched == pajer$pmtool$state$sched) %>% pull(End) %>% max;
    loginfo(paste("makespan pm tool is", tend));
    height = dfw %>% select(Position) %>% na.omit %>% pull(Position) %>% max;
    loginfo(paste("max height for makespan is", height));
    ret <- geom_text(data=data.frame(), x=tend, y=height*.5, aes(label=round(tend,0)), angle=90, size=bsize/4);
    return(ret);
}
