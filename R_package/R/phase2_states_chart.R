state_chart <- function (data = NULL, globalEndTime = NULL, ST.Outliers = TRUE, StarPU.View = FALSE)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    # Get traces
    dfw <- data$State;

    loginfo("Entry of state_chart");

    # Filter
    dfwapp = dfw %>%
        # Considering only application data
        filter(Application == TRUE) %>%
        # Considering only Worker State
        filter(Type == "Worker State");

    # Obtain time interval
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_states(data, ST.Outliers, StarPU.View);

    if (!StarPU.View){
        # The per-node ABE
        if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

        # add pmtool bound
        if (pjr(pajer$pmtool$bounds$active)) gow = gow + geom_pmtools_bounds(data);

        # add makespan
        if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

        # add idleness
        if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

        # add Global CPB
        if (pjr(pajer$st$cpb) || pjr(pajer$st$cpb_mpi$active)) gow = gow + geom_cpb(data);

        # check if task dependencies should be added
        if (pjr(pajer$st$tasks$active)){
            tasklist <- pajer$st$tasks$list;
            levels <- pjr_value(pajer$st$tasks$levels, 2);

            tasksel <- gaps_backward_deps (data = data,
                                           tasks = tasklist,
                                           levels = levels);

            gow = gow + geom_path_highlight(tasksel);
        }else{
            loginfo("DO NOT add dependencies");
        }


    }else{
        # add some color palette for StarPU States
        # Move to geom_states, as it was doing 2 times...
        # gow = gow + scale_fill_manual(values = starpu_colors());
    }

    loginfo("Exit of state_chart");
    return(gow);
}


k_chart <- function (dfw = NULL)
{
    if (is.null(dfw)) stop("dfw provided to k_chart is NULL");

    dfw <- dfw %>% filter(Application == TRUE);

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
        ylab("Cholesky\nIteration") +
        default_theme() +
        # Keep the alpha = 1 even if we use an alpha below
        guides(fill = guide_legend(override.aes = list(alpha=1))) +
        scale_y_reverse() +
        # The start border
        geom_curve(data=dfborders, aes(x=Start, xend=StartB, y=Iteration+height-height/2, yend=IterationB+height-height/2), curvature=0.1, angle=20) +
        # The end border
        geom_curve(data=dfborders, aes(x=End, xend=EndB, y=Iteration-height/2, yend=IterationB-height/2), curvature=-0.1, angle=20) +
        # The state
        geom_rect(aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Iteration-height/2,
                      ymax=Iteration+height/2), alpha=.5) -> goijk;
    return(goijk);
}

geom_states <- function (data = NULL, Show.Outliers = FALSE, StarPU = FALSE)
{
    if (is.null(data)) stop("data is NULL when given to geom_states");
    if (StarPU){
        dfw <- data$State %>%
            # Non application
            filter(Application == FALSE) %>%
            # And only Starpu Worker State
            filter(Type == "Worker State");
    }else{
        dfw <- data$State %>% filter(Application == TRUE);
    }

    loginfo("Starting geom_states");

    ret <- list();

    # Color mapping
    if (StarPU){
      ret[[length(ret)+1]] <- scale_fill_manual(values = starpu_colors());
    }else{
      ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));
    }

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab(ifelse(StarPU, "StarPU Workers", "Application Workers"));

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+Height-0.4), alpha=ifelse(Show.Outliers && !StarPU, 0.5, 1.0));

    # Add outliers conditionally
    if (Show.Outliers && !StarPU){
        ret[[length(ret)+1]] <-
            geom_rect(data=(dfw %>% filter(Outlier == TRUE)),
                      aes(fill=Value,
                          xmin=Start,
                          xmax=End,
                          ymin=Position,
                          ymax=Position+Height-0.4), alpha=1);
    }

    loginfo("Finishing geom_states");

    return(ret);
}


geom_mpistates <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_mpistates");
    dfw <- data$State;
    if (is.null(dfw)) stop("dfw is NULL when given to geom_mpistates");

    # Get only MPI states
    dfw <- dfw %>% filter(Type == "Communication Thread State");
    if (nrow(dfw) == 0) stop("there is no data after filtering for MPI states");

    # Check if there is MPI data
    loginfo("Starting geom_mpistates");

    ret <- list();

    # Calculate Y position
    ypos <- tibble(ResourceId = (dfw %>% pull(ResourceId) %>% unique)) %>%
        mutate(Height = 1) %>%
        mutate(Position = cumsum(Height));

    dfw <- dfw %>%
        # Remove existing position
        select(-Position, -Height) %>%
        # Establish new position
        left_join(ypos);

    # Color mapping
    ret[[length(ret)+1]] <- scale_fill_brewer(palette = "Dark2");

    # Y label
    ret[[length(ret)+1]] <- ylab("MPI\nThread");

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+0.6));

    loginfo("Finishing geom_mpistates");

    return(ret);
}

state_mpi_chart <- function (data = NULL)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    loginfo("Entry of state_mpi_chart");

    # Obtain time interval
    tstart <- data$State %>% pull(Start) %>% min;
    tend <- data$State %>% pull(End) %>% max;

    #Plot
    gow <- ggplot() +
        default_theme() +
        coord_cartesian(xlim=c(tstart, tend)) +
        # Add states and outliers if requested
        geom_mpistates(data);

    loginfo("Exit of state_mpi_chart");
    return(gow);
}
concurrent_mpi <- function(data = NULL)
{
    if (is.null(data)) return(NULL);

    data$Link %>%
        filter(grepl("mpicom", Key)) %>%
        select(-Nature, -Container, -Type, -Duration) -> dflink;

    dflink %>%
        select(-End) %>%
        rename(Timestamp = Start) %>%
        mutate(Start = TRUE) -> dfstart;
    dflink %>%
        select(-Start) %>%
        rename(Timestamp = End) %>%
        mutate(Start = FALSE) %>%
        bind_rows (dfstart) %>%
        arrange(Timestamp) %>%
        group_by(Origin, Dest) %>%
        mutate(Value = cumsum(as.integer(
                   case_when(
                       Start == TRUE ~ 1,
                       Start == FALSE ~ -1,
                       TRUE ~ 0)))) %>%
        arrange(Origin, Timestamp) %>%
        select(-Start) %>%
        rename(Start = Timestamp) %>%
        group_by(Origin) %>% mutate(End = lead(Start)) %>% na.omit %>% mutate(Duration = End-Start) %>% ungroup() %>%
        mutate(Type = "MPI Concurrent") %>%
        rename(ResourceId = Origin) %>%
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        mutate(Node = as.factor(Node)) %>%
        mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource))) %>%
        select(Start, End, Duration, Node, ResourceId, ResourceType, Type, Value)
}

geom_path_highlight <- function (paths = NULL)
{
    if (is.null(paths)) stop("paths is NULL when given to gaps_backward_deps");
    if ((paths %>% nrow) == 0){
        return(list());
    }

    # paths is identical to data$State, but with an additional column called Path

    ret <- list();

    # highlight the tasks involved in the path
    ret[[length(ret)+1]] <- geom_rect(data=paths,
                                      size=1,
                                      aes(color=Path,
                                          xmin=Start,
                                          xmax=End,
                                          ymin=Position,
                                          ymax=Position+Height-0.4), alpha=0);

    # let's draw lines connecting tasks in the path

    # collect each JobId coordinates
    paths %>% select(JobId, Start, End, Position, Height) %>% unique -> x1;
    # gather coordinates for the lines
    paths %>%
        select(Path, JobId, Dependent) %>%
        left_join(x1, by=c("JobId" = "JobId")) %>%
        left_join(x1, by=c("Dependent" = "JobId")) %>%
        na.omit -> pathlines;

    ret[[length(ret)+1]] <- geom_segment(data=pathlines,
                                         aes(x=Start.x,
                                             xend=End.y,
                                             y=Position.x+(Height.x/2),
                                             yend=Position.y+(Height.y/2),
                                             color=Path));
    return(ret);
}
