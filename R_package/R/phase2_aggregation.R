
time_aggregation_prep <- function(dfw = NULL)
{
    if (is.null(dfw)) return(NULL);

    dfw_initial <- dfw %>%
        rename(Task = Value) %>%
        group_by (ResourceId, Task) %>%
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
        ungroup();

    # Set max end time for NA cases
    dfw_agg_prep <- dfw_agg_prep %>%
        filter(!complete.cases(.)) %>%
        mutate(End = max(dfw$End)) %>%
        bind_rows(dfw_agg_prep %>% filter(complete.cases(.))) %>%
        mutate (Duration = End - Start) %>%
        arrange(ResourceId, Task, Start);

    return(dfw_agg_prep);
}

time_aggregation_do <- function(dfw_agg_prep = NULL, step = NA)
{
    if (is.null(dfw_agg_prep)) return(NULL);
    if (is.na(step)) return(NULL);

    dfw_agg_prep %>%
        group_by(ResourceId, Task) %>%
        do(remyTimeIntegrationPrep(., myStep = step)) %>%
        mutate(Start = Slice, End = lead(Slice), Duration = End-Start) %>%
        ungroup() %>%
        na.omit()
}

time_aggregation_post <- function(dfw = NULL, dfw_agg = NULL)
{
    if (is.null(dfw)) return(NULL);
    if (is.null(dfw_agg)) return(NULL);

    df_ResourceId <- dfw %>%
        select(Nature, ResourceId, Type,
               Node, Resource, ResourceType, Position, Height) %>%
        unique;
    df_Task <- dfw %>%
        select(Value, Color) %>%
        unique;

    dfwagg_full <- dfw_agg %>%
        left_join(df_ResourceId, by="ResourceId") %>%
        left_join(df_Task, by=c("Task" = "Value"));

    return(dfwagg_full);
}

st_time_aggregation <- function(dfw = NULL, StarPU.View = FALSE, step = 100)
{
    if (is.null(dfw)) return(NULL);

    if (StarPU.View == FALSE){
        dfw <- dfw %>% filter(Application == TRUE);
    }else{
        dfw <- dfw %>% filter(Application == FALSE);
    }

    dfw_agg_prep <- time_aggregation_prep (dfw);
    dfw_agg <- time_aggregation_do (dfw_agg_prep, step);
    dfwagg_full <- time_aggregation_post (dfw, dfw_agg);

    # Preparation for visualization (imitate geom_area)
    dfwagg_full %>%
        arrange(ResourceId, Task, Slice, Start) %>%
        group_by(ResourceId, Slice) %>%
        mutate(TaskHeight = (Height*0.9 * Value),
               TaskPosition = cumsum(TaskHeight) - TaskHeight) %>%
        ungroup() -> dfwagg_full_view;

    return(dfwagg_full_view);
}



# Vinicius agg

# This function computes chunks by ResourceId, tasks in the same chunk will be aggregated. This function is used by aggregate_trace function.
# Input: Start,End,Duration,Value,JobId columns from a common data table with ResourceId, Start, End, Duration, Value, JobId columns
#        excludeIds - list of JobIds to do not aggregate
#        min_time_pure - states longer than this value should be kept pure.
#        states - list of states to be aggregated
# Output: a list of Chunks with the same size of the input columns (Start,End,Duration,Value,JobId)
compute_chunk <- function(Start,End,Duration,Value,JobId,excludeIds="",states,min_time_pure) {
    chunk <- rep(0,length(Duration))
    if(states %in% c("all", "All")){
        v <- Duration>min_time_pure # pure states
    } else {
        v <- Duration>min_time_pure | !(Value %in% c(unlist(strsplit(states, ",")))) | (JobId %in% c(unlist(strsplit(excludeIds, ",")))) # pure states: duration > threshold OR is not present in the list of states for aggregation OR is present in excludeIds list
    }
    v2 <- c(FALSE,Start[-1]>End[0:(length(v)-1)]+min_time_pure) # "idle" states (actually white spaces once normal idle states are already present in the trace )
    chunk[v | c(FALSE,v[0:(length(v)-1)]) | v2] <-1
    cumsum(chunk)
}

# This function creates an aggregated version of the trace. Using the computed chunks, it calculates the Start/End values of each chunk. It also calculates a proportion (Activity) that represents the time spent by each different state.
# Input: df_native - a common data table with ResourceId, Start, End, Duration, Value, JobId columns
#        states - list of states to be aggregated
#        excludeIds - list of JobIds to do not aggregate
#        min_time_pure - states longer than this value should be kept pure.
# Output: a modified data table with new columns indicating the Chunk, the Number of tasks in this chunk (by state), Activity representing the proportion of time spent in this state, Aggregated that shows if a state is pure or not.
aggregate_trace <- function(df_native, states, excludeIds, min_time_pure) {
    df_native <- df_native %>% group_by(ResourceId) %>% mutate(Chunk = compute_chunk(Start, End, Duration, Value, JobId, excludeIds, states, min_time_pure))
    df_native2 <- df_native %>% group_by(ResourceId, Chunk) %>% mutate(Start=head(Start,1), End=tail(End, 1))
    df_aggregate <- df_native2 %>% group_by(ResourceId, Chunk, Value) %>%
        summarize(Duration = sum(Duration), Start=head(Start,1), End=head(End,1), Number=n()) # n() is used to get the number of lines (former length(ResourceId))
    df_aggregate <- df_aggregate %>% group_by(ResourceId, Chunk) %>% mutate(Activity = Duration/(End-Start))
    df_aggregate <- df_aggregate %>% group_by(ResourceId, Chunk) %>% mutate(aggregated=ifelse((abs(Activity-1) <= 0.00001) & Number==1, FALSE, TRUE))
    df_aggregate
}

# This function is used to compute the relative height of different states in the chunk. After, this is used to create a stacked view of each chunk.
# Input: a data table computed using the aggreate_trace function.
# Output: a data table with PosY column, representing the height of each state in the chunk. This column is computed using the Activity proportion.
compute_aggregate_coord <- function(df){
    df$Activity <- df$Activity * 0.8  # this is used to reduce the size of the rectangle (a smaller rectangle is better for visualization as we already tested in another gantt charts)
    df %>% group_by(ResourceId, Chunk) %>% mutate(PosY=(as.numeric(ResourceId)-0.4)+(cumsum(Activity))) # -0.4 is needed to put the rectangle in the middle of the y-axi
}
geom_aggregated_states <- function (data = NULL, Show.Outliers = FALSE, min_time_pure = 1000, states = NA)
{
    if (is.null(data)) stop("data is NULL when given to geom_aggregated_states");
    if (is.na(states)) stop("states is NA when given to geom_aggregated_states");

    loginfo("Starting geom_aggregated_states");

    # Define the exclude ids based on the Show.Outliers parameter
    if (Show.Outliers){
        data$State %>%
            filter(Application == TRUE) %>%
            filter(Outlier == TRUE) %>%
            pull(JobId) -> excludeIds;
    }else{
        excludeIds <- c("");
    }

    # Prepare Y coordinates for left_join
    data$Y %>%
        rename(ResourceId = Parent) %>%
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        mutate(Node = as.factor(Node)) %>%
        mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource))) -> ydf;

    # Do the aggregation
    data$State %>%
        filter(Application == TRUE) %>%
        select(ResourceId, Start, End, Duration, Value, JobId) %>%
        aggregate_trace(states, excludeIds, min_time_pure) %>%
        left_join(
            ydf %>% select(ResourceId, Resource, Node, ResourceType, Height, Position),
            by=c("ResourceId" = "ResourceId")
        ) %>%
        mutate(Color = case_when(
                   grepl("potrf", Value) ~ "#e41a1c",
                   grepl("trsm", Value) ~ "#377eb8",
                   grepl("syrk", Value) ~ "#984ea3",
                   grepl("gemm", Value) ~ "#4daf4a",
                   TRUE ~ "unspecified")) -> dfw;

    # The list of geoms
    ret <- list();

    # Add the default theme
    ret[[length(ret)+1]] <- default_theme();

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));

    ret[[length(ret)+1]] <- geom_rect(data=dfw, aes(fill=Value,
                                                    xmin=Start,
                                                    xmax=End,
                                                    ymin=Position,
                                                    ymax=Position+(Height-0.4)*Activity,
                                                    alpha=aggregated));
    ret[[length(ret)+1]] <- guides(alpha=FALSE);
    ret[[length(ret)+1]] <- scale_alpha_discrete(range=c(1, .6));
    ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    loginfo("Finishing geom_aggregated_states");

    return(ret);
}
st_time_aggregation_vinicius_plot <- function (data = NULL)
{
    if (is.null(data)) return(NULL);

    loginfo("Vinicius Entry Agg");

    # Parameters
    with.outliers = pjr_value(pajer$st$outliers, TRUE);
    with.states = pjr_value(pajer$st$aggregation$states, c("dgemm"));
    # The longer outlier
    longer.outlier = data$State %>% filter(Application == TRUE, Outlier == TRUE) %>% pull(Duration) %>% max;
    loginfo(paste("Longer outlier for min_time_pure definition is", longer.outlier));
    with.min_time_pure = pjr_value(pajer$st$aggregation$step, longer.outlier);

    # Considering only application states
    dfw <- data$State %>% filter(Application == TRUE);

    # Obtain time interval
    tstart <- dfw %>% .$Start %>% min;
    tend <- dfw %>% .$End %>% max;

    loginfo("Vinicius Plotting Agg");

    #Plot
    gow <- ggplot() +
        geom_aggregated_states(data=data,
                               Show.Outliers = with.outliers,
                               states = with.states,
                               min_time_pure = with.min_time_pure) +
        xlab("Time [ms]");

    # Y Label
    gow <- gow + ylab("Application Workers");

    # The per-node ABE
    if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

    # add makespan
    if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

    # add idleness
    if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

    # add Global CPB
    if (pjr(pajer$st$cpb)) gow = gow + geom_cpb(data);

    # check if task dependencies should be added
    if (pjr(pajer$st$tasks$active)){
        tasklist <- pajer$st$tasks$list;
        levels <- pjr_value(pajer$st$tasks$levels);

        tasksel <- gaps_backward_deps (data = data,
                                       tasks = tasklist,
                                       levels = levels);

        gow = gow + geom_path_highlight(tasksel);
    }else{
        loginfo("DO NOT add dependencies");
    }

    loginfo("ViniciusExit Agg");
    return(gow);
}

st_time_aggregation_plot <- function (data = NULL, dfw_agg = NULL, StarPU.View = FALSE)
{
    if (is.null(data)) return(NULL);
    if (is.null(dfw_agg)) return(NULL);

    loginfo("Entry Agg");

    # Considering only application or StarPU data
    if (StarPU.View == FALSE){
        dfw <- data$State %>% filter(Application == TRUE);
    }else{
        dfw <- data$State %>% filter(Application == FALSE);
    }
    # Obtain time interval
    tstart <- dfw %>% .$Start %>% min;
    tend <- dfw %>% .$End %>% max;

    #Calculate resources idleness
    total_time <- tend - tstart;

    # Prepare for colors
    choleskyColors <- extract_colors(dfw);

    #yconf
    yconfm <- yconf(dfw);

    loginfo("Plotting Agg");

    #Plot
    gow <- dfw_agg %>% ggplot() +
        default_theme() +
        coord_cartesian(xlim=c(tstart, tend)) +
        xlab("Time [ms]") +
        scale_fill_manual(values = choleskyColors) +
        scale_y_continuous(
            breaks = yconfm$Position,
            labels=yconfm$ResourceId) +
        # Print time-aggregated data
        geom_rect(aes(fill=Task,
                      xmin=Start,
                      xmax=End,
                      ymin=Position+TaskPosition,
                      ymax=Position+(TaskPosition+TaskHeight), color=Task ), alpha=.5) +
        # Print outliers on top
        geom_rect(data=(dfw %>% filter(Outlier == TRUE)),
                  aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Position,
                      ymax=Position+Height-0.4), alpha=1);

    if (!StarPU.View){
        # Y Label
        gow <- gow + ylab("Application Workers");

        # The per-node ABE
        if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

        # add makespan
        if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

        # add idleness
        if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

        # add Global CPB
        if (pjr(pajer$st$cpb)) gow = gow + geom_cpb(data);

        # check if task dependencies should be added
        if (pjr(pajer$st$tasks$active)){
            tasklist <- pajer$st$tasks$list;
            levels <- pjr_value(pajer$st$tasks$levels);

            tasksel <- gaps_backward_deps (data = data,
                                           tasks = tasklist,
                                           levels = levels);

            gow = gow + geom_path_highlight(tasksel);
        }else{
            loginfo("DO NOT add dependencies");
        }
    }else{
        # Y Label
        gow <- gow + ylab("StarPU Workers");

        # add some color palette for StarPU States
        gow = gow + scale_fill_manual(values = starpu_colors());
    }

    loginfo("Exit Agg");
    return(gow);
}
