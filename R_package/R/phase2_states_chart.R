state_chart <- function (data = NULL, globalEndTime = NULL, ST.Outliers = TRUE, StarPU.View = FALSE)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    # Filter
    dfwapp = data$Application

    # Obtain time interval
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    if(StarPU.View){
      gow <- gow + geom_states(data$Starpu,
                              ST.Outliers, StarPU.View, data$Colors);
    }else{
      gow <- gow + geom_states(data$Application,
                               ST.Outliers, StarPU.View, data$Colors);
    }

    if (!StarPU.View){

        # add idleness
        if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data$Application);

        # check if task dependencies should be added
        if (pjr(pajer$st$tasks$active)){
            tasklist <- pajer$st$tasks$list;
            levels <- pjr_value(pajer$st$tasks$levels, 2);

            tasksel <- gaps_backward_deps (data = data,
                                           tasks = tasklist,
                                           levels = levels);

            gow = gow + geom_path_highlight(tasksel);
        }

        # add Global CPB
        if (pjr(pajer$st$cpb) || pjr(pajer$st$cpb_mpi$active)) gow = gow + geom_cpb(data);

        # The per-node ABE
        if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

        # add pmtool bound
        if (pjr(pajer$pmtool$bounds$active)) gow = gow + geom_pmtool_bounds(data);

        # add makespan
        if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

    }

    return(gow);
}

k_chart <- function (dfw = NULL, middle_lines = NULL, per_node = FALSE, colors = NULL)
{
    if (is.null(dfw)) stop("dfw provided to k_chart is NULL");
    if (is.null(colors)) stop("colors provided to k_chart is NULL");

    # Prepare for colors
    colors %>% select(Value, Color) %>% unique %>% .$Color -> appColors
    appColors %>% setNames(colors %>% select(Value, Color) %>% unique %>% .$Value) -> appColors;

    # Prepare for borders
    if(per_node) {
        dfw %>% group_by(Node, Iteration) -> temp1
    }else{
        dfw %>% group_by(Iteration) -> temp1
    }
    dfborders <- temp1 %>%
        summarize(Start = min(Start), End=max(End)) %>%
        mutate(IterationB = lead(Iteration), StartB = lead(Start)) %>%
        mutate(IterationE = lead(Iteration), EndB = lead(End)) %>%
        na.omit;

    # Prepare for middle
    lapply(middle_lines, function(percentage) {
        dfw %>%
            filter(Application) %>%
            select(Node, Iteration, Start, End) -> temp1
        if(per_node){
            temp1 %>% group_by(Node, Iteration) -> temp1
        }else{
            temp1 %>% group_by(Iteration) -> temp1
        }
        temp1 %>%
            mutate(Number.Tasks = n()) %>%
            arrange(Start) %>%
            slice(unique(Number.Tasks*percentage)) %>%
            ungroup %>%
            mutate(Middle = Start + (End-Start)/2) -> temp1
        if(per_node) {
            temp1 %>% group_by(Node) -> temp1
        }
        temp1 %>%
            arrange(Iteration) %>%
            mutate(Middle.Next = lead(Middle)) %>%
            mutate(IterationB = lead(Iteration)) %>%
            mutate(Percentage = percentage) %>%
            ungroup %>%
            na.omit
    }) %>% bind_rows -> dfmiddle

    # Height of each bar
    height = 0.8;

    goijk <- dfw %>% ggplot() +
        guides(fill = guide_legend(nrow = 1)) +
        scale_fill_manual(values = appColors) +
        theme_bw(base_size=12) +
        xlab("Time [ms]") +
        ylab("Iteration") +
        default_theme() +
        # Keep the alpha = 1 even if we use an alpha below
        guides(fill = guide_legend(override.aes = list(alpha=1))) +
        scale_y_reverse(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
        # The start border
        geom_curve(data=dfborders, aes(x=Start, xend=StartB, y=Iteration+height-height/2, yend=IterationB+height-height/2), curvature=0.1, angle=20) +
        # The end border
        geom_curve(data=dfborders, aes(x=End, xend=EndB, y=Iteration-height/2, yend=IterationB-height/2), curvature=-0.1, angle=20) +
        # The state
        geom_rect(aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Iteration-height/2,
                      ymax=Iteration+height/2), alpha=.5)
    if(!is.null(middle_lines)) {
        goijk <- goijk +
            # The median line
            geom_curve(data=dfmiddle, aes(x=Middle, xend=Middle.Next, y=Iteration-height/2, yend=IterationB-height/2), curvature=-0.1, angle=20, color="black")
    }
    return(goijk);
}

geom_states <- function (dfw = NULL, Show.Outliers = FALSE, StarPU = FALSE, Colors = NULL)
{
    if (is.null(dfw)) stop("data is NULL when given to geom_states");
    if (is.null(Colors)) stop("data is NULL when given to geom_states");

    ret <- list();

    # Color mapping
    if (StarPU){
      ret[[length(ret)+1]] <- scale_fill_manual(values = starpu_colors());
    }else{
      ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw, Colors));
    }

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$st$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab(ifelse(StarPU, "StarPU Workers", "Application Workers"));

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+Height-0.2),
                  color=ifelse(pjr_value(pajer$st$rect_outline, NA), "black", NA),
                  alpha=ifelse(Show.Outliers && !StarPU, pjr_value(pajer$st$alpha, 0.5), 1.0));

    # Add outliers conditionally
    if (Show.Outliers && !StarPU){
        ret[[length(ret)+1]] <-
            geom_rect(data=(dfw %>% filter(Outlier == TRUE)),
                      aes(fill=Value,
                          xmin=Start,
                          xmax=End,
                          ymin=Position,
                          ymax=Position+Height-0.2),
                      color=ifelse(pjr_value(pajer$st$rect_outline, NA), "black", NA),
                      alpha=1);
    }

    return(ret);
}

geom_path_highlight <- function (paths = NULL)
{
    if (is.null(paths)) stop("paths is NULL when given to gaps_backward_deps");
    if ((paths %>% nrow) == 0){
        return(list());
    }

    # paths is identical to data$Starpu, but with an additional column called Path

    ret <- list();

    # highlight the tasks involved in the path
    ret[[length(ret)+1]] <- geom_rect(data=paths,
                                      size=1,
                                      aes(color=Path,
                                          xmin=Start,
                                          xmax=End,
                                          ymin=Position,
                                          ymax=Position+Height-0.2), alpha=0);

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

node_summary <- function(app){
    hl_per_node_ABE(app) -> Abes
    app %>% group_by(Node) %>%
               arrange(-End) %>% slice(1) %>%
               select(Node, End) %>% ungroup() %>%
               mutate(Node=as.integer(as.character(Node))) -> Makespans

    Makespans %>% mutate(Metric="Makespan") %>%
              rename(Value=End) -> makes
    makes$Value <- makes$Value - Abes$Result
    Abes %>% select(Node, Result) %>%
         mutate(Node=as.integer(as.character(Node))) %>%
         mutate(Metric="Abe") %>%
         rename(Value=Result) %>%
         bind_rows(makes) %>%
         mutate(Metric=factor(Metric, levels=c("Makespan", "Abe"))) -> all_data

     all_data %>% mutate(Node = as.integer(Node)) %>%
              ggplot(aes(y=Node, x=Value, fill=Metric)) +
              default_theme() +
              scale_y_reverse() +
              geom_col(width=0.8) -> splot
    return(splot);
}
