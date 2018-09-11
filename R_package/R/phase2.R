#' @useDynLib starvz
#' @importFrom Rcpp sourceCpp

extract_colors <- function(dfw = NULL)
{
    if(is.null(dfw)) return(NULL);

    dfw <- dfw %>% ungroup;

    dfw %>%
        select(Value, Color) %>%
        unique %>%
        .$Color %>%
        setNames(dfw %>% select(Value, Color) %>% unique %>% .$Value);
}

yconf <- function (dfw = NULL)
{
    if(is.null(dfw)) return(NULL);

    # Currently being ignored
    # step <- pjr_value(pajer$st$labels, 6);

    if(pjr_value(pajer$st$labels, "1") == "1CPU_per_NODE"){
        # One CPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(1) %>%
            ungroup;
    }else if(pjr_value(pajer$st$labels, "1") == "1GPU_per_NODE"){
        # One GPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(n()) %>%
            ungroup;
    }else if(pjr_value(pajer$st$labels, "1") == "ALL"){
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            ungroup;
    }else{
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(c(1, n())) %>%
            ungroup;
    }
}

userYLimit <- function(obj, configuration, xlimits)
{
    if (pjr(configuration)){
        # if there is an user vertical scale defined, use it
        tpScale <- list(
            coord_cartesian(xlim=xlimits,
                            ylim=c(0, pjr(configuration)))
        );
        obj <- obj + tpScale;
    }
    return(obj);
}

outlier_definition <- function(x) {
    (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}

title_plot <- function(title = NULL)
{
    ggplot() +
        xlim(0,1) +
        ylim(0,1) +
        theme(axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.line=element_blank(),
              axis.text=element_blank()) +
        annotate("text", x = .5, y = .5, label=title, size=5);
}

pjr_value <- function (property, default)
{
  ifelse(is.null(property), default, property);
}

pjr <- function (property)
{
    ifelse(!is.null(property) && property, property, FALSE);
}

starpu_mpi_grid_arrange <- function(atree, st, st_pm, st_mm, transf, starpu, ijk, ijk_pm, lackready, ready, submitted, mpi, mpiconc, mpistate, gpu, memory, gflops, title = NULL)
{
    # The list that will contain the plots
    P <- list();
    # The list that will contain the proportinal height of each plot
    H <- list();

    # Prepare title
    if (pjr(pajer$title$active)){
        if (!is.null(title)){
            tplot <- title_plot(title);
            P[[length(P)+1]] <- tplot;
            H[[length(H)+1]] <- pjr_value(pajer$title$height, 0.3);
        }
    }

    # Customized legend position
    loginfo("Customized legend position, plot list preparation");

    if (pjr(pajer$atree$active)){
        P[[length(P)+1]] <- atree;
        H[[length(H)+1]] <- pjr_value(pajer$atree$height, 3);
    }
    if (pjr(pajer$kiteration$active)){
        P[[length(P)+1]] <- ijk;
        H[[length(H)+1]] <- pjr_value(pajer$kiteration$height, 2);
    }
    if (pjr(pajer$st$active)){
        P[[length(P)+1]] <- st;
        H[[length(H)+1]] <- pjr_value(pajer$st$height, 4);
    }
    if (pjr(pajer$pmtool$kiteration$active)){
        P[[length(P)+1]] <- ijk_pm;
        H[[length(H)+1]] <- pjr_value(pajer$pmtool$kiteration$height, 2);
    }
    if (pjr(pajer$pmtool$state$active)){
        P[[length(P)+1]] <- st_pm;
        H[[length(H)+1]] <- pjr_value(pajer$pmtool$state$height, 4);
    }
    if (pjr(pajer$memory$state$active)){
        P[[length(P)+1]] <- st_mm;
        H[[length(H)+1]] <- pjr_value(pajer$memory$state$height, 3);
    }
    if (pjr(pajer$memory$transfers$active) && !pjr(pajer$memory$combined)){
        P[[length(P)+1]] <- transf;
        H[[length(H)+1]] <- pjr_value(pajer$memory$transfers$height, 2);
    }
    if (pjr(pajer$submitted$active)){
        P[[length(P)+1]] <- submitted;
        H[[length(H)+1]] <- pjr_value(pajer$submitted$height, 1);
    }
    if (pjr(pajer$starpu$active)){
        P[[length(P)+1]] <- starpu;
        H[[length(H)+1]] <- pjr_value(pajer$starpu$height, 4);
    }
    if (pjr(pajer$ready$active)){
        P[[length(P)+1]] <- ready;
        H[[length(H)+1]] <- pjr_value(pajer$ready$height, 1);
    }
    if (pjr(pajer$lackready$active)){
        P[[length(P)+1]] <- lackready;
        H[[length(H)+1]] <- pjr_value(pajer$lackready$height, .05);
    }
     if (pjr(pajer$gflops$active)){
        P[[length(P)+1]] <- gflops;
        H[[length(H)+1]] <- pjr_value(pajer$gflops$height, 2);
    }
    if (pjr(pajer$usedmemory$active)){
        P[[length(P)+1]] <- memory;
        H[[length(H)+1]] <- pjr_value(pajer$usedmemory$height, 2);
    }
    if (pjr(pajer$gpubandwidth$active)){
        P[[length(P)+1]] <- gpu;
        H[[length(H)+1]] <- pjr_value(pajer$gpubandwidth$height, 2);
    }
    if (pjr(pajer$mpibandwidth$active)){
        P[[length(P)+1]] <- mpi;
        H[[length(H)+1]] <- pjr_value(pajer$mpibandwidth$height, 1);
    }
    if (pjr(pajer$mpiconcurrent$active)){
        P[[length(P)+1]] <- mpiconc;
        H[[length(H)+1]] <- pjr_value(pajer$mpiconcurrent$height, 1);
    }
    if (pjr(pajer$mpistate$active)){
        P[[length(P)+1]] <- mpistate;
        H[[length(H)+1]] <- pjr_value(pajer$mpistate$height, 1);
    }

    # Add empty X and horizontal legend to all plots
    loginfo("Add empty X and horizontal legend");
    # Empty the X axis of all + add horizontal direction for legends
    emptyx <- theme (axis.text.x = element_blank(), axis.title.x = element_blank());
    leghor <- theme (legend.direction = "horizontal", legend.background = element_rect(fill = "white"));
    P <- lapply(P, function(p) { p <- p + emptyx; });

    # Vanilla configuration
    if (pjr_value(pajer$vanilla$horizontal, FALSE) == FALSE){
        # Add time scale on last plot
        loginfo("Add time scale on last plot");
        notemptyx <- theme (axis.text.x = element_text(), axis.title.x = element_text());
        P[[length(P)]] <- P[[length(P)]] + notemptyx;
    }

    if (pjr_value(pajer$vanilla$vertical, FALSE) == TRUE){
        # Remove Y scale title and text
        emptyy <- theme (axis.text.y = element_blank(), axis.title.y = element_blank());
        P <- lapply(P, function(p) { p <- p + emptyy; });
    }

    # Preparation for cowplot's plot_grid
    loginfo("Call cowplot's plot_grid function");
    g <- plot_grid(plotlist = P, align="v", ncol = 1, rel_heights = unlist(H));
    return(g);
}

the_master_function <- function(data = NULL)
{
    if(is.null(data)) return(NULL);
    if(is.null(pajer)) return(NULL);

    # Activate logs
    if(pjr(pajer$log)){
        addHandler(writeToConsole)
    }

    # Get data
    directory <- data$Origin;
    dfw <- data$State;
    dfv <- data$Variable;

    if(is.null(dfw)){
       stop("The State data was not loaded, check if the feather files exists.");
    }

    loginfo("Starting the master function");

    # Fail Checking
    if((pjr(pajer$pmtool$state$active) || pjr(pajer$pmtool$kiteration$active)) && is.null(data$pmtool_states)){
      print("Pmtool states config is active but the data is NULL")
      pajer$pmtool$state$active <<- FALSE;
      pajer$pmtool$kiteration$active <<- FALSE;
    }

    if(pjr(pajer$pmtool$bounds$active) && is.null(data$pmtool)){
      print("Pmtool bounds config is active but the data is NULL")
      pajer$pmtool$bounds$active <<- FALSE;
    }

    if(is.null(data$Link)){
      print("This dataset dont have links, disabling some options")
      pajer$memory$transfers$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

    dfevents = dfw %>% filter(Type == "Memory Node State")

    if((dfevents %>% nrow) == 0 && ( pjr(pajer$memory$new_data) && (data$Events %>% nrow) == 0) ){
      print("This dataset dont have memory node states")
      pajer$memory$state$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

    if (!is.null(pajer$time)){
        stop("pajer: you are using a deprecated parameter.");
    }

    # Adjust temporal scale
    tstart <- pjr_value(pajer$limits$start, dfw %>% pull(Start) %>% min);
    tend <- pjr_value(pajer$limits$end, dfw %>% pull(End) %>% max)
    tScale <- list(
        coord_cartesian(xlim=c(tstart, tend))
    );
    # Define the global aggregation step as 0.1% of the total window
    globalAggStep = (tend - tstart) * .001;

    # Set all possible plots to NULL
    goatreet <- geom_blank();
    gow <- geom_blank();
    gow_pm <- geom_blank();
    gow_mm <- geom_blank();
    gow_tf <- geom_blank();
    gstarpu <- geom_blank();
    goijk <- geom_blank();
    goijk_pm <- geom_blank();
    golrv <- geom_blank();
    gorv <- geom_blank();
    gosv <- geom_blank();
    gogfv <- geom_blank();
    goguv <- geom_blank();
    gomov <- geom_blank();
    gogov <- geom_blank();

    if (!is.null(data$ATree) && pjr(pajer$atree$active)){
        loginfo("Creating the temporal atree plot");
        goatreet <- atree_temporal_chart(data) + tScale;
        loginfo("Temporal atree plot completed");
    }

    # SpaceTime
    if (pjr(pajer$st$active)){
        loginfo("Creating the Space/Time");
        if (pjr(pajer$st$aggregation$active)){
            if (pjr_value(pajer$st$aggregation$method, "lucas") == "lucas"){
                loginfo("Will call st_time_aggregation");
                aggStep <- pjr_value(pajer$st$aggregation$step, globalAggStep);
                dfw_agg <- st_time_aggregation(dfw, step=aggStep);
                data %>% st_time_aggregation_plot (dfw_agg) + tScale -> gow;
                loginfo("st_time_aggregation completed");
            }else{
                loginfo("Call vinicius aggregation");
                data %>% st_time_aggregation_vinicius_plot() + tScale -> gow;
                loginfo("Finish vinicius aggregation");
            }
        }else{
            loginfo("Will call state_chart");
            data %>% state_chart (globalEndTime = tend, ST.Outliers = pjr(pajer$st$outliers), StarPU.View = FALSE) + tScale -> gow;
            loginfo("state_chart completed (no aggregation)");
        }

        # Without legend
        if (!pjr(pajer$st$legend)){
            gow <- gow + theme(legend.position="none");
        }
    }

    if (pjr(pajer$pmtool$state$active)){
        data %>% state_pmtool_chart () + tScale -> gow_pm;
    }

    memory_combined <- pjr(pajer$memory$combined) & pjr(pajer$memory$transfers$active);

    if (pjr(pajer$memory$state$active)){
        if(pjr(pajer$memory$new_data)){
            loginfo("Memory Chart using Events")
            data %>% events_memory_chart(combined = memory_combined, tstart=tstart, tend=tend) + tScale -> gow_mm;
        }else{
            loginfo("Memory Chart using States")
            data %>% memory_chart(combined = memory_combined, tstart=tstart, tend=tend) + tScale -> gow_mm;
        }
    }

    if (pjr(pajer$memory$transfers$active) & !memory_combined){
        data %>% link_chart(tstart=tstart, tend=tend) + tScale -> gow_tf;
    }



    # StarPU SpaceTime
    if (pjr(pajer$starpu$active)){
        loginfo("Creating the StarPU Space/Time");
        if (pjr(pajer$starpu$aggregation$active)){
          loginfo("Will call st_time_aggregation");
          aggStep <- pjr_value(pajer$starpu$aggregation$step, globalAggStep);
          dfw_agg <- st_time_aggregation(dfw, StarPU.View=TRUE, step=aggStep);
          data %>% st_time_aggregation_plot (dfw_agg, StarPU.View=TRUE) + tScale -> gstarpu;
          loginfo("st_time_aggregation completed");
        }else{
          data %>% state_chart (globalEndTime = tend, StarPU.View = TRUE) + tScale -> gstarpu;
          loginfo("state_chart for StarPU behavior completed (no aggregation)");
        }
        if (!pjr(pajer$starpu$legend)){
            gstarpu <- gstarpu + theme(legend.position="none");
        }
    }

    # KIteration
    if (pjr(pajer$kiteration$active)){
        loginfo("Creating the KIteration");
        goijk <- k_chart(dfw) + tScale;

        if (!pjr(pajer$kiteration$legend)){
            goijk <- goijk + theme(legend.position="none");
        }
    }

    # KIteration PMTOOL
    if (pjr(pajer$pmtool$kiteration$active)){
        loginfo("Creating the KIteration for PMTool");
        goijk_pm <- k_chart_pmtool(data$pmtool_states) + tScale;

        if (!pjr(pajer$pmtool$kiteration$legend)){
            goijk_pm <- goijk_pm + theme(legend.position="none");
        }
    }

    # Lack ready (companion for Ready Variable)
    if (pjr(pajer$lackready$active)){
        loginfo("Creating the Lack Ready Plot");
        golrv <- plot_lackready(data) + tScale;
    }

    # Ready
    if (pjr(pajer$ready$active)){
        loginfo("Creating the Ready plot");
        aggStep <- pjr_value(pajer$ready$step, globalAggStep);
        gorv <- dfv %>%
            filter(grepl("sched", ResourceId), grepl("Ready", Type)) %>%
            var_integration_segment_chart(step = aggStep) + tScale;
        if (!pjr(pajer$ready$legend)){
            gorv <- gorv + theme(legend.position="none");
        }else{
            gorv <- gorv +
                theme(legend.position=c(.99,.8),
                      legend.justification="right") +
                guides(color = guide_legend(nrow = 1))
        }
        gorv <- userYLimit(gorv, pajer$ready$limit, c(tstart, tend));
    }

    # Submitted
    if (pjr(pajer$submitted$active)){
        loginfo("Creating the Submitted plot");
        aggStep <- pjr_value(pajer$submitted$step, globalAggStep);
        gosv <- dfv %>%
            filter(grepl("sched", ResourceId), grepl("Submitted", Type)) %>%
            var_integration_segment_chart(step = aggStep) + tScale;
        if (!pjr(pajer$submitted$legend)){
            gosv <- gosv + theme(legend.position="none");
        }else{
            gosv <- gosv +
                theme(legend.position=c(.99,.8),
                      legend.justification="right") +
                guides(color = guide_legend(nrow = 1))
        }
        gosv <- userYLimit(gosv, pajer$submitted$limit, c(tstart, tend));
    }

    # GFlops
    if (pjr(pajer$gflops$active)){
        loginfo("Creating the GFlops plot");
        aggStep <- pjr_value(pajer$gflops$step, globalAggStep);
        facetted <- pjr_value(pajer$gflops$facet, TRUE);
        gogfv <- dfv %>%
            filter(Type == "GFlops") %>%
            var_integration_segment_chart (., ylabel="GFlops", step = aggStep, facetting = facetted) + tScale;

        # adjust GFlops scale
        if (pjr_value(pajer$gflops$limit, FALSE)){
            limit <- pjr_value(pajer$gflops$limit, 0); # TODO define a better default value
            gogfv <- gogfv + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, limit));
        }
        if (!pjr(pajer$gflops$legend)){
            gogfv <- gogfv + theme(legend.position="none");
        }
        # TODO: user limit
    }

    # Used Memory
    if (pjr(pajer$usedmemory$active)){
        loginfo("Creating the Used Memory plot");

        if ((dfv %>% filter(grepl("MEMMANAGER", ResourceId) & grepl("Used", Type)) %>% nrow) == 0){
            logwarn("There aren't any information about Used Memory, ignoring it.");
            pajer$usedmemory$active <<- FALSE;
        }else{
          aggStep <- pjr_value(pajer$usedmemory$step, globalAggStep);

          goguv <- dfv %>%
              filter(grepl("MEMMANAGER", ResourceId) & grepl("Used", Type)) %>%
              var_integration_segment_chart(step = aggStep) + tScale;
          if (!pjr(pajer$usedmemory$legend)){
              goguv <- goguv + theme(legend.position="none");
          }else{
              goguv <- goguv +
                  theme(legend.position=c(.99,.8),
                        legend.justification="right") +
                  guides(color = guide_legend(nrow = 1))
          }
          goguv <- userYLimit(goguv, pajer$usedmemory$limit, c(tstart, tend));
        }
    }

    # MPIBandwidth
    if (pjr(pajer$mpibandwidth$active)){
        loginfo("Creating the MPIBandwidth plot");
        aggStep <- pjr_value(pajer$mpibandwidth$step, globalAggStep);
        mpi_out <- dfv %>% filter(grepl("mpict", ResourceId), grepl("Out", Type));
        if ((mpi_out %>% nrow) == 0){
            print("There aren't any information on MPIBandwidth, ignoring it.");
            pajer$mpibandwidth$active <<- FALSE;
        }else{
            gomov <- mpi_out %>%
                var_integration_segment_chart(., ylabel="MPI\n(MB/s)", step=aggStep) + tScale;
            if (!pjr(pajer$mpibandwidth$legend)){
                gomov <- gomov + theme(legend.position="none");
            }
            gomov <- userYLimit(gomov, pajer$mpibandwidth$limit, c(tstart, tend));
        }
    }

    # MPI Concurrent
    if (pjr(pajer$mpiconcurrent$active)){
        loginfo("Creating the MPI concurrent ops plot");
        aggStep <- pjr_value(pajer$mpiconcurrent$step, globalAggStep);
        gompiconc <- data %>% concurrent_mpi() %>%
            var_integration_segment_chart(., ylabel="Concurrent\nMPI Tasks", step=aggStep) + tScale;
        if (!pjr(pajer$mpiconcurrent$legend)){
            gompiconc <- gompiconc + theme(legend.position="none");
        }
        gompiconc <- userYLimit(gompiconc, pajer$mpiconcurrent$limit, c(tstart, tend));
    }

    # MPI State
    if (pjr(pajer$mpistate$active)){
        loginfo("Creating the MPI state");
        gompistate <- data %>% state_mpi_chart() + tScale;
        if (!pjr(pajer$mpistate$legend)){
            gompistate <- gompistate + theme(legend.position="none");
        }
    }

    # GPUBandwidth
    if (pjr(pajer$gpubandwidth$active)){
        loginfo("Creating the GPU Bandwidth plot");
        aggStep <- pjr_value(pajer$gpubandwidth$step, globalAggStep);
        if(aggStep < 0){
            gogov <- dfv %>%
                # Get only GPU memory banwidth (out)
                filter(grepl("MEMMANAGER", ResourceId), grepl("Out", Type)) %>%
                # Remove the MANAGER0, which is CPU-only
                # TODO: After the logical OR there is a support for single-node StarPU traces
                filter(!grepl("MANAGER0", Resource)) %>%
                group_by(Type, Node, ResourceType, Start, End, Duration) %>%
                summarize(Value = sum(Value), N=n()) %>%
                rename(ResourceId = Node) %>%
                var_chart(ylabel = "GPU\n(MB/s)") + tScale;
        }else{
            gogov <- dfv %>%
                # Get only GPU memory banwidth (out)
                filter(grepl("MEMMANAGER", ResourceId), grepl("Out", Type)) %>%
                # Remove the MANAGER0, which is CPU-only
                # TODO: After the logical OR there is a support for single-node StarPU traces
                filter(Resource != "MEMMANAGER0" | Node != "MEMMANAGER0") %>%
                var_integration_segment_chart(., ylabel="GPU\n(MB/s)", step=aggStep) + tScale;
        }
        if (!pjr(pajer$gpubandwidth$legend)){
            gogov <- gogov + theme(legend.position="none");
        }
        # Fixed upper bound
        if (pjr(pajer$gpubandwidth$bound)) {
            lbound <- pjr_value(pajer$gpubandwidth$bound, 0); # TODO: define a better default value
            gogov <- gogov + coord_cartesian(ylim=c(0, lbound),
                                             xlim=c(tstart, tend));
        }
        if(pjr(pajer$gpubandwidth$total)){
           ms <- dfv %>%
              filter(grepl("MEMMANAGER", ResourceId), grepl("Out", Type)) %>%
              filter(Resource != "MEMMANAGER0" | Node != "MEMMANAGER0") %>%
              group_by(Type, Node, ResourceType, Start, End, Duration) %>%
              summarize(Value = sum(Value), N=n()) %>%
              rename(ResourceId = Node)
          y_size <- layer_scales(gogov)$y$range$range[2];
          gogov <- gogov + ms %>% var_chart_text(tstart=tstart, tend=tend, y_end = y_size);
        }
    }

    loginfo("Assembling the plot");

    # assembling
    g <- starpu_mpi_grid_arrange(atree = goatreet,
                                 st = gow,
                                 st_pm = gow_pm,
                                 st_mm = gow_mm,
                                 transf = gow_tf,
                                 starpu = gstarpu,
                                 ijk = goijk,
                                 ijk_pm = goijk_pm,
                                 lackready = golrv,
                                 ready = gorv,
                                 submitted = gosv,
                                 mpi = gomov,
                                 mpiconc = gompiconc,
                                 mpistate = gompistate,
                                 gpu = gogov,
                                 memory = goguv,
                                 gflops = gogfv,
                                 title = directory);


    return(g);
}
