#' @useDynLib starvz
#' @importFrom Rcpp sourceCpp

check_arrow <- function(){
  if(!arrow_available()){
    logwarn("Arrow was not property installed, use: install_arrow()")
  }
}

extract_colors <- function(dfw = NULL, colors = NULL)
{
    if(is.null(dfw)) return(NULL);
    if(is.null(colors)) return(NULL);

    dfw <- dfw %>% ungroup;

    dfw %>%
        select(Value) %>%
        unique %>%
        left_join(colors, by=c("Value")) %>%
        .$Color %>%
        setNames(dfw %>% select(Value) %>% unique %>% .$Value);
}

yconf <- function (dfw = NULL, option = "ALL")
{
    if(is.null(dfw)) return(NULL);

    # Currently being ignored
    # step <- pjr_value(pajer$st$labels, 6);
    dfw %>% mutate(Node = as.integer(as.character(Node))) -> dfw
    dfw %>% mutate(ResourceId = factor(ResourceId)) %>%
            mutate(ResourceId = factor(ResourceId, levels=mixedsort(levels(ResourceId)))) -> dfw
    if(option == "1CPU_per_NODE"){ #First
        # One CPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(1) %>%
            ungroup;
    }else if(option == "1GPU_per_NODE"){ #Last
        # One GPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(n()) %>%
            ungroup;
    }else if(option == "NODES_only"){ #First
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(ResourceId, ResourceType) %>%
            slice(1) %>%
            mutate(ResourceId = Node) %>%
            ungroup;
    }else if(option == "NODES_1_in_10"){ #First
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(ResourceId, ResourceType) %>%
            slice(1) %>%
            mutate(ResourceId = Node) %>%
            ungroup %>%
            mutate(Node = as.integer(as.character(Node))) %>%
            arrange(Node) %>%
            slice(seq(1, n(), 10))
    }else if(option == "ALL"){
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            ungroup;
    }else{ #First and Last
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

starpu_mpi_grid_arrange <- function(plist)
{
    # Decode plist to local variables (to avoid editing all function)
    atree = plist$atree
    utiltreenode = plist$utiltreenode
    utiltreedepth = plist$utiltreedepth
    st = plist$st
    st_pm = plist$st_pm
    st_mm = plist$st_mm
    transf = plist$transf
    starpu = plist$starpu
    ijk = plist$ijk
    ijk_pm = plist$ijk_pm
    lackready = plist$lackready
    ready = plist$ready
    submitted = plist$submitted
    mpi = plist$mpi
    mpiconc = plist$mpiconc
    mpiconcout = plist$mpiconcout
    mpistate = plist$mpistate
    gpu = plist$gpu
    memory = plist$memory
    imb_plot = plist$imb_plot
    imb_plot_power = plist$imb_plot_power
    imb_plot_hete = plist$imb_plot_hete
    heatmap = plist$heatmap
    gflops = plist$gflops
    activenodes = plist$activenodes
    nodememuse = plist$nodememuse
    summary_nodes = plist$summary_nodes
    computingnodes = plist$computingnodes
    title = plist$title

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

    # Letsset default height values
    # This is also experiment computed on starvz_guided_plot
    # For plots that show resources
    if(!exists("starvz_height_resources")){
      starvz_height_resources <- 4
    }
    # For plots that show nodes 10px per node
    if(!exists("starvz_height_nodes")){
      starvz_height_nodes <- 2
    }
    # For plots that show nodes 10px per node
    if(!exists("starvz_height_agg")){
      starvz_height_agg <- 2
    }
    # For variable plots, 100px
    if(!exists("starvz_height_var")){
      starvz_height_var <- 2
    }
    # For the tree, default 1.5px per tree node Position
    if(!exists("starvz_height_atree")){
      starvz_height_atree <- 4
    }
    # For plots TODO, default 200px
    if(!exists("starvz_height_todo")){
      starvz_height_todo <- 4
    }

    # For plots TODO, default small
    if(!exists("starvz_height_small")){
      starvz_height_small <- 0.05
    }

    if (pjr(pajer$atree$active)){
        P[[length(P)+1]] <- atree;
        H[[length(H)+1]] <- pjr_value(pajer$atree$height, starvz_height_atree);
    }
    if (pjr(pajer$utiltreenode$active)){
        P[[length(P)+1]] <- utiltreenode;
        H[[length(H)+1]] <- pjr_value(pajer$utiltreenode$height, starvz_height_var);
    }
    if (pjr(pajer$utiltreedepth$active)){
        P[[length(P)+1]] <- utiltreedepth;
        H[[length(H)+1]] <- pjr_value(pajer$utiltreedepth$height, starvz_height_var);
    }
    if (pjr(pajer$activenodes$active)){
        P[[length(P)+1]] <- activenodes;
        H[[length(H)+1]] <- pjr_value(pajer$activenodes$height, starvz_height_var);
    }
    if (pjr(pajer$activenodes$nodememuse$active)){
        P[[length(P)+1]] <- nodememuse;
        H[[length(H)+1]] <- pjr_value(pajer$activenodes$nodememuse$height, starvz_height_var);
    }
    if (pjr(pajer$computingnodes$active)){
        P[[length(P)+1]] <- computingnodes;
        H[[length(H)+1]] <- pjr_value(pajer$computingnodes$height, starvz_height_var);
    }
    if (pjr(pajer$kiteration$active)){
        P[[length(P)+1]] <- ijk;
        H[[length(H)+1]] <- pjr_value(pajer$kiteration$height, starvz_height_todo);
    }
    if (pjr(pajer$summary_nodes$active)){
        P[[length(P)+1]] <- summary_nodes;
        H[[length(H)+1]] <- pjr_value(pajer$summary_nodes$height, starvz_height_nodes);
    }
    if (pjr(pajer$st$active)){
        P[[length(P)+1]] <- st;
        if(pjr(pajer$st$aggregation$active) && pjr_value(pajer$st$aggregation$method, "lucas") == "nodes"){
            H[[length(H)+1]] <- pjr_value(pajer$st$height, starvz_height_agg);
        }else{
            H[[length(H)+1]] <- pjr_value(pajer$st$height, starvz_height_resources);
        }
    }
    if (pjr(pajer$pmtool$kiteration$active)){
        P[[length(P)+1]] <- ijk_pm;
        H[[length(H)+1]] <- pjr_value(pajer$pmtool$kiteration$height, starvz_height_todo);
    }
    if (pjr(pajer$pmtool$state$active)){
        P[[length(P)+1]] <- st_pm;
        H[[length(H)+1]] <- pjr_value(pajer$pmtool$state$height, starvz_height_resources);
    }
    if (pjr(pajer$memory$state$active)){
        P[[length(P)+1]] <- st_mm;
        H[[length(H)+1]] <- pjr_value(pajer$memory$state$height, starvz_height_nodes);
    }
    if (pjr(pajer$memory$transfers$active) && !pjr(pajer$memory$combined)){
        P[[length(P)+1]] <- transf;
        H[[length(H)+1]] <- pjr_value(pajer$memory$transfers$height, starvz_height_nodes);
    }
    if (pjr(pajer$submitted$active)){
        P[[length(P)+1]] <- submitted;
        H[[length(H)+1]] <- pjr_value(pajer$submitted$height, starvz_height_var);
    }
    if (pjr(pajer$starpu$active)){
        P[[length(P)+1]] <- starpu;
        H[[length(H)+1]] <- pjr_value(pajer$starpu$height, starvz_height_resources);
    }
    if (pjr(pajer$ready$active)){
        P[[length(P)+1]] <- ready;
        H[[length(H)+1]] <- pjr_value(pajer$ready$height, starvz_height_var);
    }
    if (pjr(pajer$lackready$active)){
        P[[length(P)+1]] <- lackready;
        H[[length(H)+1]] <- pjr_value(pajer$lackready$height, starvz_height_small);
    }
     if (pjr(pajer$gflops$active)){
        P[[length(P)+1]] <- gflops;
        H[[length(H)+1]] <- pjr_value(pajer$gflops$height, starvz_height_var);
    }
    if (pjr(pajer$usedmemory$active)){
        P[[length(P)+1]] <- memory;
        H[[length(H)+1]] <- pjr_value(pajer$usedmemory$height, starvz_height_var);
    }
    if (pjr(pajer$imbalance$active)){
        P[[length(P)+1]] <- imb_plot;
        H[[length(H)+1]] <- pjr_value(pajer$imbalance$height, starvz_height_var);
    }
    if (pjr(pajer$power_imbalance$active)){
        P[[length(P)+1]] <- imb_plot_power;
        H[[length(H)+1]] <- pjr_value(pajer$power_imbalance$height, starvz_height_var);
    }
    if (pjr(pajer$hete_imbalance$active)){
        P[[length(P)+1]] <- imb_plot_hete;
        H[[length(H)+1]] <- pjr_value(pajer$hete_imbalance$height, starvz_height_var);
    }
    if (pjr(pajer$utilheatmap$active)){
        P[[length(P)+1]] <- heatmap;
        H[[length(H)+1]] <- pjr_value(pajer$utilheatmap$height, starvz_height_resources);
    }
    if (pjr(pajer$gpubandwidth$active)){
        P[[length(P)+1]] <- gpu;
        H[[length(H)+1]] <- pjr_value(pajer$gpubandwidth$height, starvz_height_var);
    }
    if (pjr(pajer$mpibandwidth$active)){
        P[[length(P)+1]] <- mpi;
        H[[length(H)+1]] <- pjr_value(pajer$mpibandwidth$height, starvz_height_var);
    }
    if (pjr(pajer$mpiconcurrent$active)){
        P[[length(P)+1]] <- mpiconc;
        H[[length(H)+1]] <- pjr_value(pajer$mpiconcurrent$height, starvz_height_var);
    }
    if (pjr(pajer$mpiconcurrentout$active)){
        P[[length(P)+1]] <- mpiconcout;
        H[[length(H)+1]] <- pjr_value(pajer$mpiconcurrentout$height, starvz_height_var);
    }
    if (pjr(pajer$mpistate$active)){
        P[[length(P)+1]] <- mpistate;
        H[[length(H)+1]] <- pjr_value(pajer$mpistate$height, starvz_height_nodes);
    }

    starvz_height_total <<- sum(unlist(H))

    # Empty the X axis of all + add horizontal direction for legends
    emptyx <- theme (axis.text.x = element_blank(), axis.title.x = element_blank());
    leghor <- theme (legend.direction = "horizontal", legend.background = element_rect(fill = "white"));
    P <- lapply(P, function(p) { p <- p + emptyx; });

    # Vanilla configuration
    if (pjr_value(pajer$vanilla$horizontal, FALSE) == FALSE){
        # Add time scale on last plot
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

starvz_guided_plot <- function(data, name)
{
    #USe Y to compute state and starpu size
    #Get number of nodes

    # Lets compute some default height values
    # For plots that show resources, 10px per resource on Y
    # For plots that show nodes 10px per node
    # For variable plots, 100px
    # For atree plot 1.5px per node Position
    # For plots TODO, default 200px
    if(!is.null(pajer$selected_nodes)){
      nodes <- length(pajer$selected_nodes)
    }else if(!is.null(data$Tasks)){
      nodes <- data$Tasks %>% select(MPIRank) %>% distinct() %>% nrow()
    }else if(!is.null(data$Application)){
      nodes <- data$Application %>% select(Node) %>% distinct() %>% nrow()
    }else{
      nodes <- 1
    }

    types <- data$Application %>% select(ResourceType) %>% distinct() %>% nrow()

    if(!is.null(pajer$selected_nodes)){
      data$Y %>% separate(Parent, into=c("Node"), remove=FALSE) %>%
             filter(Node %in% pajer$selected_nodes) %>%
             arrange(Position) %>%
             mutate(New = cumsum(lag(Height, default = 0))) %>%
             select(Parent, New) -> new_y
        starvz_height_resources <<- (new_y$New %>% max()) * 10 / 100
    }else{
        starvz_height_resources <<- (data$Y$Position %>% max()) * 10 / 100
    }

    starvz_height_agg <<- max(nodes * types * pjr_value(pajer$guided$agg_type_height, 50) / 100, 1)
    starvz_height_nodes <<- max(nodes * 10 / 100, 1)
    starvz_height_small <<- 0.5
    starvz_height_atree <<- ((data$Atree$Position %>% max()) * 1.5) / 100

    p <- starvz_plot(data)

    total_dpi <- 120
    if(starvz_height_total==0){
      logwarn("Total height is 0, set to 1000")
      starvz_height_total <- 1000
    }

    final_px_height <- (starvz_height_total+5) * 100
    final_px_width <- 1000
    final_in_height <- final_px_height / total_dpi
    final_in_width <- final_px_width / total_dpi

    ggsave(name, plot=p, width = final_in_width, height = final_in_height, units = "in", dpi=total_dpi, limitsize = FALSE)
    return(list(plot=p, height=final_px_height))
}

starvz_plot_list <- function(data = NULL)
{
    if(is.null(data)) stop("data passed as parameter is null")
    if(is.null(pajer)) stop("pajer configuration is null");

    # Activate logs
    if(pjr(pajer$log)){
        addHandler(writeToConsole)
    }

    if(is.null(data$Version)){
       logwarn("This is a old StarVZ trace, trying to be retrocompatible")
       data$Application <- data$State %>% filter(Application)
       data$Application <- data$Application %>% mutate(Size = as.integer(Size))
       data$Starpu <- data$State %>% filter(Type=="Worker State", Application==FALSE) %>%
                                     mutate(Size = as.integer(Size))
       data$Comm_state <- data$State %>% filter(Type=="Communication Thread State") %>% select(-Position, -Height)
       data$Memory_state <- data$State %>% filter(Type=="Memory Node State") %>% select(-Position)
       data$Colors <- data$State %>% filter(Application) %>% select(Value, Color) %>% distinct()
    }

    # Get data
    directory <- data$Origin;

    if(is.null(data$Application)){
       stop("The Application data was not loaded, check if the feather files exists.");
    }

    if (!is.null(pajer$time)){
        stop("pajer: you are using a deprecated parameter.");
    }

    # Define makespan
    makespan <- data$Application %>% pull(End) %>% max

    #  Filter out everything after the makespan
    # TODO: Maybe this will make sense to transfer to Phase1
    #data$Application <- data$Application %>% filter(Start < makespan)
    #data$Starpu <- data$Starpu %>% filter(Start < makespan)
    #data$Dag <- data$Dag %>% filter(Start < makespan)
    #data$Events <- data$Events %>% filter(Start < makespan)
    #data$Gaps <- data$Gaps %>% filter(Start.x < makespan)
    #data$Link <- data$Link %>% filter(Start < makespan)
    #data$Variable <- data$Variable %>% filter(End < makespan)

    # Adjust temporal scale
    tstart <- pjr_value(pajer$limits$start, data$Application %>% pull(Start) %>% min);
    tend <- pjr_value(pajer$limits$end, data$Application %>% pull(End) %>% max) + 1 # Just to give space
    tScale <- list(
        coord_cartesian(xlim=c(tstart, tend))
    );

    dfv <- data$Variable;
    if(!is.null(pajer$selected_nodes)){
      dfv <- dfv %>% filter(Node %in% pajer$selected_nodes)
    }

    loginfo("Starting the Starvz plot function");

    # Fail Checking
    if((pjr(pajer$pmtool$state$active) || pjr(pajer$pmtool$kiteration$active)) && is.null(data$Pmtool_states)){
      logwarn("Pmtool states config is active but the data is NULL")
      pajer$pmtool$state$active <<- FALSE;
      pajer$pmtool$kiteration$active <<- FALSE;
    }

    if(pjr(pajer$pmtool$bounds$active) && is.null(data$Pmtool)){
      logwarn("Pmtool bounds config is active but the data is NULL")
      pajer$pmtool$bounds$active <<- FALSE;
    }

    if((data$Memory_state %>% nrow) == 0 && pjr(pajer$memory$state$active) ){
      logwarn("There is not information about memory states")
      pajer$memory$state$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

    if(is.null(data$Link) && (pjr(pajer$memory$transfers$active) || pjr(pajer$memory$combined))) {
      logwarn("This dataset dont have links, disabling some options")
      pajer$memory$transfers$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

    dfevents = data$Memory_state

    if((dfevents %>% nrow) == 0 && ( pjr(pajer$memory$new_data) && (data$Events %>% nrow) == 0) ){
      logwarn("This dataset dont have memory node states")
      pajer$memory$state$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

    if(is.null(data$Atree) && (
      pjr(pajer$utiltreenode$active) ||
      pjr(pajer$utiltreedepth$active) ||
      pjr(pajer$atree$active) ||
      pjr(pajer$activenodes$active) ||
      pjr(pajer$computingnodes$active)
    )){
      logwarn("This dataset dont have atree, disabling some options")
      pajer$utiltreenode$active <<- FALSE;
      pajer$utiltreedepth$active <<- FALSE;
      pajer$atree$active <<- FALSE;
      pajer$activenodes$active <<- FALSE;
      pajer$computingnodes$active <<- FALSE;
    }

    # Define the global aggregation step as 0.1% of the total window
    globalAggStep = (tend - tstart) * .001;

    # Set all possible plots to NULL
    goatreet <- geom_blank();
    goutiltreenode <- geom_blank();
    goutiltreedepth <- geom_blank();
    gow <- geom_blank();
    go_sn <- geom_blank();
    gow_pm <- geom_blank();
    gow_mm <- geom_blank();
    gow_tf <- geom_blank();
    gstarpu <- geom_blank();
    goijk <- geom_blank();
    goijk_pm <- geom_blank();
    golrv <- geom_blank();
    gorv <- geom_blank();
    gosv <- geom_blank();
    gomov <- geom_blank();
    gompiconc <- geom_blank();
    gompiconcout <- geom_blank();
    gompistate <- geom_blank();
    gogov <- geom_blank();
    goguv <- geom_blank();
    gogfv <- geom_blank();
    imb_plot <- geom_blank();
    imb_plot_power <- geom_blank();
    imb_plot_hete <- geom_blank();
    heatmap <- geom_blank();
    goactivenodes <- geom_blank();
    gonodememuse <- geom_blank();
    gocomputingnodes <- geom_blank();

    # Atree space/time view
    if (!is.null(data$Atree) && pjr(pajer$atree$active)){

        # Reorganize tree Position
        data_reorder <- data$Application %>%
          filter(grepl("lapack", Value)) %>%
          select(ANode, SubmitOrder) %>% unique() %>%
          group_by(ANode) %>%
          mutate(SubmitOrder = as.integer(SubmitOrder)) %>%
          arrange(SubmitOrder) %>%
          slice(1) %>%
          ungroup() %>%
          arrange(SubmitOrder) %>%
          mutate(Position = 1:n(), Height=1) %>%
          select(-SubmitOrder);

        data$Atree <- data$Atree %>%
              # Replace Position and Height by new ordering
              select(-Position, -Height) %>%
              left_join(data_reorder, by="ANode")

        loginfo("Creating the temporal atree plot");
        aggStep <- pjr_value(pajer$atree$step, globalAggStep);
        goatreet <- atree_temporal_chart(data, step=aggStep) + tScale;
        if (!pjr(pajer$atree$legend)){
            goatreet <- goatreet + theme(legend.position="none");
        }else{
            goatreet <- goatreet + theme(legend.position = "top")
        }
        # Vertical zoom
        z.start <- pjr_value(pajer$atree$zoom$start, 0)
        z.end <- pjr_value(pajer$atree$zoom$end, 100)
        max.y.coordinate <- (data$Atree %>% pull(Position) %>% max) +
            (data$Atree %>% pull(Height) %>% max)
        tzScale <- list(
            coord_cartesian(xlim=c(tstart, tend),
                            ylim=c(z.start/100 * max.y.coordinate,
                                   z.end/100 * max.y.coordinate))
        );
        goatreet <- goatreet + tzScale
    }

    # Resource utilization by tree node
    if (!is.null(data$Atree) && pjr(pajer$utiltreenode$active)){
        loginfo("Creating the resource utilization by node plot");
        aggStep <- pjr_value(pajer$utiltreenode$step, globalAggStep);
        resource_utilization_tree_node_plot(data=data, step=aggStep) + tScale -> goutiltreenode;
        if (!pjr(pajer$utiltreenode$legend)){
            goutiltreenode <- goutiltreenode + theme(legend.position="none");
        }else{
            goutiltreenode <- goutiltreenode + theme(legend.position = "top")
        }
    }

    # Resource utilization by tree depth
    if (!is.null(data$Atree) && pjr(pajer$utiltreedepth$active)){
        loginfo("Creating the resource utilization by depth plot");
        aggStep <- pjr_value(pajer$utiltreenode$step, globalAggStep);
        resource_utilization_tree_depth_plot(data, step=aggStep) + tScale -> goutiltreedepth;
        if (!pjr(pajer$utiltreedepth$legend)){
            goutiltreedepth <- goutiltreedepth + theme(legend.position="none");
        }else{
            goutiltreedepth <- goutiltreedepth + theme(legend.position = "top")
        }
    }

    # SpaceTime
    if (pjr(pajer$st$active)){
        loginfo("Creating the Space/Time");
        if (pjr(pajer$st$aggregation$active)){
            if (pjr_value(pajer$st$aggregation$method, "lucas") == "lucas"){
                aggStep <- pjr_value(pajer$st$aggregation$step, globalAggStep);
                dfw_agg <- st_time_aggregation(data$Application, step=aggStep);
                data %>% st_time_aggregation_plot (dfw_agg) + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, NA)) -> gow;
            }else if(pjr_value(pajer$st$aggregation$method, "lucas") == "vinicius"){
                loginfo("Call vinicius aggregation");
                data %>% st_time_aggregation_vinicius_plot() + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, NA)) -> gow;
            }else if(pjr_value(pajer$st$aggregation$method, "lucas") == "nodes"){
                loginfo("Call Node aggregation");
                node_aggregation(data) + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, NA)) -> gow;
            }
        }else{
            data %>% state_chart (globalEndTime = tend, ST.Outliers = pjr(pajer$st$outliers), StarPU.View = FALSE) +
                coord_cartesian(xlim=c(tstart, tend), ylim=c(0, NA)) -> gow;
        }

        # Without legend
        if (!pjr(pajer$st$legend)){
            gow <- gow + theme(legend.position="none");
        }
    }

    if (pjr(pajer$summary_nodes$active)){
        loginfo("Creating node summary");
        node_summary(data$Application) + tScale -> go_sn
        if (!pjr(pajer$summary_nodes$legend)){
            go_sn <- go_sn + theme(legend.position="none");
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
          dfw_agg <- st_time_aggregation(data$Starpu, StarPU.View=TRUE, step=aggStep);
          data %>% st_time_aggregation_plot (dfw_agg, StarPU.View=TRUE) + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, NA)) -> gstarpu;
        }else{
          data %>% state_chart (globalEndTime = tend, StarPU.View = TRUE) + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, NA)) -> gstarpu;
        }
        if (!pjr(pajer$starpu$legend)){
            gstarpu <- gstarpu + theme(legend.position="none");
        }
    }

    # KIteration
    if (pjr(pajer$kiteration$active)){
        loginfo("Creating the KIteration");
        ml <- pajer$kiteration$middlelines
        if(length(ml) == 0){
            ml <- NULL
        }
        pn <- pjr_value(pajer$kiteration$pernode, FALSE)
        goijk <- k_chart(data$Application,
                         middle_lines=ml,
                         per_node=pn, colors=data$Colors) + tScale;

        if (!pjr(pajer$kiteration$legend)){
            goijk <- goijk +
                     theme(legend.position="none")
        }else{
             goijk <- goijk +
                      theme(legend.spacing.x = unit(0.2, 'cm'))
        }
        if (pn == TRUE){
            goijk <- goijk + facet_wrap(~Node, ncol=1)
        }
    }

    # KIteration PMTOOL
    if (pjr(pajer$pmtool$kiteration$active)){
        loginfo("Creating the KIteration for PMTool");
        goijk_pm <- k_chart_pmtool(data$Pmtool_states, colors=data$Colors) + tScale;

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
                theme(legend.position = "top")
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
                theme(legend.position = "top")
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

        if ((dfv %>% filter(grepl("Used", Type)) %>% nrow) == 0){
            logwarn("There aren't any information about Used Memory, ignoring it.");
            pajer$usedmemory$active <<- FALSE;
        }else{
          aggStep <- pjr_value(pajer$usedmemory$step, globalAggStep);

          goguv <- dfv %>%
              filter(grepl("Used", Type)) %>%
              var_integration_segment_chart(step = aggStep) + tScale;
          if (!pjr(pajer$usedmemory$legend)){
              goguv <- goguv + theme(legend.position="none");
          }else{
              goguv <- goguv +
                  theme(legend.position = "top") #+
                  #guides(color = guide_legend(nrow = 1))
          }
          goguv <- userYLimit(goguv, pajer$usedmemory$limit, c(tstart, tend));
        }
    }


    # Imbalance Metrics
    if (pjr(pajer$imbalance$active)){
        loginfo("Creating the Imbalance Metrics plot");

        Step <- as.double(pjr_value(pajer$imbalance$step, globalAggStep));

        imb_plot <- data$Application %>% filter(Start>=0) %>% var_imbalance(Step)
        if (!pjr(pajer$imbalance$legend)){
            imb_plot <- imb_plot + theme(legend.position="none");
        }else{
            imb_plot <- imb_plot + theme(legend.position = "top")
        }
        imb_plot <- userYLimit(imb_plot, pajer$imbalance$limit, c(tstart, tend));
    }

    # Imbalance Metrics Power
    if (pjr(pajer$power_imbalance$active)){
        loginfo("Creating the Imbalance Power Metrics plot");

        Step <- as.double(pjr_value(pajer$power_imbalance$step, globalAggStep));

        imb_plot_power <- data$Application %>% filter(Start>=0) %>% var_imbalance_power(Step)
        if (!pjr(pajer$power_imbalance$legend)){
            imb_plot_power <- imb_plot_power + theme(legend.position="none");
        }else{
            imb_plot_power <- imb_plot_power + theme(legend.position = "top")
        }
        imb_plot_power <- userYLimit(imb_plot_power, pajer$power_imbalance$limit, c(tstart, tend));
    }

    # Imbalance Metrics hete
    if (pjr(pajer$hete_imbalance$active)){
        loginfo("Creating the Imbalance Hetero Metrics plot");

        Step <- as.double(pjr_value(pajer$hete_imbalance$step, globalAggStep));

        imb_plot_hete <- data$Application %>% filter(Start>=0) %>% var_imbalance_double_hetero(Step)
        if (!pjr(pajer$hete_imbalance$legend)){
            imb_plot_hete <- imb_plot_hete + theme(legend.position="none");
        }else{
            imb_plot_hete <- imb_plot_hete + theme(legend.position = "top")
        }
        imb_plot_hete <- userYLimit(imb_plot_hete, pajer$hete_imbalance$limit, c(tstart, tend));
    }

    if (pjr(pajer$utilheatmap$active)){
        loginfo("Creating the HeatMap Imbalance plot");

        Step <- as.double(pjr_value(pajer$utilheatmap$step, globalAggStep));

        heatmap <- data$Application %>% filter(Start>=0) %>% utilization_heatmap(data$Y, Step)
        if (!pjr(pajer$utilheatmap$legend)){
            heatmap <- heatmap + theme(legend.position="none");
        }else{
            heatmap <- heatmap + theme(legend.position = "top")
        }
        heatmap <- userYLimit(heatmap, pajer$utilheatmap$limit, c(tstart, tend));
    }

    # MPIBandwidth
    if (pjr(pajer$mpibandwidth$active)){
        loginfo("Creating the MPIBandwidth plot");
        aggStep <- pjr_value(pajer$mpibandwidth$step, globalAggStep);
        mpi_out <- dfv %>% filter(grepl("mpict", ResourceId), grepl("Out", Type));
        if ((mpi_out %>% nrow) == 0){
            logwarn("There aren't any information on MPIBandwidth, ignoring it.");
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
        if ((data$Link %>% filter(grepl("mpicom", Key)) %>% nrow) == 0){
            logwarn("There aren't any information on MPI, ignoring it.");
            pajer$mpiconcurrent$active <<- FALSE;
        }else{
          aggStep <- pjr_value(pajer$mpiconcurrent$step, globalAggStep);
          gompiconc <- data %>% concurrent_mpi() %>%
              var_integration_segment_chart(., ylabel="Concurrent\nMPI Tasks Send", step=aggStep) + tScale;
          if (!pjr(pajer$mpiconcurrent$legend)){
              gompiconc <- gompiconc + theme(legend.position="none");
          }
          gompiconc <- userYLimit(gompiconc, pajer$mpiconcurrent$limit, c(tstart, tend));
        }
    }

    # MPI Concurrent Out
    if (pjr(pajer$mpiconcurrentout$active)){
        loginfo("Creating the MPI concurrent ops plot");
        if ((data$Link %>% filter(grepl("mpicom", Key)) %>% nrow) == 0){
            logwarn("There aren't any information on MPI, ignoring it.");
            pajer$mpiconcurrentout$active <<- FALSE;
        }else{
          aggStep <- pjr_value(pajer$mpiconcurrentout$step, globalAggStep);
          gompiconcout <- data %>% concurrent_mpi_out() %>%
              var_integration_segment_chart(., ylabel="Concurrent\nMPI Tasks Recv", step=aggStep) + tScale;
          if (!pjr(pajer$mpiconcurrentout$legend)){
              gompiconcout <- gompiconcout + theme(legend.position="none");
          }
          gompiconcout <- userYLimit(gompiconcout, pajer$mpiconcurrentout$limit, c(tstart, tend));
        }
    }

    # MPI State
    if (pjr(pajer$mpistate$active)){
        loginfo("Creating the MPI state");
        if ( is.null(data$Comm_state) || (data$Comm_state %>% nrow) == 0 ){
          logwarn("There aren't any information on MPI, ignoring it.");
          pajer$mpistate$active <<- FALSE;
        }else{
          gompistate <- data$Comm_state %>% state_mpi_chart() + tScale;
          if (!pjr(pajer$mpistate$legend)){
              gompistate <- gompistate + theme(legend.position="none");
          }
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

    # Active Nodes
    if (pjr(pajer$activenodes$active)){
      loginfo("Creating the Active Nodes plot");

      if ( (data$Application %>% filter(!is.na(ANode)) %>% nrow) == 0 ){
        logwarn("There aren't any information on ANode, ignoring it.");
        pajer$activenodes$active <<- FALSE;
      }else{
        goactivenodes <- data$Application %>% active_nodes_chart() + tScale;
        if (!pjr(pajer$activenodes$legend)){
          goactivenodes <- goactivenodes + theme(legend.position="none");
        }else{
          goactivenodes <- goactivenodes + theme(legend.position = "top")
        }
      }
    }
    # Node memory usage
    if (pjr(pajer$activenodes$nodememuse$active)) {
      loginfo("Creating the Node Memory Usage plot");

      if ( (data$Application %>% filter(grepl("front", Value) & GFlop != 0) %>% nrow) == 0 ){
        logwarn("There is no memory information on data, ignoring it.");
        pajer$activenodes$nodememuse$active <<- FALSE;
      }else{
        gonodememuse <- nodes_memory_usage_plot(data=data) + tScale;
      }
    }

    # Computing Nodes
    if (pjr(pajer$computingnodes$active)){
        loginfo("Creating the Computing Nodes plot");
        aggStep <- pjr_value(pajer$computingnodes$step, globalAggStep);
        if ( (data$Application %>% filter(!is.na(ANode)) %>% nrow) == 0 ) {
          logwarn("There aren't any information on ANode, ignoring it.");
          pajer$computingnodes$active <<- FALSE;
        }else{
          gocomputingnodes <- computing_nodes_chart(data=data, step=aggStep) + tScale;
          loginfo("Exit of computing_nodes_chart");
        }
    }

    # Create a named list with the ggplot objects + title
    plot_list <- list(
        atree = goatreet,
        utiltreenode = goutiltreenode,
        utiltreedepth = goutiltreedepth,
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
        mpiconcout = gompiconcout,
        mpistate = gompistate,
        gpu = gogov,
        memory = goguv,
        imb_plot = imb_plot,
        imb_plot_power = imb_plot_power,
        imb_plot_hete = imb_plot_hete,
        heatmap = heatmap,
        gflops = gogfv,
        activenodes = goactivenodes,
        nodememuse = gonodememuse,
        computingnodes = gocomputingnodes,
        summary_nodes = go_sn,
        title = directory
    )
    return (plot_list);
}

starvz_plot <- function(data = NULL)
{
    if(is.null(data)) return(NULL);
    if(is.null(pajer)) return(NULL);

    plist <- starvz_plot_list(data)

    loginfo("Assembling the plot");

    # assembling
    g <- starpu_mpi_grid_arrange(plist);

    loginfo("Ending Starvz plot function");

    return(g);
}

# Keep the old name
the_master_function <- starvz_plot
