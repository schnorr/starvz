#' @include starvz_data.R

starvz_compute_plot_heights <- function(plist, config) {
  # The list that will contain the plots
  P <- list()
  # The list that will contain the proportinal height of each plot
  H <- list()

  # Just make this homogeneous across package
  data <- list()
  data$config <- config

  # Letsset default height values
  # This is also experiment computed on starvz_guided_plot
  # For plots that show resources
  if (is.null(data$config$guided$starvz_height_resources)) {
    data$config$guided$starvz_height_resources <- 4
  }
  # For plots that show nodes 10px per node
  if (is.null(data$config$guided$starvz_height_nodes)) {
    data$config$guided$starvz_height_nodes <- 2
  }
  # For plots that show nodes 10px per node
  if (is.null(data$config$guided$starvz_height_agg)) {
    data$config$guided$starvz_height_agg <- 2
  }
  # For variable plots
  if (is.null(data$config$guided$starvz_height_var)) {
    data$config$guided$starvz_height_var <- 2
  }
  # For the tree, default 1.5px per tree node Position
  if (is.null(data$config$guided$starvz_height_atree)) {
    data$config$guided$starvz_height_atree <- 4
  }
  # For plots TODO
  if (is.null(data$config$guided$starvz_height_todo)) {
    data$config$guided$starvz_height_todo <- 4
  }

  # For plots Small
  if (is.null(data$config$guided$starvz_height_small)) {
    data$config$guided$starvz_height_small <- 0.05
  }

  # Prepare title
  if (data$config$title$active) {
    P[[length(P) + 1]] <- plist$tplot
    H[[length(H) + 1]] <- config_value(data$config$title$height, 0.3)
  }

  if (data$config$atree$active) {
    P[[length(P) + 1]] <- plist$atree
    H[[length(H) + 1]] <- config_value(data$config$atree$height, data$config$guided$starvz_height_atree)
  }
  if (data$config$utiltreenode$active) {
    P[[length(P) + 1]] <- plist$utiltreenode
    H[[length(H) + 1]] <- config_value(data$config$utiltreenode$height, data$config$guided$starvz_height_var)
  }
  if (data$config$utiltreedepth$active) {
    P[[length(P) + 1]] <- plist$utiltreedepth
    H[[length(H) + 1]] <- config_value(data$config$utiltreedepth$height, data$config$guided$starvz_height_var)
  }
  if (data$config$activenodes$active) {
    P[[length(P) + 1]] <- plist$activenodes
    H[[length(H) + 1]] <- config_value(data$config$activenodes$height, data$config$guided$starvz_height_var)
  }
  if (data$config$activenodes$nodememuse$active) {
    P[[length(P) + 1]] <- plist$nodememuse
    H[[length(H) + 1]] <- config_value(data$config$activenodes$nodememuse$height, data$config$guided$starvz_height_var)
  }
  if (data$config$kiteration$active) {
    P[[length(P) + 1]] <- plist$ijk
    H[[length(H) + 1]] <- config_value(data$config$kiteration$height, data$config$guided$starvz_height_todo)
  }
  if (data$config$summary_nodes$active) {
    P[[length(P) + 1]] <- plist$summary_nodes
    H[[length(H) + 1]] <- config_value(data$config$summary_nodes$height, data$config$guided$starvz_height_nodes)
  }
  if (data$config$st$active) {
    P[[length(P) + 1]] <- plist$st
    if (data$config$st$aggregation$active && data$config$st$aggregation$method == "nodes") {
      H[[length(H) + 1]] <- config_value(data$config$st$height, data$config$guided$starvz_height_agg)
    } else {
      H[[length(H) + 1]] <- config_value(data$config$st$height, data$config$guided$starvz_height_resources)
    }
  }
  if (data$config$pmtool$kiteration$active) {
    P[[length(P) + 1]] <- plist$ijk_pm
    H[[length(H) + 1]] <- config_value(data$config$pmtool$kiteration$height, data$config$guided$starvz_height_todo)
  }
  if (data$config$pmtool$state$active) {
    P[[length(P) + 1]] <- plist$st_pm
    H[[length(H) + 1]] <- config_value(data$config$pmtool$state$height, data$config$guided$starvz_height_resources)
  }
  if (data$config$memory$state$active) {
    P[[length(P) + 1]] <- plist$st_mm
    H[[length(H) + 1]] <- config_value(data$config$memory$state$height, data$config$guided$starvz_height_nodes)
  }
  if (data$config$memory$transfers$active && !data$config$memory$combined) {
    P[[length(P) + 1]] <- plist$transf
    H[[length(H) + 1]] <- config_value(data$config$memory$transfers$height, data$config$guided$starvz_height_nodes)
  }
  if (data$config$submitted$active) {
    P[[length(P) + 1]] <- plist$submitted
    H[[length(H) + 1]] <- config_value(data$config$submitted$height, data$config$guided$starvz_height_var)
  }
  if (data$config$starpu$active) {
    P[[length(P) + 1]] <- plist$starpu
    H[[length(H) + 1]] <- config_value(data$config$starpu$height, data$config$guided$starvz_height_resources)
  }
  if (data$config$ready$active) {
    P[[length(P) + 1]] <- plist$ready
    H[[length(H) + 1]] <- config_value(data$config$ready$height, data$config$guided$starvz_height_var)
  }
  if (data$config$lackready$active) {
    P[[length(P) + 1]] <- plist$lackready
    H[[length(H) + 1]] <- config_value(data$config$lackready$height, data$config$guided$starvz_height_small)
  }
  if (data$config$gflops$active) {
    P[[length(P) + 1]] <- plist$gflops
    H[[length(H) + 1]] <- config_value(data$config$gflops$height, data$config$guided$starvz_height_var)
  }
  if (data$config$usedmemory$active) {
    P[[length(P) + 1]] <- plist$memory
    H[[length(H) + 1]] <- config_value(data$config$usedmemory$height, data$config$guided$starvz_height_var)
  }
  if (data$config$imbalance$active) {
    P[[length(P) + 1]] <- plist$imb_plot
    H[[length(H) + 1]] <- config_value(data$config$imbalance$height, data$config$guided$starvz_height_var)
  }
  if (data$config$power_imbalance$active) {
    P[[length(P) + 1]] <- plist$imb_plot_power
    H[[length(H) + 1]] <- config_value(data$config$power_imbalance$height, data$config$guided$starvz_height_var)
  }
  if (data$config$hete_imbalance$active) {
    P[[length(P) + 1]] <- plist$imb_plot_hete
    H[[length(H) + 1]] <- config_value(data$config$hete_imbalance$height, data$config$guided$starvz_height_var)
  }
  if (data$config$utilheatmap$active) {
    P[[length(P) + 1]] <- plist$heatmap
    H[[length(H) + 1]] <- config_value(data$config$utilheatmap$height, data$config$guided$starvz_height_resources)
  }
  if (data$config$gpubandwidth$active) {
    P[[length(P) + 1]] <- plist$gpu
    H[[length(H) + 1]] <- config_value(data$config$gpubandwidth$height, data$config$guided$starvz_height_var)
  }
  if (data$config$mpibandwidth$active) {
    P[[length(P) + 1]] <- plist$mpi
    H[[length(H) + 1]] <- config_value(data$config$mpibandwidth$height, data$config$guided$starvz_height_var)
  }
  if (data$config$mpiconcurrent$active) {
    P[[length(P) + 1]] <- plist$mpiconc
    H[[length(H) + 1]] <- config_value(data$config$mpiconcurrent$height, data$config$guided$starvz_height_var)
  }
  if (data$config$mpiconcurrentout$active) {
    P[[length(P) + 1]] <- plist$mpiconcout
    H[[length(H) + 1]] <- config_value(data$config$mpiconcurrentout$height, data$config$guided$starvz_height_var)
  }
  if (data$config$mpistate$active) {
    P[[length(P) + 1]] <- plist$mpistate
    H[[length(H) + 1]] <- config_value(data$config$mpistate$height, data$config$guided$starvz_height_nodes)
  }

  starvz_height_total <- sum(unlist(H))

  return(list(P = P, H = H, starvz_height_total = starvz_height_total))
}

#' Assemble multiple StarVZ panel lists
#'
#' Take a panel list, or a list of list of panels and assemble it
#'
#' @param ... Panel list or list of panel lists
#' @param config StarVZ configurations for determaning panels heights
#' @param remove_Y_info remove Y labels for second and subsequent list of panels
#' @param remove_legends remove legends for second and subsequent list of panels
#' @return The ggplot plot
#' @examples
#' # starvz_assemble(TODO)
#' @export
starvz_assemble <- function(..., config = NULL, remove_Y_info = TRUE, remove_legends = TRUE) {
  plists <- list()
  for (i in list(...)) {

    # Check if arguments are a list or a plot
    # is.list is not adequated here
    if (isTRUE(class(i[[1]]) == "list")) {
      plists <- append(plists, i)
    } else if (is(i[[1]], "gg")) {
      plists <- append(plists, list(i))
    } else {
      logerror("starvz_assemble needs a list of plots or a list of list of plots")
      return(NULL)
    }
  }

  if(is.null(config)){
    logerror("config is null")
    stop()
  }

  number_plots <- length(plists)

  loginfo(paste0("Assembling ", number_plots, " groups of plots"))

  AllP <- list()
  AllH <- list()

  # Clean X
  emptyx <- theme(axis.text.x = element_blank(), axis.title.x = element_blank())

  # Return X
  notemptyx <- theme(axis.text.x = element_text(), axis.title.x = element_text())

  # Remove Legends
  nlegend <- theme(legend.position = "none")

  # Remove Y
  noy <- theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

  first_plot <- TRUE

  starvz_height_total <- 0

  for (plot_list in plists) {
    # Check if its a group of plots
    # TODO: check is weak but will protect for many errors
    if (!is(plot_list[[1]], "gg")) {
      logerror("starvz_assemble element is not a plot list")
      return(NULL)
    }

    # Computer Heights and sequence
    elem <- starvz_compute_plot_heights(plot_list, config)

    # Get size if bigger
    starvz_height_total <- max(starvz_height_total, elem$starvz_height_total)

    # Clean all X
    elem$P <- lapply(elem$P, function(p) {
      p <- p + emptyx
    })

    # But not last
    last <- length(elem$P)
    elem$P[[last]] <- elem$P[[last]] + notemptyx

    # Remove Y of second group of plots
    if (!first_plot && remove_Y_info) {
      elem$P <- lapply(elem$P, function(p) {
        p <- p + noy
      })
    }

    # Remove legend of second group of plots
    if (!first_plot && remove_legends) {
      elem$P <- lapply(elem$P, function(p) {
        p <- p + nlegend
      })
    }

    # Reassembly
    AllP <- append(AllP, elem$P)

    # Only one H can be consider, consider First one
    if (first_plot) {
      AllH <- elem$H
    }

    first_plot <- FALSE
  }

  final_plot <- wrap_plots(AllP, heights = AllH, ncol = number_plots, byrow = FALSE)
  attr(final_plot, "starvz_height_total") <- starvz_height_total
  return(final_plot)
}

#' Create a StarVZ plot automaticaly computing height
#'
#' Use data to create a starvz plot and save in name
#'
#' @param data starvz_data with trace data
#' @param name Path for saved image
#' @return The ggplot plot
#' @examples
#' # starvz_guided_plot("data/", "file.png")
#' @export
starvz_guided_plot <- function(data, name) {
  # USe Y to compute state and starpu size
  # Get number of nodes

  # Lets compute some default height values
  # For plots that show resources, 10px per resource on Y
  # For plots that show nodes 10px per node
  # For variable plots, 100px
  # For atree plot 1.5px per node Position
  # For plots TODO, default 200px
  if (!is.null(data$config$selected_nodes)) {
    nodes <- length(data$config$selected_nodes)
  } else if (!is.null(data$Tasks)) {
    nodes <- data$Tasks %>%
      select(.data$MPIRank) %>%
      distinct() %>%
      nrow()
  } else if (!is.null(data$Application)) {
    nodes <- data$Application %>%
      select(.data$Node) %>%
      distinct() %>%
      nrow()
  } else {
    nodes <- 1
  }

  types <- data$Application %>%
    select(.data$ResourceType) %>%
    distinct() %>%
    nrow()

  if (!is.null(data$config$selected_nodes)) {
    data$Y %>%
      separate(.data$Parent, into = c("Node"), remove = FALSE) %>%
      filter(.data$Node %in% data$config$selected_nodes) %>%
      arrange(.data$Position) %>%
      mutate(New = cumsum(lag(.data$Height, default = 0))) %>%
      select(.data$Parent, .data$New) -> new_y
    data$config$guided$starvz_height_resources <- (new_y$New %>% max()) * 10 / 100
  } else {
    data$config$guided$starvz_height_resources <- (data$Y$Position %>% max()) * 10 / 100
  }

  data$config$guided$starvz_height_agg <- max(nodes * types * data$config$guided$agg_type_height / 100, 1)
  data$config$guided$starvz_height_nodes <- max(nodes * data$config$guided$node_height / 100, 1)
  data$config$guided$starvz_height_small <- 0.5
  if (!is.null(data$Atree)) {
    data$config$guided$starvz_height_atree <- ((data$Atree$Position %>% max()) * 1.5) / 100
  } else {
    data$config$guided$starvz_height_atree <- 1
  }

  p <- starvz_plot(data)

  total_dpi <- 120
  starvz_height_total <- attr(p, "starvz_height_total")
  if (is.null(starvz_height_total) || starvz_height_total == 0) {
    logwarn("Total height is 0, set to 1000")
    starvz_height_total <- 1000
  }

  final_px_height <- (starvz_height_total + 5) * 100
  final_px_width <- 1000
  final_in_height <- final_px_height / total_dpi
  final_in_width <- final_px_width / total_dpi

  ggsave(name, plot = p, width = final_in_width, height = final_in_height, units = "in", dpi = total_dpi, limitsize = FALSE)
  return(list(plot = p, height = final_px_height))
}

#' Generate the StarVZ Plots
#'
#' Use data to create the list of StarVZ plots
#'
#' @param data starvz_data with trace data
#' @return A list of ggplot plots
#' @examples
#' # starvz_plot_list(data)
#' @export
starvz_plot_list <- function(data = NULL) {
  if (is.null(data)) stop("data passed as parameter is null")

  # Activate logs
  if (!data$config$log) {
    removeHandler(writeToConsole)
  }

  if (is.null(data$Version)) {
    logwarn("This is a old StarVZ trace, trying to be retrocompatible")
    data$Application <- data$State %>% filter(.data$Application)
    data$Application <- data$Application %>% mutate(Size = as.integer(.data$Size))
    data$Starpu <- data$State %>%
      filter(.data$Type == "Worker State", .data$Application == FALSE) %>%
      mutate(Size = as.integer(.data$Size))
    data$Comm_state <- data$State %>%
      filter(.data$Type == "Communication Thread State") %>%
      select(-.data$Position, -.data$Height)
    data$Memory_state <- data$State %>%
      filter(.data$Type == "Memory Node State") %>%
      select(-.data$Position)
    data$Colors <- data$State %>%
      filter(.data$Application) %>%
      select(.data$Value, .data$Color) %>%
      distinct()
  }

  # Get data
  directory <- data$Origin

  if (is.null(data$Application)) {
    stop("The Application data was not loaded, check if the feather files exists.")
  }

  # Define makespan
  makespan <- data$Application %>%
    pull(.data$End) %>%
    max()

  #  Filter out everything after the makespan
  # TODO: Maybe this will make sense to transfer to Phase1
  # data$Application <- data$Application %>% filter(Start < makespan)
  # data$Starpu <- data$Starpu %>% filter(Start < makespan)
  # data$Dag <- data$Dag %>% filter(Start < makespan)
  # data$Events <- data$Events %>% filter(Start < makespan)
  # data$Gaps <- data$Gaps %>% filter(Start.x < makespan)
  # data$Link <- data$Link %>% filter(Start < makespan)
  # data$Variable <- data$Variable %>% filter(End < makespan)

  # Adjust temporal scale
  if(is.null(data$config$limits$start)){
    data$config$limits$start <- data$Application %>% pull(.data$Start) %>% min()
  }
  if(is.null(data$config$limits$end)){
    data$config$limits$end <- data$Application %>% pull(.data$End) %>% max() + 1 # Just to give space
  }

  # Define the global aggregation step as 0.1% of the total window
  data$config$global_agg_step <- (data$config$limits$end - data$config$limits$start) * .001

  # To be deprecated
  tstart <- data$config$limits$start
  tend <- data$config$limits$end
  globalAggStep <- data$config$global_agg_step
  tScale <- list(
    coord_cartesian(xlim = c(tstart, tend))
  )

  if (!is.null(data$config$selected_nodes)) {
    data$Variable <- data$Variable %>% filter(.data$Node %in% data$config$selected_nodes)
  }

  loginfo("Starting the Starvz plot function")

  # Fail Checking
  if ((data$config$pmtool$state$active || data$config$pmtool$kiteration$active) && is.null(data$Pmtool_states)) {
    logwarn("Pmtool states config is active but the data is NULL")
    data$config$pmtool$state$active <<- FALSE
    data$config$pmtool$kiteration$active <<- FALSE
  }

  if (data$config$pmtool$bounds$active && is.null(data$Pmtool)) {
    logwarn("Pmtool bounds config is active but the data is NULL")
    data$config$pmtool$bounds$active <<- FALSE
  }

  if ((data$Memory_state %>% nrow()) == 0 && data$config$memory$state$active) {
    logwarn("There is not information about memory states")
    data$config$memory$state$active <<- FALSE
    data$config$memory$combined <<- FALSE
  }

  if (is.null(data$Link) && (data$config$memory$transfers$active || data$config$memory$combined)) {
    logwarn("This dataset dont have links, disabling some options")
    data$config$memory$transfers$active <<- FALSE
    data$config$memory$combined <<- FALSE
  }

  dfevents <- data$Memory_state
  if (!is.null(dfevents) && ((dfevents %>% nrow()) == 0) && data$config$memory$new_data) {
    logwarn("This dataset dont have memory node states")
    data$config$memory$state$active <<- FALSE
    data$config$memory$combined <<- FALSE
  }

  if (is.null(data$Atree) && (
    data$config$utiltreenode$active ||
      data$config$utiltreedepth$active ||
      data$config$atree$active ||
      data$config$activenodes$active
  )) {
    logwarn("This dataset dont have atree, disabling some options")
    data$config$utiltreenode$active <<- FALSE
    data$config$utiltreedepth$active <<- FALSE
    data$config$atree$active <<- FALSE
    data$config$activenodes$active <<- FALSE
  }

  # Set all possible plots to NULL
  goatreet <- geom_blank()
  goutiltreenode <- geom_blank()
  goutiltreedepth <- geom_blank()
  gow <- geom_blank()
  go_sn <- geom_blank()
  gow_pm <- geom_blank()
  gow_mm <- geom_blank()
  gow_tf <- geom_blank()
  gstarpu <- geom_blank()
  goijk <- geom_blank()
  goijk_pm <- geom_blank()
  golrv <- geom_blank()
  gorv <- geom_blank()
  gosv <- geom_blank()
  gomov <- geom_blank()
  gompiconc <- geom_blank()
  gompiconcout <- geom_blank()
  gompistate <- geom_blank()
  gogov <- geom_blank()
  goguv <- geom_blank()
  gogfv <- geom_blank()
  imb_plot <- geom_blank()
  imb_plot_power <- geom_blank()
  imb_plot_hete <- geom_blank()
  heatmap <- geom_blank()
  goactivenodes <- geom_blank()
  gonodememuse <- geom_blank()
  tplot <- geom_blank()

  # Atree space/time view
  if (!is.null(data$Atree) && data$config$atree$active) {
    loginfo("Creating the temporal atree plot")
    # get default values for parameters
    aggStep <- config_value(data$config$atree$step, globalAggStep)

    legend <- data$config$atree$legend
    computation <- data$config$atree$computation$active
    pruned <- data$config$atree$computation$pruned$active
    communication <- data$config$atree$communication$active
    initialization <- data$config$atree$initialization$active
    anomalies <- data$config$atree$anomalies$active

    goatreet <- panel_atree(data=data, step=aggStep, legend=legend, zoom=FALSE,
      computation=computation, pruned=pruned, communication=communication,
      initialization=initialization, anomalies=anomalies) + tScale
  }

  # Resource utilization by tree node
  if (!is.null(data$Atree) && data$config$utiltreenode$active) {
    loginfo("Creating the resource utilization by node plot")
    aggStep <- config_value(data$config$utiltreenode$step, globalAggStep)
    goutiltreenode <- panel_utiltreenode(data=data, step=aggStep) + tScale
  }

  # Resource utilization by tree depth
  if (!is.null(data$Atree) && data$config$utiltreedepth$active) {
    loginfo("Creating the resource utilization by depth plot")
    aggStep <- config_value(data$config$utiltreenode$step, globalAggStep)
    goutiltreedepth <- panel_utiltreedepth(data=data, step=aggStep,
      legend=data$config$utiltreedepth$legend) + tScale
  }

  # SpaceTime
  if (data$config$st$active) {
    loginfo("Creating the Space/Time")


    if (data$config$st$aggregation$active) {
      if (data$config$st$aggregation$method == "lucas") {
        aggStep <- config_value(data$config$st$aggregation$step, globalAggStep)
        dfw_agg <- st_time_aggregation(data$Application, step = aggStep)
        data %>% st_time_aggregation_plot(dfw_agg) + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, NA)) -> gow
      } else if (data$config$st$aggregation$method == "vinicius") {
        loginfo("Call vinicius aggregation")
        data %>% st_time_aggregation_vinicius_plot() + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, NA)) -> gow
      } else if (data$config$st$aggregation$method == "nodes") {
        loginfo("Call Node aggregation")
        node_aggregation(data) + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, NA)) -> gow
      }
    } else {
      gow <- panel_st_raw(data=data, StarPU.View = FALSE) +
        coord_cartesian(xlim = c(tstart, tend), ylim = c(0, NA))
    }
  }

  if (data$config$summary_nodes$active) {
    loginfo("Creating node summary")
    panel_node_summary(data) + tScale -> go_sn
  }

  if (data$config$pmtool$state$active) {
    data %>% state_pmtool_chart() + tScale -> gow_pm
  }

  memory_combined <- data$config$memory$combined & data$config$memory$transfers$active

  if (data$config$memory$state$active) {
    data %>% events_memory_chart(combined = memory_combined, tstart = tstart, tend = tend) + tScale -> gow_mm
  }

  if (data$config$memory$transfers$active & !memory_combined) {
    data %>% link_chart(tstart = tstart, tend = tend) + tScale -> gow_tf
  }

  # StarPU SpaceTime
  if (data$config$starpu$active) {
    loginfo("Creating the StarPU Space/Time")
    if (data$config$starpu$aggregation$active) {
      loginfo("Will call st_time_aggregation")
      aggStep <- config_value(data$config$starpu$aggregation$step, globalAggStep)
      dfw_agg <- st_time_aggregation(data$Starpu, StarPU.View = TRUE, step = aggStep)
      data %>% st_time_aggregation_plot(dfw_agg, StarPU.View = TRUE) + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, NA)) -> gstarpu
    } else {
      gstarpu <- panel_st_raw(data=data, StarPU.View = TRUE) +
        coord_cartesian(xlim = c(tstart, tend), ylim = c(0, NA))
    }
  }

  # KIteration
  if (data$config$kiteration$active) {
    loginfo("Creating the KIteration")
    ml <- data$config$kiteration$middlelines
    if (length(ml) == 0) {
      ml <- NULL
    }
    pn <- data$config$kiteration$pernode
    goijk <- k_chart(data,
      middle_lines = ml,
      per_node = pn, colors = data$Colors
    ) + tScale

    if (!data$config$kiteration$legend) {
      goijk <- goijk +
        theme(legend.position = "none")
    } else {
      goijk <- goijk +
        theme(legend.spacing.x = unit(0.2, "cm"))
    }
    if (pn == TRUE) {
      goijk <- goijk + facet_wrap(~Node, ncol = 1)
    }
  }

  # KIteration PMTOOL
  if (data$config$pmtool$kiteration$active) {
    loginfo("Creating the KIteration for PMTool")
    goijk_pm <- k_chart_pmtool(data, colors = data$Colors) + tScale

    if (!data$config$pmtool$kiteration$legend) {
      goijk_pm <- goijk_pm + theme(legend.position = "none")
    }
  }

  # Lack ready (companion for Ready Variable)
  if (data$config$lackready$active) {
    loginfo("Creating the Lack Ready Plot")
    golrv <- panel_lackready(data) + tScale
  }

  # Ready
  if (data$config$ready$active) {
    loginfo("Creating the Ready plot")
    gorv <- panel_ready(data)
  }

  # Submitted
  if (data$config$submitted$active) {
    loginfo("Creating the Submitted plot")
    gosv <- panel_submitted(data)
  }

  # GFlops
  if (data$config$gflops$active) {
    loginfo("Creating the GFlops plot")
    aggStep <- config_value(data$config$gflops$step, globalAggStep)
    facetted <- data$config$gflops$facet
    gogfv <- data$Variable %>%
      filter(.data$Type == "GFlops") %>%
      var_integration_segment_chart(., ylabel = "GFlops", step = aggStep, facetting = facetted, base_size=data$config$base_size, expand=data$config$expand) + tScale

    # adjust GFlops scale
    if (data$config$gflops$limit) {
      limit <- data$config$gflops$limit
      gogfv <- gogfv + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, limit))
    }
    if (!data$config$gflops$legend) {
      gogfv <- gogfv + theme(legend.position = "none")
    }
    # TODO: user limit
  }

  # Used Memory
  if (data$config$usedmemory$active) {
    loginfo("Creating the Used Memory plot")
    goguv <- panel_usedmemory(data)
  }


  # Imbalance Metrics
  if (data$config$imbalance$active) {
    loginfo("Creating the Imbalance Metrics plot")

    Step <- as.double(config_value(data$config$imbalance$step, globalAggStep))

    imb_plot <- data %>% var_imbalance(Step)
    if (!data$config$imbalance$legend) {
      imb_plot <- imb_plot + theme(legend.position = "none")
    } else {
      imb_plot <- imb_plot + theme(legend.position = "top")
    }
    imb_plot <- imb_plot + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, data$config$imbalance$limit))
  }

  # Imbalance Metrics Power
  if (data$config$power_imbalance$active) {
    loginfo("Creating the Imbalance Power Metrics plot")

    Step <- as.double(config_value(data$config$power_imbalance$step, globalAggStep))

    imb_plot_power <- data %>% var_imbalance_power(Step)
    if (!data$config$power_imbalance$legend) {
      imb_plot_power <- imb_plot_power + theme(legend.position = "none")
    } else {
      imb_plot_power <- imb_plot_power + theme(legend.position = "top")
    }
    imb_plot_power <- imb_plot_power + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, data$config$power_imbalance$limit))
  }

  # Imbalance Metrics hete
  if (data$config$hete_imbalance$active) {
    loginfo("Creating the Imbalance Hetero Metrics plot")

    Step <- as.double(config_value(data$config$hete_imbalance$step, globalAggStep))

    imb_plot_hete <- data %>% var_imbalance_double_hetero(Step)
    if (!data$config$hete_imbalance$legend) {
      imb_plot_hete <- imb_plot_hete + theme(legend.position = "none")
    } else {
      imb_plot_hete <- imb_plot_hete + theme(legend.position = "top")
    }
    imb_plot_hete <- imb_plot_hete + coord_cartesian(xlim = c(tstart, tend), ylim = c(0, data$config$hete_imbalance$limit))
  }

  if (data$config$utilheatmap$active) {
    loginfo("Creating the HeatMap Imbalance plot")

    Step <- as.double(config_value(data$config$utilheatmap$step, globalAggStep))

    heatmap <- data %>%
      utilization_heatmap(data$Y, Step)
    if (!data$config$utilheatmap$legend) {
      heatmap <- heatmap + theme(legend.position = "none")
    } else {
      heatmap <- heatmap + theme(legend.position = "top")
    }
    heatmap <- heatmap + tScale
  }

  # MPIBandwidth
  if (data$config$mpibandwidth$active) {
    loginfo("Creating the MPIBandwidth plot")
    aggStep <- config_value(data$config$mpibandwidth$step, globalAggStep)
    mpi_out <- data$Variable %>% filter(grepl("mpict", .data$ResourceId), grepl("Out", .data$Type))
    if ((mpi_out %>% nrow()) == 0) {
      logwarn("There aren't any information on MPIBandwidth, ignoring it.")
      data$config$mpibandwidth$active <<- FALSE
    } else {
      gomov <- mpi_out %>%
        var_integration_segment_chart(., ylabel = "MPI\n(MB/s)", step = aggStep, base_size=data$config$base_size, expand=data$config$expand) + tScale
      if (!data$config$mpibandwidth$legend) {
        gomov <- gomov + theme(legend.position = "none")
      }
      gomov <- userYLimit(gomov, data$config$mpibandwidth$limit, c(tstart, tend))
    }
  }

  # MPI Concurrent
  if (data$config$mpiconcurrent$active) {
    loginfo("Creating the MPI concurrent ops plot")
    if ((data$Link %>% filter(grepl("mpicom", .data$Key)) %>% nrow()) == 0) {
      logwarn("There aren't any information on MPI, ignoring it.")
      data$config$mpiconcurrent$active <<- FALSE
    } else {
      aggStep <- config_value(data$config$mpiconcurrent$step, globalAggStep)
      gompiconc <- data %>%
        concurrent_mpi() %>%
        var_integration_segment_chart(., ylabel = "Concurrent\nMPI Tasks Send", step = aggStep, base_size=data$config$base_size, expand=data$config$expand) + tScale
      if (!data$config$mpiconcurrent$legend) {
        gompiconc <- gompiconc + theme(legend.position = "none")
      }
      gompiconc <- userYLimit(gompiconc, data$config$mpiconcurrent$limit, c(tstart, tend))
    }
  }

  # MPI Concurrent Out
  if (data$config$mpiconcurrentout$active) {
    loginfo("Creating the MPI concurrent ops out plot")
    if ((data$Link %>% filter(grepl("mpicom", .data$Key)) %>% nrow()) == 0) {
      logwarn("There aren't any information on MPI, ignoring it.")
      data$config$mpiconcurrentout$active <<- FALSE
    } else {
      aggStep <- config_value(data$config$mpiconcurrentout$step, globalAggStep)
      gompiconcout <- data %>%
        concurrent_mpi_out() %>%
        var_integration_segment_chart(., ylabel = "Concurrent\nMPI Tasks Recv", step = aggStep, base_size=data$config$base_size, expand=data$config$expand) + tScale
      if (!data$config$mpiconcurrentout$legend) {
        gompiconcout <- gompiconcout + theme(legend.position = "none")
      }
      gompiconcout <- userYLimit(gompiconcout, data$config$mpiconcurrentout$limit, c(tstart, tend))
    }
  }

  # MPI State
  if (data$config$mpistate$active) {
    loginfo("Creating the MPI state")
    if (is.null(data$Comm_state) || (data$Comm_state %>% nrow()) == 0) {
      logwarn("There aren't any information on MPI, ignoring it.")
      data$config$mpistate$active <<- FALSE
    } else {
      gompistate <- data %>% state_mpi_chart() + tScale
      if (!data$config$mpistate$legend) {
        gompistate <- gompistate + theme(legend.position = "none")
      }
    }
  }

  # GPUBandwidth
  if (data$config$gpubandwidth$active) {
    loginfo("Creating the GPU Bandwidth plot")
    aggStep <- config_value(data$config$gpubandwidth$step, globalAggStep)
    if (aggStep < 0) {
      gogov <- data$Variable %>%
        # Get only GPU memory banwidth (out)
        filter(grepl("MEMMANAGER", .data$ResourceId), grepl("Out", .data$Type)) %>%
        # Remove the MANAGER0, which is CPU-only
        # TODO: After the logical OR there is a support for single-node StarPU traces
        filter(!grepl("MANAGER0", .data$Resource)) %>%
        group_by(.data$Type, .data$Node, .data$ResourceType, .data$Start, .data$End, .data$Duration) %>%
        summarize(Value = sum(.data$Value), N = n()) %>%
        rename(ResourceId = .data$Node) %>%
        var_chart(ylabel = "GPU\n(MB/s)", data$config$base_size, data$config$expand) + tScale
    } else {
      gogov <- data$Variable %>%
        # Get only GPU memory banwidth (out)
        filter(grepl("MEMMANAGER", .data$ResourceId), grepl("Out", .data$Type)) %>%
        # Remove the MANAGER0, which is CPU-only
        # TODO: After the logical OR there is a support for single-node StarPU traces
        filter(.data$Resource != "MEMMANAGER0" | .data$Node != "MEMMANAGER0") %>%
        var_integration_segment_chart(., ylabel = "GPU\n(MB/s)", step = aggStep, base_size=data$config$base_size, expand=data$config$expand) + tScale
    }
    if (!data$config$gpubandwidth$legend) {
      gogov <- gogov + theme(legend.position = "none")
    }
    # Fixed upper bound
    if (is.double(data$config$gpubandwidth$bound)) {
      lbound <- data$config$gpubandwidth$bound
      gogov <- gogov + coord_cartesian(
        ylim = c(0, lbound),
        xlim = c(tstart, tend)
      )
    }
    if (data$config$gpubandwidth$total) {
      ms <- data$Variable %>%
        filter(grepl("MEMMANAGER", .data$ResourceId), grepl("Out", .data$Type)) %>%
        filter(.data$Resource != "MEMMANAGER0" | .data$Node != "MEMMANAGER0") %>%
        group_by(.data$Type, .data$Node, .data$ResourceType, .data$Start, .data$End, .data$Duration) %>%
        summarize(Value = sum(.data$Value), N = n()) %>%
        rename(ResourceId = .data$Node)
      y_size <- layer_scales(gogov)$y$range$range[2]
      gogov <- gogov + ms %>% var_chart_text(tstart = tstart, tend = tend, y_end = y_size)
    }
  }

  # Active Nodes
  if (data$config$activenodes$active) {
    loginfo("Creating the Active Nodes plot")

    if ((data$Application %>% filter(!is.na(.data$ANode)) %>% nrow()) == 0) {
      logwarn("There aren't any information on ANode, ignoring it.")
      data$config$activenodes$active <<- FALSE
    } else {
      aggStep <- config_value(data$config$activenodes$aggregation$step, globalAggStep)
      goactivenodes <- panel_activenodes(data=data, step=aggStep, aggregation=data$config$activenodes$aggregation$active,
        legend=data$config$activenodes$legend) + tScale
    }
  }

  # Node memory usage
  if (data$config$activenodes$nodememuse$active) {
    loginfo("Creating the Node Memory Usage plot")

    if ((data$Application %>% filter(grepl("front", .data$Value) & .data$GFlop != 0) %>% nrow()) == 0) {
      logwarn("There is no memory information on data, ignoring it.")
      data$config$activenodes$nodememuse$active <<- FALSE
    } else {
      aggStep <- config_value(data$config$activenodes$aggregation$step, globalAggStep)
      gonodememuse <- panel_nodememuse(data=data, step=aggStep, aggregation=data$config$activenodes$aggregation$active,
        legend=data$config$activenodes$nodememuse$legend) + tScale
    }
  }

  # Title
  if (data$config$title$active) {
    if (!is.null(directory)) {
      tplot <- title_plot(directory)
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
    summary_nodes = go_sn,
    tplot = tplot
  )

  if (data$config$vertical_lines$active) {
    ret[[length(ret) + 1]] <- geom_vline(
      xintercept = data$config$vertical_lines$x_list,
      linetype = "longdash",
      size = 1,
      colour = data$config$vertical_lines$color_list
    )
  }

  return(plot_list)
}

#' Make a StarVZ plot
#'
#' Create a StarVZ plot considering the data suplied
#'
#' @param data starvz_data class with $config
#' @return ggplot object with all starvz plots
#' @examples
#' # starvz_plot(data)
#' @export
starvz_plot <- function(data = NULL) {
  if (is.null(data)) {
    return(NULL)
  }

  plist <- starvz_plot_list(data)

  loginfo("Assembling the plot")

  # assembling
  g <- starvz_assemble(plist, config = data$config)

  loginfo("Ending Starvz plot function")

  return(g)
}
