
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

  if (data$config$submitted$active) {
    P[[length(P) + 1]] <- plist$submitted
    H[[length(H) + 1]] <- config_value(data$config$submitted$height, data$config$guided$starvz_height_var)
  }
  if (data$config$starpu$active) {
    P[[length(P) + 1]] <- plist$starpu
    H[[length(H) + 1]] <- config_value(data$config$starpu$height, data$config$guided$starvz_height_resources)
  }
  if (data$config$node_events$active) {
    P[[length(P) + 1]] <- plist$node_events
    H[[length(H) + 1]] <- config_value(data$config$node_events$height, data$config$guided$starvz_height_nodes)
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
  if (data$config$power_imbalance$active && !is.null(plist$imb_plot_power)) {
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
#' @param remove_Y_info remove Y labels for a second and subsequent list of panels
#' @param remove_legends remove legends for a second and subsequent list of panels
#' @return The ggplot plot
#' @examples
#' \donttest{
#' starvz_assemble(starvz_plot_list(starvz_sample_lu),
#'   config = starvz_sample_lu$config
#' )
#' }
#' @export
starvz_assemble <- function(..., config = NULL, remove_Y_info = TRUE, remove_legends = TRUE) {
  defaut_config <- starvz_default_config()
  config <- modifyList(defaut_config, config)

  plists <- list()
  for (i in list(...)) {
    # Check if arguments are a list or a plot
    # is.list is not adequated here
    if (isTRUE(class(i[[1]]) == "list")) {
      plists <- append(plists, i)
    } else if (is(i[[1]], "gg")) {
      plists <- append(plists, list(i))
    } else {
      stop("starvz_assemble needs a list of plots or a list of list of plots")
      return(NULL)
    }
  }

  if (is.null(config)) {
    stop("config is null")
  }

  number_plots <- length(plists)

  starvz_log(paste0("Assembling ", number_plots, " groups of plots"))

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
      stop("starvz_assemble element is not a plot list")
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

#' Generate the StarVZ Plots
#'
#' Use data to create the list of StarVZ plots
#'
#' @param data starvz_data with trace data
#' @return A list of ggplot plots
#' @examples
#' \donttest{
#' starvz_plot_list(starvz_sample_lu)
#' }
#' @export
starvz_plot_list <- function(data = NULL) {
  if (is.null(data)) stop("data passed as parameter is null")

  if (is.null(data$Version)) {
    starvz_warn("This is an old StarVZ trace, trying to be retrocompatible")
    data <- convert_state(data)
  }

  starvz_check_data(data, tables = list("Application" = c("Start", "End")))

  defaut_config <- starvz_default_config()
  data$config <- modifyList(defaut_config, data$config)

  # Adjust temporal scale
  if (is.null(data$config$limits$start)) {
    data$config$limits$start <- data$Application %>%
      pull(.data$Start) %>%
      min()
  }
  if (is.null(data$config$limits$end)) {
    data$config$limits$end <- data$Application %>%
      pull(.data$End) %>%
      max() + 1 # Just to give space
  }

  # Define the global aggregation step as 0.1% of the total window
  data$config$global_agg_step <- (data$config$limits$end - data$config$limits$start) * 0.001

  if (!is.null(data$config$selected_nodes)) {
    data$Variable <- data$Variable %>% filter(.data$Node %in% data$config$selected_nodes)
  }

  starvz_log("Starting the Starvz plot function")

  # Create a named list with the ggplot objects + title
  plot_list <- list(
    atree = geom_blank(),
    utiltreenode = geom_blank(),
    utiltreedepth = geom_blank(),
    st = geom_blank(),
    st_pm = geom_blank(),
    st_mm = geom_blank(),
    starpu = geom_blank(),
    node_events = geom_blank(),
    ijk = geom_blank(),
    ijk_pm = geom_blank(),
    lackready = geom_blank(),
    ready = geom_blank(),
    submitted = geom_blank(),
    mpi = geom_blank(),
    mpiconc = geom_blank(),
    mpiconcout = geom_blank(),
    mpistate = geom_blank(),
    gpu = geom_blank(),
    memory = geom_blank(),
    imb_plot = geom_blank(),
    imb_plot_power = geom_blank(),
    imb_plot_hete = geom_blank(),
    heatmap = geom_blank(),
    gflops = geom_blank(),
    activenodes = geom_blank(),
    nodememuse = geom_blank(),
    summary_nodes = geom_blank(),
    tplot = geom_blank()
  )

  # Atree space/time view
  if (data$config$atree$active) {
    starvz_log("Creating the temporal atree plot")
    plot_list$atree <- panel_atree(data)
  }

  # Resource utilization by tree node
  if (data$config$utiltreenode$active) {
    starvz_log("Creating the resource utilization by node plot")
    plot_list$utiltreenode <- panel_utiltreenode(data)
  }

  # Resource utilization by tree depth
  if (data$config$utiltreedepth$active) {
    starvz_log("Creating the resource utilization by depth plot")
    plot_list$utiltreedepth <- panel_utiltreedepth(data)
  }

  # SpaceTime
  if (data$config$st$active) {
    starvz_log("Creating the Space/Time")
    plot_list$st <- panel_st(data)
  }

  if (data$config$summary_nodes$active) {
    starvz_log("Creating node summary")
    plot_list$summary_nodes <- panel_node_summary(data)
  }

  if (data$config$pmtool$state$active) {
    starvz_log("Creating pmtool states")
    plot_list$st_pm <- panel_pmtool_st(data)
  }

  if (data$config$memory$state$active) {
    starvz_log("Creating memory states")
    plot_list$st_mm <- panel_memory_state(data)
  }

  # StarPU SpaceTime
  if (data$config$starpu$active) {
    starvz_log("Creating the StarPU Space/Time")
    plot_list$starpu <- panel_st_runtime(data)
  }

  # Node Events
  if (data$config$node_events$active) {
    starvz_log("Creating the Node Events")
    plot_list$node_events <- panel_node_events(data)
  }


  # KIteration
  if (data$config$kiteration$active) {
    starvz_log("Creating the KIteration")
    plot_list$ijk <- panel_kiteration(data)
  }

  # KIteration PMTOOL
  if (data$config$pmtool$kiteration$active) {
    starvz_log("Creating the KIteration for PMTool")
    plot_list$ijk_pm <- panel_pmtool_kiteration(data)
  }

  # Lack ready (companion for Ready Variable)
  if (data$config$lackready$active) {
    starvz_log("Creating the Lack Ready Plot")
    plot_list$lackready <- panel_lackready(data)
  }

  # Ready
  if (data$config$ready$active) {
    starvz_log("Creating the Ready plot")
    plot_list$ready <- panel_ready(data)
  }

  # Submitted
  if (data$config$submitted$active) {
    starvz_log("Creating the Submitted plot")
    plot_list$submitted <- panel_submitted(data)
  }

  # GFlops
  if (data$config$gflops$active) {
    starvz_log("Creating the GFlops plot")
    plot_list$gflops <- panel_gflops(data)
  }

  # Used Memory
  if (data$config$usedmemory$active) {
    starvz_log("Creating the Used Memory plot")
    plot_list$memory <- panel_usedmemory(data)
  }

  # Imbalance Metrics
  if (data$config$imbalance$active) {
    starvz_log("Creating the Imbalance Metrics plot")
    plot_list$imb_plot <- panel_imbalance(data)
  }

  # Imbalance Metrics Power
  if (data$config$power_imbalance$active) {
    starvz_log("Creating the Imbalance Power Metrics plot")
    plot_list$imb_plot_power <- panel_power_imbalance(data)
  }

  # Imbalance Metrics hete
  if (data$config$hete_imbalance$active) {
    starvz_log("Creating the Imbalance Hetero Metrics plot")
    plot_list$imb_plot_hete <- panel_hete_imbalance(data)
  }

  if (data$config$utilheatmap$active) {
    starvz_log("Creating the HeatMap Imbalance plot")
    plot_list$heatmap <- panel_utilheatmap(data)
  }

  # MPIBandwidth
  if (data$config$mpibandwidth$active) {
    starvz_log("Creating the MPIBandwidth plot")
    plot_list$mpi <- panel_mpibandwidth(data)
  }

  # MPI Concurrent
  if (data$config$mpiconcurrent$active) {
    starvz_log("Creating the MPI concurrent ops plot")
    plot_list$mpiconc <- panel_mpiconcurrent(data)
  }

  # MPI Concurrent Out
  if (data$config$mpiconcurrentout$active) {
    starvz_log("Creating the MPI concurrent ops out plot")
    plot_list$mpiconcout <- panel_mpiconcurrentout(data)
  }

  # MPI State
  if (data$config$mpistate$active) {
    starvz_log("Creating the MPI state")
    plot_list$mpistate <- panel_mpistate(data)
  }

  # GPUBandwidth
  if (data$config$gpubandwidth$active) {
    starvz_log("Creating the GPU Bandwidth plot")
    plot_list$gpu <- panel_gpubandwidth(data)
  }

  # Active Nodes
  if (data$config$activenodes$active) {
    starvz_log("Creating the Active Nodes plot")
    plot_list$activenodes <- panel_activenodes(data)
  }

  # Node memory usage
  if (data$config$activenodes$nodememuse$active) {
    starvz_log("Creating the Node Memory Usage plot")
    plot_list$nodememuse <- panel_nodememuse(data)
  }

  # Title
  if (data$config$title$active) {
    starvz_log("Creating the title plot")
    plot_list$tplot <- panel_title(data)
  }

  if (data$config$vertical_lines$active) {
    starvz_log("Creating vertical lines")
    verticallines <- geom_vline(
      xintercept = data$config$vertical_lines$x_list,
      linetype = "longdash",
      size = 1,
      colour = data$config$vertical_lines$color_list
    )
    for (name in names(plot_list)) {
      if (name != "tplot" && is(plot_list[[name]], "ggplot") == TRUE) {
        plot_list[[name]] <- (plot_list[[name]] + verticallines)
      }
    }
  }

  return(plot_list)
}

#' Make a StarVZ plot
#'
#' Create a StarVZ plot considering the data supplied
#'
#' @param data starvz_data class with $config
#' @param save call ggplot to save the image
#' @param name Path for saved image
#' @param guided compute ideal figure height
#' @param dpi dpi for ggsave
#' @return ggplot object with all starvz plots
#' @examples
#' \donttest{
#' starvz_plot(starvz_sample_lu)
#' }
#' @export
starvz_plot <- function(data = NULL, name = NULL, save = FALSE, guided = data$config$guided$active, dpi = 120) {
  if (is.null(data)) {
    return(NULL)
  }

  # Normalize Config
  defaut_config <- starvz_default_config()
  data$config <- modifyList(defaut_config, data$config)

  if (!is.null(guided) && guided) {
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
        select("MPIRank") %>%
        distinct() %>%
        nrow()
    } else if (!is.null(data$Application)) {
      nodes <- data$Application %>%
        select("Node") %>%
        distinct() %>%
        nrow()
    } else {
      nodes <- 1
    }

    types <- data$Application %>%
      select("ResourceType") %>%
      distinct() %>%
      nrow()

    if (!is.null(data$config$selected_nodes)) {
      data$Y %>%
        separate(.data$Parent, into = c("Node"), remove = FALSE, extra = "drop", fill = "right") %>%
        filter(.data$Node %in% data$config$selected_nodes) %>%
        arrange(.data$Position) %>%
        mutate(New = cumsum(lag(.data$Height, default = 0))) %>%
        select("Parent", "New") -> new_y
      data$config$guided$starvz_height_resources <- (new_y$New %>% max()) * 15 / 100
    } else {
      data$config$guided$starvz_height_resources <- (data$Y$Position %>% max()) * 15 / 100
    }

    data$config$guided$starvz_height_agg <- max(nodes * types * data$config$guided$agg_type_height / 100, 1)
    data$config$guided$starvz_height_nodes <- max(nodes * data$config$guided$node_height / 100, 1)
    data$config$guided$starvz_height_small <- 0.5
    if (!is.null(data$Atree)) {
      data$config$guided$starvz_height_atree <- ((data$Atree$Position %>% max()) * 1.5) / 100
    } else {
      data$config$guided$starvz_height_atree <- 1
    }
  }

  plist <- starvz_plot_list(data)

  starvz_log("Assembling the plot")

  # assembling
  plot <- starvz_assemble(plist, config = data$config)

  starvz_height_total <- attr(plot, "starvz_height_total")
  if (is.null(starvz_height_total) || starvz_height_total == 0) {
    starvz_height_total <- 1000
  }

  final_px_height <- (starvz_height_total + 5) * 100
  attr(plot, "starvz_height_total") <- NULL
  attr(plot, "height") <- final_px_height

  if (save && !is.null(name)) {
    total_dpi <- 120
    final_px_width <- 1000
    final_in_height <- final_px_height / total_dpi
    final_in_width <- final_px_width / total_dpi

    ggsave(name, plot = plot, width = final_in_width, height = final_in_height, units = "in", dpi = dpi, limitsize = FALSE)
  }

  starvz_log("Ending Starvz plot function")

  return(plot)
}
