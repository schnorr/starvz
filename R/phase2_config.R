#' @include starvz_data.R
NULL

#' Read config files
#'
#' Read starvz configuration yaml files. This function is design to replace
#' an already existing configuration on starvz data.
#'
#' @param file The path to a file
#' @param warn Give a warn in case the config file is not found
#' @return A list containing starvz configuration
#' @examples
#' \donttest{
#' example_file <- system.file("extdata", "config.yaml", package = "starvz")
#' config <- starvz_read_config(example_file)
#' }
#' @export
starvz_read_config <- function(file = NULL, warn = TRUE) {
  defaut_config <- starvz_default_config()
  if (is.null(file)) {
    starvz_log("Using default config")
    return(defaut_config)
  } else if (!file.exists(file)) {
    if (warn) starvz_warn(paste0("StarVZ yaml configuration file [", file, "] doesn't exist, using default"))
    return(defaut_config)
  } else {
    config <- yaml::read_yaml(file)
    # Retrocompatible with default classes
    if (isTRUE(names(config) == "default")) {
      config <- config$default
    }
    final_config <- modifyList(defaut_config, config)
    return(final_config)
  }
}


config_value <- function(property, default) {
  ifelse(is.null(property), default, property)
}

starvz_default_config <- function() {
  config <- list()
  config$base_size <- 22
  config$expand <- 0.05
  config$idleness_factor <- 5.5
  config$selected_nodes <- NULL
  config$log <- TRUE

  config$title$active <- FALSE

  config$utiltreenode$active <- FALSE
  config$utiltreenode$legend <- FALSE

  config$utiltreedepth$active <- FALSE
  config$utiltreedepth$legend <- FALSE

  config$computingnodes$active <- FALSE

  config$atree$active <- FALSE
  config$atree$zoom$start <- 0
  config$atree$zoom$end <- 100
  config$atree$legend <- FALSE
  config$atree$computation$active <- FALSE
  config$atree$computation$pruned$active <- FALSE
  config$atree$initialization$active <- FALSE
  config$atree$communication$active <- FALSE
  config$atree$anomalies$active <- FALSE

  config$activenodes$active <- FALSE
  config$activenodes$nodememuse$active <- FALSE
  config$activenodes$nodememuse$legend <- TRUE
  config$activenodes$nodememuse$aggregation$active <- FALSE
  config$activenodes$nodememuse$aggregation$step <- 100
  config$activenodes$aggregation$active <- FALSE
  config$activenodes$aggregation$step <- 100
  config$activenodes$legend <- FALSE

  config$lackready$aggregation <- 200

  config$kiteration$active <- FALSE
  config$kiteration$pernode <- FALSE
  config$kiteration$subite <- FALSE

  config$st$active <- TRUE
  config$st$idleness_all <- TRUE
  config$st$labels <- "ALL"
  config$st$legend <- TRUE
  config$st$makespan <- TRUE
  config$st$cpb <- FALSE
  config$st$cpb_mpi$active <- FALSE
  config$st$cpb_mpi$tile_size <- NULL
  config$st$cpb_mpi$bandwidth <- NULL
  config$st$cpb_mpi$theoretical <- FALSE
  config$st$expand <- 0.05
  config$st$idleness <- FALSE
  config$st$tasks$active <- FALSE
  config$st$tasks$list <- NULL
  config$st$tasks$levels <- 2
  config$st$outliers <- TRUE
  config$st$abe$active <- FALSE
  config$st$aggregation$active <- FALSE
  config$st$aggregation$method <- "static"
  config$st$aggregation$states <- c("dgemm")
  config$st$rect_outline <- FALSE
  config$st$abe$size <- 5
  config$st$abe$bar_color <- "grey"
  config$st$abe$text <- TRUE
  config$st$abe$label <- TRUE
  config$st$alpha <- 0.5
  config$st$drop_small <- 0.0

  config$summary_nodes$active <- FALSE
  config$summary_nodes$legend <- FALSE

  config$pmtool$kiteration$active <- FALSE
  config$pmtool$state$active <- FALSE
  config$pmtool$bounds$active <- FALSE
  config$pmtool$makespan <- TRUE
  config$pmtool$state$sched <- NULL
  config$pmtool$bounds$label <- TRUE

  config$memory$state$active <- FALSE
  config$memory$transfers$active <- FALSE
  config$memory$transfers$arrow <- FALSE
  config$memory$transfers$border <- FALSE
  config$memory$transfers$total <- FALSE
  config$memory$combined <- FALSE
  config$memory$state$height <- 2
  config$memory$state$border <- FALSE
  config$memory$state$depth$height <- 0
  config$memory$state$text <- FALSE
  config$memory$state$total <- FALSE
  config$memory$state$select <- "Allocating"
  config$memory$state$angle <- 90

  config$submitted$active <- FALSE

  config$starpu$active <- FALSE
  config$starpu$aggregation$active <- FALSE

  config$node_events$active <- FALSE
  config$node_events$legend <- TRUE

  config$ready$active <- FALSE
  config$ready$limit <- NA
  config$ready$lack_ready$active <- FALSE

  config$lackready$active <- FALSE

  config$gflops$active <- FALSE
  config$gflops$facet <- TRUE
  config$gflops$limit <- FALSE

  config$usedmemory$active <- FALSE
  config$usedmemory$legend <- FALSE

  config$imbalance$active <- FALSE
  config$imbalance$limit <- 1

  config$power_imbalance$active <- FALSE
  config$power_imbalance$task <- NULL
  config$power_imbalance$limit <- 1

  config$hete_imbalance$active <- FALSE
  config$hete_imbalance$limit <- 1

  config$utilheatmap$active <- FALSE
  config$utilheatmap$labels <- "1"

  config$gpubandwidth$active <- FALSE
  config$gpubandwidth$bound <- NULL

  config$mpibandwidth$active <- FALSE

  config$mpiconcurrent$active <- FALSE

  config$mpiconcurrentout$active <- FALSE

  config$mpistate$active <- FALSE
  config$mpistate$label <- "ALL"

  config$guided$active <- FALSE
  config$guided$agg_type_height <- 50
  config$guided$node_height <- 10

  config$vertical_lines$active <- FALSE
  config$vertical_lines$x_list <- NULL
  config$vertical_lines$color_list <- NULL
  return(config)
}
