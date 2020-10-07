#' @include starvz_data.R

check_arrow <- function() {
  if (!arrow_available()) {
    starvz_warn("Arrow was not property installed, use: install_arrow()")
  }
}

extract_colors <- function(dfw = NULL, colors = NULL) {
  if (is.null(dfw)) {
    return(NULL)
  }
  if (is.null(colors)) {
    return(NULL)
  }

  dfw <- dfw %>% ungroup()

  dfw %>%
    select(.data$Value) %>%
    unique() %>%
    left_join(colors, by = c("Value")) %>%
    .$Color %>%
    setNames(dfw %>% select(.data$Value) %>% unique() %>% .$Value)
}

yconf <- function(dfw = NULL, option = "ALL", Y=NULL, show_mpi=TRUE) {
  if (is.null(dfw)) {
    return(NULL)
  }

  dfw %>% mutate(Node = as.integer(as.character(.data$Node))) -> dfw
  dfw %>%
    mutate(ResourceId = factor(.data$ResourceId)) %>%
    mutate(ResourceId = factor(.data$ResourceId,
      levels = mixedsort(levels(.data$ResourceId))
    )) -> dfw
  if (option == "1CPU_per_NODE") { # First
    # One CPU per node
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      ungroup()
  } else if (option == "1GPU_per_NODE") { # Last
    # One GPU per node
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(n()) %>%
      ungroup()
  } else if (option == "NODES_only") { # First
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      mutate(ResourceId = .data$Node) %>%
      ungroup()
  } else if (option == "NODES_1_in_10") { # First
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      mutate(ResourceId = .data$Node) %>%
      ungroup() %>%
      mutate(Node = as.integer(as.character(.data$Node))) %>%
      arrange(.data$Node) %>%
      slice(seq(1, n(), 10))
  } else if (option == "ALL") {
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node, .data$ResourceType) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      ungroup()
  } else { # First and Last ("FIRST_LAST") or anything else
    y_conf <- dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node, .data$ResourceType) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(c(1, n())) %>%
      ungroup()
  }
  if(!is.null(Y) & show_mpi==TRUE){
      y_conf <- y_conf %>%
          mutate(ResourceId = as.character(ResourceId),
                 ResourceType = as.character(ResourceType))
      Y %>% filter(Type=="Communication Thread State") %>%
            mutate(ResourceId = Parent) %>%
            separate(Parent, c("Node", "ResourceType")) %>%
            mutate(Node = as.integer(Node),
                   ResourceId = as.character(ResourceId)) %>%
            select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      bind_rows(y_conf) %>%
      mutate(ResourceId = as.factor(ResourceId),
             ResourceType = as.factor(ResourceType)) -> y_conf
  }
  return(y_conf)
}

userYLimit <- function(obj, configuration, xlimits) {
  if (!is.null(configuration)) {
    # if there is an user vertical scale defined, use it
    tpScale <- list(
      coord_cartesian(
        xlim = xlimits,
        ylim = c(0, configuration)
      )
    )
    obj <- obj + tpScale
  }
  return(obj)
}

outlier_definition <- function(x) {
  (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}

#' Create the title of StarVZ plot
#'
#' Use the directory of traces name to create a plot title
#'
#' @param data starvz_data with trace data
#' @param title title text, if NULL it will fallback to data$Origin then to "Null Title"
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_title(data = starvz_sample_lu)
#' @export
panel_title <- function(data, title=data$config$title$text) {
  if(is.null(title)){
    if(is.null(data$Origin)){
      title <- "Null Title"
    }else{
      title <- data$Origin
    }
  }
  ggplot() +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = .5, y = .5, label = title, size = rel(8))
}

#' Create the diagnostig plot for the regression model
#'
#' Use the starvz Application data to observe how the regression model used
#' in the task anomaly classification fits the data.
#'
#' @param data starvz_data with trace data
#' @param freeScales free X,Y scales for each task and resource type combination
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \dontrun{
#' panel_model_gflops(data = starvz_sample_data)
#' }
#' @export
panel_model_gflops <- function(data, freeScales=TRUE) {

  model_panel <- data$Application %>%
    filter(.data$Value %in% c("geqrt", "gemqrt", "tpqrt", "tpmqrt")) %>%
    ggplot(aes(x=.data$GFlop, y=.data$Duration, color=.data$Outlier)) +
      theme_bw(base_size=data$config$base_size) +
      geom_point(alpha=.5) +
      labs(y="Duration (ms)", x="GFlops") +
      scale_color_brewer(palette="Set1") +
      theme(legend.position="top") +
      labs(color = "Anomaly") +
      # ~ 0 forces the model to pass through origin
      geom_smooth(method="lm", formula="y ~ 0 + I(x^(2/3))", color="green", fill="blue")

    if (freeScales) {
      model_panel <- model_panel + facet_wrap(.data$ResourceType~.data$Value, scales="free", ncol=4)
    } else {
      model_panel <- model_panel + facet_grid(.data$ResourceType~.data$Value, scales="free")
    }

    return(model_panel)
}
