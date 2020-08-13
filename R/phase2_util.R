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

yconf <- function(dfw = NULL, option = "ALL") {
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
    dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      ungroup()
  } else if (option == "1GPU_per_NODE") { # Last
    # One GPU per node
    dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(n()) %>%
      ungroup()
  } else if (option == "NODES_only") { # First
    dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node) %>%
      arrange(.data$ResourceId, .data$ResourceType) %>%
      slice(1) %>%
      mutate(ResourceId = .data$Node) %>%
      ungroup()
  } else if (option == "NODES_1_in_10") { # First
    dfw %>%
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
    dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node, .data$ResourceType) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      ungroup()
  } else { # First and Last
    dfw %>%
      select(.data$Node, .data$ResourceId, .data$ResourceType, .data$Position, .data$Height) %>%
      distinct() %>%
      group_by(.data$Node, .data$ResourceType) %>%
      arrange(.data$Node, .data$ResourceId, .data$ResourceType) %>%
      slice(c(1, n())) %>%
      ungroup()
  }
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
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_title(data = starvz_sample_lu)
#' @export
panel_title <- function(data) {
  ggplot() +
    xlim(0, 1) +
    ylim(0, 1) +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = .5, y = .5, label = data$Origin, size = 5)
}
