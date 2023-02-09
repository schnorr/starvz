#' Shows if the runtimes is lacking ready tasks
#'
#' Plot a bar over time that shows when the runtime is lacking ready tasks
#'
#' @param data starvz_data with trace data
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_lackready(data = starvz_sample_lu)
#' @export
panel_lackready <- function(data = NULL,
                            x_start = data$config$limits$start,
                            x_end = data$config$limits$end) {
  starvz_check_data(data, tables = list("Starpu" = c("Node"), "Variable" = c("Type")))

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  data$Starpu %>%
    select("Node", "Resource") %>%
    unique() %>%
    group_by(.data$Node) %>%
    summarize(N = n()) %>%
    .$N %>%
    min() -> minResources

  aggStep <- data$config$lackready$aggregation
  # starvz_log(paste("lack ready aggregation is", aggStep));
  threshold <- config_value(data$config$lackready$threshold, minResources)
  # starvz_log(paste("lack ready threshold is", threshold));

  data$Variable %>%
    filter(.data$Type == "Ready") %>%
    group_by(.data$Type, .data$Node, .data$ResourceId, .data$ResourceType) %>%
    do(remyTimeIntegrationPrep(., myStep = aggStep)) %>%
    mutate(Start = .data$Slice, End = lead(.data$Slice), Duration = .data$End - .data$Start) %>%
    ungroup() %>%
    filter(!is.na(.data$End)) %>%
    group_by(.data$Type, .data$Node, .data$ResourceType, .data$Start, .data$End, .data$Duration) %>%
    summarize(Value = sum(.data$Value), N = n()) %>%
    ungroup() %>%
    rename(ResourceId = "Node") %>%
    filter(.data$Value < threshold) %>%
    group_by(.data$Type, .data$Start, .data$End) %>%
    summarize(Value = n()) %>%
    ungroup() %>%
    ggplot() +
    default_theme(data$config$base_size, data$config$expand) +
    geom_lackready() +
    coord_cartesian(
      xlim = c(x_start, x_end)
    ) -> panel

  return(panel)
}

geom_lackready <- function() {
  ret <- list()

  ret[[length(ret) + 1]] <- scale_fill_gradient(low = "lightsalmon", high = "red1")
  ret[[length(ret) + 1]] <- geom_rect(aes(
    fill = .data$Value,
    xmin = .data$Start,
    xmax = .data$End,
    ymin = 0,
    ymax = 1
  ), alpha = 1)
  ret[[length(ret) + 1]] <- theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

  return(ret)
}
