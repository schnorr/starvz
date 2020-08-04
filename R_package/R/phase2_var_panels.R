#' Create a line chart panel with ready tasks submission
#'
#' Use the Variable traces to create a line chart panel with ready tasks
#' submission per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_ready(data=starvz_sample_lu)
#' @export
panel_ready <- function(data, legend=data$config$ready$legend,
                              base_size=data$config$base_size,
                              expand_x=data$config$expand,
                              x_start=data$config$limits$start,
                              x_end=data$config$limits$end,
                              y_start=0,
                              y_end=data$config$ready$limit,
                              step=data$config$ready$step){

  ## Check for non-valid arguments
  if(is.null(legend) || !is.logical(legend)){
    legend <- TRUE
  }

  if(is.null(base_size) || !is.numeric(base_size)){
    base_size <- 22
  }

  if(is.null(expand_x) || !is.numeric(expand_x)){
    expand_x <- 0.05
  }

  if(is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start)) ){
    x_start <- NA
  }

  if(is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end)) ){
    x_end <- NA
  }

  if(is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start)) ){
    y_start <- NA
  }

  if(is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end)) ){
    y_end <- NA
  }

  if(is.null(step) || !is.numeric(step)){
    if(is.null(data$config$global_agg_step)){
      agg_step <- 100
    }else{
      agg_step <- data$config$global_agg_step
    }
  }else{
      agg_step <- step
  }

  panel <- data$Variable %>%
    filter(grepl("sched", .data$ResourceId), grepl("Ready", .data$Type)) %>%
    var_integration_segment_chart(step = agg_step,
      base_size=base_size,
      expand=expand_x,
      legend=legend) +
    coord_cartesian(
        xlim = c(x_start, x_end),
        ylim = c(0, y_end)
    )
  return(panel)
}


#' Create a line chart panel with submmited tasks submission
#'
#' Use the Variable traces to create a line chart panel with submmited tasks
#' submission per node, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_submitted(data=starvz_sample_lu)
#' @export
panel_submitted <- function(data, legend=data$config$submmited$legend,
                              base_size=data$config$base_size,
                              expand_x=data$config$expand,
                              x_start=data$config$limits$start,
                              x_end=data$config$limits$end,
                              y_start=0,
                              y_end=data$config$submmited$limit,
                              step=data$config$submmited$step){

  ## Check for non-valid arguments
  if(is.null(legend) || !is.logical(legend)){
    legend <- TRUE
  }

  if(is.null(base_size) || !is.numeric(base_size)){
    base_size <- 22
  }

  if(is.null(expand_x) || !is.numeric(expand_x)){
    expand_x <- 0.05
  }

  if(is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start)) ){
    x_start <- NA
  }

  if(is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end)) ){
    x_end <- NA
  }

  if(is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start)) ){
    y_start <- NA
  }

  if(is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end)) ){
    y_end <- NA
  }

  if(is.null(step) || !is.numeric(step)){
    if(is.null(data$config$global_agg_step)){
      agg_step <- 100
    }else{
      agg_step <- data$config$global_agg_step
    }
  }else{
      agg_step <- step
  }

  panel <- data$Variable %>%
    filter(grepl("sched", .data$ResourceId), grepl("Submitted", .data$Type)) %>%
    var_integration_segment_chart(step = agg_step,
      base_size=base_size,
      expand=expand_x,
      legend=legend) +
    coord_cartesian(
        xlim = c(x_start, x_end),
        ylim = c(0, y_end)
    )
  return(panel)
}


#' Create a line chart panel with used memory
#'
#' Use the Variable traces to create a line chart panel with used memory
#' per resource, aggregated by a configurable time step
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @param step time step for aggregation
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_submitted(data=starvz_sample_lu)
#' @export
panel_usedmemory <- function(data, legend=data$config$usedmemory$legend,
                              base_size=data$config$base_size,
                              expand_x=data$config$expand,
                              x_start=data$config$limits$start,
                              x_end=data$config$limits$end,
                              y_start=0,
                              y_end=data$config$usedmemory$limit,
                              step=data$config$usedmemory$step){

  ## Check for non-valid arguments
  if(is.null(legend) || !is.logical(legend)){
    legend <- TRUE
  }

  if(is.null(base_size) || !is.numeric(base_size)){
    base_size <- 22
  }

  if(is.null(expand_x) || !is.numeric(expand_x)){
    expand_x <- 0.05
  }

  if(is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start)) ){
    x_start <- NA
  }

  if(is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end)) ){
    x_end <- NA
  }

  if(is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start)) ){
    y_start <- NA
  }

  if(is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end)) ){
    y_end <- NA
  }

  if(is.null(step) || !is.numeric(step)){
    if(is.null(data$config$global_agg_step)){
      agg_step <- 100
    }else{
      agg_step <- data$config$global_agg_step
    }
  }else{
      agg_step <- step
  }

  panel <- data$Variable %>%
    filter(grepl("Used", .data$Type)) %>%
    var_integration_segment_chart(step = agg_step,
      base_size=base_size,
      expand=expand_x,
      legend=legend) +
    coord_cartesian(
        xlim = c(x_start, x_end),
        ylim = c(0, y_end)
    )
  return(panel)
}
