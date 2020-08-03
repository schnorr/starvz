#' @include starvz_data.R

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

  agg_step <- config_value(step, data$config$global_agg_step)

  if(is.null(agg_step) || !is.numeric(agg_step)){
    agg_step <- 100
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

var_chart <- function(dfv = NULL, ylabel = NA, base_size = 22, expand = 0.05) {
  if (is.null(dfv)) {
    return(NULL)
  }

  variable <- dfv %>%
    select(.data$Type) %>%
    .$Type %>%
    unique()
  if (is.na(ylabel)) ylabel <- variable

  dfv %>%
    .$ResourceId %>%
    unique() %>%
    length() -> n_resources
  mycolors <- rep(brewer.pal(8, "Dark2"), (n_resources / 8) + 1)

  k <- dfv %>%
    rename(x = .data$Start, xend = .data$End, y = .data$Value) %>%
    mutate(yend = .data$y) %>%
    select(-.data$Duration)
  v <- k %>%
    group_by(.data$ResourceId, .data$Type) %>%
    mutate(xend = .data$x, y = .data$y, yend = lag(.data$y)) %>%
    na.omit()
  k %>%
    ggplot() +
    default_theme(base_size, expand) +
    geom_segment(aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, color = .data$ResourceId)) +
    geom_segment(data = v, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, color = .data$ResourceId)) +
    geom_point(size = .1, aes(x = .data$x, y = .data$y, color = .data$ResourceId)) +
    #        coord_cartesian(xlim=c(0, max(dfv$End))) +
    ylim(0, NA) +
    ylab(ylabel) +
    scale_colour_manual(values = mycolors)
}

var_chart_text <- function(dfv = NULL, tstart = NULL, tend = NULL, y_end = NULL) {
  if (is.null(dfv)) {
    return(NULL)
  }
  max_value <- y_end
  ms <- dfv %>% filter(.data$Start < tend & .data$End > tstart)
  ret <- list()
  # Calculate selected state % in time
  total_time <- tend - tstart
  ms <- ms %>%
    group_by(.data$ResourceId) %>%
    summarize(xvar = round(sum(.data$Value * (.data$Duration / 1000) / 1024), 2))
  ms %>%
    .$ResourceId %>%
    unique() %>%
    length() -> n_resources
  mycolors <- rep(brewer.pal(8, "Dark2"), (n_resources / 8) + 1)

  if (nrow(ms) != 0) {
    globalEndTime <- tend * 1.01
    ms <- ms %>% rowid_to_column("Position")
    ms$Position <- max_value * 0.9 - (ms$Position - 1) * (max_value / nrow(ms))
    ms$xvar <- paste0(ms$xvar, " GB")
    ret[[length(ret) + 1]] <- geom_label(
      data = ms, x = globalEndTime, colour = "white", fontface = "bold",
      aes(y = .data$Position, label = .data$xvar, fill = .data$ResourceId), alpha = 1.0, show.legend = FALSE
    )
    ret[[length(ret) + 1]] <- scale_fill_manual(values = mycolors)
  }
  return(ret)
}

var_integration_chart <- function(dfv = NULL, ylabel = NA, step = 250, facetting = FALSE, base_size = 22, expand = 0.05) {
  if (is.null(dfv)) {
    return(NULL)
  }

  variable <- dfv %>%
    select(.data$Type) %>%
    .$Type %>%
    unique()
  if (is.na(ylabel)) ylabel <- variable

  dfv %>%
    group_by(.data$Node, .data$ResourceType) %>%
    arrange(.data$Node, .data$Start) %>%
    do(remyTimeIntegrationPrep(., myStep = step)) %>%
    ggplot(aes(x = .data$Slice, y = .data$Value, color = .data$Node)) +
    default_theme(base_size, expand) +
    geom_point(size = 1) +
    geom_line() +
    # coord_cartesian(xlim=c(0, max(dfv$End))) +
    ylim(0, NA) +
    ylab(ylabel) +
    scale_fill_brewer(palette = "Set1") -> result
  if (facetting) {
    result <- result +
      facet_wrap(~ResourceType, ncol = 1, scales = "free_y") +
      theme(
        strip.background = element_rect(fill = "white"),
        strip.placement = "inside",
        panel.spacing = unit(1, "mm")
      )
  }
  return(result)
}
var_integration_segment_chart <- function(dfv = NULL, ylabel = NA, step = 250, facetting = FALSE, base_size = 22, expand = 0.05, legend=TRUE) {
  if (is.null(dfv)) {
    return(NULL)
  }
  if (nrow(dfv) == 0) {
    return(NULL)
  }

  variable <- dfv %>%
    select(.data$Type) %>%
    .$Type %>%
    unique()
  if (is.na(ylabel)) ylabel <- variable
  dfv %>%
    group_by(.data$Type, .data$Node, .data$ResourceId, .data$ResourceType) %>%
    do(remyTimeIntegrationPrep(.data, myStep = step)) %>%
    mutate(Start = .data$Slice, End = lead(.data$Slice), Duration = .data$End - .data$Start) %>%
    ungroup() %>%
    filter(!is.na(.data$End)) %>%
    group_by(.data$Type, .data$Node, .data$ResourceId, .data$Start, .data$End, .data$Duration) %>%
    summarize(Value = sum(.data$Value), N = n()) %>%
    # rename(ResourceId = Node) %>%
    ungroup() %>%
    var_chart(., ylabel = ylabel, base_size = base_size, expand = expand) -> result
  if (facetting) {
    result <- result +
      facet_wrap(~ .data$ResourceType, ncol = 1, scales = "free_y") + # , strip.position="right") + # cowplot can't align this
      theme(
        strip.background = element_rect(fill = "white"),
        strip.placement = "inside",
        panel.spacing = unit(1, "mm")
      )
  }
  if(legend){
    result <- result + theme(legend.position = "top")
  }else{
    result <- result + theme(legend.position = "none")
  }
  return(result)
}
