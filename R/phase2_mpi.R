#' @include starvz_data.R

geom_mpistates <- function(dfw = NULL, label = "1", expand = 0.05, Y = NULL) {
  if (is.null(dfw)) stop("dfw is NULL when given to geom_mpistates")

  if (nrow(dfw) == 0) stop("there is no data on MPI states")

  ret <- list()

  # Calculate Y position
  ypos <- tibble(ResourceId = (dfw %>% arrange(as.integer(as.character(.data$Node))) %>% pull(.data$ResourceId) %>% unique())) %>%
    mutate(Height = 1) %>%
    mutate(Position = cumsum(.data$Height))

  dfw <- dfw %>%
    # Establish new position
    left_join(ypos, by = c("ResourceId"))
  dfw %>%
    .$Value %>%
    unique() %>%
    length() -> n_values
  mycolors <- rep(brewer.pal(8, "Dark2"), (n_values / 8) + 1)

  # Color mapping
  ret[[length(ret) + 1]] <- scale_fill_manual(values = mycolors)

  # Y label
  ret[[length(ret) + 1]] <- ylab("MPI\nThread")

  # Y axis breaks and their labels
  yconfm <- yconf(dfw, label, Y)
  ret[[length(ret) + 1]] <- scale_y_continuous(breaks = yconfm$Position + (yconfm$Height / 3), labels = yconfm$ResourceId, expand = c(expand, 0))

  # Add states
  ret[[length(ret) + 1]] <-
    geom_rect(data = dfw, aes(
      fill = .data$Value,
      xmin = .data$Start,
      xmax = .data$End,
      ymin = .data$Position,
      ymax = .data$Position + 0.6
    ))

  return(ret)
}

#' Create a space-time view of MPI controlers
#'
#' Create a space-time view of MPI controlers
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param y_start Y-axis start value
#' @param y_end Y-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_mpistate(data = starvz_sample_lu)
#' @export
panel_mpistate <- function(data = NULL,
                           legend = data$config$mpistate$legend,
                           base_size = data$config$base_size,
                           expand_x = data$config$expand,
                           x_start = data$config$limits$start,
                           x_end = data$config$limits$end,
                           y_start = 0,
                           y_end = data$config$mpistate$limit) {
  starvz_check_data(data, tables = list("Comm_state" = c("Node")))

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  if (is.null(y_start) || (!is.na(y_start) && !is.numeric(y_start))) {
    y_start <- NA
  }

  if (is.null(y_end) || (!is.na(y_end) && !is.numeric(y_end))) {
    y_end <- NA
  }

  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  # Plot
  gow <- ggplot() +
    default_theme(base_size, expand_x) +
    # Add states and outliers if requested
    geom_mpistates(data$Comm_state, data$config$mpistate$label, expand_x, Y = data$Y) +
    coord_cartesian(
      xlim = c(x_start, x_end),
      ylim = c(0, y_end)
    )

  if (!legend) {
    gow <- gow + theme(legend.position = "none")
  }

  return(gow)
}

concurrent_mpi <- function(data = NULL, out = FALSE) {
  if (is.null(data)) {
    return(NULL)
  }

  col_case <- "Origin"

  if (out) {
    col_case <- "Dest"
  }

  data$Link %>%
    filter(grepl("mpicom", .data$Key)) %>%
    select(-.data$Container, -.data$Type, -.data$Duration) -> dflink

  dflink %>%
    select(-.data$End) %>%
    rename(Timestamp = .data$Start) %>%
    mutate(Start = TRUE) -> dfstart
  dflink %>%
    select(-.data$Start) %>%
    rename(Timestamp = .data$End) %>%
    mutate(Start = FALSE) %>%
    bind_rows(dfstart) %>%
    arrange(.data$Timestamp) %>%
    group_by(.data$Origin, .data$Dest) %>%
    mutate(Value = cumsum(as.integer(
      case_when(
        .data$Start == TRUE ~ 1,
        .data$Start == FALSE ~ -1,
        TRUE ~ 0
      )
    ))) %>%
    arrange({{ col_case }}, .data$Timestamp) %>%
    select(-.data$Start) %>%
    rename(Start = .data$Timestamp) %>%
    group_by({{ col_case }}) %>%
    mutate(End = lead(.data$Start)) %>%
    na.omit() %>%
    mutate(Duration = .data$End - .data$Start) %>%
    ungroup() %>%
    mutate(Type = "MPI Concurrent") %>%
    rename(ResourceId = {{ col_case }}) %>%
    separate(.data$ResourceId, into = c("Node", "Resource"), remove = FALSE, extra = "drop", fill = "right") %>%
    mutate(Node = as.factor(.data$Node)) %>%
    mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource))) %>%
    select(.data$Start, .data$End, .data$Duration, .data$Node, .data$ResourceId, .data$ResourceType, .data$Type, .data$Value)
}
