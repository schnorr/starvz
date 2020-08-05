#' @include starvz_data.R

geom_mpistates <- function(dfw = NULL, label = "1", expand = 0.05) {
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

  mycolors <- c(brewer.pal(8, "Dark2"), "blue")

  # Color mapping
  ret[[length(ret) + 1]] <- scale_fill_manual(values = mycolors)

  # Y label
  ret[[length(ret) + 1]] <- ylab("MPI\nThread")

  # Y axis breaks and their labels
  yconfm <- yconf(dfw, label)
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

state_mpi_chart <- function(data = NULL) {
  if (is.null(data)) stop("data provided to state_chart is NULL")

  # Plot
  gow <- ggplot() +
    default_theme(data$config$base_size, data$config$expand) +
    # Add states and outliers if requested
    geom_mpistates(data$Comm_state, data$config$mpistate$label, data$config$expand)

  return(gow)
}
concurrent_mpi <- function(data = NULL, out=FALSE) {
  if (is.null(data)) {
    return(NULL)
  }

  col_case <- "Origin"

  if(out){
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
    arrange({{col_case}}, .data$Timestamp) %>%
    select(-.data$Start) %>%
    rename(Start = .data$Timestamp) %>%
    group_by({{col_case}}) %>%
    mutate(End = lead(.data$Start)) %>%
    na.omit() %>%
    mutate(Duration = .data$End - .data$Start) %>%
    ungroup() %>%
    mutate(Type = "MPI Concurrent") %>%
    rename(ResourceId = {{col_case}}) %>%
    separate(.data$ResourceId, into = c("Node", "Resource"), remove = FALSE) %>%
    mutate(Node = as.factor(.data$Node)) %>%
    mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource))) %>%
    select(.data$Start, .data$End, .data$Duration, .data$Node, .data$ResourceId, .data$ResourceType, .data$Type, .data$Value)
}
