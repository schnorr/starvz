
#' Create a memory state space time
#'
#' Show memory events
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @param combined shows links
#' @param show_state_total Show the percentage of selected state
#' @param show_transfer_total Show total transfer amount
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' panel_memory_state(data = starvz_sample_lu)
#' @export
panel_memory_state <- function(data = NULL,
                               combined = data$config$memory$combined,
                               legend = data$config$memory$legend,
                               base_size = data$config$base_size,
                               expand_x = data$config$expand,
                               x_start = data$config$limits$start,
                               x_end = data$config$limits$end,
                               show_state_total = data$config$memory$state$total,
                               show_transfer_total = data$config$memory$transfer$total) {
  starvz_check_data(data, tables = list("Events_memory" = c("Type", "Container", "Handle")))

  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }

  starvz_log("Entry of events_memory_chart")

  memory_states <- c("Allocating Async Start", "Allocating Async End", "Allocating Start", "Allocating End", "WritingBack Start", "WritingBack End", "Free Start", "Free End")
  memory_states_start <- c("Allocating Async Start", "Allocating Start", "WritingBack Start", "Free Start")

  # Filter
  dfwapp <- data$Events_memory %>%
    filter(.data$Type %in% memory_states) %>%
    group_by(.data$Container, .data$Handle, .data$Tid, .data$Src) %>%
    arrange(.data$Start) %>%
    mutate(End = lead(.data$Start)) %>%
    filter(.data$Type %in% memory_states_start) %>%
    mutate(Duration = .data$End - .data$Start) %>%
    mutate(Type = gsub(" Start", "", .data$Type))

  # Plot
  gow <- ggplot() +
    default_theme(base_size, expand_x)

  # Add states and outliers if requested
  gow <- gow + geom_events(data, dfwapp,
    combined = combined, tstart = x_start, tend = x_end,
    show_total = show_state_total
  )
  if (combined) {
    gow <- gow + geom_links(data, dfwapp,
      combined = TRUE, tstart = x_start, tend = x_end,
      arrow_active = data$config$memory$transfers$arrow,
      border_active = data$config$memory$transfers$border,
      total_active = show_transfer_total
    )
  }

  gow <- gow + coord_cartesian(
    xlim = c(x_start, x_end)
  )

  if (!legend) {
    gow <- gow + theme(legend.position = "none")
  } else {
    gow <- gow + theme(legend.position = "top")
  }

  starvz_log("Exit of events_memory_chart")
  return(gow)
}

geom_events <- function(main_data = NULL, data = NULL,
                        combined = FALSE, tstart = NULL,
                        tend = NULL, show_total = FALSE) {
  if (is.null(data)) stop("data is NULL when given to geom_events")

  starvz_log("Starting geom_events")

  dfw <- data

  dfl <- main_data$Link %>%
    filter(.data$Type %in% c(
      "MPI communication",
      "Intra-node data Fetch",
      "Intra-node data TaskPreFetch", "Intra-node data PreFetch"
    )) %>%
    mutate(Origin = str_replace(.data$Origin, "mpict", "MEMMANAGER0")) %>%
    mutate(Dest = str_replace(.data$Dest, "mpict", "MEMMANAGER0"))

  col_pos_1 <- data.frame(Container = unique(dfl$Dest)) %>%
    arrange(.data$Container) %>%
    rowid_to_column("Position")

  col_pos_2 <- data.frame(Container = unique(dfw$Container)) %>%
    arrange(.data$Container) %>%
    rowid_to_column("Position")

  if (nrow(col_pos_1) > nrow(col_pos_2)) {
    col_pos <- col_pos_1
  } else {
    col_pos <- col_pos_2
  }

  col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors = FALSE)
  dfw <- dfw %>% left_join(col_pos, by = c("Container" = "Container"))

  ret <- list()

  dfw$Height <- 1

  # Y axis breaks and their labels
  # Hardcoded here because yconf is specific to Resources Workers
  yconfm <- dfw %>%
    ungroup() %>%
    select(.data$Container, .data$Position, .data$Height) %>%
    distinct() %>%
    group_by(.data$Container) %>%
    arrange(.data$Container) %>%
    ungroup()

  yconfm <- yconfm %>%
    arrange(.data$Container)

  if (!combined) {
    ret[[length(ret) + 1]] <- scale_y_continuous(breaks = yconfm$Position + (yconfm$Height / 3), labels = yconfm$Container, expand = c(main_data$config$expand, 0))
  } else {
    dfw$Position <- dfw$Position - 0.3
  }

  # Y label
  ret[[length(ret) + 1]] <- ylab("Mem Managers")


  border <- 0
  if (main_data$config$memory$state$border) {
    border <- 1
  }

  # Add states
  ret[[length(ret) + 1]] <- geom_rect(
    data = dfw,
    aes(fill = .data$Type, xmin = .data$Start, xmax = .data$End, ymin = .data$Position, ymax = .data$Position + (2.0 - 0.2 - .data$Height)), color = "black", linetype = border, size = 0.4, alpha = 0.5
  )



  if (main_data$config$memory$state$text) {
    dx <- dfw %>%
      filter(.data$Type == "Allocating") %>%
      left_join(main_data$Data_handles, by = c("Handle" = "Handle")) %>%
      select(-.data$Tid, -.data$Src, -.data$Value)
    dx$Coordinates <- gsub(" ", "x", dx$Coordinates)

    ret[[length(ret) + 1]] <- geom_text(
      data = dx, colour = "black", fontface = "bold",
      aes(
        x = .data$Start + .data$Duration / 2, y = .data$Position + (2.0 - 0.2 - .data$Height) / 2,
        label = .data$Coordinates
      ), size = 5, alpha = 1.0,
      angle = main_data$config$memory$state$angle, show.legend = FALSE
    )
  }

  ret[[length(ret) + 1]] <- theme(
    legend.spacing.x = unit(2, "mm")
  )

  if (show_total) {
    select <- main_data$config$memory$state$select
    ms <- dfw %>%
      filter(.data$Type == select, .data$Start < tend, .data$End > tstart) %>%
      mutate(Start = ifelse(.data$Start < tstart, tstart, .data$Start)) %>%
      mutate(End = ifelse(.data$End > tend, tend, .data$End))

    # Calculate selected state % in time
    total_time <- tend - tstart

    ms <- ms %>%
      group_by(.data$Container, .data$Position) %>%
      summarize(percent_time = round((sum(.data$End - .data$Start) / total_time) * 100, 2))
    if (nrow(ms) != 0) {
      # ms[2] <- data.frame(lapply(ms[2], as.character), stringsAsFactors=FALSE);
      # ms <- ms %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
      ms$Value <- select
      globalEndTime <- tend - (tend - tstart) * 0.05
      ms$percent_time <- paste0(ms$percent_time, "%")
      ret[[length(ret) + 1]] <- geom_label(
        data = ms, x = globalEndTime, colour = "black", fontface = "bold",
        aes(y = .data$Position + 0.4, label = .data$percent_time, fill = .data$Value), alpha = 1.0, show.legend = FALSE, size = 5
      )
    }
  }

  starvz_log("Finishing geom_events")

  return(ret)
}

geom_links <- function(data = NULL, dfw = NULL, combined = FALSE,
                       tstart = NULL, tend = NULL,
                       arrow_active = FALSE,
                       border_active = FALSE,
                       total_active = FALSE) {
  starvz_check_data(data, tables = list("Link" = c("Dest", "Origin")))

  # Get the start info on states because link dont have nodes & Position
  # Consider that MPI comm are between RAMs (TODO: This is not true for direct inter-nodes GPU transfers)

  dfl <- data$Link %>%
    filter(.data$Type %in% c(
      "MPI communication",
      "Intra-node data Fetch",
      "Intra-node data TaskPreFetch", "Intra-node data PreFetch"
    )) %>%
    mutate(Origin = str_replace(.data$Origin, "mpict", "MEMMANAGER0")) %>%
    mutate(Dest = str_replace(.data$Dest, "mpict", "MEMMANAGER0"))

  starvz_log("Starting geom_links")

  # TODO MPI HERE
  col_pos_1 <- data.frame(Container = unique(dfl$Dest)) %>%
    arrange(.data$Container) %>%
    rowid_to_column("Position")

  col_pos_2 <- data.frame(Container = unique(dfw$Container)) %>%
    arrange(.data$Container) %>%
    rowid_to_column("Position")

  if (nrow(col_pos_1) > nrow(col_pos_2)) {
    col_pos <- col_pos_1
  } else {
    col_pos <- col_pos_2
  }

  col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors = FALSE)

  ret <- list()

  dfl <- dfl %>%
    left_join(col_pos, by = c("Origin" = "Container")) %>%
    rename(O_Position = .data$Position) %>%
    left_join(col_pos, by = c("Dest" = "Container")) %>%
    rename(D_Position = .data$Position)
  stride <- 0.3

  dfl$Height <- 1

  yconfm <- dfl %>%
    select(.data$Origin, .data$O_Position, .data$Height) %>%
    distinct() %>%
    group_by(.data$Origin) %>%
    arrange(.data$Origin) %>%
    ungroup()

  yconfm$Height <- 1
  yconfm$Origin <- lapply(yconfm$Origin, function(x) gsub("MEMMANAGER", "MM", x))

  if (combined) {
    ret[[length(ret) + 1]] <- scale_y_continuous(breaks = yconfm$O_Position, labels = yconfm$Origin, expand = c(0.10, 0.1))
    stride <- 0.0
  }

  if (!combined) {

    # dfw <- dfw %>% select(-Position) %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
    # Hardcoded here because yconf is specific to Resource Workers

    ret[[length(ret) + 1]] <- scale_y_continuous(breaks = yconfm$D_Position, labels = yconfm$Dest, expand = c(0.10, 0.5))

    # Color mapping
    # ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y label
    ret[[length(ret) + 1]] <- ylab("Transfers")
    stride <- 0.0
  }
  dfl$O_Position <- dfl$O_Position + stride
  dfl$D_Position <- dfl$D_Position + stride
  arrow_g <- NULL
  if (arrow_active) {
    arrow_g <- arrow(length = unit(0.15, "cm"))
  }
  if (border_active) {
    ret[[length(ret) + 1]] <- geom_segment(
      data = dfl,
      aes(x = .data$Start, xend = .data$End, y = .data$O_Position, yend = .data$D_Position),
      arrow = arrow_g, alpha = 0.5, size = 1.5, color = "black", show.legend = FALSE
    )
  }

  ret[[length(ret) + 1]] <- geom_segment(data = dfl, aes(
    x = .data$Start, xend = .data$End,
    y = .data$O_Position, yend = .data$D_Position, color = .data$Origin
  ), arrow = arrow_g, alpha = 1.0, show.legend = FALSE)
  selected_dfl <- dfl %>%
    filter(.data$End > tstart) %>%
    filter(.data$Start < tend)

  # ret[[length(ret)+1]] <- geom_text(data=dfl, colour = "black", fontface = "bold", aes(x = Start, y = O_Position, label=Key), size = 3, alpha=1.0, show.legend = FALSE);
  if (total_active) {
    total_links <- data.frame(with(selected_dfl, table(Origin)))
    if (nrow(total_links) != 0 & !combined) {
      total_links[1] <- data.frame(lapply(total_links[1], as.character), stringsAsFactors = FALSE)
      total_links <- total_links %>% left_join(col_pos, by = c("Origin" = "ResourceId"))

      globalEndTime <- tend - (tend - tstart) * 0.05

      ret[[length(ret) + 1]] <- geom_label(
        data = total_links, x = globalEndTime, colour = "white", fontface = "bold",
        aes(y = .data$Position, label = .data$Freq, fill = .data$Origin), alpha = 1.0, show.legend = FALSE
      )
    }
  }
  starvz_log("Finishing geom_links")

  return(ret)
}

#' Computes presence of handles over resources
#'
#' Use for precomputation of other memory-related functions
#'
#' @param data starvz_data with trace data
#' @return Time-Step aggregated handle presences
#' @include starvz_data.R
#' @examples
#' \donttest{
#' handles_presence_states(starvz_sample_lu)
#' }
#' @export
handles_presence_states <- function(data) {
  # Selecting only the data state events
  data$Events_data %>%
    filter(.data$Type == "data state invalid" |
      .data$Type == "data state owner" |
      .data$Type == "data state shared") %>%
    select(.data$Container, .data$Start, .data$Type, .data$Value) -> data_state_events

  end <- max(data$Starpu$End)

  fini_end <- unlist(end)

  data_state_events %>%
    mutate(rep = case_when(
      .data$Type == "data state owner" ~ 1,
      .data$Type == "data state invalid" ~ 2,
      TRUE ~ 5
    )) %>%
    group_by(.data$Value, .data$Container) %>%
    mutate(flow = c(1, diff(.data$rep))) %>%
    mutate(need = .data$flow != 0) %>%
    filter(.data$need == TRUE) %>%
    mutate(flow = c(1, diff(.data$rep)), t_diff = c(diff(.data$Start), 1)) %>%
    ungroup() %>%
    mutate(need = .data$flow != 0 & .data$t_diff > 0.001) %>%
    filter(.data$need == TRUE) %>%
    group_by(.data$Value, .data$Container) %>%
    mutate(flow = c(1, diff(.data$rep))) %>%
    ungroup() %>%
    mutate(need = .data$flow != 0) %>%
    filter(.data$need == TRUE) %>%
    group_by(.data$Value, .data$Container) %>%
    mutate(End = lead(.data$Start, default = unlist(fini_end))) %>%
    ungroup() %>%
    filter(.data$Type != "data state invalid") %>%
    select(-.data$rep, -.data$flow, -.data$t_diff, -.data$need) %>%
    group_by(.data$Value, .data$Container) -> f_data

  return(f_data)
}

#' Handles Name coordinates
#'
#' Give handles name by their coordinates
#'
#' @param df data_handle table of Starvz data
#' @return data_handle table  with new column Value with the name
#' @include starvz_data.R
#' @examples
#' \donttest{
#' data_name_coordinates(starvz_sample_lu$Data_handle)
#' }
#' @export
data_name_coordinates <- function(df) {
  df %>% mutate(Value = paste0("Memory Block ", .data$Coordinates, ""))
}

#' Handles Name Tag
#'
#' Give handles name by their tag
#'
#' @param df data_handle table of Starvz data
#' @return data_handle table  with new column Value with the name
#' @include starvz_data.R
#' @examples
#' \donttest{
#' data_name_tag(starvz_sample_lu$Data_handle)
#' }
#' @export
data_name_tag <- function(df) {
  if ("MPITag" %in% names(df)) {
    df %>% mutate(Value = paste0("Memory Block ", as.character(.data$MPITag), "")) -> ret
  } else {
    df %>% mutate(Value = paste0("Memory Block ", as.character(.data$Tag), "")) -> ret
  }
  return(ret)
}

#' Handles Name address
#'
#' Give handles name by their address
#'
#' @param df data_handle table of Starvz data
#' @return data_handle table  with new column Value with the name
#' @include starvz_data.R
#' @examples
#' \donttest{
#' data_name_handle(starvz_sample_lu$Data_handle)
#' }
#' @export
data_name_handle <- function(df) {
  df %>% mutate(Value = paste0("Memory Block ", .data$Handle, ""))
}

#' Pre-Computation for memory handles panel
#'
#' Use for precomputation of memory handles panel
#'
#' @param data starvz_data with trace data
#' @param name_func function to give names to handles
#' @return Pre-Computated data for panel_handles
#' @include starvz_data.R
#' @examples
#' \donttest{
#' pre_handle_gantt(data = starvz_sample_lu)
#' }
#' @export
pre_handle_gantt <- function(data, name_func = NULL) {
  # If not user defined lets try to select the best
  # function to give name to our handles
  # We will try first to use coordinates
  # good case in linear algebra where block dont repeat coordinates
  # if not available it will fail to TAGs, this is safe in recent
  # StarPU versions but may be unavailable
  # if these two fail the only option is to assume the handle address
  # that will not match between MPI executions...
  if (is.null(name_func)) {
    use_coord <- FALSE
    if ("Coordinates" %in% names(data$Data_handles)) {
      data$Data_handles %>% .$Coordinates -> cc
      if (!is.null(cc[[1]]) && !is.na(cc[[1]]) && cc[[1]] != "") {
        use_coord <- TRUE
      }
    }
    name_func <- data_name_handle
    if ("MPITag" %in% names(data$Data_handles)) {
      name_func <- data_name_tag
    }
    if (use_coord) {
      name_func <- data_name_coordinates
    }
  }

  data$Events_memory <- data$Events_memory %>%
    mutate(Type = as.character(.data$Type)) %>%
    mutate(Type = case_when(
      .data$Type == "Allocating Start" ~ "Allocation Request",
      .data$Type == "Request Created" ~ "Transfer Request",
      TRUE ~ .data$Type
    ))

  if (is.null(data$handle_states)) {
    data$handle_states <- handles_presence_states(data)
  }

  position <- data$handle_states %>%
    ungroup() %>%
    select(.data$Container) %>%
    distinct() %>%
    arrange(.data$Container) %>%
    mutate(y1 = 1:n())

  p_data <- data$handle_states %>%
    mutate(Colour = ifelse(.data$Type == "data state owner", "Owner", "Shared")) %>%
    inner_join(position, by = c("Container" = "Container")) %>%
    select(.data$Container, .data$Start, .data$End, .data$Value, .data$y1, .data$Colour)


  p_data %>%
    select(.data$Container, .data$Value, .data$y1) %>%
    distinct() -> pre_p_data

  data$Task_handles %>%
    filter(!is.na(.data$JobId)) %>%
    mutate(Modes = as.character(.data$Modes)) %>%
    inner_join(data$Tasks %>% filter(!is.na(.data$JobId)), by = c("JobId" = "JobId")) %>%
    select(.data$JobId, .data$Handles, .data$MPIRank, .data$MemoryNode, .data$Modes) %>%
    mutate(sContainer = paste0(.data$MPIRank, "_MEMMANAGER", .data$MemoryNode)) -> job_handles

  job_handles %>%
    inner_join(pre_p_data, by = c("Handles" = "Value")) %>%
    select(.data$Container, .data$sContainer, .data$JobId, .data$Handles, .data$y1, .data$MemoryNode, .data$Modes) %>%
    filter(.data$sContainer == .data$Container) %>%
    mutate(JobId = as.character(.data$JobId)) %>%
    inner_join(data$Application, by = c("JobId" = "JobId")) %>%
    select(.data$Container, .data$JobId, .data$Handles, .data$Start, .data$End, .data$y1, .data$Value, .data$Modes) %>%
    rename(Colour = .data$Value) %>%
    rename(Value = .data$Handles) -> jobs_p_data
  p_data$size <- 0.8
  jobs_p_data$size <- 0.6

  all_st_m_data <- bind_rows(p_data, jobs_p_data) %>%
    inner_join(data$Data_handle, by = c("Value" = "Handle")) %>%
    rename(Handle=.data$Value) %>%
    ungroup() %>%
    name_func() %>%
    select(.data$Container, .data$Start, .data$End, .data$Value, .data$y1, .data$Colour, .data$size, .data$JobId, .data$Modes) %>%
    group_by(.data$Value, .data$Container) %>%
    mutate(Modes = case_when(
      is.na(.data$Modes) ~ "1",
      .data$Modes == "R" ~ "0",
      .data$Modes == "W" ~ "1",
      .data$Modes == "RW" ~ "1"
    ))

  # Processing the Events: Request & Allocation

  data$Events_memory %>% filter(.data$Type == "Transfer Request") -> TR
  if (TR %>% nrow() > 0) {
    TR %>%
      mutate(P = substring(.data$Tid, 5)) %>%
      mutate(G = substr(.data$Container, 1, nchar(as.character(.data$Container)) - 1)) %>%
      mutate(Container = paste0(.data$G, .data$P)) %>%
      select(-.data$P, -.data$G) %>%
      select(-.data$Tid) %>%
      inner_join(data$Data_handle, by = c("Handle" = "Handle")) %>%
      inner_join(position, by = c("Container" = "Container")) %>%
      name_func() %>%
      select(.data$Container, .data$Type, .data$Start, .data$Value, .data$Info, .data$y1) %>%
      filter(.data$Type == "Transfer Request") %>%
      mutate(Pre = as.character(.data$Info)) -> request_events
  } else {
    request_events <- NULL
  }

  data$Task_handles %>%
    select(.data$Handles) %>%
    distinct() %>%
    .$Handles -> h_used
  data$Events_memory %>%
    filter(.data$Handle %in% h_used) %>%
    select(-.data$Tid) %>%
    inner_join(data$Data_handle, by = c("Handle" = "Handle")) %>%
    inner_join(position, by = c("Container" = "Container")) %>%
    name_func() %>%
    select(.data$Container, .data$Type, .data$Start, .data$Value, .data$Info, .data$y1) %>%
    filter(.data$Type == "Allocation Request") -> allocation_events

  allocation_events %>%
    group_by(.data$Value, .data$Container, .data$Type) %>%
    mutate(Old = lag(.data$Start, default = -5), R = abs(.data$Start - .data$Old)) %>%
    filter(.data$R > 1) %>%
    select(-.data$Old, -.data$R) -> allocation_events_filtered

  allocation_events_filtered$Pre <- "0"

  events_points <- bind_rows(request_events, allocation_events_filtered)

  # Processing Links (Transfers)
  data$Events_memory %>%
    filter(.data$Type == "DriverCopy Start") %>%
    select(.data$Handle, .data$Info, .data$Container) %>%
    mutate(Info = as.integer(as.character(.data$Info))) -> links_handles

  links <- data$Link %>%
    filter(.data$Type == "Intra-node data Fetch" |
      .data$Type == "Intra-node data PreFetch" |
      .data$Type == "Intra-node data TaskPreFetch") %>%
    select(-.data$Container, -.data$Size) %>%
    mutate(Con = as.integer(substring(.data$Key, 5))) %>%
    select(-.data$Key)

  final_links <- links %>%
    select(-any_of(c("Handle"))) %>%
    inner_join(links_handles, by = c("Con" = "Info", "Dest" = "Container")) %>%
    inner_join(position, by = c("Origin" = "Container")) %>%
    rename(origin_y = .data$y1) %>%
    inner_join(position, by = c("Dest" = "Container")) %>%
    rename(dest_y = .data$y1) %>%
    inner_join(data$Data_handle, by = c("Handle" = "Handle")) %>%
    name_func() %>%
    select(.data$Type, .data$Start, .data$End, .data$Value, .data$origin_y, .data$dest_y) %>%
    rename(Transfer = .data$Type)

  if ("MPI communication" %in% unique(data$Link$Type)) {
    mpi_links <- data$Link %>%
      select(-any_of(c("Handle"))) %>%
      filter(.data$Type == "MPI communication") %>%
      select(-.data$Container, -.data$Size) %>%
      mutate(Origin = str_replace(.data$Origin, "mpict", "MEMMANAGER0")) %>%
      mutate(Dest = str_replace(.data$Dest, "mpict", "MEMMANAGER0")) %>%
      inner_join(position, by = c("Origin" = "Container")) %>%
      rename(origin_y = .data$y1) %>%
      inner_join(position, by = c("Dest" = "Container")) %>%
      rename(dest_y = .data$y1) %>%
      mutate(Tag = as.numeric(as.character(.data$Tag))) %>%
      inner_join(data$Data_handle, by = c("Tag" = "MPITag")) %>%
      name_func() %>%
      select(.data$Type, .data$Start, .data$End, .data$Value, .data$origin_y, .data$dest_y) %>%
      rename(Transfer = .data$Type) %>%
      unique()

    all_links <- bind_rows(mpi_links, final_links)
  } else {
    all_links <- final_links
  }

  return(list(
    all_st_m_data = all_st_m_data,
    events_points = events_points,
    final_links = all_links,
    position = position,
    name_func = name_func
  ))
}

#' Create a space time visualization of data handles
#'
#' Visualizate data handles movement
#' To accelerate the process:\preformatted{
#' data$handle_states <- handles_presence_states(data)
#' data$handle_gantt_data <- pre_handle_gantt(data)
#' To Select time:
#' handles_gantt(data, JobId=c(jobid))
#' snap_data <- pre_snap(data, data$handle_states)
#' memory_snap(snap_data, 1000, tasks_size=200, step=1)}
#'
#' @param data starvz_data with trace data
#' @param JobId Select handles of jobid
#' @param lines vertical lines
#' @param lHandle select handles
#' @param name_func function to give names to handles
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_handles(data = starvz_sample_lu)
#' }
#' @export
panel_handles <- function(data, JobId = NA, lines = NA, lHandle = NA, name_func = NULL,
                          legend = data$config$handles$legend,
                          base_size = data$config$base_size,
                          expand_x = data$config$expand,
                          x_start = data$config$limits$start,
                          x_end = data$config$limits$end) {
  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }

  if (is.null(data$handle_gantt_data)) {
    data$handle_gantt_data <- pre_handle_gantt(data, name_func = name_func)
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  if (is.na(JobId) && is.na(lHandle)) {
    final_st_data <- data$handle_gantt_data$all_st_m_data
    final_events_data <- data$handle_gantt_data$events_points
    final_links_data <- data$handle_gantt_data$final_links
  } else if (!is.na(lHandle)) {
    final_st_data <- data$handle_gantt_data$all_st_m_data %>% filter(.data$Value %in% lHandle)
    final_events_data <- data$handle_gantt_data$events_points %>% filter(.data$Value %in% lHandle)
    final_links_data <- data$handle_gantt_data$final_links %>% filter(.data$Value %in% lHandle)
  } else {
    myjobid <- JobId
    data$Task_handles %>%
      filter(.data$JobId == myjobid) %>%
      inner_join(data$Data_handle, by = c("Handles" = "Handle")) %>%
      ungroup() -> xx
    data$handle_gantt_data$name_func(xx) %>%
      .$Value -> selected_handles

    final_st_data <- data$handle_gantt_data$all_st_m_data %>% filter(.data$Value %in% selected_handles)
    final_events_data <- data$handle_gantt_data$events_points %>% filter(.data$Value %in% selected_handles)
    final_links_data <- data$handle_gantt_data$final_links %>% filter(.data$Value %in% selected_handles)
  }

  events_colors <- brewer.pal(n = 7, name = "Dark2")

  extra <- c(
    "Owner" = "darksalmon",
    "Shared" = "steelblue1",
    " " = "white",
    "  " = "white"
  )

  data$Colors %>% select(.data$Value, .data$Color) -> lc

  lc %>%
    .$Color %>%
    setNames(lc %>% .$Value) -> fc

  fills <- append(extra, fc)

  colors <- c(
    "Allocation Request" = events_colors[[1]],
    "Transfer Request" = events_colors[[2]],
    "Intra-node data Fetch" = events_colors[[3]],
    "Intra-node data PreFetch" = events_colors[[4]],
    "Intra-node data TaskPreFetch" = events_colors[[7]],
    "MPI communication" = events_colors[[5]],
    "Last Job on same Worker" = events_colors[[6]]
  )

  arrow_g <- arrow(length = unit(rel(0.3), "cm"))

  p <- ggplot(data = final_st_data) +
    default_theme(base_size, expand_x) +
    geom_point(
      data = final_events_data,
      aes(
        x = .data$Start,
        y = .data$y1 + 0.4,
        colour = .data$Type,
        shape = .data$Pre
      ),
      size = 2.5, stroke = 1
    ) +
    geom_rect(aes(
      xmin = .data$Start,
      xmax = .data$End,
      fill = .data$Colour,
      ymin = .data$y1 + ifelse(is.na(.data$JobId), 0, 0.2),
      ymax = .data$y1 + .data$size,
      linetype = .data$Modes,
      alpha = ifelse(is.na(.data$JobId), "0", "1")
    ),
    colour = "black",
    size = 0.1
    ) +
    scale_fill_manual(
      name = "State         Task", values = fills,
      drop = FALSE,
      limits = names(fills),
      guide = guide_legend(
        nrow = 4, title.position = "top", order = 1,
        override.aes =
          list(shape = NA, colour = NA)
      )
    ) +
    scale_alpha_manual(
      values = c("0" = 1, "1" = 0.7), guide = "none"
    ) +
    scale_linetype_manual(
      values = c("1" = "solid", "0" = "dotted"), guide = "none"
    ) +
    scale_colour_manual(
      name = "Event", values = colors,
      drop = FALSE,
      breaks = c(
        "Allocation Request",
        "Transfer Request",
        "Intra-node data Fetch",
        "Intra-node data PreFetch",
        "Intra-node data TaskPreFetch",
        "MPI communication"
      ),
      limits = c(
        "Allocation Request",
        "Transfer Request",
        "Intra-node data Fetch",
        "Intra-node data PreFetch",
        "Intra-node data TaskPreFetch",
        "MPI communication"
      ),
      guide = guide_legend(
        nrow = 6, title.position = "top", order = 0,
        override.aes =
          list(
            arrow = NA, linetype = 0, shape = c(19, 19, 15, 15, 15, 15),
            yintercept = NA
          )
      )
    ) +
    scale_shape_manual(
      name = "Event Type", labels = c("Fetch", "Prefetch", "TaskPreFetch", "Idle Fetch"), values = c(19, 21, 22, 23),
      guide = guide_legend(nrow = 3, title.position = "top")
    ) +
    # Arrow Border
    geom_segment(
      data = final_links_data,
      aes(
        x = .data$Start,
        xend = .data$End,
        y = .data$origin_y + 0.4,
        yend = .data$dest_y + 0.4
      ),
      arrow = arrow_g,
      colour = "black",
      alpha = 0.8,
      size = rel(1.4)
    ) +
    geom_segment(
      data = final_links_data,
      aes(
        x = .data$Start,
        xend = .data$End,
        y = .data$origin_y + 0.4,
        yend = .data$dest_y + 0.4,
        colour = .data$Transfer
      ),
      arrow = arrow_g,
      size = rel(1), show.legend = FALSE
    ) +
    geom_segment(
      data = final_links_data,
      aes(
        x = .data$Start,
        xend = .data$End,
        y = .data$origin_y + 0.4,
        yend = .data$dest_y + 0.4,
        colour = .data$Transfer
      ),
      size = rel(1)
    ) +
    scale_y_continuous(
      breaks = data$handle_gantt_data$position$y1 + 0.4,
      labels = lapply(data$handle_gantt_data$position$Container, function(x) gsub("MEMMANAGER", "MM", x))
    ) +
    # geom_segment(data=handle_end_m,
    #             aes(x = End, y = MemoryNode+1, xend = End, yend = MemoryNode+1.8), color = "red") +
    facet_wrap(.data$Value ~ ., strip.position = "top", ncol = 1) +
    # scale_x_continuous(
    # breaks = c(5000, 5185, 5486, 5600, 5676, 5900),
    #  labels = function(x) format(x, big.mark = "", scientific = FALSE)
    # ) +
    # coord_cartesian(xlim=c(5000, 6000)) +
    # scale_color_manual(values=c("red"="red", "blue"="blue")) +
    # scale_colour_identity() +
    theme(
      strip.text.y = element_text(angle = 0),
      legend.box.margin = margin(-10, -10, -rel(1.0), -10),
      legend.background = element_rect(fill = "transparent")
    ) +
    ylab("Memory Manager")


  if (!is.na(lines)) {
    p <- p + geom_vline(data = lines, aes(xintercept = .data$x, color = .data$colors), alpha = 0.7, size = 1)
  }
  # if(!is.na(JobId)){
  #   my_job <- JobId
  #   data$Starpu %>% filter(JobId==my_job) %>% .$Start -> job_start
  #   data$Starpu %>% filter(JobId==my_job) %>% .$Duration -> job_dur
  #   data$Tasks %>% filter(JobId==my_job) %>% .$ MemoryNode -> job_node
  #   text <- data.frame(x=c(job_start+job_dur/2), y=c(job_node+1.4), text=c(my_job))
  #   p <- p + geom_text(data=text, aes(x=x, y=y, label=my_job), color="black", size=2,
  # 		  fontface="bold",
  # 	  alpha=0.8)
  # }

  p <- p + coord_cartesian(
    xlim = c(x_start, x_end)
  )

  if (!legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = "top")
  }

  return(p)
}

pre_snap <- function(data, f_data) {
  data$Data_handles %>%
    separate(.data$Coordinates, c("Y", "X"), extra = "drop", fill = "right") %>%
    mutate(X = as.numeric(.data$X), Y = as.numeric(.data$Y)) -> new_handles


  new_handles %>% select(.data$Handle, .data$X, .data$Y) -> hand

  f_data %>%
    ungroup() %>%
    select(.data$Container) %>%
    distinct() %>%
    .$Container -> cont
  hand <- hand %>% mutate(Container = list(cont))
  hand %>% unnest(cols = c(.data$Container)) -> hand

  f_data %>% mutate(st = ifelse(.data$Type == "data state owner", "Owner", "Shared")) -> d_presence

  data$Application %>%
    mutate(JobId = .data$JobId) %>%
    inner_join(data$Tasks, by = c("JobId" = "JobId")) %>%
    select(.data$Start, .data$End, .data$Value, .data$JobId, .data$MemoryNode, .data$MPIRank) %>%
    inner_join(data$Task_handles, by = c("JobId" = "JobId")) %>%
    mutate(Container = ifelse(.data$MPIRank >= 0, paste0(.data$MPIRank, "_MEMMANAGER", .data$MemoryNode), paste0("MEMMANAGER", .data$MemoryNode))) %>%
    select(.data$Handles, .data$Modes, .data$Start, .data$End, .data$Value, .data$JobId, .data$Container) -> tasks

  return(list(d_presence, hand, tasks))
}

#' Create a snapshot of memory
#'
#' Visualizate memory in a specific time
#'
#' @param data starvz_data with trace data
#' @param selected_time time
#' @param step for discreate events
#' @param tasks_size size of tasks in the visualization
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @param x_start X-axis start value
#' @param x_end X-axis end value
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_memory_snap(data = starvz_sample_lu, 100, 10)
#' }
#' @export
panel_memory_snap <- function(data, selected_time, step,
                              legend = data$config$memory_snap$legend,
                              base_size = data$config$base_size,
                              expand_x = data$config$expand,
                              x_start = data$config$limits$start,
                              x_end = data$config$limits$end,
                              tasks_size = 30) {
  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(x_start) || (!is.na(x_start) && !is.numeric(x_start))) {
    x_start <- NA
  }

  if (is.null(x_end) || (!is.na(x_end) && !is.numeric(x_end))) {
    x_end <- NA
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }

  if (is.null(data$handle_states)) {
    data$handle_states <- handles_presence_states(data)
  }

  if (is.null(data$pre_snap)) {
    data$pre_snap <- pre_snap(data, data$handle_states)
  }

  extra <- c(
    "Owner" = "darksalmon",
    "Shared" = "steelblue1"
  )

  data$Colors %>% select(.data$Value, .data$Color) -> lc

  lc %>%
    .$Color %>%
    setNames(lc %>% .$Value) -> fc

  fills <- append(fc, extra)

  data$pre_snap[[1]] %>%
    ungroup() %>%
    filter(.data$Start < selected_time, .data$End > selected_time) %>%
    inner_join(data$pre_snap[[2]], by = c("Value" = "Handle", "Container" = "Container")) %>%
    mutate(Container = gsub("MEMMANAGER", "MM", .data$Container)) -> d_presence

  task_presence <- data$pre_snap[[3]] %>%
    ungroup() %>%
    filter(.data$Start <= selected_time, .data$End >= selected_time) %>%
    inner_join(data$pre_snap[[2]], by = c("Handles" = "Handle", "Container" = "Container")) %>%
    mutate(Container = gsub("MEMMANAGER", "MM", .data$Container))

  task_presence_alpha <- data$pre_snap[[3]] %>%
    ungroup() %>%
    filter(.data$Start > selected_time - step, .data$End <= selected_time) %>%
    inner_join(data$pre_snap[[2]], by = c("Handles" = "Handle", "Container" = "Container")) %>%
    mutate(Container = gsub("MEMMANAGER", "MM", .data$Container))

  max_x <- data$pre_snap[[2]] %>%
    arrange(-.data$X) %>%
    slice(1) %>%
    .$X %>%
    unlist()

  p <- ggplot(d_presence, aes(.data$Y, .data$X)) +
    geom_tile(aes(fill = .data$st),
      colour = "white"
    ) +
    geom_point(
      data = task_presence_alpha,
      aes(
        fill = .data$Value,
        x = .data$Y,
        y = .data$X,
        shape = .data$Modes
      ),
      colour = "black",
      size = (tasks_size / max_x), stroke = 0.2, alpha = 0.3
    ) +
    geom_point(
      data = task_presence,
      aes(
        fill = .data$Value,
        x = .data$Y,
        y = .data$X,
        shape = .data$Modes
      ),
      colour = "black",
      size = (tasks_size / max_x), stroke = 0.2
    ) +
    scale_shape_manual(
      values = c("R" = 21, "W" = 22, "RW" = 22), drop = FALSE,
      limits = c("R", "W", "RW"),
      guide = guide_legend(title.position = "top", nrow = 2)
    ) +
    scale_fill_manual(
      name = "State", values = fills, drop = FALSE,
      limits = names(fills),
      guide = guide_legend(
        title.position = "top", override.aes =
          list(shape = NA, stroke = 1)
      )
    ) +
    scale_y_reverse(limits = c(max_x + 0.6, -0.6), expand = c(0, 0)) +
    facet_wrap(~Container) +
    default_theme(base_size, expand_x) +
    labs(x = "Block X Coordinate", y = "Block Y Coordinate") +
    theme(
      plot.margin = unit(c(0, 10, 0, 0), "mm"),
      legend.box.margin = margin(-5, 0, -rel(1), 0),
      # strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
      legend.background = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "mm")
    )

  p <- p + coord_cartesian(
    xlim = c(x_start, x_end)
  )

  if (!legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = "top")
  }

  return(p)
}

#' Create multiple snapshot of memory
#'
#' Create multiple visualizations of memory
#' Useful for continuing views
#'
#' @param data starvz_data with trace data
#' @param start start time
#' @param end end time
#' @param step between snaps
#' @param path path to save files
#' @param scale for ggsave
#' @param width for ggsave
#' @param height for ggsave
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \dontrun{
#' multiple_snaps(data = starvz_sample_lu, 100, 200, 10, ".")
#' }
#' @export
multiple_snaps <- function(data = NULL,
                           start = 0,
                           end = 1000,
                           step = 100,
                           path = ".",
                           scale = 8,
                           width = 4,
                           height = 3) {
  if (is.null(data$handle_states)) {
    data$handle_states <- handles_presence_states(data)
  }

  if (is.null(data$pre_snap)) {
    data$pre_snap <- pre_snap(data, data$handle_states)
  }
  se <- seq(start, end, step)
  se <- c(se, end)
  i <- 1
  for (time in se) {
    p <- panel_memory_snap(data, time, step, tasks_size = 40)
    p <- p + ggtitle(paste0("Time: ", as.character(time)))
    ggsave(paste0(path, i, ".png"), plot = p, scale = scale, width = width, height = height, units = "cm")
    i <- i + 1
  }
}

#' Heatmap of memory presence
#'
#' Visualizate the presence of memory handles across memory managers
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_memory_heatmap(data = starvz_sample_lu)
#' }
#' @export
panel_memory_heatmap <- function(data,
                                 legend = data$config$memory_heatmap$legend,
                                 base_size = data$config$base_size,
                                 expand_x = data$config$expand) {
  if (is.null(data$handle_states)) {
    data$handle_states <- handles_presence_states(data)
  }

  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(base_size) || !is.numeric(base_size)) {
    base_size <- 22
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }


  data$handle_states %>% mutate(Duration = .data$End - .data$Start) -> d_data

  data$Data_handles %>%
    separate(.data$Coordinates, c("Y", "X"), extra = "drop", fill = "right") %>%
    mutate(across(c(.data$X, .data$Y), as.integer)) %>%
    select(.data$Handle, .data$X, .data$Y) -> hand
  d_data %>%
    group_by(.data$Value, .data$Container) %>%
    summarize(sum = sum(.data$Duration), n = n()) %>%
    inner_join(hand, by = c("Value" = "Handle")) -> d_presence
  d_presence %>%
    ungroup() %>%
    group_by(.data$Value) %>%
    mutate(per = .data$sum / sum(.data$sum)) %>%
    mutate(Container = gsub("MEMMANAGER", "MM", .data$Container)) -> d_percent

  max_x <- data[[2]] %>%
    arrange(-.data$X) %>%
    slice(1) %>%
    .$X %>%
    unlist()

  panel <- ggplot(d_percent, aes(.data$Y, .data$X)) +
    default_theme(base_size, expand, skip_x = TRUE) +
    geom_tile(aes(fill = .data$per),
      colour = "white"
    ) +
    scale_fill_gradient(
      name = "Presence Percentage [%]",
      breaks = c(0.25, 0.5, 0.75, 1.00),
      labels = c("25%", "50%", "75%", "100%"),
      limits = c(0.0, 1),
      low = "white",
      high = "steelblue",
      guide = guide_legend(title.position = "top")
    ) +
    scale_y_reverse(limits = c(max_x + 0.6, -0.6), expand = c(0, 0)) +
    scale_x_continuous(limits = c(-0.6, max_x + 0.6), expand = c(0, 0)) +
    facet_wrap(~ .data$Container) +
    labs(x = "Block X Coordinate", y = "Block Y Coordinate")

  if (legend) {
    panel <- panel + theme(
      legend.position = "top",
      panel.spacing = unit(1, "mm")
    ) +
      guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
  } else {
    panel <- panel + theme(legend.position = "none")
  }

  return(panel)
}


#' Show the 2D MPI distribution
#'
#' Visualizate the data distribution across nodes of 2D structured data
#'
#' @param data starvz_data with trace data
#' @param legend enable/disable legends
#' @param base_size base_size base font size
#' @param expand_x expand size for scale_x_continuous padding
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_dist2d(data = starvz_sample_lu)
#' }
#' @export
panel_dist2d <- function(data,
                         legend = data$config$dist2d$legend,
                         base_size = data$config$base_size,
                         expand_x = data$config$expand) {
  if (is.null(legend) || !is.logical(legend)) {
    legend <- TRUE
  }

  if (is.null(base_size) || !is.numeric(base_size)) {
    base_size <- 22
  }

  if (is.null(expand_x) || !is.numeric(expand_x)) {
    expand_x <- 0.05
  }

  data$Data_handle %>%
    .$MPIOwner %>%
    unique() %>%
    length() -> n_nodes

  panel <- data$Data_handle %>%
    select(.data$MPIOwner, .data$Coordinates) %>%
    unique() %>%
    mutate(MPIOwner = factor(.data$MPIOwner)) %>%
    separate(.data$Coordinates, c("Y", "X"), extra = "drop", fill = "right") %>%
    mutate(X = as.numeric(.data$X), Y = as.numeric(.data$Y)) %>%
    ggplot(aes(x = .data$X, y = .data$Y, fill = .data$MPIOwner)) +
    default_theme(base_size, expand, skip_x = TRUE) +
    geom_tile(alpha = 0.8) +
    geom_text(aes(label = factor(.data$MPIOwner)), size = 2) +
    scale_y_reverse(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    guides(fill = guide_legend(ncol = 15)) +
    xlab("Column") +
    ylab("Line")

  if (requireNamespace("viridis", quietly = TRUE)) {
    panel <- panel + viridis::scale_fill_viridis(name = "Node", breaks = seq(0, n_nodes), discrete = TRUE)
  } else {
    starvz_warn("In panel_dist2d: We suggest package viridis for high number of nodes")
  }

  if (legend) {
    panel <- panel + theme(legend.position = "top")
  } else {
    panel <- panel + theme(legend.position = "none")
  }

  return(panel)
}
