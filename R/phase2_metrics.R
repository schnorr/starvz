#' @include starvz_data.R

abe_cpu_cuda <- function(dfl, debug = FALSE) {
  result <- abe_cpu_cuda_inner(dfl, debug)
  ret <- result %>% pull(.data$Result)
  return(tibble(Result = ret[[1]]$objval))
}

abe_cpu_cuda_details <- function(dfl, Colors = NULL, debug = FALSE) {
  node <- dfl %>%
    slice(1) %>%
    pull(.data$Node)
  lpresult <- abe_cpu_cuda_inner(dfl, debug)

  # Extract the solution from the LP
  lpresult %>% pull(.data$Result) -> result
  lpresult %>%
    select("Types", "Values") %>%
    unnest(cols = c("Types")) %>%
    unnest(cols = c("Values")) %>%
    mutate(
      Count = result[[1]]$solution[1:n()],
      Estimation = TRUE
    ) %>%
    rename(
      ResourceType = "Types",
      Value = "Values"
    ) -> ret

  # Get the actual counts and merge everything together
  dfl %>%
    mutate(ResourceType = as.character(.data$ResourceType)) %>%
    group_by(.data$ResourceType, .data$Value) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    mutate(Estimation = FALSE) %>%
    bind_rows(ret) -> ret

  ret %>%
    left_join(Colors, by = c("Value" = "Value")) -> ret

  return(ret)
}

abe_cpu_cuda_inner <- function(dfl, debug = FALSE) {
  columnNames <- c("Node", "Resource", "ResourceType", "Value", "Duration")
  if (!all(columnNames %in% names(dfl))) {
    print(paste("Obligatory columns names not present. They are ", columnNames))
    return(NULL)
  }

  #############
  # Input Section

  # The amount of nodes
  nnodes <- dfl %>%
    select("Node") %>%
    unique() %>%
    .$Node %>%
    sort() %>%
    length()
  # The amount of resources
  df1.res_quantity <- dfl %>%
    select("Node", "Resource", "ResourceType") %>%
    unique() %>%
    group_by(.data$ResourceType) %>%
    summarize(Quantity = n())

  # The mean duration of each task per resource
  df1.num_mean <- dfl %>%
    group_by(.data$ResourceType, .data$Value) %>%
    summarize(Num = n(), Mean = mean(.data$Duration)) %>%
    as.data.frame()

  if (debug) {
    print("ABE: Inicial metrics (v1)")
    print(nnodes)
    print(df1.res_quantity)
    print(df1.num_mean)
  }

  #############
  # Derived variables

  # Initial parameters simplification
  values <- df1.num_mean %>%
    select("Value") %>%
    arrange(.data$Value) %>%
    .$Value %>%
    unique()
  types <- df1.num_mean %>%
    select("ResourceType") %>%
    arrange(.data$ResourceType) %>%
    .$ResourceType %>%
    unique() %>%
    as.character()
  names <- c(unlist(lapply(types, function(x) paste(x, values, sep = "_"))), "Time")
  nnames <- length(names)
  nvalues <- length(values)
  ntypes <- length(types)
  size <- nvalues * ntypes

  if (debug) {
    print("ABE: Inicial metrics (v2)")
    print(paste("values:", values))
    print(paste("types:", types))
    print(paste("names:", names))
    print(paste("nnames:", names))
    print(paste("nvalues:", nvalues))
    print(paste("ntypes:", ntypes))
    print(paste("size:", size))
  }

  ############
  # The three parts

  # Part 1
  m.con1 <- cbind(do.call(cbind, replicate(ntypes, diag(1, nvalues, nvalues), simplify = FALSE)), rep(0, nvalues)) %>% set_colnames(names)
  m.dir1 <- rep("=", nvalues)
  m.rhs1 <- df1.num_mean %>%
    group_by(.data$Value) %>%
    summarize(Sum = sum(.data$Num)) %>%
    arrange(.data$Value) %>%
    .$Sum

  if (debug) {
    print(m.con1)
    print(m.dir1)
    print(m.rhs1)
    print("End of Part 1")
  }

  # Part 2
  m <- df1.num_mean %>%
    arrange(.data$ResourceType, .data$Value) %>%
    pivot_wider(
      id_cols = "ResourceType", names_from = "Value", values_from = "Mean",
      names_sort = TRUE, values_fill = 1e10
    ) %>%
    select(-"ResourceType") %>%
    set_colnames(NULL) %>%
    as.matrix()
  M <- matrix(data = rep(0, ntypes * (nnames - 1)), nrow = ntypes)
  for (i in 1:ntypes) { # for each ResourceType
    for (j in 1:nvalues) { # for each Kernel
      M[i, nvalues * (i - 1) + j] <- m[i, j]
    }
  }
  m.con2 <- cbind(M, df1.res_quantity %>%
    arrange(.data$ResourceType) %>%
    mutate(Quantity = as.numeric(.data$Quantity) * -1) %>% .$Quantity) # %>% set_colnames(names);
  m.dir2 <- rep("<=", length(types))
  m.rhs2 <- rep(0, length(types))

  if (debug) {
    print(m.con2)
    print(m.dir2)
    print(m.rhs2)
    print("End of Part 2")
  }

  # Part 3
  m.con3 <- cbind(diag(1, size), rep(0, size)) # %>% set_colnames(names);
  m.dir3 <- rep(">=", size)
  m.rhs3 <- rep(0, size)

  if (debug) {
    print(m.con3)
    print(m.dir3)
    print(m.rhs3)
    print("End of Part 3")
  }

  ##########
  # Final + objective function

  # Row bind and concatenate everything:
  m.con <- rbind(m.con1, m.con2, m.con3)
  m.dir <- c(m.dir1, m.dir2, m.dir3)
  m.rhs <- c(m.rhs1, m.rhs2, m.rhs3)

  # Define the objective function
  f.obj <- c(rep(0, length(values) * length(types)), 1)

  # Call lp
  result <- lp("min", f.obj, m.con, m.dir, m.rhs)
  return(tibble(Result = list(result), Values = list(values), Types = list(types)))
}

hl_per_node_ABE <- function(dfw = NULL) {
  if (is.null(dfw)) stop("Input data frame is NULL")

  dftemp <- dfw %>%
    filter(grepl("CPU|CUDA", .data$ResourceId)) %>%
    select("Node", "Resource", "ResourceType", "Duration", "Value", "Position", "Height")
  pernodeABE <- dftemp %>%
    group_by(.data$Node) %>%
    do(abe_cpu_cuda(.data))
  # Y position
  pernodeABE <- dftemp %>%
    group_by(.data$Node) %>%
    summarize(MinPosition = min(.data$Position), MaxPosition = max(.data$Position) + min(.data$Height) / 1.25) %>%
    left_join(pernodeABE, by = "Node")

  return(pernodeABE)
}

hl_per_node_ABE_details <- function(data = NULL) {
  if (is.null(data$Application)) stop("Input data is NULL")

  data$Application %>%
    filter(grepl("CPU|CUDA", .data$ResourceId)) %>%
    select("Node", "Resource", "ResourceType", "Duration", "Value", "Position", "Height") %>%
    group_by(.data$Node) %>%
    do(abe_cpu_cuda_details(.data, Colors = data$Colors)) %>%
    ungroup()
}
hl_global_cpb <- function(data = NULL) {
  if (is.null(data)) {
    return(NULL)
  }

  dfdag <- data$Dag %>% filter(!is.na(.data$Dependent))

  # Create unique _integer_ identifiers
  identifiers <- c((dfdag %>% .$JobId), (dfdag %>% .$Dependent)) %>%
    unique() %>%
    tibble(JobId = .) %>%
    mutate(JobIdInt = 1:n())

  # Create the structure necessary for calling the Rcpp CPB function
  dfdag %>%
    # Select only the necessary information
    select("JobId", "Dependent", "Start", "Cost", "Value") %>%
    # Change to the appropriate data type to enable left_join
    mutate(JobId = as.character(.data$JobId)) %>%
    # Merge with identifiers so the JobId gets an unique id
    left_join(identifiers, by = c("JobId" = "JobId")) %>%
    # Rename the new column
    rename(JobIdIntU = "JobIdInt") %>%
    # Merge with identifiers _again_ so the Dependent gets an unique id
    left_join(identifiers, by = c("Dependent" = "JobId")) %>%
    # Rename the new column _again_
    rename(DepIntU = "JobIdInt") %>%
    # Re-ordering
    select("JobId", "JobIdIntU", "Dependent", "DepIntU", "Start", "Cost", "Value") %>%
    # Rename things
    rename(
      JobIdStr = "JobId", DepStr = "Dependent",
      JobId = "JobIdIntU", Dependent = "DepIntU"
    ) %>%
    as_tibble() -> appdagcost

  # Define the origin (the first task in the trace)
  stask <- appdagcost %>%
    arrange(.data$Start) %>%
    slice(1) %>%
    as.data.frame() %>%
    .$JobId
  tasksOnCriticalPath <- sort(boost_shortest_path(stask, appdagcost))
  tasksOnCriticalPath <- identifiers %>%
    filter(.data$JobIdInt %in% tasksOnCriticalPath) %>%
    .$JobId %>%
    unique()

  # Gather the CPB, sum the durations, return
  States <- bind_rows(data$Application, data$Starpu)
  States %>%
    filter(.data$JobId %in% tasksOnCriticalPath) %>%
    pull(.data$Duration) -> sel1
  sel1 %>% sum() -> sum1
  if (!is.null(data$Link)) {
    data$Link %>%
      filter(.data$Key %in% tasksOnCriticalPath) %>%
      pull(.data$Duration) -> sel2
    sel2 %>% sum() -> sum2
    ret <- list(
      "CPBMPI" = sum1 + sum2,
      "NMPI" = sel2 %>% length()
    )
  } else {
    ret <- list()
  }
  ret <- c(ret, list(
    "CPB" = sum1,
    "tasks" = tasksOnCriticalPath
  ))
  return(ret)
}

hl_global_abe <- function(dfw = NULL) {
  if (is.null(dfw)) stop("Input data frame is NULL")

  dfw %>%
    filter(grepl("CPU|CUDA", .data$ResourceId)) %>%
    select("Node", "Resource", "ResourceType", "Duration", "Value", "Position", "Height") %>%
    do(abe_cpu_cuda(.data)) -> globalABE

  return(globalABE)
}

calculate_resource_idleness <- function(dfw = NULL, max_only = TRUE) {
  if (is.null(dfw)) stop("Input data frame is NULL")

  # Get only application states
  dfw <- dfw %>%
    distinct(.data$ResourceType, .data$ResourceId, .data$Node,
      .data$Position, .data$Height, .data$JobId,
      .data$Value, .data$Duration,
      .keep_all = TRUE
    )

  # Obtain time interval
  tstart <- dfw %>%
    .$Start %>%
    min()
  tend <- dfw %>%
    .$End %>%
    max()

  # Calculate resources idleness
  total_time <- tend - tstart
  dfw <- dfw %>%
    group_by(.data$ResourceType, .data$ResourceId, .data$Node, .data$Position, .data$Height) %>%
    summarize(
      Idleness = round((1 - (sum(.data$End - .data$Start) / total_time)) * 100, 2),
      End = max(.data$End)
    )
  if (max_only) {
    dfw <- dfw %>%
      group_by(.data$Node, .data$ResourceType) %>%
      filter(.data$Idleness %in% c(max(.data$Idleness))) # %>% #, min(Idleness))) %>%
  }
  dfw %>% ungroup()
}

geom_idleness <- function(data = NULL) {
  if (is.null(data$Application)) stop("data provided for geom_idleness is NULL")

  dfidle <- calculate_resource_idleness(data$Application %>% filter(.data$Start >= 0), !data$config$st$idleness_all)

  bsize <- data$config$base_size
  expand <- data$config$expand
  idleness_factor <- data$config$idleness_factor

  globalEndTime <- dfidle %>%
    pull(.data$End) %>%
    na.omit() %>%
    max()
  ret <- NULL
  ret <- geom_label(
    data = dfidle,
    # The size of the idle values for each resource
    size = bsize / idleness_factor,
    # The X position of each one
    x = 0, # 2.5% before 0.0
    hjust = 0,
    fill = "white",
    fontface = "bold",
    # The Y position (depends on the Resource, so use "aes"
    aes(
      y = .data$Position + (.data$Height / 2.5), # vertical
      # The idleness number followed by % as text
      label = gsub("$", "%", .data$Idleness)
    )
  )
  return(ret)
}
geom_makespan <- function(dfw = NULL, bsize = 22) {
  if (is.null(dfw)) stop("data provided for geom_makespan is NULL")

  tend <- dfw %>%
    pull(.data$End) %>%
    max()
  starvz_log(paste("Makespan is", tend))
  height <- dfw %>%
    select("Position") %>%
    na.omit() %>%
    pull(.data$Position) %>%
    max()
  ret <- geom_text(data = data.frame(), x = tend, y = height * .5, aes(label = round(tend, 0)), angle = 90, size = bsize / 4)
  return(ret)
}



geom_cpb <- function(data = NULL) {
  if (is.null(data)) stop("data is NULL when given to geom_cpb")
  if (is.null(data$Dag)) {
    starvz_warn("CPB is active but data$Dag is NULL")
    return(list())
  }

  # Calculate the global CPB
  cpbs <- hl_global_cpb(data)

  ret <- list()
  if (data$config$st$cpb) {
    ret <- c(ret, geom_cpb_internal(data$Application, cpbs$CPB, "CPB:", bsize = data$config$base_size))
  }
  if (data$config$st$cpb_mpi$active) {
    if (is.null(data$config$st$cpb_mpi$tile_size)) {
      starvz_warn("CPB_MPI is active and st$cpb_mpi$tile_size is NULL")
    }
    if (is.null(data$config$st$cpb_mpi$bandwidth)) {
      starvz_warn("CPB_MPI is active and st$cpb_mpi$bandwidth is NULL")
    }
    tile_size <- data$config$st$cpb_mpi$tile_size
    bandwidth <- data$config$st$cpb_mpi$bandwidth
    cpbmpit <- cpbs$CPB + cpbs$NMPI * (tile_size * tile_size * 8) / bandwidth / 1000000
    ret <- c(ret, geom_cpb_internal(data$Application, cpbs$CPBMPI, "CPB-MPI:", bsize = data$config$base_size))
    if (data$config$st$cpb_mpi$theoretical) {
      ret <- c(ret, geom_cpb_internal(data$Application, cpbmpit, "CPB-MPI*:", bsize = data$config$base_size))
    }
  }
  return(ret)
}

geom_cpb_internal <- function(dfw = NULL, value = NULL, desc = NULL, bsize = 22) {
  if (!is.null(value) && !is.null(desc)) {
    minPos <- dfw %>%
      select("Position") %>%
      na.omit() %>%
      pull(.data$Position) %>%
      min()
    maxPos <- dfw %>%
      select("Position") %>%
      na.omit() %>%
      pull(.data$Position) %>%
      max()
    corr <- dfw %>%
      select("Height") %>%
      na.omit() %>%
      pull(.data$Height) %>%
      min()
    ret <- list(
      # the gray band
      geom_segment(
        data = data.frame(
          x = value,
          xend = value,
          y = minPos,
          yend = maxPos + corr / 1.25
        ),
        aes(
          x = .data$x,
          xend = .data$xend,
          y = .data$y,
          yend = .data$yend
        ),
        linewidth = 5,
        alpha = .7,
        color = "gray"
      ),
      # the text on top of the gray brand
      geom_text(
        data = data.frame(
          x = value,
          y = minPos + (maxPos - minPos) / 2
        ),
        aes(x = .data$x, y = .data$y),
        label = paste0(desc, " ", round(value, 0)),
        angle = 90,
        color = "black",
        size = bsize / 5
      )
    )
    return(ret)
  }
  return(list())
}
geom_abe <- function(data = NULL) {
  if (is.null(data)) stop("data is NULL when given to geom_abe")

  # states and k
  pernodeABEdf <- hl_per_node_ABE(data$Application)
  return(geom_abe_internal(pernodeABEdf,
    base_size = data$config$base_size,
    abesize = data$config$st$abe$size,
    bar_color = data$config$st$abe$bar_color,
    text = data$config$st$abe$text,
    label = data$config$st$abe$label
  ))

  return(list())
}

geom_abe_internal <- function(pernodeABEdf = NULL,
                              base_size = 22,
                              abesize = 5,
                              bar_color = "grey",
                              text = TRUE,
                              label = TRUE) {
  bsize <- base_size / 5

  if (!is.null(pernodeABEdf)) {
    ret <- list(
      geom_segment(
        data = pernodeABEdf, aes(
          x = .data$Result, xend = .data$Result,
          y = .data$MinPosition, yend = .data$MaxPosition
        ),
        linewidth = abesize, alpha = .7, color = bar_color
      )
    )
    if (text) {
      ret <- list(
        ret,
        geom_text(data = pernodeABEdf, aes(
          x = .data$Result,
          y = .data$MinPosition + (.data$MaxPosition - .data$MinPosition) / 2,
          label = paste0(ifelse(label, "ABE: ", ""), round(.data$Result, 0))
        ), angle = 90, color = "black", size = bsize)
      )
    }
    return(ret)
  }
}

#' Create a plot with the solution computed by ABE
#'
#' Plot per-node and per-tasktype repartion among resource types
#'
#' @param data starvz_data with trace data
#' @param base_size base_size base font size
#' @return A ggplot object
#' @include starvz_data.R
#' @examples
#' \donttest{
#' panel_abe_solution(data = starvz_sample_lu)
#' }
#' @export
panel_abe_solution <- function(data,
                               base_size = data$config$base_size) {
  starvz_check_data(data, tables = list(
    "Colors" = c("Value", "Color", "Use"),
    "Application" = c("ResourceId", "Node", "Resource", "ResourceType", "Duration", "Value", "Position", "Height")
  ))

  sol <- hl_per_node_ABE_details(data)
  nnodes <- length(unique(sol$Node))
  colors <- extract_colors(data$Application, data$Colors)

  ggplot(data = sol, aes(x = .data$ResourceType, y = .data$Count, fill = .data$Value)) +
    geom_bar(data = sol %>% filter(.data$Estimation == FALSE), position = "stack", stat = "identity", alpha = .6) +
    geom_point(data = sol %>% filter(.data$Estimation == TRUE), shape = 21, color = "black", size = 2, stroke = 1, alpha = .7) +
    facet_wrap(Node ~ Value, scales = "free", nrow = nnodes) +
    scale_fill_manual(values = colors) +
    theme_bw(base_size = base_size) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

# Nesi Implementation of ABE
# Resources extra info is the time per task type per resource type with:
# ResourceType, codelet, mean
# Tasks per node is how much tasks per node and type
# Node, Value, freq

starpu_freq_abe_all_info <- function(tasks_per_node, resources_extra_info) {
  resources_extra_info %>%
    select("ResourceType", "codelet") %>%
    unique() %>%
    arrange(.data$codelet) -> codelet_combination

  number_codelet_combination <- codelet_combination %>% nrow()

  f.obj <- c(rep(0, number_codelet_combination), 1)

  codelets <- codelet_combination %>%
    select("codelet") %>%
    unique() %>%
    .$codelet

  set_codelet <- function(codelet_name, codelet_combination) {
    codelet_combination %>%
      mutate(v = case_when(
        .data$codelet == codelet_name ~ 1L,
        TRUE ~ 0L
      )) %>%
      pull(.data$v) -> value
    # add zero for time
    return(c(value, 0))
  }

  types <- resources_extra_info %>%
    arrange(.data$codelet) %>%
    select("ResourceType") %>%
    unique() %>%
    .$ResourceType

  set_resource <- function(resource_name, resources_extra_info) {
    resources_extra_info %>%
      mutate(v = case_when(
        .data$ResourceType == resource_name ~ (mean / n),
        TRUE ~ 0
      )) %>%
      pull(.data$v) -> value
    # add -1 for time
    return(c(value, -1))
  }

  cond1 <- unlist(lapply(codelets, set_codelet, codelet_combination))

  cond2 <- unlist(lapply(types, set_resource, resources_extra_info %>% arrange(.data$codelet)))
  total_rows <- length(codelets) + length(types)

  f.con <- matrix(c(cond1, cond2), nrow = total_rows, byrow = TRUE)

  f.dir <- c(rep("=", length(codelets)), rep("<=", length(types)))

  tasks_per_node %>%
    arrange(.data$Value) %>%
    .$freq -> tasks_freq

  f.rhs <- c(tasks_freq, rep(0, length(types)))

  result <- lp("min", f.obj, f.con, f.dir, f.rhs)

  return(result)
}

# Only return obj
starpu_freq_abe <- function(tasks_per_node, resources_extra_info) {
  result <- starpu_freq_abe_all_info(tasks_per_node, resources_extra_info)
  return(result$objval)
}

# Compute ABE per slice
starpu_apply_abe_per_slice <- function(step, resources_extra_info, tasks, max_res = NULL) {
  if (!is.null(max_res)) {
    max_res %>%
      filter(.data$Step == step) %>%
      .$ResourceType -> rt
    resources_extra_info <- resources_extra_info %>%
      mutate(n = ifelse(.data$ResourceType == rt, n - 1, n))
  }
  tasks %>% filter(.data$Step == step) -> ts
  ts %>% .$Value -> used
  return(starpu_freq_abe(
    ts,
    resources_extra_info %>% filter(.data$codelet %in% used, .data$Step == step)
  ))
}
