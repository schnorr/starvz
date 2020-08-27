#' @include starvz_data.R
NULL

#' Execute StarVZ Phase one.
#'
#' This function calls all CSV-converter inner-functions to pre-process
#' they into StarVZ files. Although this can be directly used in a folder
#' where all CSV compressed (gzip) files reside, we suggest to use the
#' shell tool \code{starvz} or \code{phase1-workflow.sh} in the \code{tools/}
#' directory.
#'
#' @param directory Directory of CSV files
#' @param app_states_fun Function to determine application
#' @param state_filter Type of filder
#' @param whichApplication Name of Application
#' @param input.parquet Use or not of parquet files
#' @return ggplot object with all starvz plots
#' @family phase1 functions
#'
#' @examples
#' \donttest{
#' example_folder <- system.file("extdata", "lu_trace", package = "starvz")
#' starvz_phase1(directory = example_folder)
#' }
#' @export
starvz_phase1 <- function(directory = ".", app_states_fun = lu_colors, state_filter = 0, whichApplication = "", input.parquet = "1") {
  # Start of reading procedure
  if (is.null(app_states_fun)) stop("app_states_fun is obligatory for reading")

  file_can_be_read <- function(filename) {
    if ((file.exists(filename)) & (file.size(filename) > 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Read entities.csv and register the hierarchy (with Y coordinates)
  entities <- hl_y_paje_tree(where = directory)
  dfhie <- entities$workertreedf

  # Read Worker States
  Worker <- read_worker_csv(
    where = directory,
    app_states_fun = app_states_fun,
    outlier_fun = outlier_definition,
    state_filter = state_filter,
    whichApplication = whichApplication
  )
  Worker$Application <- Worker$Application %>%
    hl_y_coordinates(dfhie = dfhie) %>%
    select(-.data$Type)

  Worker$StarPU <- Worker$StarPU %>%
    hl_y_coordinates(dfhie = dfhie) %>%
    select(-.data$Type)

  if (Worker$Application %>% nrow() == 0) stop("After reading states, number of application rows is zero.")

  # This can be isolated
  gc()
  isolate_read_write(input.parquet, read_vars_set_new_zero, "Variable", directory, Worker$ZERO)
  gc()
  isolate_read_write(input.parquet, pmtool_bounds_csv_parser, "Pmtool", directory, Worker$ZERO)
  gc()
  isolate_read_write(input.parquet, data_handles_csv_parser, "Data_handles", directory, Worker$ZERO)
  gc()
  isolate_read_write(input.parquet, papi_csv_parser, "Papi", directory, Worker$ZERO)
  gc()
  isolate_read_write(input.parquet, read_memory_state_csv, "Memory_state", directory, Worker$ZERO)
  gc()
  isolate_read_write(input.parquet, read_comm_state_csv, "Comm_state", directory, Worker$ZERO)
  gc()
  isolate_read_write(input.parquet, read_other_state_csv, "Other_state", directory, Worker$ZERO)
  gc()
  isolate_read_write_m(input.parquet, events_csv_parser, directory, Worker$ZERO)
  gc()
  isolate_read_write_m(input.parquet, tasks_csv_parser, directory, Worker$ZERO)
  gc()

  # Read links
  dfl <- read_links(where = directory, Worker$ZERO)

  # Read the elimination tree
  dfa <- atree_load(where = directory)

  # QRMumps case:
  # If the Atree is available and loaded, we create new columns for each task
  # to hold Y coordinates for the temporal elimination tree plot
  if (!is.null(dfa)) {
    dfap <- dfa %>%
      select(-.data$Parent, -.data$Depth) %>%
      rename(Height.ANode = .data$Height, Position.ANode = .data$Position)
    Worker$Application <- Worker$Application %>% left_join(dfap, by = "ANode")
    dfap <- NULL
    # Reorder the elimination tree
    dfa <- reorder_elimination_tree(dfa, Worker$Application)
  }

  # Read DAG
  dfdag <- read_dag(where = directory, Worker$Application %>% mutate(Application = TRUE), dfl)

  dpmts <- pmtool_states_csv_parser(where = directory, whichApplication = whichApplication, Y = dfhie, States = Worker$Application)

  # Ending
  # Enframe ZERO
  ZERO <- enframe(Worker$ZERO, name = NULL)

  # Enframe Version
  Version <- enframe("0.2.0", name = NULL)

  starvz_log("Assembling the named list with the data from this case.")

  data <- list(
    Origin = directory,
    Application = Worker$Application,
    StarPU = Worker$StarPU,
    Colors = Worker$Colors,
    Link = dfl, DAG = dfdag, Y = dfhie, Atree = dfa,
    Pmtool_states = dpmts, entities = entities$dfe,
    Zero = ZERO, Version = Version
  )

  starvz_log("Call Gaps.")
  data$Gaps <- gaps(data)

  if (input.parquet == "1") {
    starvz_log("Saving as parquet")
    starvz_write_parquet(data, directory = directory)
  } else {
    starvz_log("Saving as feather")
    starvz_write_feather(data, directory = directory)
  }
}

isolate_read_write <- function(input.parquet, fun, name, directory, ZERO) {
  data <- list()
  data[[name]] <- fun(where = directory, ZERO = ZERO)
  if (input.parquet == "1") {
    starvz_log("Saving as parquet")
    starvz_write_parquet(data, directory = directory)
  } else {
    starvz_log("Saving as feather")
    starvz_write_feather(data, directory = directory)
  }
  return(NULL)
}

isolate_read_write_m <- function(input.parquet, fun, directory, ZERO) {
  data <- fun(where = directory, ZERO = ZERO)
  if (input.parquet == "1") {
    starvz_log("Saving as parquet")
    starvz_write_parquet(data, directory = directory)
  } else {
    starvz_log("Saving as feather")
    starvz_write_feather(data, directory = directory)
  }
  return(NULL)
}

# This function gets a data.tree object and calculate three properties
# H: height, P: position, D: depth
atree_coordinates <- function(atree, height = 1) {
  defineHeightPosition <- function(node, curPos, depth) {
    if (length(node$children) == 0) {
      # My coordinates are the same of the parents
      node$H <- node$parent$H
      node$P <- node$parent$P
      node$D <- node$parent$D
    } else {
      # Defined this node properties
      node$H <- height
      node$P <- curPos
      node$D <- depth
      # Before recursion, set the new Y position at curPos
      curPos <- curPos + node$H
      # Recurse
      for (child in node$children) {
        curPos <- defineHeightPosition(child, curPos, (depth + 1))
      }
    }
    return(curPos)
  }
  defineHeightPosition(atree, 0, 0)
  return(atree)
}

# This function gets a data.tree object and converts it to a tibble
# Expects three properties on each node (H, P, and D), as defined above
atree_to_df <- function(node) {
  ndf <- tibble(
    Parent = ifelse(is.null(node$parent), NA, node$parent$name),
    ANode = node$name,
    Height = node$H,
    Position = node$P,
    Depth = node$D
  )
  for (child in node$children) ndf <- ndf %>% bind_rows(atree_to_df(child))
  return(ndf)
}

reorder_elimination_tree <- function(Atree, Application) {

  # Reorganize tree Position, consider only not pruned nodes and submission order
  data_reorder <- Application %>%
    filter(grepl("qrt", .data$Value)) %>%
    select(.data$ANode, .data$SubmitOrder) %>%
    unique() %>%
    group_by(.data$ANode) %>%
    mutate(SubmitOrder = as.integer(.data$SubmitOrder)) %>%
    arrange(.data$SubmitOrder) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(.data$SubmitOrder) %>%
    mutate(Position = 1:n(), Height = 1) %>%
    select(-.data$SubmitOrder)

  Atree <- Atree %>%
    # Replace Position and Height by new ordering
    select(-.data$Position, -.data$Height) %>%
    left_join(data_reorder, by = "ANode")

  # Define Position for pruned nodes as the same of its Parent
  data_pruned_position <- Application %>%
    filter(grepl("qrt", .data$Value) | grepl("do_subtree", .data$Value)) %>%
    mutate(NodeType = case_when(.data$Value == "do_subtree" ~ "Pruned", TRUE ~ "Not Pruned")) %>%
    select(-.data$Position, -.data$Height) %>%
    left_join(Atree, by = "ANode") %>%
    select(.data$ANode, .data$Parent, .data$NodeType, .data$Position, .data$Height) %>%
    unique() %>%
    left_join(Atree %>%
      select(.data$ANode, .data$Position, .data$Height),
    by = c("Parent" = "ANode"), suffix = c("", ".Parent")
    ) %>%
    # pruned child node have the same position as its father
    mutate(
      Position = case_when(.data$NodeType == "Pruned" ~ .data$Position.Parent, TRUE ~ .data$Position),
      Height = case_when(.data$NodeType == "Pruned" ~ .data$Height.Parent, TRUE ~ .data$Height)
    ) %>%
    select(-.data$Parent, -.data$Position.Parent, -.data$Height.Parent)

  Atree <- Atree %>%
    # Replace Position and Height for pruned nodes
    select(-.data$Position, -.data$Height) %>%
    left_join(data_pruned_position, by = "ANode")

  return(Atree)
}

hl_y_paje_tree <- function(where = ".") {
  entities.feather <- paste0(where, "/entities.feather")
  entities.csv <- paste0(where, "/entities.csv")

  if (file.exists(entities.feather)) {
    starvz_log(paste("Reading ", entities.feather))
    dfe <- read_feather(entities.feather)
  } else if (file.exists(entities.csv)) {
    starvz_log(paste("Reading ", entities.csv))
    dfe <- read_csv(entities.csv,
      trim_ws = TRUE,
      col_types = cols(
        Parent = col_character(),
        Name = col_character(),
        Type = col_character(),
        Nature = col_character()
      )
    )
  } else {
    starvz_log(paste("Files", entities.feather, "or", entities.csv, "do not exist."))
    return(NULL)
  }

  # first part: read entities, calculate Y
  # If this file is read with readr's read_csv function, the data.tree does not like

  if ((dfe %>% nrow()) == 0) stop(paste("After reading the entities file, the number of rows is zero"))

  workertree <- tree_filtering(
    dfe,
    c("Link", "Event", "Variable"),
    c("GFlops", "Memory Manager", "Scheduler State", "User Thread", "Thread State")
  )

  # Customize heigth of each object
  dfheights <- data.frame(Type = c("Worker State", "Communication Thread State"), Height = c(1, 1))
  # Customize padding between containers
  dfpaddings <- data.frame(Type = c("MPI Program"), Padding = c(2))

  # Calculate the Y coordinates with what has left
  workertree <- y_coordinates(workertree, dfheights, dfpaddings)

  # print(workertree, "Type", "Nature", "H", "P", limit=200);
  # Convert back to data frame
  workertreedf <- dt_to_df(workertree) %>% select(-.data$Nature)

  if ((workertreedf %>% nrow()) == 0) stop("After converting the tree back to DF, number of rows is zero.")

  return(list(workertreedf = workertreedf, dfe = dfe))
}

hl_y_coordinates <- function(dfw = NULL, dfhie = NULL) {
  if (is.null(dfw)) stop("The input data frame with states is NULL")

  # first part: read entities, calculate Y
  workertreedf <- dfhie

  # second part: left join with Y
  dfw <- dfw %>%
    # the left join to get new Y coordinates
    left_join(workertreedf, by = c("ResourceId" = "Parent"))

  return(dfw)
}

tree_filtering <- function(dfe, natures, types) {
  starvz_log("Starting the tree filtering to create Y coordinates")

  dfe %>%
    # Mutate things to character since data.tree don't like factors
    mutate(Type = as.character(.data$Type), Nature = as.character(.data$Nature)) %>%
    # Filter things I can't filter using Prune (because Prune doesn't like grepl)
    # Note that this might be pottentially dangerous and works only for StarPU traces
    filter(!grepl("InCtx", .data$Parent), !grepl("InCtx", .data$Name)) %>%
    # Rename the reserved word root
    mutate(
      Name = gsub("root", "ROOT", .data$Name),
      Parent = gsub("root", "ROOT", .data$Parent)
    ) %>%
    # Remove a node named 0 whose parent is also named 0
    filter(.data$Name != 0 & .data$Parent != 0) %>%
    # Convert to data.frame to avoid compatibility problems between tibble and data.tree
    as.data.frame() -> x
  # Sort by machines ID
  y <- x[mixedorder(as.character(x$Name), decreasing = TRUE), ]
  # Continue
  y %>%
    # Remove all variables
    # filter (Nature != "Variable") %>%
    # Remove bogus scheduler (should use a new trace)
    # filter (Name != "scheduler") %>%
    # Convert to data.tree object
    as.Node(mode = "network") -> tree
  # Remove all nodes that are present in the natures list
  if (!is.null(natures)) {
    Prune(tree, function(node) !(node$Nature %in% natures))
  }
  # Remove all types that are present in the types list
  if (!is.null(types)) {
    Prune(tree, function(node) !(node$Type %in% types))
  }

  return(tree)
}
y_coordinates <- function(atree, heights, paddings) {
  starvz_log("Starting y_coordinates")
  defineHeightPosition <- function(node, dfhs, dfps, curPos) {
    node$P <- curPos
    if (!is.null(node$Nature) && node$Nature == "State") {
      node$H <- dfhs %>%
        filter(.data$Type == node$Type) %>%
        .$Height
      # This is a StarPU+MPI hack to make CUDA resources look larger
      if (grepl("CUDA", node$parent$name)) node$H <- node$H * 2
      curPos <- curPos + node$H
    } else {
      padding <- 0
      if (!is.null(node$Type)) {
        padding <- dfps %>% filter(.data$Type == node$Type)
        if (nrow(padding) == 0) {
          padding <- 0
        } else {
          padding <- padding %>% .$Padding
        }
      }

      for (child in node$children) {
        curPos <- defineHeightPosition(child, dfhs, dfps, (curPos + padding))
      }
      if (length(node$children)) {
        node$H <- sum(sapply(node$children, function(child) child$H))
      } else {
        node$H <- 0
      }
    }
    return(curPos)
  }

  atree$Set(H = NULL)
  atree$Set(P = NULL)
  defineHeightPosition(atree, heights, paddings, 0)
  return(atree)
}
dt_to_df <- function(node) {
  ret <- dt_to_df_inner(node)
  return(ret)
}

dt_to_df_inner <- function(node) {
  cdf <- data.frame()
  ndf <- data.frame()
  if (!is.null(node$Nature) && node$Nature == "State") {
    ndf <- data.frame(
      Parent = node$parent$name,
      Type = node$name,
      Nature = node$Nature,
      Height = node$H,
      Position = node$P
    )
  } else {
    for (child in node$children) {
      cdf <- rbind(cdf, dt_to_df_inner(child))
    }
  }
  ret <- rbind(ndf, cdf)
  return(ret)
}

gaps.f_backward <- function(data) {
  # Create the seed chain
  if (TRUE %in% grepl("mpicom", data$DAG$JobId)) {
    data$DAG %>%
      filter(grepl("mpicom", .data$JobId)) -> tmpdag
  } else {
    data$DAG -> tmpdag
  }
  tmpdag %>%
    rename(DepChain = .data$JobId, Member = .data$Dependent) %>%
    select(.data$DepChain, .data$Member) -> seedchain

  f2 <- function(dfdag, chain.i) {
    dfdag %>% select(.data$JobId, .data$Dependent, .data$Application, .data$Value) -> full.i
    # qr mumps has duplicated data in these dfs and the left_join did not work correctly. unique() solves this problem
    full.i %>% unique() -> full.i
    chain.i %>% unique() -> chain.i
    full.i %>% left_join(chain.i, by = c("JobId" = "Member")) -> full.o

    # If there are no application tasks in dependency chains, keep looking
    if ((full.o %>% filter(!is.na(.data$DepChain), .data$Application == TRUE) %>% nrow()) == 0) {
      # Prepare the new chain
      full.o %>%
        filter(!is.na(.data$DepChain)) %>%
        rename(Member = .data$Dependent) %>%
        select(.data$DepChain, .data$Member) -> chain.o
      return(f2(full.o, chain.o))
    } else {
      return(full.o)
    }
  }
  return(f2(data$DAG, seedchain))
}

gaps.f_forward <- function(data) {
  # Create the seed chain
  if (TRUE %in% grepl("mpicom", data$DAG$Dependent)) {
    data$DAG %>%
      filter(grepl("mpicom", .data$Dependent)) -> tmpdag
  } else {
    data$DAG -> tmpdag
  }
  tmpdag %>%
    rename(DepChain = .data$Dependent, Member = .data$JobId) %>%
    select(.data$DepChain, .data$Member) -> seedchain

  f2 <- function(dfdag, chain.i) {
    dfdag %>% select(.data$JobId, .data$Dependent, .data$Application, .data$Value) -> full.i
    # qr mumps has duplicated data in these dfs and the left_join did not work correctly. unique() solves this problem
    full.i %>% unique() -> full.i
    chain.i %>% unique() -> chain.i
    full.i %>% left_join(chain.i, by = c("Dependent" = "Member")) -> full.o

    # If there are no application tasks in dependency chains, keep looking
    if ((full.o %>% filter(!is.na(.data$DepChain), .data$Application == TRUE) %>% nrow()) == 0) {
      # Prepare the new chain
      full.o %>%
        filter(!is.na(.data$DepChain)) %>%
        rename(Member = .data$JobId) %>%
        select(.data$DepChain, .data$Member) -> chain.o
      return(f2(full.o, chain.o))
    } else {
      return(full.o)
    }
  }
  return(f2(data$DAG, seedchain))
}

gaps <- function(data) {
  starvz_log("Starting the gaps calculation.")

  if (is.null(data$DAG)) {
    return(NULL)
  }
  if (is.null(data$Application)) {
    return(NULL)
  }
  # if(is.null(data$Link)) return(NULL);

  gaps.f_backward(data) %>%
    filter(!is.na(.data$DepChain)) %>%
    select(.data$JobId, .data$DepChain) %>%
    rename(Dependent = .data$JobId) %>%
    rename(JobId = .data$DepChain) %>%
    select(.data$JobId, .data$Dependent) %>%
    unique() -> data.b

  gaps.f_forward(data) %>%
    filter(!is.na(.data$DepChain)) %>%
    select(.data$JobId, .data$DepChain) %>%
    rename(Dependent = .data$DepChain) %>%
    select(.data$JobId, .data$Dependent) %>%
    unique() -> data.f

  data$DAG %>%
    filter(.data$Application == TRUE) %>%
    select(.data$JobId, .data$Dependent) -> data.z

  # Create the new gaps DAG
  dfw <- data$Application %>%
    select(.data$JobId, .data$Value, .data$ResourceId, .data$Node, .data$Start, .data$End)
  if (is.null(data$Link)) {
    dfl <- data.frame()
    data.b.dag <- data.frame()
    data.f.dag <- data.frame()
  } else {
    dfl <- data$Link %>%
      filter(grepl("mpicom", .data$Key)) %>%
      mutate(Value = NA, ResourceId = .data$Origin, Node = NA) %>%
      rename(JobId = .data$Key) %>%
      select(.data$JobId, .data$Value, .data$ResourceId, .data$Node, .data$Start, .data$End)
    data.b %>%
      left_join(dfl, by = c("JobId" = "JobId")) %>%
      left_join(dfw, by = c("Dependent" = "JobId")) -> data.b.dag
    data.f %>%
      left_join(dfw, by = c("JobId" = "JobId")) %>%
      left_join(dfl, by = c("Dependent" = "JobId")) -> data.f.dag
  }
  data.z %>%
    left_join(dfw, by = c("JobId" = "JobId")) %>%
    left_join(dfw, by = c("Dependent" = "JobId")) -> data.z.dag

  return(bind_rows(data.z.dag, data.b.dag, data.f.dag))
}
outlier_definition <- function(x) {
  (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}
