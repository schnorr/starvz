#' @include starvz_data.R
NULL

read_worker_csv <- function(where = ".",
                            app_states_fun = NULL,
                            outlier_fun = NULL,
                            state_filter = 0,
                            whichApplication = NULL,
                            config = NULL) {
  # Check obligatory parameters
  if (is.null(whichApplication)) stop("whichApplication is NULL, it should be provided")
  if (is.null(app_states_fun)) stop("app_states_fun should be provided to read_state_csv")
  if (!is.data.frame(app_states_fun())) stop("app_states_fun is not returning a data frame")
  if (is.null(outlier_fun)) stop("outlier_fun should be provided to read_state_csv")

  state.csv <- paste0(where, "/paje.worker_state.csv.gz")
  if (file.exists(state.csv)) {
    starvz_log(paste("Reading", state.csv))
    dfw <- starvz_suppressWarnings(read_csv(
      file = state.csv,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Nature = col_character(),
        ResourceId = col_character(),
        Type = col_character(),
        Start = col_double(),
        End = col_double(),
        Duration = col_double(),
        Depth = col_double(),
        Value = col_character(),
        Size = col_character(),
        Params = col_character(),
        Footprint = col_character(),
        Tag = col_character(),
        JobId = col_character(),
        GFlop = col_character(),
        SubmitOrder = col_character(),
        X = col_character(),
        Y = col_character(),
        Iteration = col_character(),
        Subiteration = col_character()
      )
    ))
  } else {
    stop(paste("File", state.csv, "do not exist"))
  }

  # Remove Nature and Type (as it always be Worker Node State)
  dfw <- dfw %>% select(-.data$Nature, -.data$Type)

  # Convert To Factor
  dfw <- dfw %>%
    mutate(
      ResourceId = as.factor(.data$ResourceId),
      Footprint = as.factor(.data$Footprint),
      Tag = as.factor(.data$Tag),
      Value = as.factor(.data$Value),
      Size = as.integer(.data$Size),
      Params = as.factor(.data$Params),
      GFlop = as.numeric(.data$GFlop),
      X = as.integer(.data$X),
      Y = as.integer(.data$Y),
      Iteration = as.integer(.data$Iteration),
      Subiteration = as.integer(.data$Subiteration)
    ) %>% filter(!is.na(.data$ResourceId))

  if ((dfw %>% nrow()) == 0) stop("After reading worker states, number of rows is zero.")

  # QRMumps: fix qrmumps kernels names so we have a clean color definition
  if (whichApplication == "qrmumps") {
    dfw <- dfw %>%
      mutate(Value = gsub("_perf.*", "", .data$Value)) %>%
      mutate(Value = gsub("qrm_", "", .data$Value)) %>%
      mutate(Value = case_when(
        grepl("geqrt", Value) ~ "geqrt",
        grepl("gemqrt", Value) ~ "gemqrt",
        grepl("tpqrt", Value) ~ "tpqrt",
        grepl("tpmqrt", Value) ~ "tpmqrt",
        grepl("tpqrt", Value) ~ "tpqrt",
        grepl("block_extadd", Value) ~ "block_copy",
        TRUE ~ Value
      ))
  }

  # Split application and starpu behavior
  # state_filter:
  # 0 = Based on Runtime fixed States
  # 1 = Based on stricted application name states
  # 2 = Based on non-stricted application name states
  if (state_filter == 0) {
    starvz_log("Selecting application states based on runtime states.")
    dfw <- dfw %>% mutate(Application = case_when(.data$Value %in% all_starpu_states() ~ FALSE, TRUE ~ TRUE))
  } else if (state_filter == 1) {
    starvz_log("Selecting application states based on custom application stricted states names.")
    # If strict, states need to be exact
    dfw <- dfw %>% mutate(Application = case_when(.data$Value %in% (app_states_fun() %>% .$Kernel) ~ TRUE, TRUE ~ FALSE))
  } else if (state_filter == 2) {
    starvz_log("Selecting application states based on custom application non-stricted states names.")
    # If not strict, we mark using app_states_fun() Kernel field as RE
    state_condition <- paste((app_states_fun() %>% .$Kernel), collapse = "|")
    dfw <- dfw %>% mutate(Application = case_when(grepl(state_condition, .data$Value) ~ TRUE, TRUE ~ FALSE))
  }

  if ((dfw %>% nrow()) == 0) stop("After application states check, number of rows is zero.")

  # Create three new columns (Node, Resource, ResourceType) - This is StarPU-specific
  # But first, check if this is a multi-node trace (if there is a _, it is a multi-node trace)
  # TODO This is a very weak test, should find something else instead
  firstResourceId <- dfw %>%
    .$ResourceId %>%
    unique() %>%
    as.character() %>%
    sort() %>%
    head(n = 1)
  if (grepl("CUDA|CPU", unlist(strsplit(firstResourceId, "_"))[2])) {
    starvz_log("This is multi-node trace")
    # This is the case for multi-node trace
    dfw <- dfw %>%
      mutate(ResourceId=as.factor(.data$ResourceId)) %>%
      separate_res() %>%
      tibble() %>%
      mutate(Resource = as.factor(.data$Resource)) %>%
      mutate(Node = as.factor(.data$Node)) %>%
      mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource))) %>%
      mutate(Resource = as.factor(.data$Resource))
  } else {
    starvz_log("This is a single-node trace...")
    # This is the case for SINGLE node trace
    dfw <- dfw %>%
      mutate(Node = as.factor(0)) %>%
      mutate(Resource = .data$ResourceId) %>%
      mutate(ResourceType = as.factor(gsub("[_[:digit:]]+", "", .data$ResourceId)))
  }

  # remove all application states with NA
  # StarPU is dumping two lines per application state (so, fix in R)
  # Also BREAK STARPU and Application in two different data frames
  Application <- dfw %>%
    filter(.data$Application == TRUE & !is.na(.data$JobId)) %>%
    select(-.data$Application)
  StarPU <- dfw %>%
    filter(.data$Application == FALSE) %>%
    select(
      -.data$Params, -.data$Footprint, -.data$Application,
      -.data$Tag,
      -.data$JobId,
      -.data$GFlop,
      -.data$SubmitOrder,
      -.data$X,
      -.data$Y,
      -.data$Iteration,
      -.data$Subiteration
    )

  # In case application is not specified
  if (whichApplication == "") {
    # Get only application states
    dfcolors <- Application %>%
      select(.data$Value) %>%
      unique()

    # Get the number of states to generate colors
    nc <- dfcolors %>% nrow()

    # TODO: Using set1 right now to generate colors, max of 9 states
    c <- rep(brewer.pal(9, "Set1"), (nc / 9) + 1)
    c <- head(c, nc)

    # Match States and Colors
    dfcolors <- dfcolors %>%
      mutate(Color = c) %>%
      arrange(.data$Value, .data$Color) %>%
      mutate(
        Color = replace_na(.data$Color, "black"),
        Use = TRUE
      ) %>%
      unique()
    # If config is present try to use it for colors
    if (!is.null(config)) {
      tasks_colors <- lapply(config$app_tasks, data.frame, stringsAsFactors = FALSE)
      config_colors <- bind_rows(tasks_colors, .id = "Value")
      if (config_colors %>% nrow() > 0) {
        dfcolors <- dfcolors %>%
          left_join(config_colors, by = "Value") %>%
          mutate(Color = ifelse(is.na(.data$color), .data$Color, .data$color)) %>%
          mutate(Use = ifelse(is.na(.data$use), .data$Use, .data$use)) %>%
          select(.data$Value, .data$Color, .data$Use)
      }
    }
  } else {
    partial_join <- function(x, y, by_x, pattern_y) {
      idx_x <- sapply(y[[pattern_y]], grep, x[[by_x]])
      idx_y <- sapply(seq_along(idx_x), function(i) rep(i, length(idx_x[[i]])))

      df <- bind_cols(
        x[unlist(idx_x), , drop = F],
        y[unlist(idx_y), , drop = F]
      )
      return(df)
    }
    dfw %>%
      select(.data$Value) %>%
      unique() -> tasks

    # Try to partial Match
    dfcolors <- partial_join(tasks, app_states_fun(), "Value", "Kernel") %>%
      select(.data$Value, .data$Color, .data$Use)
  }
  # Apply
  Colors <- dfcolors

  # Detect outliers
  if (whichApplication == "cholesky") {
    Application <- Application %>%
      group_by(.data$Value, .data$ResourceType) %>%
      mutate(Outlier = ifelse(.data$Duration > outlier_fun(.data$Duration), TRUE, FALSE)) %>%
      ungroup()
  } else if (whichApplication == "qrmumps") {
    starvz_log("Using Regression models to detect task duration anomalies")

    # Step 0: Define the linear models for outlier classification
    model_LR <- function(df) {
      lm(Duration ~ GFlop, data = df)
    }
    model_WLR <- function(df) {
      lm(Duration ~ GFlop, data = df, weights = 1 / df$GFlop)
    }
    model_NLR <- function(df) {
      lm(Duration ~ I(GFlop**(2 / 3)), data = df)
    }
    model_LR_log <- function(df) {
      lm(log(Duration) ~ log(GFlop), data = df)
    }

    # set dummy variable for Cluster
    Application <- Application %>% mutate(Cluster = 1)
    Application <- regression_based_outlier_detection(Application, model_WLR, "_WLR", level = 0.95)
    Application <- regression_based_outlier_detection(Application, model_LR, "_LR", level = 0.95)
    Application <- regression_based_outlier_detection(Application, model_NLR, "_NLR", level = 0.95)
    Application <- regression_based_outlier_detection(Application, model_LR_log, "_LR_LOG", level = 0.95)

    if (!requireNamespace("flexmix", quietly = TRUE)) {
      # configure the flexmix model to clusterize tasks before running the model_LR_log
      model_flexmix_log <- function(df) {
        flexmix::stepFlexmix(Duration ~ GFlop,
          data = df, k = 1:2,
          model = flexmix::FLXMRglm(log(Duration) ~ log(GFlop)),
          control = list(nrep = 30)
        )
      }

      # need to create the clusters before calling the function, let's do the clustering for all
      # types of tasks for now, replacing the dummy Cluster variable
      Application <- Application %>% select(-.data$Cluster)
      Application <- Application %>%
        filter(grepl("qrt", .data$Value)) %>%
        filter(.data$GFlop > 0) %>%
        group_by(.data$ResourceType, .data$Value) %>%
        nest() %>%
        mutate(flexmix_model = map(.data$data, model_flexmix_log)) %>%
        mutate(Cluster = map(.data$flexmix_model, function(m) {
          # pick the best fitted model according to BIC metric
          flexmix::getModel(m, which = "BIC")@cluster
        })) %>%
        select(-.data$flexmix_model) %>%
        unnest(cols = c(.data$Cluster, .data$data)) %>%
        ungroup() %>%
        select(.data$JobId, .data$Cluster) %>%
        full_join(Application, by = "JobId")
      Application <- regression_based_outlier_detection(Application, model_LR_log, "_FLEXMIX", level = 0.95)
    }else{
      starvz_warn("qrmumps can use the suggested package flexmix (that is not installed) to do another outlier classification")
    }

    # Use the Outlier_LR_LOG (log~log) as the default Outlier classification
    Application <- Application %>% rename(Outlier = .data$Outlier_LR_LOG)
  } else {
    starvz_log("Outlier detection using standard model")
    Application <- Application %>%
      group_by(.data$Value, .data$ResourceType) %>%
      mutate(Outlier = ifelse(.data$Duration > outlier_fun(.data$Duration), TRUE, FALSE)) %>%
      ungroup()
  }

  # Define the global ZERO (to be used with other trace date)
  ZERO <- Application %>%
    filter(.data$Value %in% (Colors %>% filter(.data$Use) %>% .$Value)) %>%
    .$Start %>%
    min()

  # The new zero because of the long initialization phase
  Application <- Application %>% mutate(Start = .data$Start - ZERO, End = .data$End - ZERO)
  StarPU <- StarPU %>% mutate(Start = .data$Start - ZERO, End = .data$End - ZERO)

  # QRMumps case:
  # When the trace is from qr_mumps (by Ian), the elimination tree
  # node is encoded in the Tag field, we need to convert it to the
  # appropriate ANode using the following code. We do that for all kind
  # of traces, but the ANode column is only valid for the qr_mump traces.
  if (whichApplication == "qrmumps") {
    Application <- Application %>% mutate(ANode = NA, ANode = as.character(strtoi(as.integer(paste0("0x", substr(.data$Tag, 9, 16))))))
  }

  return(list(Application = Application, StarPU = StarPU, Colors = Colors, ZERO = ZERO))
}

read_memory_state_csv <- function(where = ".", ZERO = 0) {
  csv_file <- paste0(where, "/paje.memory_state.csv.gz")
  if (file.exists(csv_file)) {
    starvz_log(paste("Reading ", csv_file))
    dfw <- starvz_suppressWarnings(read_csv(
      file = csv_file,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Nature = col_character(),
        ResourceId = col_character(),
        Type = col_character(),
        Start = col_double(),
        End = col_double(),
        Duration = col_double(),
        Depth = col_double(),
        Value = col_character()
      )
    ))
  } else {
    starvz_warn(paste("File ", csv_file, " do not exist"))
  }
  # Remove Nature and Type (as it always be Memory Node State)
  dfw <- dfw %>% select(-.data$Nature, -.data$Type)

  # Convert To Factor
  dfw <- dfw %>%
    mutate(
      ResourceId = as.factor(.data$ResourceId),
      Value = as.factor(.data$Value)
    )

  if ((dfw %>% nrow()) == 0) {
    starvz_log("After reading Memory States, number of rows is zero.")
    return(NULL)
  }

  # Create three new columns (Node, Resource, ResourceType) - This is StarPU-specific
  # But first, check if this is a multi-node trace (if there is a _, it is a multi-node trace)
  # TODO This is a very weak test, should find something else instead
  firstResourceId <- dfw %>%
    .$ResourceId %>%
    unique() %>%
    as.character() %>%
    sort() %>%
    head(n = 1)
  if (grepl("CUDA|CPU", unlist(strsplit(firstResourceId, "_"))[2])) {
    # This is the case for multi-node trace
    dfw <- dfw %>%
      mutate(ResourceId=as.factor(.data$ResourceId)) %>%
      separate_res() %>%
      tibble() %>%
      mutate(Resource = as.factor(.data$Resource)) %>%
      mutate(Node = as.factor(.data$Node)) %>%
      mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource)))
  } else {
    # This is the case for SINGLE node trace
    dfw <- dfw %>%
      mutate(Node = as.factor(0)) %>%
      mutate(Resource = .data$ResourceId) %>%
      mutate(ResourceType = as.factor(gsub("[_[:digit:]]+", "", .data$ResourceId)))
  }

  dfw <- dfw %>% mutate(Start = .data$Start - ZERO, End = .data$End - ZERO)

  return(dfw)
}

read_comm_state_csv <- function(where = ".", ZERO = 0) {
  csv_file <- paste0(where, "/paje.comm_state.csv.gz")
  if (file.exists(csv_file)) {
    starvz_log(paste("Reading ", csv_file))
    dfw <- starvz_suppressWarnings(read_csv(
      file = csv_file,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Nature = col_character(),
        ResourceId = col_character(),
        Type = col_character(),
        Start = col_double(),
        End = col_double(),
        Duration = col_double(),
        Depth = col_double(),
        Value = col_character()
      )
    ))
  } else {
    starvz_warn(paste("File ", csv_file, " do not exist"))
  }
  # Remove Nature and Type (as it always be Comm Node State)
  dfw <- dfw %>% select(-.data$Nature, -.data$Type)

  # Convert To Factor
  dfw <- dfw %>%
    mutate(
      ResourceId = as.factor(.data$ResourceId),
      Value = as.factor(.data$Value)
    )

  if ((dfw %>% nrow()) == 0) {
    starvz_log("After reading Comm States, number of rows is zero.")
    return(NULL)
  }

  # Create three new columns (Node, Resource, ResourceType) - This is StarPU-specific
  # But first, check if this is a multi-node trace (if there is a _, it is a multi-node trace)
  # TODO This is a very weak test, should find something else instead
  firstResourceId <- dfw %>%
    .$ResourceId %>%
    unique() %>%
    as.character() %>%
    sort() %>%
    head(n = 1)
  if (grepl("mpict", unlist(strsplit(firstResourceId, "_"))[2])) {
    # This is the case for multi-node trace
    dfw <- dfw %>%
      mutate(ResourceId=as.factor(.data$ResourceId)) %>%
      separate_res() %>%
      tibble() %>%
      mutate(Resource = as.factor(.data$Resource)) %>%
      mutate(Node = as.factor(.data$Node)) %>%
      mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource)))
  } else {
    # This is the case for SINGLE node trace
    dfw <- dfw %>%
      mutate(Node = as.factor(0)) %>%
      mutate(Resource = .data$ResourceId) %>%
      mutate(ResourceType = as.factor(gsub("[_[:digit:]]+", "", .data$ResourceId)))
  }

  dfw <- dfw %>% mutate(Start = .data$Start - ZERO, End = .data$End - ZERO)

  return(dfw)
}

read_other_state_csv <- function(where = ".", ZERO = 0) {
  csv_file <- paste0(where, "/paje.other_state.csv.gz")
  if (file.exists(csv_file)) {
    starvz_log(paste("Reading ", csv_file))
    dfw <- starvz_suppressWarnings(read_csv(
      file = csv_file,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Nature = col_character(),
        ResourceId = col_character(),
        Type = col_character(),
        Start = col_double(),
        End = col_double(),
        Duration = col_double(),
        Depth = col_double(),
        Value = col_character()
      )
    ))
  } else {
    starvz_warn(paste("File ", csv_file, " do not exist"))
  }
  # Remove Nature
  dfw <- dfw %>% select(-.data$Nature)

  # Convert To Factor
  dfw <- dfw %>%
    mutate(
      ResourceId = as.factor(.data$ResourceId),
      Value = as.factor(.data$Value),
      Type = as.factor(.data$Type)
    )

  if ((dfw %>% nrow()) == 0) {
    starvz_log("After reading Other States, number of rows is zero.")
    return(NULL)
  }

  # Create three new columns (Node, Resource, ResourceType) - This is StarPU-specific
  # But first, check if this is a multi-node trace (if there is a _, it is a multi-node trace)
  # TODO This is a very weak test, should find something else instead
  firstResourceId <- dfw %>%
    .$ResourceId %>%
    unique() %>%
    as.character() %>%
    sort() %>%
    head(n = 1)
  if (grepl("CUDA|CPU", unlist(strsplit(firstResourceId, "_"))[2])) {
    # This is the case for multi-node trace
    dfw <- dfw %>%
      mutate(ResourceId=as.factor(.data$ResourceId)) %>%
      separate_res() %>%
      tibble() %>%
      mutate(Resource = as.factor(.data$Resource)) %>%
      mutate(Node = as.factor(.data$Node)) %>%
      mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource)))
  } else {
    # This is the case for SINGLE node trace
    dfw <- dfw %>%
      mutate(Node = as.factor(0)) %>%
      mutate(Resource = .data$ResourceId) %>%
      mutate(ResourceType = as.factor(gsub("[_[:digit:]]+", "", .data$ResourceId)))
  }

  dfw <- dfw %>% mutate(Start = .data$Start - ZERO, End = .data$End - ZERO)

  return(dfw)
}

read_vars_set_new_zero <- function(where = ".", ZERO = 0) {
  variable.csv <- paste0(where, "/paje.variable.csv.gz")
  if (file.exists(variable.csv)) {
    starvz_log(paste("Reading ", variable.csv))
    dfv <- starvz_suppressWarnings(read_csv(variable.csv,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Nature = col_character(),
        ResourceId = col_character(),
        Type = col_character(),
        Start = col_double(),
        End = col_double(),
        Duration = col_double(),
        Value = col_double()
      )
    ))
  } else {
    stop(paste("File", variable.csv, "do not exist"))
  }

  firstResourceId <- dfv %>%
    .$ResourceId %>%
    unique() %>%
    as.character() %>%
    sort() %>%
    head(n = 1)

  dfv %>%
    select(-.data$Nature) %>%
    # the new zero because of the long initialization phase
    mutate(Start = .data$Start - ZERO, End = .data$End - ZERO) -> dfv

    # create three new columns (Node, Resource, ResourceType)
    # This is StarPU-specific
    if (grepl("CUDA|CPU", unlist(strsplit(firstResourceId, "_"))[2])) {
      starvz_log("This is multi-node trace")
      # This is the case for multi-node trace
      dfv %>%
        mutate(ResourceId=as.factor(.data$ResourceId)) %>%
        separate_res() %>%
        tibble() %>%
        mutate(Resource = as.factor(.data$Resource)) %>%
        mutate(Node = as.factor(.data$Node)) %>%
        mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource))) %>%
        mutate(Type = as.factor(.data$Type)) -> tmp
    } else {
      starvz_log("This is a single-node trace...")
      # This is the case for SINGLE node trace
      dfv %>%
        mutate(Node = as.factor(0)) %>%
        mutate(Resource = .data$ResourceId) %>%
        mutate(ResourceType = as.factor(gsub("[_[:digit:]]+", "", .data$ResourceId))) %>%
        mutate(Type = as.factor(.data$Type)) -> tmp
    }

    # manually rename variables names
    tmp %>% mutate(
      Type = gsub("Number of Ready Tasks", "Ready", .data$Type),
      Type = gsub("Number of Submitted Uncompleted Tasks", "Submitted", .data$Type),
      Type = gsub("Bandwidth In \\(MB/s)", "B. In (MB/s)", .data$Type),
      Type = gsub("Bandwidth Out \\(MB/s)", "B. Out (MB/s)", .data$Type)
    ) -> dfv
  return(dfv)
}

atree_load <- function(where = ".") {
  atree.csv <- paste0(where, "/atree.csv")

  if (file.exists(atree.csv)) {
    starvz_log(paste("Reading ", atree.csv))
    df <- starvz_suppressWarnings(read_csv(
      file = atree.csv,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Node = col_integer(),
        DependsOn = col_integer()
      )
    ))
  } else {
    starvz_log(paste("File", atree.csv, "do not exist."))
    return(NULL)
  }

  intermediary_nodes <- df %>%
    select(.data$Node) %>%
    .$Node %>%
    unique()

  starvz_log(paste("Calculating graphical properties of the elimination tree"))

  df %>%
    # Mutate things to character since data.tree don't like anything else
    mutate(Node = as.character(.data$Node), DependsOn = as.character(.data$DependsOn)) %>%
    # Convert to data.frame to avoid compatibility issues between tibble and data.tree
    as.data.frame() %>%
    # Convert to data.tree object
    as.Node(mode = "network") %>%
    # Calculate Y coordinates
    atree_coordinates() %>%
    # Convert back to data frame
    atree_to_df() %>%
    # Mark intermediary nodes
    mutate(Intermediary = case_when(.data$ANode %in% intermediary_nodes ~ TRUE, TRUE ~ FALSE)) -> df
  return(df)
}


pmtool_bounds_csv_parser <- function(where = ".", ZERO = 0) {
  entities.feather <- paste0(where, "/pmtool.feather")
  entities.csv <- paste0(where, "/pmtool.csv")

  if (file.exists(entities.feather)) {
    starvz_log(paste("Reading ", entities.feather))
    pm <- read_feather(entities.feather)
  } else if (file.exists(entities.csv)) {
    starvz_log(paste("Reading ", entities.csv))
    pm <- starvz_suppressWarnings(read_csv(entities.csv,
      trim_ws = TRUE,
      col_types = cols(
        Alg = col_character(),
        Bound = col_logical(),
        Time = col_double()
      )
    ))
    # pmtool gives time in microsecounds
    pm[[3]] <- pm[[3]] / 1000
  } else {
    starvz_log(paste("Files", entities.feather, "or", entities.csv, "do not exist."))
    return(NULL)
  }
  ret <- pm
  return(ret)
}

pmtool_states_csv_parser <- function(where = ".", whichApplication = NULL, Y = NULL, States = NULL) {
  entities.csv <- paste0(where, "/pmtool_states.csv")

  if (file.exists(entities.csv)) {
    starvz_log(paste("Reading ", entities.csv))

    # sched Tid   worker taskType JobId start duration end

    pm <- starvz_suppressWarnings(read_csv(entities.csv,
      trim_ws = TRUE,
      col_types = cols(
        sched = col_character(),
        Tid = col_integer(),
        worker = col_integer(),
        taskType = col_character(),
        JobId = col_character(),
        start = col_double(),
        duration = col_double(),
        end = col_double()
      )
    ))
    # pmtool states gives time in milisecounds

    pm[[6]] <- pm[[6]] / 1000
    pm[[7]] <- pm[[7]] / 1000
    pm[[8]] <- pm[[8]] / 1000

    names(pm)[names(pm) == "taskType"] <- "Value"
    names(pm)[names(pm) == "start"] <- "Start"
    names(pm)[names(pm) == "end"] <- "End"
    names(pm)[names(pm) == "duration"] <- "Duration"
    names(pm)[names(pm) == "worker"] <- "ResourceId"

    pm <- separate(data = pm, col = .data$JobId, into = c("JobId", "Tag"), sep = "\\:", extra = "drop", fill = "right")

    fileName <- paste0(where, "/platform_file.rec")
    conn <- file(fileName, open = "r")
    linn <- readLines(conn)

    devices <- c()

    for (i in 1:length(linn)) {
      if (substr(linn[i], 1, 18) == "%rec: worker_count") {
        for (y in i:length(linn)) {
          if (substr(linn[y], 1, 12) == "%rec: timing") {
            break
          }
          if (substr(linn[y], 1, 14) == "Architecture: ") {
            hard <- substr(linn[y], 15, nchar(linn[y]))
            if (substr(hard, 1, 3) == "cpu") {
              hard <- "CPU"
            } else {
              hard <- paste0(toupper(hard), "_")
            }
            y <- y + 1
            i <- i + 1
            num <- as.numeric(substr(linn[y], 12, nchar(linn[y])))
            for (z in 1:num) {
              devices <- c(devices, paste0(hard, z - 1))
            }
          }
        }
      } else if (substr(linn[i], 1, 12) == "%rec: timing") {
        break
      }
    }

    close(conn)

    pm[[3]] <- devices[pm[[3]] + 1]

    pm <- pm %>% left_join((Y %>% select(-.data$Type)), by = c("ResourceId" = "Parent"))
    # print(States)
    # print(pm)
    pm <- pm %>% left_join((States %>% select(.data$Iteration, .data$JobId)), by = c("JobId" = "JobId"))

    if (whichApplication == "cholesky") {
      pm <- pm %>%
        mutate(Color = case_when(
          .data$Value == "dpotrf" ~ "#e41a1c",
          .data$Value == "dtrsm" ~ "#377eb8",
          .data$Value == "dsyrk" ~ "#984ea3",
          .data$Value == "dgemm" ~ "#4daf4a",
          .data$Value == "dplgsy" ~ "yellow",
          TRUE ~ "#000"
        ))
    }

    # print(pm)
  } else {
    starvz_log(paste("File", entities.csv, "do not exist."))
    return(NULL)
  }
  ret <- pm
  return(ret)
}

data_handles_csv_parser <- function(where = ".", ZERO = 0) {
  entities.csv <- paste0(where, "/rec.data_handles.csv.gz")

  if (file.exists(entities.csv)) {
    starvz_log(paste("Reading ", entities.csv))
    pm <- starvz_suppressWarnings(read_csv(entities.csv,
      trim_ws = TRUE,
      show_col_types=FALSE
    ))
  } else {
    starvz_log(paste("File", entities.csv, "do not exist."))
    return(NULL)
  }
  ret <- pm %>% mutate(Handle = as.factor(.data$Handle))
  if ("Description" %in% colnames(ret)) {
    ret <- ret %>% mutate(Description = as.factor(.data$Description))
  }
  if ("Name" %in% colnames(ret)) {
    ret <- ret %>% mutate(Name = as.factor(.data$Name))
  }

  return(ret)
}

papi_csv_parser <- function(where = ".", ZERO = 0) {
  entities.csv <- paste0(where, "/rec.papi.csv.gz")

  if (file.exists(entities.csv)) {
    starvz_log(paste("Reading ", entities.csv))
    pm <- starvz_suppressWarnings(read_csv(entities.csv,
      trim_ws = TRUE,
      col_types = cols(
        JobId = col_character(),
        PapiEvent = col_character(),
        Value = col_integer()
      )
    ))
  } else {
    starvz_log(paste("File", entities.csv, "do not exist."))
    return(NULL)
  }
  ret <- pm

  return(ret)
}

task_handles_parser <- function(where = ".") {
  entities.feather <- paste0(where, "/task_handles.feather")

  if (file.exists(entities.feather)) {
    starvz_log(paste("Reading ", entities.feather))
    ret <- read_feather(entities.feather)
    return(ret)
  }

  return(NULL)
}

tasks_csv_parser <- function(where = ".", ZERO = 0) {
  entities.csv <- paste0(where, "/rec.tasks.csv.gz")

  task_handles <- task_handles_parser(where = where)

  if (file.exists(entities.csv) & file.info(entities.csv)$size > 0) {
    starvz_log(paste("Reading ", entities.csv))
    pm <- starvz_suppressWarnings(read_csv(entities.csv,
      trim_ws = TRUE,
      col_types = cols(
        Control = col_character(),
        JobId = col_character(),
        SubmitOrder = col_integer(),
        SubmitTime = col_double(),
        Handles = col_character(),
        MPIRank = col_integer(),
        DependsOn = col_character(),
        Tag = col_character(),
        Footprint = col_character(),
        Iteration = col_integer(),
        Name = col_character(),
        Model = col_character(),
        Priority = col_integer(),
        WorkerId = col_integer(),
        MemoryNode = col_integer(),
        StartTime = col_double(),
        EndTime = col_double(),
        Parameters = col_character(),
        Modes = col_character(),
        Sizes = col_character()
      )
    ))
    # sort the data by the submit order
    pm <- pm[with(pm, order(SubmitOrder)), ]
    # Set correct time
    pm <- pm %>%
      # the new zero because of the long initialization phase
      mutate(
        SubmitTime = .data$SubmitTime - ZERO,
        StartTime = .data$StartTime - ZERO,
        EndTime = .data$EndTime - ZERO
      )

    if ("Handles" %in% names(pm)) {
      # Tasks have multiple handles, get them in a different structure
      handles_dep <- pm %>%
        select(.data$JobId) %>%
        mutate(
          Handles = strsplit(pm$Handles, " "),
          Modes = strsplit(pm$Modes, " "),
          Sizes = lapply(strsplit(pm$Sizes, " "), as.integer)
        )
      # unnest the lists
      task_handles <- unnest(handles_dep, cols = c(.data$Handles, .data$Modes, .data$Sizes)) %>%
        mutate(
          Handles = as.factor(.data$Handles),
          Modes = as.factor(.data$Modes)
        )

      # We will save the task_handle structre, we can remove these columns
      pm <- pm %>% select(-.data$Handles, -.data$Modes, -.data$Sizes)
    }
  } else {
    starvz_log(paste("File", entities.csv, "do not exist."))
    return(NULL)
  }

  return(list(Tasks = pm, Task_handles = task_handles))
}

events_csv_parser <- function(where = ".", ZERO = 0) {
  entities.csv <- paste0(where, "/paje.events.csv.gz")

  if (file.exists(entities.csv)) {
    starvz_log(paste("Reading ", entities.csv))

    pm <- starvz_suppressWarnings(read_csv(entities.csv,
      trim_ws = TRUE,
      col_types = cols(
        Nature = col_character(),
        Container = col_character(),
        Type = col_character(),
        Start = col_double(),
        Value = col_character(),
        Handle = col_character(),
        Info = col_integer(),
        Size = col_integer(),
        Tid = col_character(),
        Src = col_character()
      )
    ))
    # sort the data by the start time
    pm <- pm[with(pm, order(Start)), ]

    # Read links
    pm <- pm %>%
      # the new zero because of the long initialization phase
      mutate(Start = .data$Start - ZERO)
    # Global modifications
    pm <- pm %>% mutate(
      Container = as.factor(.data$Container),
      Type = as.factor(.data$Type)
    )
    # Break in Events normal
    Events_normal <- pm %>%
      filter(
        .data$Type != "Allocating Async Start", .data$Type != "Allocating Async End",
        .data$Type != "Allocating Start", .data$Type != "Allocating End",
        .data$Type != "DriverCopy Start", .data$Type != "DriverCopy End",
        .data$Type != "DriverCopyAsync Start", .data$Type != "DriverCopyAsync End",
        .data$Type != "Free Start", .data$Type != "Free End",
        .data$Type != "Request Created", .data$Type != "data registration",
        .data$Type != "data state invalid",
        .data$Type != "data state owner", .data$Type != "data state shared",
        .data$Type != "data wont use"
      ) %>%
      select(.data$Container, .data$Type, .data$Start, .data$Value) %>%
      mutate(Value = as.factor(.data$Value))
    # Break in Events Data
    Events_data <- pm %>%
      filter(.data$Type == "data registration" | .data$Type == "data state invalid" |
        .data$Type == "data state owner" | .data$Type == "data state shared" |
        .data$Type == "data wont use") %>%
      select(.data$Container, .data$Type, .data$Start, .data$Value) %>%
      mutate(Value = as.factor(.data$Value))
    Events_memory <- pm %>%
      filter(.data$Type == "Allocating Async Start" | .data$Type == "Allocating Async End" |
        .data$Type == "Allocating Start" | .data$Type == "Allocating End" |
        .data$Type == "DriverCopy Start" | .data$Type == "DriverCopy End" |
        .data$Type == "DriverCopyAsync Start" | .data$Type == "DriverCopyAsync End" |
        .data$Type == "Free Start" | .data$Type == "Free End" |
        .data$Type == "Request Created") %>%
      select(
        .data$Container, .data$Type, .data$Start,
        .data$Value, .data$Handle, .data$Info,
        .data$Size, .data$Tid, .data$Src
      ) %>%
      mutate(
        Value = as.factor(.data$Value),
        Handle = as.factor(.data$Handle),
        Info = as.factor(.data$Info),
        Tid = as.factor(.data$Tid),
        Src = as.factor(.data$Src)
      )
  } else {
    starvz_log(paste("File", entities.csv, "do not exist."))
    return(NULL)
  }
  if (Events_memory %>% nrow() == 0) {
    Events_memory <- NULL
  }
  if (Events_data %>% nrow() == 0) {
    Events_data <- NULL
  }
  if (Events_normal %>% nrow() == 0) {
    Events_normal <- NULL
  }

  return(list(Events = Events_normal, Events_data = Events_data, Events_memory = Events_memory))
}


read_dag <- function(where = ".", Application = NULL, dfl = NULL) {
  dag.csv <- paste0(where, "/dag.csv.gz")
  if (file.exists(dag.csv)) {
    starvz_log(paste("Reading ", dag.csv))
    dfdag <- starvz_suppressWarnings(read_csv(dag.csv,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Node = col_integer(),
        DependsOn = col_integer()
      )
    ))
  } else {
    starvz_warn(paste("File", dag.csv, "do not exist"))
    return(NULL)
  }

  # Read the DAG in the CSV format, do some clean-ups
  dfdag <- dfdag %>%
    # Put in the right order
    select(.data$JobId, .data$Dependent) %>%
    # Communication task ids have too much information, clean-up both columns (JobId, Dependent)
    mutate(JobId = gsub("mpi_.*_", "mpicom_", .data$JobId)) %>%
    mutate(Dependent = gsub("mpi_.*_", "mpicom_", .data$Dependent))

  # Check Application existence
  stopifnot(!is.null(Application))

  starvz_log("Merge state data with the DAG")

  # Do the two merges (states and links)
  dfdags <- dfdag %>%
    # Get only non-MPI tasks JobIds
    filter(!grepl("mpicom", .data$JobId)) %>%
    # Merge task information from the trace
    full_join(Application, by = "JobId")

  # Check dfl existence
  if (!is.null(dfl)) {
    starvz_log("Get MPI tasks (links) to enrich the DAG")

    dfdagl <- dfdag %>%
      # Get only MPI tasks JobIds
      filter(grepl("mpicom", .data$JobId)) %>%
      # Merge MPI communicaton task information from the trace (links: dfl)
      full_join(dfl, by = c("JobId" = "Key")) %>%
      # Align columns with state-based tasks
      # 1. Remove columns
      select(-.data$Container, -.data$Origin) %>%
      # 2. Dest becomes ResourceId for these MPI tasks
      rename(ResourceId = .data$Dest) %>%
      mutate(ResourceId=as.factor(.data$ResourceId)) %>%
      separate_res() %>%
      tibble() %>%
      mutate(Resource = as.factor(.data$Resource)) %>%
      mutate(Node = as.factor(.data$Node)) %>%
      mutate(ResourceType = as.factor(gsub("[[:digit:]]+", "", .data$Resource)))
    dfdag <- dfdags %>% bind_rows(dfdagl)
  } else {
    dfdag <- dfdags
  }

  # Finally, bind everything together, calculate cost to CPB
  dfdag <- dfdag %>%
    mutate(Dependent = as.factor(.data$Dependent)) %>%
    # Calculate the cost as the inverse of the duration (so boost's CPB code can work)
    mutate(Cost = ifelse(is.na(.data$Duration), 0, -.data$Duration)) %>%
    # Force the result as tibble for performance reasons
    select(.data$JobId, .data$Dependent, .data$Start, .data$End, .data$Cost, .data$Value) %>%
    as_tibble()
}


read_links <- function(where = ".", ZERO = 0) {
  link.csv <- paste0(where, "/paje.link.csv.gz")
  if (file.exists(link.csv)) {
    starvz_log(paste("Reading ", link.csv))
    dfl <- starvz_suppressWarnings(read_csv(link.csv,
      trim_ws = TRUE,
      progress = FALSE,
      col_types = cols(
        Nature = col_character(),
        Container = col_character(),
        Type = col_character(),
        Start = col_double(),
        End = col_double(),
        Duration = col_double(),
        Size = col_integer(),
        Origin = col_character(),
        Dest = col_character(),
        Key = col_character(),
        Tag = col_character(),
        MPIType = col_character(),
        Priority = col_integer(),
        Handle = col_character()
      )
    ))
  } else {
    starvz_log(paste("File", link.csv, "do not exist"))
    return(NULL)
  }

  # Check if number of lines is greater than zero
  if ((dfl %>% nrow()) == 0) {
    starvz_log("After attempt to read links, number of rows is zero")
    return(NULL)
  }

  all_cols <- c(MPIType = "", Priority = "", Handle = "")

  # Read links
  dfl <- dfl %>%
    add_column(!!!all_cols[!names(all_cols) %in% names(.)]) %>%
    # the new zero because of the long initialization phase
    mutate(Start = .data$Start - ZERO, End = .data$End - ZERO) %>%
    select(-.data$Nature) %>%
    mutate(
      Container = as.factor(.data$Container),
      Type = as.factor(.data$Type),
      Origin = as.factor(.data$Origin),
      Dest = as.factor(.data$Dest),
      Key = as.factor(.data$Key),
      Tag = as.factor(.data$Tag),
      MPIType = as.factor(.data$MPIType),
      Handle = as.factor(.data$Handle)
    )

  return(dfl)
}
