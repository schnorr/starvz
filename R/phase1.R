# see:
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script (Suppressingfire answer)
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
other.name <- paste(sep="/", script.basename, "deps.R")
print(paste("Sourcing",other.name,"from",script.name))

if(file.exists(other.name)){
  source(other.name)
}else{
  source("deps.R")
}

# Code starts here - Do not remove this line

read_state_csv <- function(applicationName, app_states_fun, strict_state_filter, dfw) {
  if (is.null(applicationName) ||
      is.null(app_states_fun) ||
      !is.data.frame(app_states_fun()) ||
      !all((app_states_fun() %>% names) == c('Kernel', 'Color'))) {
    stop('A mandatory parameter is invalid.');
  }
  
  dfw <- dfw %>% mutate(Value = as.factor(Value),
                        GFlop = as.numeric(GFlop),
                        X = as.integer(X),
                        Y = as.integer(Y),
                        Iteration = as.integer(Iteration),
                        Subiteration = as.integer(Subiteration));
  
  # QRMumps: fix qrmumps kernels names so we have a clean color definition
  if (applicationName == "qrmumps"){
    dfw <- dfw %>%
      mutate(Value = gsub("_perf.*", "", Value)) %>%
      mutate(Value = gsub("qrm_", "", Value));
    loginfo("Fix of qrmumps state naming completed.");
  }
  
  # Split application and StarPU behavior
  if (strict_state_filter) {
    dfw <- dfw %>% mutate(Application = case_when(.$Value %in% (app_states_fun() %>% .$Kernel) ~ TRUE, TRUE ~ FALSE));
  } else {
    # If not strict, we mark useing app_states_fun() Kernel field as RE
    state_condition = paste((app_states_fun() %>% .$Kernel), collapse = '|');
    dfw <- dfw %>% mutate(Application = case_when(grepl(state_condition, .$Value) ~ TRUE, TRUE ~ FALSE));
  }
  if (nrow(dfw) == 0) stop('After application states check, number of rows is zero');
  loginfo('App state filter completed');
  
  # remove all application states with NA
  # StarPU is dumping two lines per application state (so, fix in R)
  dfw <- dfw %>% dplyr::filter(Application == FALSE | !is.na(JobId));
  
  filename <- './tmp-rawDfw.feather';
  write_feather(dfw, filename);
  return(filename);
}

read_zero <- function(dfw) {
  loginfo('Defining the ZERO timestamp');
  return(dfw %>% 
           dplyr::filter(Application == TRUE) %>% 
           .$Start %>% 
           min %>%
           as.double);
}

normalize_dfw <- function(dfw, zero, applicationName, app_states_fun, outlier_definition) {
  # Create three new columns (Node, Resource, ResourceType) - This is StarPU-specific
  # But first, check if this is a multi-node trace (if there is a _, it is a multi-node trace)
  # TODO This is a very weak test, should find something else instead
  firstResourceId <- dfw %>% .$ResourceId %>% unique %>% sort %>% head(n=1);
  if(grepl('CUDA|CPU', unlist(strsplit(firstResourceId, '_'))[2])) {
    loginfo('This is a multi-node trace');
    dfw <- dfw %>%
      separate(ResourceId, into=c('Node', 'Resource'), remove=FALSE) %>%
      mutate(Node = as.factor(Node)) %>%
      mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource)));
  } else {
    loginfo('This is a single-node trace');
    dfw <- dfw %>%
      mutate(Node = as.factor(0)) %>%
      mutate(Resource = ResourceId) %>%
      mutate(ResourceType = as.factor(gsub('[_[:digit:]]+', '', ResourceId)));
  }
  
  loginfo('Define colors is starting now.');
  dfcolors <- dfw %>%
    select(Value) %>%
    unique %>%
    mutate(Value = as.character(Value), Color = NA) %>%
    rbind(app_states_fun() %>% rename(Value = Kernel)) %>%
    arrange(Value, Color) %>%
    mutate(Color = na.locf(Color, na.rm = FALSE)) %>%
    unique;
  loginfo('Colors data.frame is defined.');
  dfw <- dfw %>% left_join(dfcolors, by='Value');
  loginfo('Left joining the colors has completed');
  
  # Specific cholesky colors: sufficiently harmless
  if(applicationName == 'cholesky') {
    loginfo('This is a cholesky application, colors are hardcoded');
    dfw <- dfw %>%
      mutate(Color = case_when(
        grepl("potrf", .$Value) ~ "#e41a1c",
        grepl("trsm", .$Value) ~ "#377eb8",
        grepl("syrk", .$Value) ~ "#984ea3",
        grepl("gemm", .$Value) ~ "#4daf4a",
        TRUE ~ .$Color
        ));
  }
  
  # Detect outliers
  if(applicationName == 'cholesky') {
    loginfo('Attempting to detect outliers using a basic model.');
    dfw <- dfw %>%
      group_by(Value, ResourceType) %>%
      mutate(Outlier = Duration > outlier_definition(Duration)) %>%
      ungroup();
  } else {
    loginfo('No outlier detection; using NA in the corresponding column.');
    dfw <- dfw %>% mutate(Outlier = NA);
  }
  
  # Set times according to the new zero
  dfw <- dfw %>% mutate(Start = Start - zero, End = End - zero);
  
  # Get rid of all the observations before zero
  dfw <- dfw %>% dplyr::filter(Start >= 0);
  
  # The problem is that Vinicius traces do not have "Iteration" column
  # We need to create it based on the Tag
  if (dfw %>% filter(Application == TRUE) %>% slice(1) %>% .$Iteration %>% is.na){
    dfw <- dfw %>%
      mutate(Iteration = case_when(
        grepl("potrf", .$Value) ~ as.integer(paste0("0x", substr(.$Tag, 14, 16))),
        grepl("trsm", .$Value) ~ as.integer(paste0("0x", substr(.$Tag, 11, 13))),
        grepl("syrk", .$Value) ~ as.integer(paste0("0x", substr(.$Tag, 8, 10))),
        grepl("gemm", .$Value) ~ as.integer(paste0("0x", substr(.$Tag, 8, 10))),
        TRUE ~ as.integer(-10)));
  }
  
  # QRMumps case:
  # When the trace is from qr_mumps (by Ian), the elimination tree
  # node is encoded in the Tag field, we need to convert it to the
  # appropriate ANode using the following code. We do that for all kind
  # of traces, but the ANode column is only valid for the qr_mump traces.
  if (applicationName == "qrmumps"){
    dfw <- dfw %>% 
      mutate(ANode = NA, 
             ANode = as.character(strtoi(as.integer(paste0("0x", substr(.$Tag, 9, 16))))));
  }
  
  return(dfw);
}

hl_y_coordinates <- function (dfw = NULL, dfhie = NULL)
{
    if (is.null(dfw)) stop("The input data frame with states is NULL");
    # left join with Y
    dfw <- dfw %>>%
        # the left join to get new Y coordinates
        left_join (dfhie, by=c("ResourceId" = "Parent", "Type" = "Type", "Nature" = "Nature"));

    filename <- 'tmp-highlightedDfw.feather';
    write_feather(dfw, filename);
    return(filename);
}

hl_y_paje_tree <- function (dfe)
{
    # first part: read entities, calculate Y
    workertree <- tree_filtering (dfe,
                                  c("Link", "Event", "Variable"),
                                  c("GFlops", "Memory Manager", "Scheduler State", "User Thread", "Thread State"));

    # Customize heigth of each object
    dfheights = data.frame(Type = c("Worker State", "Communication Thread State"), Height = c(1, 1));
    # Customize padding between containers
    dfpaddings = data.frame(Type = c("MPI Program"), Padding = c(2));

    # Calculate the Y coordinates with what has left
    workertree <- y_coordinates(workertree, dfheights, dfpaddings);

    #print(workertree, "Type", "Nature", "H", "P", limit=200);
    # Convert back to data frame
    workertreedf <- dt_to_df (workertree);

    if ((workertreedf %>% nrow) == 0) stop("After converting the tree back to DF, number of rows is zero.");

    return(workertreedf);
}

tree_filtering <- function (dfe, natures, types)
{
    loginfo("Starting the tree filtering to create Y coordinates");

    dfe %>%
    # Mutate things to character since data.tree don't like factors
    mutate (Type = as.character(Type), Nature = as.character(Nature)) %>%
    # Filter things I can't filter using Prune (because Prune doesn't like grepl)
    # Note that this might be pottentially dangerous and works only for StarPU traces
    dplyr::filter (!grepl("InCtx", Parent), !grepl("InCtx", Name)) %>%
    # Rename the reserved word root
    mutate (Name = gsub("root", "ROOT", Name),
            Parent = gsub("root", "ROOT", Parent)) %>%
    # Remove a node named 0 whose parent is also named 0
    filter (Name != 0 & Parent != 0) %>%
    # Convert to data.frame to avoid compatibility problems between tibble and data.tree
    as.data.frame() %>>%
    # Remove all variables
    #filter (Nature != "Variable") %>%
    # Remove bogus scheduler (should use a new trace)
    #filter (Name != "scheduler") %>%
    # Convert to data.tree object
    as.Node(mode="network") -> tree;
    # Remove all nodes that are present in the natures list
    if (!is.null(natures)){
        tree <- tree %>>% (~ Prune(., function(node) !(node$Nature %in% natures)) );
    }
    # Remove all types that are present in the types list
    if (!is.null(types)){
        tree <- tree %>>% (~ Prune(., function(node) !(node$Type %in% types)) );
    }

    loginfo("Tree filtering completed.");

    return(tree);
}

y_coordinates <- function (atree, heights, paddings)
{
    loginfo ("Starting y_coordinates");
    defineHeightPosition <- function (node, dfhs, dfps, curPos)
    {
        node$P = curPos;
        if(!is.null(node$Nature) && node$Nature == "State"){
            node$H = dfhs %>% filter(Type == node$Type) %>% .$Height;
            # This is a StarPU+MPI hack to make CUDA resources look larger
            if (grepl("CUDA", node$parent$name)) node$H = node$H * 2;
            curPos = curPos + node$H;
        }else{
            padding = 0;
            if (!is.null(node$Type)){
                padding = dfps %>% filter(Type == node$Type);
                if (nrow(padding) == 0){
                    padding = 0;
                }else{
                    padding = padding %>% .$Padding;
                }
            }

            for (child in node$children){
                curPos = defineHeightPosition (child, dfhs, dfps, (curPos+padding));

            }
            if(length(node$children)){
                node$H = sum(sapply(node$children, function(child) child$H));
            }else{
                node$H = 0;
            }
        }
        return(curPos);
    }

    atree$Set(H = NULL);
    atree$Set(P = NULL);
    defineHeightPosition(atree, heights, paddings, 0);
    loginfo ("The call for y_coordinates has completed.");
    return(atree);
}

dt_to_df <- function (node)
{
    loginfo ("Converting data.tree to data.frame");
    ret <- dt_to_df_inner (node);
    loginfo ("Conversion from data.tree to data.frame has completed.");
    return(ret);
}

dt_to_df_inner <- function (node)
{
    cdf <- data.frame();
    ndf <- data.frame();
    if(!is.null(node$Nature) && node$Nature == "State"){
        ndf <- data.frame(
            Parent = node$parent$name,
            Type = node$name,
            Nature = node$Nature,
            Height = node$H,
            Position = node$P);
    }else{
        for (child in node$children){
            cdf <- rbind(cdf, dt_to_df_inner(child));
        }
    }
    ret <- rbind(ndf, cdf);
    return(ret);
}

atree_load <- function(dfa){
    intermediary_nodes <- dfa %>% select(Node) %>% .$Node %>% unique;

    loginfo(paste("Calculating graphical properties of the elimination tree"));

    dfa %>%
        # Mutate things to character since data.tree don't like anything else
        mutate(Node = as.character(Node), DependsOn = as.character(DependsOn)) %>%
        # Convert to data.frame to avoid compatibility issues between tibble and data.tree
        as.data.frame() %>%
        # Convert to data.tree object
        as.Node(mode="network") %>%
        # Calculate Y coordinates
        atree_coordinates %>%
        # Convert back to data frame
        atree_to_df %>%
        # Mark intermediary nodes
        mutate(Intermediary = case_when(.$ANode %in% intermediary_nodes ~ TRUE, TRUE ~ FALSE)) -> dfa;
    
    return(dfa);
}

# This function gets a data.tree object and calculate three properties
# H: height, P: position, D: depth
atree_coordinates <- function (atree, height = 1)
{
    defineHeightPosition <- function (node, curPos, depth)
    {
        if (length(node$children) == 0){
            # My coordinates are the same of the parents
            node$H = node$parent$H;
            node$P = node$parent$P;
            node$D = node$parent$D;
        }else{
            # Defined this node properties
            node$H = height;
            node$P = curPos;
            node$D = depth;
            # Before recursion, set the new Y position at curPos
            curPos = curPos + node$H;
            # Recurse
            for (child in node$children){
                curPos = defineHeightPosition (child, curPos, (depth+1));
            }
        }
        return(curPos);
    }
    defineHeightPosition(atree, 0, 0);
    return(atree);
}

# This function gets a data.tree object and converts it to a tibble
# Expects three properties on each node (H, P, and D), as defined above
atree_to_df <- function (node)
{
    ndf <- tibble(
        Parent = ifelse(is.null(node$parent), NA, node$parent$name),
        ANode = node$name,
        Height = node$H,
        Position = node$P,
        Depth = node$D);
    for (child in node$children) ndf <- ndf %>% bind_rows(atree_to_df(child));
    return(ndf);
}

build_dfap <- function(dfa) {
    if (is.null(dfa)) return(NULL);
    dfap <- dfa %>% 
        select(-Parent, -Depth) %>%
        rename(Height.ANode = Height, Position.ANode = Position);
    
    return(dfap);
}

join_dfw_dfap <- function(dfw, dfap) {
    if (!is.null(dfap)) {
        dfw <- left_join(dfw, dfap, by='ANode');
    }
    return(dfw);
}

read_vars_set_new_zero <- function (dfv, zero)
    dfv <- dfv %>%
        # the new zero because of the long initialization phase
        mutate(Start = Start - zero, End = End - zero) %>%
        # filter all variables during negative timings
        dplyr::filter(Start >= 0, End >= 0) %>%
        # create three new columns (Node, Resource, ResourceType)
        # This is StarPU-specific
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        mutate(Node = as.factor(Node)) %>%
        mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource))) %>>%
        # abbreviate names so they are smaller
        # This does not work fine.
        # mutate(Type = abbreviate(Type, minlength=10));
        # manually rename variables names
        mutate (Type = gsub("Number of Ready Tasks", "Ready", Type),
                Type = gsub("Number of Submitted Uncompleted Tasks", "Submitted", Type),
                Type = gsub("Bandwidth In \\(MB/s)", "B. In (MB/s)", Type),
                Type = gsub("Bandwidth Out \\(MB/s)", "B. Out (MB/s)", Type));
    return(dfv);
}

read_links <- function (dfl, zero)
{
    # Read links
    dfl <- dfl %>%
        # the new zero because of the long initialization phase
        mutate(Start = Start - zero, End = End - zero) %>%
        # filter all variables during negative timings
        dplyr::filter(Start >= 0, End >= 0);

    filename <- 'pre.link.feather';
    write_feather(dfl, filename);
    return(filename);
}
read_dag <- function (dfdag, dfw = NULL, dfl = NULL)
{
    # Read the DAG in the CSV format, do some clean-ups
    dfdag <- dfdag %>%
        # Put in the right order
        select(JobId, Dependent) %>%
        # Communication task ids have too much information, clean-up both columns (JobId, Dependent)
        mutate(JobId = gsub("mpi_.*_", "mpicom_", JobId)) %>%
        mutate(Dependent = gsub("mpi_.*_", "mpicom_", Dependent));

    # Check dfw existence
    stopifnot(!is.null(dfw));

    loginfo("Merge state data with the DAG");

    # Do the two merges (states and links)
    dfdags <- dfdag %>%
        # Get only non-MPI tasks JobIds
        filter(!grepl("mpicom", JobId)) %>%
        # Merge task information from the trace (states: dfw)
        full_join((dfw %>% filter(Application == TRUE)), by="JobId");

    loginfo("Merge state data with the DAG completed");

    # Check dfl existence
    if (!is.null(dfl)){
        loginfo("Get MPI tasks (links) to enrich the DAG");

        dfdagl <- dfdag %>%
            # Get only MPI tasks JobIds
            filter(grepl("mpicom", JobId)) %>%
            # Merge MPI communicaton task information from the trace (links: dfl)
            full_join(dfl, by=c("JobId" = "Key")) %>%
            # Align columns with state-based tasks
            # 1. Remove columns
            select(-Container, -Origin) %>%
            # 2. Change types
            mutate(Size = as.character(Size)) %>%
            # 3. Dest becomes ResourceId for these MPI tasks
            rename(ResourceId = Dest) %>%
            separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
            mutate(Node = as.factor(Node)) %>%
            mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource)));
        dfdag <- dfdags %>% bind_rows(dfdagl);

        loginfo("Merge link data with the DAG completed");
    }else{
        dfdag <- dfdags;
    }

    # Finally, bind everything together, calculate cost to CPB
    dfdag <- dfdag %>%
        # Calculate the cost as the inverse of the duration (so boost's CPB code can work)
        mutate(Cost = ifelse(is.na(Duration), 0, -Duration)) %>%
        # Force the result as tibble for performance reasons
        as_tibble();
    
    return(dfdag);
}

pmtools_states_csv_parser <- function (dpmts, whichApplication = NULL, Y = NULL, States = NULL)
{
    if(!is.null(dpmts)) {
        dpmts[[6]] <- dpmts[[6]]/1000
        dpmts[[7]] <- dpmts[[7]]/1000
        dpmts[[8]] <- dpmts[[8]]/1000

        names(dpmts)[names(dpmts) == 'taskType'] <- 'Value'
        names(dpmts)[names(dpmts) == 'start'] <- 'Start'
        names(dpmts)[names(dpmts) == 'end'] <- 'End'
        names(dpmts)[names(dpmts) == 'duration'] <- 'Duration'
        names(dpmts)[names(dpmts) == 'worker'] <- 'ResourceId'

        dpmts <- separate(data = dpmts, col = JobId, into = c("JobId", "Tag"), sep = "\\:")

        fileName <- "platform_file.rec"
        conn <- file(fileName,open="r")
        linn <-readLines(conn)

        devices <- c()

        for (i in 1:length(linn)){
        	if(substr(linn[i], 1, 18)=="%rec: worker_count"){
        		for (y in i:length(linn)){
        			if(substr(linn[y], 1, 12)=="%rec: timing"){
        				break;
        			}
        			if(substr(linn[y], 1, 14)=="Architecture: "){
                hard <- substr(linn[y], 15, nchar(linn[y]))
                if(substr(hard,1,3)=="cpu"){
                  hard <- "CPU"
                }else{
                  hard <- paste0(toupper(hard),"_")
                }
        				y <- y + 1
        				i <- i + 1
                num <- as.numeric(substr(linn[y], 12, nchar(linn[y])))
                for(z in 1:num){
                  devices <- c(devices, paste0(hard, z-1))
                }
        			}
        		}
        	}else if(substr(linn[i], 1, 12)=="%rec: timing"){
        		break;
        	}
        }

        dpmts[[3]] <- devices[dpmts[[3]]+1]

        dpmts <- dpmts %>% left_join((Y %>% select(-Type, -Nature)), by=c("ResourceId" = "Parent"))
        #print(States)
        #print(dpmts)
        dpmts <- dpmts %>% left_join((States %>% select(Iteration, JobId)), by=c("JobId" = "JobId"))

        if (whichApplication == "cholesky"){
            dpmts <- dpmts %>%
                mutate(Color = case_when(
                           Value=="dpotrf" ~ "#e41a1c",
                           Value=="dtrsm" ~ "#377eb8",
                           Value=="dsyrk" ~ "#984ea3",
                           Value=="dgemm" ~ "#4daf4a",
                           TRUE ~ "#000"));
        }

        #print(dpmts)
    }else{
        dpmts <- NULL;
    }
    return(dpmts);
}

pmtools_bounds_csv_parser <- function (dpmtb) {
    if (!is.null(dpmtb)) {
        # pmtools gives time in microsecounds
        dpmtb[[2]] <- dpmtb[[2]]/1000
    }
    return (dpmtb);
}

data_handles_csv_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/data_handles.feather");
    entities.csv = paste0(where, "/rec.data_handles.csv");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));
        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Handle = col_character(),
                            HomeNode = col_integer(),
                            Size = col_integer(),
                            Coordinates = col_character()
                        ));

        # Not supported in feather
        # pm$Coordinates <- lapply(strsplit(pm$Coordinates, " "), as.integer);
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }
    
    filename <- 'pre.data_handles.feather';
    write_feather(pm, filename);
    return(filename);
}

tasks_csv_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/tasks.feather");
    entities.csv = paste0(where, "/rec.tasks.csv");

    task_handles <- task_handles_parser(where = where);

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));
        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Control = col_character(),
                            JobId = col_integer(),
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
                        ));
        # sort the data by the submit order
        pm <- pm[with(pm, order(SubmitOrder)), ]
        # Tasks have multiple handles, get them in a different structure
        handles_dep = pm %>% select(JobId) %>% mutate(Handles = strsplit(pm$Handles, " "),
                                Modes = strsplit(pm$Modes, " "),
                                Sizes = lapply(strsplit(pm$Sizes, " "), as.integer))
        # unnest the lists
        task_handles <- unnest(handles_dep);

        # We will save the task_handle structre, we can remove these columns
        pm <- pm %>% select(-Handles, -Modes, -Sizes)

        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }

    tasksFilename <- 'pre.tasks.feather';
    write_feather(pm, tasksFilename);
    handlesFilename <- 'pre.task_handles.feather';
    write_feather(task_handles, handlesFilename);
    return(list(tasks = tasksFilename, handles = handlesFilename));
}

task_handles_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/task_handles.feather");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        ret <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
        return(ret);
    }

    return(NULL);
}

the_fast_reader_function <- function (directory = ".")
{
    names <- c("State", "Variable", "Link", "DAG", "Y", "ATree", "Gaps", "pmtool",
               "pmtool_states", "data_handles", "tasks", "task_handles");

    filenames <- gsub("^", "pre.", gsub("$", ".feather", tolower(names)));

    l1 <- list(Origin = directory);
    l2 <- lapply(filenames, function(x) {
        filename = paste0(directory, "/", x);
        if (file.exists(filename)){
            read_feather(filename);
        }else{
            loginfo(paste("The file", x, "does not exist on that directory. Ignore."));
            NULL;
        }
    });
    names(l2) <- names;
    c(l1, l2);
}

aggregate_data <- function(directory, dfw, dfv, dfl, dfdag, dfhie, dfa, dpmtb, dpmts, ddh, dtasks) {
    return (list(Origin=directory, State=dfw, Variable=dfv, Link=dfl, DAG=dfdag, Y=dfhie, ATree=dfa,
                 pmtool=dpmtb, pmtool_states=dpmts, data_handles=ddh, tasks=dtasks$tasks, task_handles=dtasks$handles));
}

calculate_gaps <- function(applicationName, dfwFilename, dfdagFilename, dflFilename) {
    if(applicationName == 'cholesky') {
        return(gaps(dfwFilename, dfdagFilename, dflFilename));
    } else {
        return(NULL)
    }
}

gaps.f_backward <- function (data)
{
    # Create the seed chain
    if(TRUE %in% grepl("mpicom", data$DAG$JobId)){
        data$DAG %>%
            filter(grepl("mpicom", JobId)) -> tmpdag
    } else {
        data$DAG -> tmpdag
    }
    tmpdag %>%
        rename(DepChain = JobId, Member = Dependent) %>%
        select(DepChain, Member) -> seedchain;

    f2 <- function (dfdag, chain.i)
    {
        dfdag %>% select(JobId, Dependent, Application, Value) -> full.i;

        full.i %>% left_join(chain.i, by=c("JobId" = "Member")) -> full.o;

        # If there are no application tasks in dependency chains, keep looking
        if ((full.o %>% filter(!is.na(DepChain), Application == TRUE) %>% nrow) == 0) {
            # Prepare the new chain
            full.o %>%
                filter(!is.na(DepChain)) %>%
                rename(Member = Dependent) %>%
                select(DepChain, Member) -> chain.o;
            return(f2(full.o, chain.o));
        }else{
            return(full.o);
        }
    }
    return(f2(data$DAG, seedchain));
}

gaps.f_forward <- function (data)
{
    # Create the seed chain
    if(TRUE %in% grepl("mpicom", data$DAG$Dependent)){
        data$DAG %>%
            filter(grepl("mpicom", Dependent)) -> tmpdag
    } else {
        data$DAG -> tmpdag
    }
    tmpdag %>%
        rename(DepChain = Dependent, Member = JobId) %>%
        select(DepChain, Member) -> seedchain;

    f2 <- function (dfdag, chain.i)
    {
        dfdag %>% select(JobId, Dependent, Application, Value) -> full.i;

        full.i %>% left_join(chain.i, by=c("Dependent" = "Member")) -> full.o;

        # If there are no application tasks in dependency chains, keep looking
        if ((full.o %>% filter(!is.na(DepChain), Application == TRUE) %>% nrow) == 0) {
            # Prepare the new chain
            full.o %>%
                filter(!is.na(DepChain)) %>%
                rename(Member = JobId) %>%
                select(DepChain, Member) -> chain.o;
            return(f2(full.o, chain.o));
        }else{
            return(full.o);
        }
    }
    return(f2(data$DAG, seedchain));
}

gaps <- function (dfwFilename, dfdagFilename, dflFilename)
{
    loginfo("Starting the gaps calculation.");
    dfw <- read_feather(dfwFilename);
    dfdag <- read_feather(dfdagFilename);
    dfl <- read_feather(dflFilename);
    data <- list(State = dfw, DAG = dfdag, Link = dfl);

    if(is.null(data$DAG)) return(NULL);
    if(is.null(data$State)) return(NULL);
    #if(is.null(data$Link)) return(NULL);

    gaps.f_backward(data) %>%
        filter(!is.na(DepChain)) %>%
        select(JobId, DepChain) %>%
        rename(Dependent = JobId) %>%
        rename(JobId = DepChain) %>%
        select(JobId, Dependent) %>% unique -> data.b;

    loginfo("backward completed");

    gaps.f_forward(data) %>%
        filter(!is.na(DepChain)) %>%
        select(JobId, DepChain) %>%
        rename(Dependent = DepChain) %>%
        select(JobId, Dependent) %>% unique -> data.f;

    loginfo("forward completed");

    data$DAG %>%
        filter(Application == TRUE) %>%
        select(JobId, Dependent) -> data.z;

    loginfo("z completed");

    # Create the new gaps DAG
    dfw <- data$State %>%
        filter(Application == TRUE) %>%
        select(JobId, Value, ResourceId, Node, Start, End);
    if(is.null(data$Link)){
        dfl <- data.frame()
        data.b.dag <- data.frame()
        data.f.dag <- data.frame()
    } else {
        dfl <- data$Link %>%
            filter(grepl("mpicom", Key)) %>%
            mutate(Value = NA, ResourceId = Origin, Node = NA) %>%
            rename(JobId = Key) %>%
            select(JobId, Value, ResourceId, Node, Start, End);
            data.b %>%
                left_join(dfl, by=c("JobId" = "JobId")) %>%
                left_join(dfw, by=c("Dependent" = "JobId")) -> data.b.dag;
        data.f %>%
            left_join(dfw, by=c("JobId" = "JobId")) %>%
            left_join(dfl, by=c("Dependent" = "JobId")) -> data.f.dag;
    }
    data.z %>%
        left_join(dfw, by=c("JobId" = "JobId")) %>%
        left_join(dfw, by=c("Dependent" = "JobId")) -> data.z.dag;

    loginfo("The gaps calculation has completed.");

    gaps <- bind_rows(data.z.dag, data.b.dag, data.f.dag);

    filename <- 'pre.gaps.feather';
    write_feather(gaps, filename);
    return(filename);
}

save_feathers <- function(data, gaps) {
    # State
    filename <- "pre.state.feather";
    loginfo(filename);
    if (!is.null(data$State)){
        write_feather(data$State, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # Variable
    filename <- "pre.variable.feather";
    loginfo(filename);
    if (!is.null(data$Variable)){
        write_feather(data$Variable, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # Link
    filename <- "pre.link.feather";
    loginfo(filename);
    if (!is.null(data$Link)){
        write_feather(data$Link, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # DAG
    filename <- "pre.dag.feather";
    loginfo(filename);
    if (!is.null(data$DAG)){
        write_feather(data$DAG, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # Y
    filename <- "pre.y.feather";
    loginfo(filename);
    if (!is.null(data$Y)){
        write_feather(data$Y, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # ATree
    filename <- "pre.atree.feather";
    loginfo(filename);
    if (!is.null(data$ATree)){
        write_feather(data$ATree, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # Gaps
    filename <- "pre.gaps.feather";
    loginfo(filename);
    if (!is.null(gaps)){
        write_feather(gaps, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # PMtool
    filename <- "pre.pmtool.feather";
    loginfo(filename);
    if (!is.null(data$pmtool)){
        write_feather(data$pmtool, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    filename <- "pre.pmtool_states.feather";
    loginfo(filename);
    if (!is.null(data$pmtool_states)){
        write_feather(data$pmtool_states, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # Data Rec
    filename <- "pre.data_handles.feather";
    loginfo(filename);
    if (!is.null(data$data_handles)){
        write_feather(data$data_handles, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    # Tasks Rec
    filename <- "pre.tasks.feather";
    loginfo(filename);
    if (!is.null(data$tasks)){
        write_feather(data$tasks, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }

    filename <- "pre.task_handles.feather";
    loginfo(filename);
    if (!is.null(data$task_handles)){
        write_feather(data$task_handles, filename);
    }else{
        loginfo(paste("Data for", filename, "has not been feathered because is empty."));
    }
}

starpu_states <- function()
{
    c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task");
}

cholesky_states <- function()
{
    cholesky_colors() %>% .$Kernel;
}

scalfmm_states <- function()
{
    scalfmm_colors() %>% .$Kernel;
}
cholesky_colors <- function()
{
    tibble(
        Kernel = c("potrf", "trsm", "syrk", "gemm"),
        Color = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"));
}

cfd_colors <- function()
{
    tibble(
        Kernel = c("fluid_bound", "diffuse_1", "diffuse_relax", "macCormack_commit", "macCormack_2", "macCormack_1", "obstacle_boundary_1", "conserve_1", "conserve_relax", "conserve_commit", "obstacle_velocity", "initial_state"),
        Color = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#ea1a1c", "#37beb8", "#4eaf4a", "#9a4ea3"));
}

scalfmm_colors <- function()
{
    tibble(
# For the trace I've been given
        Kernel = c("L2L-level", "L2P",     "M2L-level", "M2L-out-level", "M2M",     "P2M",     "P2P",     "P2P-out"),
        Color =  c("#e41a1c",   "#377eb8", "#4daf4a",   "#984ea3",       "#ff7f00", "#ffff33", "#a65628", "#f781bf"));

# For paper https://hal.inria.fr/hal-01474556/document
#        Kernel = c("L2L",     "L2P",     "M2L_in",  "M2L_out", "M2M",     "P2M",     "P2P_in",  "P2P_out"),
#        Color =  c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf"));
}

library(RColorBrewer);
starpu_colors <- function()
{
    tibble(Value = starpu_states()) %>%
        # Get colors from Set3
        mutate(Color = brewer.pal(nrow(.), "Set3")) %>%
        # Adopt Luka suggestion: Idle = orange; Sleeping = rose
        mutate(Color = case_when(Value == "Idle" ~ "#FDB462",
                                 Value == "PushingOutput" ~ "#BEBADA",
                                 TRUE ~ Color)) -> t;
    # Transform to a nice named list for ggplot
    ret <- t %>% pull(Color)
    names(ret) <- t %>% pull(Value);
    return(ret);
}

cholesky_pastix_colors <- function()
{
    tibble(
        Kernel = c("blok_dpotrfsp1d_panel",
                   "cblk_dpotrfsp1d_panel",
                   "blok_dtrsmsp",
                   "blok_dgemmsp",
                   "cblk_dgemmsp"),
        Color = c("#e41a1c",
                  "#000000",
                  "#377eb8",
                  "#4daf4a",
                  "#c0c0c0"));
}


outlier_definition <- function(x) {
    (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}