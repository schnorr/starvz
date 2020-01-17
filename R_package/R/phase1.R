
read_state_csv <- function (where = ".",
                            app_states_fun = NULL,
                            outlier_fun = NULL,
                            state_filter = 0,
                            whichApplication = NULL)
{
    # Check obligatory parameters
    if(is.null(whichApplication)) stop("whichApplication is NULL, it should be provided");
    if(is.null(app_states_fun)) stop("app_states_fun should be provided to read_state_csv");
    if(!is.data.frame(app_states_fun())) stop("app_states_fun is not returning a data frame");
    if(!all((app_states_fun() %>% names) == c("Kernel", "Color"))) stop("Expecting that app_states_fun returns a dataframe with two columns: Kernel, Color")
    if(is.null(outlier_fun)) stop("outlier_fun should be provided to read_state_csv");

    state.feather = paste0(where, "/paje.state.feather");
    state.csv = paste0(where, "/paje.state.csv");
    if(file.exists(state.feather)){
        loginfo(paste("Reading ", state.feather));
        dfw <- read_feather(state.feather);
        loginfo(paste("Read of", state.feather, "completed"));
    }else if(file.exists(state.csv)){
        loginfo(paste("Reading ", state.csv));
        dfw <- read_csv(file=state.csv,
                        trim_ws=TRUE,
                        progress=FALSE,
                        col_types=cols(
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
                        ));
        loginfo(paste("Read of", state.csv, "completed"));
    }else{
        stop(paste("Files", state.feather, "or", state.csv, "do not exist"));
    }

    dfw <- dfw %>%
        mutate(Value = as.factor(Value),
               GFlop = as.numeric(GFlop),
               X = as.integer(X),
               Y = as.integer(Y),
               Iteration = as.integer(Iteration),
               Subiteration = as.integer(Subiteration));

    if ((dfw %>% nrow) == 0) stop("After reading states, number of rows is zero.");

    # QRMumps: fix qrmumps kernels names so we have a clean color definition
    if (whichApplication == "qrmumps"){
        dfw <- dfw %>%
            mutate(Value = gsub("_perf.*", "", Value)) %>%
            mutate(Value = gsub("qrm_", "", Value));
        loginfo("Fix of qrmumps state naming completed.");
    }

    # Split application and starpu behavior
    # state_filter:
    # 0 = Based on Runtime fixed States
    # 1 = Based on stricted application name states
    # 2 = Based on non-stricted application name states
    if (state_filter == 0){
        loginfo("Selecting application states based on runtime states.");
        dfw <- dfw %>% mutate(Application = case_when( .$Value %in% all_starpu_states() ~ FALSE, TRUE ~ TRUE));
    }else if (state_filter == 1){
        loginfo("Selecting application states based on custom application stricted states names.");
        # If strict, states need to be exact
        dfw <- dfw %>% mutate(Application = case_when(.$Value %in% (app_states_fun() %>% .$Kernel) ~ TRUE, TRUE ~ FALSE));
    }else if (state_filter == 2){
        loginfo("Selecting application states based on custom application non-stricted states names.");
        # If not strict, we mark using app_states_fun() Kernel field as RE
        state_condition = paste((app_states_fun() %>% .$Kernel), collapse='|');
        dfw <- dfw %>% mutate(Application = case_when(grepl(state_condition, .$Value) ~ TRUE, TRUE ~ FALSE));
    }

    if ((dfw %>% nrow) == 0) stop("After application states check, number of rows is zero.");

    loginfo("App state filter completed");

    # remove all application states with NA
    # StarPU is dumping two lines per application state (so, fix in R)
    dfw <- dfw %>% filter(Application == FALSE | (Application == TRUE & !is.na(JobId)));

    # Create three new columns (Node, Resource, ResourceType) - This is StarPU-specific
    # But first, check if this is a multi-node trace (if there is a _, it is a multi-node trace)
    # TODO This is a very weak test, should find something else instead
    firstResourceId <- dfw %>% .$ResourceId %>% unique %>% sort %>% head(n=1);
    if (grepl("CUDA|CPU", unlist(strsplit(firstResourceId, "_"))[2])){
        loginfo("This is multi-node trace");
        # This is the case for multi-node trace
        dfw <- dfw %>%
            separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
            mutate(Node = as.factor(Node)) %>%
            mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource)));
    }else{
        loginfo("This is a single-node trace...");
        # This is the case for SINGLE node trace
        dfw <- dfw %>%
            mutate(Node = as.factor(0)) %>%
            mutate(Resource = ResourceId) %>%
            mutate(ResourceType = as.factor(gsub('[_[:digit:]]+', '', ResourceId)));
        loginfo("Node, Resource, ResourceType definition is ready.");
    }

    loginfo("Define colors is starting right now.");

    # In case application is not specified
    if(whichApplication==""){
      # Get only application states
      dfcolors <- dfw %>% filter(Application == TRUE) %>%
          select(Value) %>%
          unique()

      # Get the number of states to generate colors
      nc <- dfcolors %>% nrow()

      # TODO: Using set1 right now to generate colors, max of 9 states
      c <- brewer.pal(n = nc, name = "Set1")

      # Match States and Colors
      dfcolors <- dfcolors %>% mutate(Color = c) %>%
          arrange(Value, Color) %>%
          mutate(Color = na.locf(Color, na.rm=FALSE)) %>%
          unique;

    }else{

      # Define colors
      dfcolors <- dfw %>%
          select(Value) %>%
          unique %>%
          mutate(Value = as.character(Value), Color = NA) %>%
          rbind(app_states_fun() %>% rename(Value = Kernel)) %>%
          arrange(Value, Color) %>%
          mutate(Color = na.locf(Color, na.rm=FALSE)) %>%
          unique;

      loginfo("Colors data.frame is defined.");
    }
    # Apply
    dfw <- dfw %>% left_join(dfcolors, by="Value");

    loginfo("Left joining the colors has completed.");

    # Specific cholesky colors: sufficiently harmless
    if (whichApplication == "cholesky"){
        loginfo("This is a cholesky application, colors are hard-coded");
        dfw <- dfw %>%
            mutate(Color = case_when(
                       grepl("potrf", .$Value) ~ "#e41a1c",
                       grepl("trsm", .$Value) ~ "#377eb8",
                       grepl("syrk", .$Value) ~ "#984ea3",
                       grepl("gemm", .$Value) ~ "#4daf4a",
		                   grepl("plgsy", .$Value) ~ "yellow",
                       TRUE ~ .$Color));
    }

    if (whichApplication == "lu"){
        loginfo("This is a lu application, colors are hard-coded");
        dfw <- dfw %>%
            mutate(Color = case_when(
                       grepl("getrf", .$Value) ~ "#e41a1c",
                       grepl("trsm", .$Value) ~ "#377eb8",
                       grepl("gemm", .$Value) ~ "#4daf4a",
                       grepl("plgsy", .$Value) ~ "yellow",
                       TRUE ~ .$Color));
    }

    # Detect outliers
    if (whichApplication == "cholesky"){
        loginfo("Attempt to detect outliers using a basic model.");
        dfw <- dfw %>%
            group_by(Value, ResourceType) %>%
            mutate(Outlier = ifelse(Duration > outlier_fun(Duration), TRUE, FALSE)) %>%
            ungroup ();
    }else if(whichApplication == "qrmumps"){

        # Step 0: Define the linear models for outlier classification and select one based on ib
        task_model_ib_other <- function(df) {
          model = lm(Duration ~ I(GFlop**(2/3)), data = df)
        }
        task_model_ib_1 <- function(df) {
          model = lm(Duration ~ GFlop, data = df)
        }

        if(file.exists("conf.txt")) {
          ib <- as.integer(gsub("\\D", "", c(grep("qrm_ib", readLines("conf.txt"), value = TRUE))))
        } else {
          stop(paste("File conf.txt do not exist!"));
        }

        if(ib != 1) {
          loginfo("Attempt to detect outliers for QRMumps using Duration ~ GFlops**(2/3)")
          task_model <- task_model_ib_other
        } else {
          loginfo("Attempt to detect outliers for QRMumps using Duration ~ GFlops")
          task_model <- task_model_ib_1
        }

        # Step 1: apply the model to each task, considering the ResourceType
        dfw %>%
            filter(grepl("lapack_", Value)) %>%
            filter(Application, Type == "Worker State") %>%
            unique() %>%
            group_by(ResourceType, Value) %>%
            nest() %>%
            mutate(model = map(data, task_model)) %>%
            mutate(outliers = map(model, function(m) {
                tibble(Row = names(outlierTest(m, n.max=Inf)$rstudent))
            })) -> df.pre.outliers

        # Step 2: identify outliers rows
        df.pre.outliers %>%
            unnest(outliers) %>%
            mutate(Row = as.integer(Row), Outlier=TRUE) %>%
            ungroup() -> df.pos.outliers

        # Step 3: unnest all data and tag create the Outiler field according to the Row value
        df.pre.outliers %>%
            unnest(data) %>%
            # this must be identical to the grouping used in the step 1
            group_by(Value, ResourceType) %>%
            mutate(Row = 1:n()) %>%
            ungroup() %>%
            # the left join must be by exactly the same as the grouping + Row
            left_join(df.pos.outliers, by=c("Value", "Row", "ResourceType")) %>%
            mutate(Outlier = ifelse(is.na(Outlier), FALSE, Outlier)) %>%
            select(-Row) %>%
            ungroup() -> df.outliers

        # Step 4: regroup the Outlier data to the original dfw
        dfw <- dfw %>%
            left_join(df.outliers %>%
                        select(JobId, Outlier), by=c("JobId"))

    }else{
        loginfo("No outlier detection; use NA in the corresponding column.");
        dfw <- dfw %>%
            mutate(Outlier = NA);
    }

    loginfo("Define the ZERO timestamp.");

    # Define the global ZERO (to be used with other trace date)
    ZERO <<- dfw %>% filter(Application == TRUE) %>% .$Start %>% min;

    # The new zero because of the long initialization phase
    dfw <- dfw %>% mutate(Start = Start - ZERO, End = End - ZERO);

    # Get rid of all observations before ZERO
    dfw <- dfw %>% filter(Start >= 0);

    # The problem is that Vinicius traces do not have "Iteration" column
    if (whichApplication == "cholesky"){
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
    }

    # QRMumps case:
    # When the trace is from qr_mumps (by Ian), the elimination tree
    # node is encoded in the Tag field, we need to convert it to the
    # appropriate ANode using the following code. We do that for all kind
    # of traces, but the ANode column is only valid for the qr_mump traces.
    if (whichApplication == "qrmumps"){
        dfw <- dfw %>% mutate(ANode = NA, ANode = as.character(strtoi(as.integer(paste0("0x", substr(.$Tag, 9, 16))))));
    }

    return(dfw);
}
read_vars_set_new_zero <- function (where = ".")
{
    variable.feather = paste0(where, "/paje.variable.feather");
    variable.csv = paste0(where, "/paje.variable.csv");
    if(file.exists(variable.feather)){
        loginfo(paste("Reading ", variable.feather));
        dfv <- read_feather(variable.feather);
        loginfo(paste("Read of", variable.feather, "completed"));
    }else if(file.exists(variable.csv)){
        loginfo(paste("Reading ", variable.csv));
        dfv <- read_csv(variable.csv,
                        trim_ws=TRUE,
                        progress=FALSE,
                        col_types=cols(
                            Nature = col_character(),
                            ResourceId = col_character(),
                            Type = col_character(),
                            Start = col_double(),
                            End = col_double(),
                            Duration = col_double(),
                            Value = col_double()
                        ));
        loginfo(paste("Read of", variable.csv, "completed"));
    }else{
        stop(paste("Files", variable.feather, "or", variable.csv, "do not exist"));
    }

    dfv <- dfv %>%
        # the new zero because of the long initialization phase
        mutate(Start = Start - ZERO, End = End - ZERO) %>%
        # filter all variables during negative timings
        filter(Start >= 0, End >= 0) %>%
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

atree_load <- function(where = "."){
    atree.feather = paste0(where, "/atree.feather");
    atree.csv = paste0(where, "/atree.csv");

    if (file.exists(atree.feather)){
        loginfo(paste("Reading ", atree.feather));
        df <- read_feather(atree.feather);
    }else if (file.exists(atree.csv)){
        loginfo(paste("Reading ", atree.csv));
        df <- read_csv(file=atree.csv,
                       trim_ws=TRUE,
                       progress=FALSE,
                       col_types=cols(
                           Node = col_integer(),
                           DependsOn = col_integer()
                       ));
    }else{
        loginfo(paste("Files", atree.feather, "or", atree.csv, "do not exist."));
        return(NULL);
    }

    intermediary_nodes <- df %>% select(Node) %>% .$Node %>% unique;

    loginfo(paste("Calculating graphical properties of the elimination tree"));

    df %>%
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
        mutate(Intermediary = case_when(.$ANode %in% intermediary_nodes ~ TRUE, TRUE ~ FALSE)) -> df;
    return(df);
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


the_reader_function <- function (directory = ".", app_states_fun = NULL, state_filter = 0, whichApplication = NULL)
{
    # Logging configuration
    basicConfig();
    logForOrg <- function(record) { paste(record$levelname, record$logger, record$msg, sep=':') }
    addHandler(writeToConsole, formatter=logForOrg);
    removeHandler('basic.stdout');

    # Start of reading procedure
    if(is.null(app_states_fun)) stop("app_states_fun is obligatory for reading");

    file_can_be_read <- function(filename)
    {
        if ((file.exists(filename)) & (file.size(filename) > 0)){
            return(TRUE);
        }else{
            return(FALSE);
        }
    }

    # Read the elimination tree
    dfa <- atree_load(where = directory);

    # Read states
    dfw <- read_state_csv (where = directory,
                           app_states_fun=app_states_fun,
                           outlier_fun=outlier_definition,
                           state_filter=state_filter,
                           whichApplication = whichApplication) %>%
        hl_y_coordinates(where = directory);

    # QRMumps case:
    # If the Atree is available and loaded, we create new columns for each task
    # to hold Y coordinates for the temporal elimination tree plot
    if (!is.null(dfa)){
        dfap <- dfa %>% select(-Parent, -Depth) %>% rename(Height.ANode = Height, Position.ANode = Position);
        dfw <- dfw %>% left_join(dfap, by="ANode");
        dfap <- NULL;
    }

    if(dfw %>% nrow == 0) stop("After reading states, number of rows is zero.");

    # Read variables
    dfv <- read_vars_set_new_zero(where = directory);

    # Read links
    dfl <- read_links (where = directory);

    # Read DAG
    dfdag <- read_dag (where = directory, dfw, dfl);
    if (is.null(dfdag)){
        # If dag is not available, try Vinicius DAG
        dagVinCSV <- paste0(directory, "/dag_vinicius.csv");
        loginfo(paste("Reading DAG", dagVinCSV));

        # Check if this a DAG from Vinicius
        if (file_can_be_read(dagVinCSV)){
            dfdag <- read_csv(dagVinCSV, trim_ws=TRUE, col_names=TRUE) %>%
                mutate(Dependent = strsplit(Dependent, " ")) %>%
                unnest(Dependent) %>%
                merge(., (dfw), #%>% filter(Application == TRUE))
                      by.x="JobId", by.y="JobId", all=TRUE) %>%
                mutate(Cost = ifelse(is.na(Duration), 0, -Duration)) %>%
                as_tibble();
        }else{
            dfdag <- NULL;
        }
    }

    # Read entities.csv and register the hierarchy (with Y coordinates)
    dfhie <- hl_y_paje_tree (where = directory);

    # PMTool information
    dpmtb <- pmtool_bounds_csv_parser (where = directory);

    dpmts <- pmtool_states_csv_parser (where = directory, whichApplication = whichApplication, Y=dfhie, States = dfw);

    # Data.rec
    ddh <- data_handles_csv_parser (where = directory);

    # Papi.rec
    dpapi <- papi_csv_parser (where = directory);

    # Tasks.rec
    dtasks <- tasks_csv_parser (where = directory);

    loginfo("Assembling the named list with the data from this case.");

    # Events
    devents <- events_csv_parser (where = directory);

    # Enframe ZERO
    ZERO <- enframe(ZERO, name = NULL)
    
    data <- list(Origin=directory, State=dfw, Variable=dfv, Link=dfl, DAG=dfdag, Y=dfhie, Atree=dfa,
                 Pmtool=dpmtb, Pmtool_states=dpmts, Data_handles=ddh, Papi=dpapi, Tasks=dtasks$Tasks, Task_handles=dtasks$handles, Events=devents, Zero=ZERO);

    loginfo("Call Gaps.");
    data$Gaps <- gaps(data);

    return(data);
}

hl_y_paje_tree <- function (where = ".")
{
    entities.feather = paste0(where, "/entities.feather");
    entities.csv = paste0(where, "/entities.csv");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        dfe <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));
        dfe <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Parent = col_character(),
                            Name = col_character(),
                            Type = col_character(),
                            Nature = col_character()
                        ));
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }

    # first part: read entities, calculate Y
    # If this file is read with readr's read_csv function, the data.tree does not like

    if ((dfe %>% nrow) == 0) stop(paste("After reading the entities file, the number of rows is zero"));

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

pmtool_bounds_csv_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/pmtool.feather");
    entities.csv = paste0(where, "/pmtool.csv");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));
        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Alg = col_character(),
                            Time = col_double()
                        ));
        # pmtool gives time in microsecounds
        pm[[2]] <- pm[[2]]/1000
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }
    ret <- pm;
    return(ret);
}

pmtool_states_csv_parser <- function (where = ".", whichApplication = NULL, Y = NULL, States = NULL)
{
    entities.feather = paste0(where, "/pmtool_states.feather");
    entities.csv = paste0(where, "/pmtool_states.csv");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));

        #sched Tid   worker taskType JobId start duration end

        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            sched = col_character(),
                            Tid = col_integer(),
                            worker = col_integer(),
                            taskType = col_character(),
                            JobId = col_character(),
                            start = col_double(),
                            duration = col_double(),
                            end = col_double()
                        ));
        #pmtool states gives time in milisecounds

        pm[[6]] <- pm[[6]]/1000
        pm[[7]] <- pm[[7]]/1000
        pm[[8]] <- pm[[8]]/1000

        names(pm)[names(pm) == 'taskType'] <- 'Value'
        names(pm)[names(pm) == 'start'] <- 'Start'
        names(pm)[names(pm) == 'end'] <- 'End'
        names(pm)[names(pm) == 'duration'] <- 'Duration'
        names(pm)[names(pm) == 'worker'] <- 'ResourceId'

        pm <- separate(data = pm, col = JobId, into = c("JobId", "Tag"), sep = "\\:")

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

        pm[[3]] <- devices[pm[[3]]+1]

        pm <- pm %>% left_join((Y %>% select(-Type, -Nature)), by=c("ResourceId" = "Parent"))
        #print(States)
        #print(pm)
        pm <- pm %>% left_join((States %>% select(Iteration, JobId)), by=c("JobId" = "JobId"))

        if (whichApplication == "cholesky"){
            pm <- pm %>%
                mutate(Color = case_when(
                           Value=="dpotrf" ~ "#e41a1c",
                           Value=="dtrsm" ~ "#377eb8",
                           Value=="dsyrk" ~ "#984ea3",
                           Value=="dgemm" ~ "#4daf4a",
                           Value=="dplgsy" ~ "yellow",
                           TRUE ~ "#000"));
        }

        #print(pm)
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }
    ret <- pm;
    return(ret);
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
                            Coordinates = col_character(),
                            MPIOwner = col_integer(),
                            Description = col_character(),
                            MPITag = col_integer()
                        ));

        # Not supported in feather
        # pm$Coordinates <- lapply(strsplit(pm$Coordinates, " "), as.integer);
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }
    ret <- pm;

    return(ret);
}

papi_csv_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/papi.feather");
    entities.csv = paste0(where, "/rec.papi.csv");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));
        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            JobId = col_character(),
                            PapiEvent = col_character(),
                            Value = col_integer()
                        ));
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }
    ret <- pm;

    return(ret);
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

tasks_csv_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/tasks.feather");
    entities.csv = paste0(where, "/rec.tasks.csv");

    task_handles <- task_handles_parser(where = where);

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv) & file.info(entities.csv)$size > 0){
        loginfo(paste("Reading ", entities.csv));
        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
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

    return( list(tasks = pm, handles=task_handles) );
}

events_csv_parser <- function (where = ".")
{
    entities.feather = paste0(where, "/events.feather");
    entities.csv = paste0(where, "/paje.events.csv");


    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        pm <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));

        pm <- read_csv(entities.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Nature = col_character(),
                            Contairner = col_character(),
                            Type = col_character(),
                            Start = col_double(),
                            Value = col_character(),
                            Handle = col_character(),
                            Info = col_integer(),
                            Size = col_integer(),
                            Tid = col_character(),
                            Src = col_character()
                        ));
        # sort the data by the start time
        pm <- pm[with(pm, order(Start)), ]

        # Read links
        pm <- pm %>%
            # the new zero because of the long initialization phase
            mutate(Start = Start - ZERO)

        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }

    return(pm);
}

hl_y_coordinates <- function (dfw = NULL, where = ".")
{
    if (is.null(dfw)) stop("The input data frame with states is NULL");

    # first part: read entities, calculate Y
    workertreedf <- hl_y_paje_tree (where);

    # second part: left join with Y
    dfw <- dfw %>>%
        # the left join to get new Y coordinates
        left_join (workertreedf, by=c("ResourceId" = "Parent", "Type" = "Type", "Nature" = "Nature"));

    return(dfw);
}

tree_filtering <- function (dfe, natures, types)
{
    loginfo("Starting the tree filtering to create Y coordinates");

    dfe %>%
    # Mutate things to character since data.tree don't like factors
    mutate (Type = as.character(Type), Nature = as.character(Nature)) %>%
    # Filter things I can't filter using Prune (because Prune doesn't like grepl)
    # Note that this might be pottentially dangerous and works only for StarPU traces
    filter (!grepl("InCtx", Parent), !grepl("InCtx", Name)) %>%
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
        # qr mumps has duplicated data in these dfs and the left_join did not work correctly. unique() solves this problem
        full.i %>% unique() -> full.i;
        chain.i %>% unique() -> chain.i;
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
        # qr mumps has duplicated data in these dfs and the left_join did not work correctly. unique() solves this problem
        full.i %>% unique() -> full.i;
        chain.i %>% unique() -> chain.i;
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

gaps <- function (data)
{
    loginfo("Starting the gaps calculation.");

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

    return(bind_rows(data.z.dag, data.b.dag, data.f.dag));
}
read_links <- function (where = ".")
{
    link.feather = paste0(where, "/paje.link.feather");
    link.csv = paste0(where, "/paje.link.csv");
    if(file.exists(link.feather)){
        loginfo(paste("Reading ", link.feather));
        dfl <- read_feather(link.feather) %>%
            mutate(Size = as.integer(Size));
        loginfo(paste("Read of", link.feather, "completed"));
    }else if(file.exists(link.csv)){
        loginfo(paste("Reading ", link.csv));
        dfl <- read_csv(link.csv,
                        trim_ws=TRUE,
                        progress=FALSE,
                        col_types=cols(
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
                            Tag = col_character()
                        ));
        loginfo(paste("Read of", link.csv, "completed"));
    }else{
        loginfo(paste("Files", link.feather, "or", link.csv, "do not exist"));
        return(NULL);
    }

    # Check if number of lines is greater than zero
    if ((dfl %>% nrow) == 0){
        logwarn("After attempt to read links, number of rows is zero");
        return(NULL);
    }

    # Read links
    dfl <- dfl %>%
        # the new zero because of the long initialization phase
        mutate(Start = Start - ZERO, End = End - ZERO) %>%
        # filter all variables during negative timings
        filter(Start >= 0, End >= 0);

    return(dfl);
}
read_dag <- function (where = ".", dfw = NULL, dfl = NULL)
{
    dag.feather = paste0(where, "/dag.feather");
    dag.csv = paste0(where, "/dag.csv");
    if(file.exists(dag.feather)){
        loginfo(paste("Reading ", dag.feather));
        dfdag <- read_feather(dag.feather);
        loginfo(paste("Read of", dag.feather, "completed"));
    }else if(file.exists(dag.csv)){
        loginfo(paste("Reading ", dag.csv));
        dfdag <- read_csv(dag.csv,
                          trim_ws=TRUE,
                          progress=FALSE,
                          col_types=cols(
                              Node = col_integer(),
                              DependsOn = col_integer()
                          ));
        loginfo(paste("Read of", dag.csv, "completed"));
    }else{
        logwarn(paste("Files", dag.feather, "or", dag.csv, "do not exist"));
        return(NULL);
    }

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
}


starpu_states <- function()
{
    c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task", "Waiting all tasks", "Building task", "Deinitializing");
}

all_starpu_states <- function()
{
    c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task", "Waiting all tasks", "Building task", "Deinitializing", "Executing");
}

cholesky_states <- function()
{
    cholesky_colors() %>% .$Kernel;
}

qr_states <- function()
{
    qr_colors() %>% .$Kernel;
}

scalfmm_states <- function()
{
    scalfmm_colors() %>% .$Kernel;
}
lu_colors <- function()
{
    tibble(
        Kernel = c("getrf", "trsm", "gemm", "plgsy"),
        Color = c("#e41a1c", "#377eb8", "#4daf4a", "yellow"));
}

cholesky_colors <- function()
{
    tibble(
        Kernel = c("potrf", "trsm", "syrk", "gemm", "plgsy"),
        Color = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a", "yellow"));
}

cfd_colors <- function()
{
    tibble(
        Kernel = c("fluid_bound", "diffuse_1", "diffuse_relax", "macCormack_commit", "macCormack_2", "macCormack_1", "obstacle_boundary_1", "conserve_1", "conserve_relax", "conserve_commit", "obstacle_velocity", "initial_state"),
        Color = c("#e41a1c", "#377eb8", "#984ea3", "#9a4ea3", "#4daf4a", "#ffff33", "#a65628", "#f781bf", "#ea1a1c", "#37beb8", "#4eaf4a", "#ff7f00"));
}

qr_colors <- function()
{
    tibble(
        Kernel = c("dgeqrt", "dlarfb" , "dtpqrt" , "dtpmqrt", "lapack_dgeqrt", "lapack_dlarfb" , "lapack_dtpqrt" , "lapack_dtpmqrt"),
        Color = c("#96e3a2", "#f68285", "#d194d0",  "#9bb6dd", "#96e3a2", "#f68285", "#d194d0",  "#9bb6dd"));
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
  pre_colors <- brewer.pal(12, "Set3");
  pre_colors[13] = "#000000"
  pre_colors[14] = "#000000"
  tibble(Value = starpu_states()) %>%
      # Get colors from Set3
      mutate(Color = pre_colors) %>%
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

qrmumps_states_level_order <- function ()
{
    c(
        "Do_subtree",
        "INIT",
        "GEQRT",
        "GEMQRT",
        "TPQRT",
        "TPMQRT",
        "ASM",
        "CLEAN",
        "Idle");
}
qrmumps_states <- function ()
{
    c(
        "ASM",
        "GEMQRT",
        "Do_subtree",
        "CLEAN",
        "GEQRT",
        "INIT",
        "TPMQRT",
        "TPQRT",
        "Idle");
}

qrmumps_color_mapping <- function()
{
    #This vector changes the color ordering
    states = qrmumps_states();
    kcol <- data.frame(RGB=as.character(brewer.pal(9, "Set1")),
#These are only the color names I put manually here to try to understand the color mapping
#                       ColorName=c("red", "blue", "green", "purple", "orange", "yellow", "brown", "pink", "gray"),
                       StateName=factor(states, levels=qrmumps_states_level_order()),
                       xmin=1:length(states),
                       xmax=1:length(states)+1,
                       ymin=0,
                       ymax=1);
   kcol;
}

qrmumps_colors <- function()
{
qrmumps_color_mapping() %>%
    # Rename
    rename(Kernel = StateName, Color=RGB) %>%
    # Remove Idle
    filter(Kernel != "Idle") %>%
    # Change to character
    mutate(Kernel = as.character(Kernel), Color=as.character(Color)) %>%
    # Select only those necessary
    select(Kernel, Color) %>%
    # Change names according to latest modifications
    mutate(Kernel = case_when(
               .$Kernel == "ASM" ~ "assemble_block",
               .$Kernel == "GEMQRT" ~ "lapack_gemqrt",
               .$Kernel == "GEQRT" ~ "lapack_geqrt",
               .$Kernel == "TPMQRT" ~ "lapack_tpmqrt",
               .$Kernel == "TPQRT" ~ "lapack_tpqrt",
               .$Kernel == "Do_subtree" ~ "do_subtree",
               .$Kernel == "CLEAN" ~ "clean_front",
               .$Kernel == "INIT" ~ "init_front",
               TRUE ~ .$Kernel)) %>%
    # Add new kernels
    bind_rows (tibble(Kernel = c("init_block", "clean_block", "block_copy"),
                      Color = c("#FFFF33", "#984EA3", "#ad0a18")));
}

outlier_definition <- function(x) {
    (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}
