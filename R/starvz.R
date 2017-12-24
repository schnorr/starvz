suppressMessages(library(readr));
suppressMessages(library(zoo));
suppressMessages(library(feather));
suppressMessages(library(logging));
read_state_csv <- function (where = ".",
                            app_states_fun = NULL,
                            outlier_fun = NULL,
                            strict_state_filter = FALSE,
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
                        progress=TRUE,
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
    if (strict_state_filter){
        # If strict, states need to be exact
        dfw <- dfw %>% mutate(Application = case_when(.$Value %in% (app_states_fun() %>% .$Kernel) ~ TRUE, TRUE ~ FALSE));
    }else{
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
                       TRUE ~ .$Color));
    }

    # Detect outliers
    if (whichApplication == "cholesky"){
        loginfo("Attempt to detect outliers using a basic model.");
        dfw <- dfw %>%
            group_by(Value, ResourceType) %>%
            mutate(Outlier = ifelse(Duration > outlier_fun(Duration), TRUE, FALSE)) %>%
            ungroup ();
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
    if (whichApplication == "qrmumps"){
        dfw <- dfw %>% mutate(ANode = NA, ANode = as.character(strtoi(as.integer(paste0("0x", substr(.$Tag, 9, 16))))));
    }

    return(dfw);
}
suppressMessages(library(readr));
suppressMessages(library(dplyr));
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
                        progress=TRUE,
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
suppressMessages(library(data.tree));
suppressMessages(library(tidyverse));
suppressMessages(library(feather));
suppressMessages(library(logging));

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
                       progress=TRUE,
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
time_aggregation_prep <- function(dfw = NULL)
{
    if (is.null(dfw)) return(NULL);

    dfw_initial <- dfw %>%
        rename(Task = Value) %>%
        group_by (ResourceId, Task) %>%
        mutate(Value = 1) %>%
        select(-Duration, -Color, -Nature, -Type,
               -Size, -Depth, -Params, -JobId, -Footprint, -Tag,
               -GFlop, -X, -Y, -Z, -Iteration, -Subiteration,
               -Node, -Resource, -ResourceType, -Outlier, -Height,
               -Position);

    # Define the first zero
    dfw_zero_1 <- dfw_initial %>% slice(1) %>% mutate(StartN = 0, EndN = Start, Value = 0);

    # Define other zeroes
    dfw_zero_N <- dfw_initial %>% mutate(StartN = End, EndN = lead(Start), Value = 0);

    # Row bind them
    dfw_agg_prep <- dfw_zero_1 %>%
        bind_rows(dfw_zero_N) %>%
        mutate(Start = StartN, End = EndN) %>%
        select(-StartN, -EndN) %>%
        bind_rows(dfw_initial) %>%
        ungroup();

    # Set max end time for NA cases
    dfw_agg_prep <- dfw_agg_prep %>%
        filter(!complete.cases(.)) %>%
        mutate(End = max(dfw$End)) %>%
        bind_rows(dfw_agg_prep %>% filter(complete.cases(.))) %>%
        mutate (Duration = End - Start) %>%
        arrange(ResourceId, Task, Start);

    return(dfw_agg_prep);
}

time_aggregation_do <- function(dfw_agg_prep = NULL, step = NA)
{
    if (is.null(dfw_agg_prep)) return(NULL);
    if (is.na(step)) return(NULL);

    dfw_agg_prep %>%
        group_by(ResourceId, Task) %>%
        do(remyTimeIntegrationPrep(., myStep = step)) %>%
        mutate(Start = Slice, End = lead(Slice), Duration = End-Start) %>%
        ungroup() %>%
        na.omit()
}

time_aggregation_post <- function(dfw = NULL, dfw_agg = NULL)
{
    if (is.null(dfw)) return(NULL);
    if (is.null(dfw_agg)) return(NULL);

    df_ResourceId <- dfw %>%
        select(Nature, ResourceId, Type,
               Node, Resource, ResourceType, Position, Height) %>%
        unique;
    df_Task <- dfw %>%
        select(Value, Color) %>%
        unique;

    dfwagg_full <- dfw_agg %>%
        left_join(df_ResourceId, by="ResourceId") %>%
        left_join(df_Task, by=c("Task" = "Value"));

    return(dfwagg_full);
}

st_time_aggregation <- function(dfw = NULL, StarPU.View = FALSE, step = 100)
{
    if (is.null(dfw)) return(NULL);

    if (StarPU.View == FALSE){
        dfw <- dfw %>% filter(Application == TRUE);
    }
    
    dfw_agg_prep <- time_aggregation_prep (dfw);
    dfw_agg <- time_aggregation_do (dfw_agg_prep, step);
    dfwagg_full <- time_aggregation_post (dfw, dfw_agg);

    # Preparation for visualization (imitate geom_area)
    dfwagg_full %>%
        arrange(ResourceId, Task, Slice, Start) %>%
        group_by(ResourceId, Slice) %>%
        mutate(TaskHeight = (Height*0.9 * Value),
               TaskPosition = cumsum(TaskHeight) - TaskHeight) %>%
        ungroup() -> dfwagg_full_view;

    return(dfwagg_full_view);
}
extract_colors <- function(dfw = NULL)
{
    if(is.null(dfw)) return(NULL);

    dfw <- dfw %>% ungroup;

    dfw %>%
        select(Value, Color) %>%
        unique %>%
        .$Color %>%
        setNames(dfw %>% select(Value, Color) %>% unique %>% .$Value);
}
yconf <- function (dfw = NULL)
{
    if(is.null(dfw)) return(NULL);

    # Currently being ignored
    # step <- pjr_value(pajer$st$labels, 6);

    if(pjr_value(pajer$st$labels, "1") == "1CPU_per_NODE"){
        # One CPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(1) %>%
            ungroup;
    }else if(pjr_value(pajer$st$labels, "1") == "1GPU_per_NODE"){
        # One GPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(n()) %>%
            ungroup;
    }else{
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(c(1, n())) %>%
            ungroup;
    }
}
outlier_definition <- function(x) {
    (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}
suppressMessages(library(dplyr));
suppressMessages(library(reshape2));
suppressMessages(library(magrittr));
library(lpSolve);

abe_cpu_cuda <- function(dfl, debug=FALSE)
{
    result <- abe_cpu_cuda_inner(dfl, debug);
    ret <- result %>% pull(Result);
    return(tibble(Result=ret[[1]]$objval));
}

abe_cpu_cuda_details <- function(dfl, debug=FALSE)
{
    node <- dfl %>% slice(1) %>% pull(Node);
    lpresult <- abe_cpu_cuda_inner(dfl, debug);

    # Extract the solution from the LP
    lpresult %>% pull(Result) -> result;
    lpresult %>%
        select(Types, Values) %>%
        unnest(Types, .drop=FALSE) %>%
        unnest(Values, .drop=FALSE) %>%
        mutate(Count = result[[1]]$solution[1:nrow(.)],
               Estimation = TRUE) %>%
        rename(ResourceType = Types,
               Value = Values) -> ret;

    # Get the actual counts and merge everything together
    dfl %>%
        mutate(ResourceType = as.character(ResourceType)) %>%
        group_by(ResourceType, Value) %>%
        summarize(Count = n()) %>%
        ungroup %>%
        mutate(Estimation = FALSE) %>%
        bind_rows(ret) -> ret;

    # Get unique colors
    dfl %>%
        select(Value, Color) %>%
        unique -> dfcolor;

    ret %>%
        left_join(dfcolor, by=c("Value" = "Value")) -> ret;

    return(ret);
}

abe_cpu_cuda_inner <- function(dfl, debug=FALSE)
{
  columnNames = c("Node", "Resource", "ResourceType", "Value", "Duration");
  if (!all(columnNames %in% names(dfl))){
      print(paste("Obligatory columns names not present. They are ", columnNames));
      return(NULL);
  }

  #############
  # Input Section

  # The amount of nodes
  nnodes = dfl %>% select(Node) %>% unique %>% .$Node %>% sort %>% length;
  # The amount of resources
  df1.res_quantity <- dfl %>% select(Resource, ResourceType) %>% group_by(ResourceType) %>% summarize(Quantity=nnodes*length(unique(Resource))) %>% as.data.frame();
  # The mean duration of each task per resource
  df1.num_mean <- dfl %>% group_by(ResourceType, Value) %>% summarize(Num = n(), Mean=mean(Duration)) %>% as.data.frame();

  if(debug){
      print("ABE: Inicial metrics (v1)");
      print(nnodes);
      print(df1.res_quantity);
      print(df1.num_mean);
  }

  #############
  # Derived variables

  #Initial parameters simplification
  values <- df1.num_mean %>% select(Value) %>% arrange(Value) %>% .$Value %>% unique;
  types <- df1.num_mean %>% select(ResourceType) %>% arrange(ResourceType) %>% .$ResourceType %>% unique %>% as.character;
  names <- c(unlist(lapply(types, function(x) paste(x, values, sep="_"))), "Time");
  nvalues <- length(values);
  ntypes <- length(types);
  size = nvalues*ntypes;

  if (debug){
      print("ABE: Inicial metrics (v2)");
      print(values);
      print(types);
      print(names);
      print(nvalues);
      print(ntypes);
      print(size);
  }

  ############
  # The three parts

  #Part 1
  m.con1 <- cbind(do.call(cbind, replicate(ntypes, diag(1, nvalues, nvalues), simplify=FALSE)), rep(0, nvalues)) %>% set_colnames(names);
  m.dir1 <- rep("=", nvalues);
  m.rhs1 <- df1.num_mean %>% group_by(Value) %>% summarize(Sum = sum(Num)) %>% arrange(Value) %>% .$Sum;

  if (debug){
      print(m.con1);
      print(m.dir1);
      print(m.rhs1);
      print("End of Part 1");
  }

  #Part 2
  m <- df1.num_mean %>%
      arrange(ResourceType, Value) %>%
      dcast(ResourceType~Value, value.var="Mean", fill = 1E10) %>%
      select(-ResourceType) %>%
      set_colnames(NULL) %>%
      as.matrix();
  if (ntypes > 1){
      zeroes <- do.call("cbind", replicate((ntypes-1), matrix(0, ncol=nvalues, nrow=ntypes), simplify=FALSE))
      m <- cbind(m, zeroes) %>% set_colnames(NULL);
  }
  # Put zeroes in the beginning of the second line for the GPU
  # TODO: the next condition line is not generic for more than two resource types (but works with only one)
  if (ntypes > 1){
      m[2,] <- lag(m[2,], n = length(values), default=0.0);
  }
  m.con2 <- cbind(m, df1.res_quantity %>% arrange(ResourceType) %>% mutate(Quantity=as.numeric(Quantity)*-1) %>% .$Quantity); # %>% set_colnames(names);
  m.dir2 <- rep("<=", length(types));
  m.rhs2 <- rep(0, length(types));

  if (debug){
      print(m.con2);
      print(m.dir2);
      print(m.rhs2);
      print("End of Part 2");
  }

  #Part 3
  m.con3 <- cbind (diag(1, size), rep(0, size)); # %>% set_colnames(names);
  m.dir3 <- rep(">=", size);
  m.rhs3 <- rep(0, size);

  if (debug){
      print(m.con3);
      print(m.dir3);
      print(m.rhs3);
      print("End of Part 3");
  }

  ##########
  # Final + objective function

  #Row bind and concatenate everything:
  m.con <- rbind(m.con1, m.con2, m.con3);
  m.dir <- c(m.dir1, m.dir2, m.dir3);
  m.rhs <- c(m.rhs1, m.rhs2, m.rhs3);

  #Define the objective function
  f.obj <- c(rep(0, length(values) * length(types)), 1);

  #Call lp
  result <- lp("min", f.obj, m.con, m.dir, m.rhs);
  return(tibble(Result=list(result), Values=list(values), Types=list(types)));
}
library(Rcpp);
cppFunction(
    includes=c(
        "#include<boost/graph/dag_shortest_paths.hpp>",
        "#include<boost/graph/adjacency_list.hpp>"),'
NumericVector boost_shortest_path(int s, DataFrame df) {

//Properties
typedef boost::property<boost::edge_weight_t, double> WeightProperty;
typedef boost::property<boost::vertex_name_t, int> NameProperty;

//Graph Type
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, NameProperty, WeightProperty> Graph;

//Vertex Descriptor
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;

//The IndexMap
typedef boost::property_map<Graph,boost::vertex_index_t>::type IndexMap;

//The Predecessor and Distance Maps
typedef boost::iterator_property_map <Vertex*, IndexMap, Vertex, Vertex& > PredecessorMap;
typedef boost::iterator_property_map <double*, IndexMap, double, double& > DistanceMap;

//Graph Instance
Graph g1;

//Create the Vertices
std::map<int, Vertex> VertexMap;
std::map<Vertex, int> JobIdMap;
IntegerVector JobId = df["JobId"];
for (int i = 0; i < JobId.size(); i++) {
  if (VertexMap.find(JobId[i]) == VertexMap.end()) {
    Vertex v = boost::add_vertex(JobId[i], g1);
    VertexMap[JobId[i]] = v;
    JobIdMap[v] = JobId[i];
  }
}
IntegerVector Dependent = df["Dependent"];
for (int i = 0; i < Dependent.size(); i++) {
  if (VertexMap.find(Dependent[i]) == VertexMap.end()) {
    Vertex v = boost::add_vertex(Dependent[i], g1);
    VertexMap[Dependent[i]] = v;
    JobIdMap[v] = Dependent[i];
  }
}
//Get reference to starting vertex
Vertex source = VertexMap.find(s)->second;

//Create the Edges with the associated Cost
NumericVector Cost = df["Cost"];
for (int i = 0; i < JobId.size(); i++) {
   Vertex from = VertexMap.find(Dependent[i])->second;
   Vertex to = VertexMap.find(JobId[i])->second;
   boost::add_edge(from, to, Cost[i], g1);
}

// Create things necessary to calculate the costlier shortest path
std::vector<Vertex> predecessors(boost::num_vertices(g1)); // To store parents
std::vector<double> distances(boost::num_vertices(g1)); // To store distances

IndexMap indexMap = boost::get(boost::vertex_index, g1);
PredecessorMap predecessorMap(&predecessors[0], indexMap);
DistanceMap distanceMap(&distances[0], indexMap);

// Call the Function
boost::dag_shortest_paths(g1, source, boost::distance_map(distanceMap).predecessor_map(predecessorMap));

// Find out the costlier path
Vertex vtemp;
double cost = std::numeric_limits<double>::max();
boost::graph_traits<Graph>::vertex_iterator vi, vi_end;
for (boost::tie(vi, vi_end) = boost::vertices(g1); vi != vi_end; ++vi){
  if (distanceMap[*vi] < cost) {
    vtemp = *vi;
    cost = distanceMap[*vi];
  }
}

// Get the path
NumericVector ret;
while (vtemp != source){
  ret.push_back(JobIdMap[vtemp]);
  vtemp = predecessorMap[vtemp];
}
ret.push_back(JobIdMap[source]);
return ret;
}')
suppressMessages(library(tidyverse));
suppressMessages(library(data.tree));
suppressMessages(library(pipeR));
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
library(tidyverse);
library(ggplot2);
state_chart <- function (data = NULL, globalEndTime = NULL, ST.Outliers = TRUE, StarPU.View = FALSE)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    # Get traces
    dfw <- data$State;

    loginfo("Entry of state_chart");

    # Filter
    dfwapp = dfw %>%
        # Considering only application data
        filter(Application == TRUE) %>%
        # Considering only Worker State
        filter(Type == "Worker State");

    # Obtain time interval
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_states(data, ST.Outliers, StarPU.View);

    if (!StarPU.View){
        # The per-node ABE
        if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

        # add makespan
        if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

        # add idleness
        if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

        # add Global CPB
        if (pjr(pajer$st$cpb) || pjr(pajer$st$cpb_mpi$active)) gow = gow + geom_cpb(data);

        # check if task dependencies should be added
        if (pjr(pajer$st$tasks$active)){
            tasklist <- pajer$st$tasks$list;
            levels <- pjr_value(pajer$st$tasks$levels, 2);

            tasksel <- gaps_backward_deps (data = data,
                                           tasks = tasklist,
                                           levels = levels);

            gow = gow + geom_path_highlight(tasksel);
        }else{
            loginfo("DO NOT add dependencies");
        }


    }else{
        # add some color palette for StarPU States
        gow = gow + scale_fill_manual(values = starpu_colors());
    }

    loginfo("Exit of state_chart");
    return(gow);
}
library(ggplot2);
st_time_aggregation_plot <- function (data = NULL, dfw_agg = NULL, StarPU.View = FALSE)
{
    if (is.null(data)) return(NULL);
    if (is.null(dfw_agg)) return(NULL);

    loginfo("Entry Agg");

    # Considering only application or StarPU data
    if (StarPU.View == FALSE){
        dfw <- data$State %>% filter(Application == TRUE);
    }else{
        dfw <- data$State %>% filter(Application == FALSE);
    }
    # Obtain time interval
    tstart <- dfw %>% .$Start %>% min;
    tend <- dfw %>% .$End %>% max;

    #Calculate resources idleness
    total_time <- tend - tstart;

    # Prepare for colors
    choleskyColors <- extract_colors(dfw);

    #yconf
    yconfm <- yconf(dfw);

    loginfo("Plotting Agg");

    #Plot
    gow <- dfw_agg %>% ggplot() +
        default_theme() +
        coord_cartesian(xlim=c(tstart, tend)) +
        xlab("Time [ms]") +
        scale_fill_manual(values = choleskyColors) +
        scale_y_continuous(
            breaks = yconfm$Position,
            labels=yconfm$ResourceId) +
        # Print time-aggregated data
        geom_rect(aes(fill=Task,
                      xmin=Start,
                      xmax=End,
                      ymin=Position+TaskPosition,
                      ymax=Position+(TaskPosition+TaskHeight)), alpha=.5) +
        # Print outliers on top
        geom_rect(data=(dfw %>% filter(Outlier == TRUE)), 
                  aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Position,
                      ymax=Position+Height-0.4), alpha=1);

    if (!StarPU.View){
        # Y Label
        gow <- gow + ylab("Application Workers");

        # The per-node ABE
        if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

        # add makespan
        if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

        # add idleness
        if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

        # add Global CPB
        if (pjr(pajer$st$cpb)) gow = gow + geom_cpb(data);

        # check if task dependencies should be added
        if (pjr(pajer$st$tasks$active)){
            tasklist <- pajer$st$tasks$list;
            levels <- pjr_value(pajer$st$tasks$levels);

            tasksel <- gaps_backward_deps (data = data,
                                           tasks = tasklist,
                                           levels = levels);

            gow = gow + geom_path_highlight(tasksel);
        }else{
            loginfo("DO NOT add dependencies");
        }
    }else{
        # Y Label
        gow <- gow + ylab("StarPU Workers");

        # add some color palette for StarPU States
        gow = gow + scale_fill_brewer(palette = "Set1");
    }

    loginfo("Exit Agg");
    return(gow);
}
k_chart <- function (dfw)
{
    dfw %>%
        filter(Value == "spotrf") %>%
        mutate(k = as.integer(paste0("0x", substr(Tag, 14, 16)))) -> dft1;
    dfw %>%
        filter(Value %in% c("ssyrk", "strsm")) %>%
        mutate(k = as.integer(paste0("0x", substr(Tag, 8, 10)))) -> dft2;
    dfw %>%
        filter(Value == "sgemm") %>%
        mutate(k = as.integer(paste0("0x", substr(Tag, 8, 10)))) -> dft3;
    dfwijk <- rbind(dft1, dft2, dft3);
    dfwijk %>%
        group_by(k) %>%
        summarize(Start = min(Start), End=max(End), Duration=End-Start) %>%
        ggplot() +
        theme_bw(base_size=12) +
        xlab("Time [ms]") +
        ylab("Iteration") +
        scale_y_reverse() +
        geom_errorbarh(aes(x=(Start+((End-Start)/2)), xmin=Start, xmax=End, y=k)) +
        theme (
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.margin = unit(0, "mm"),
            panel.grid = element_blank(),
            legend.position = "top",
            legend.title = element_blank()) -> goijk;
    return(goijk);
}
k_chart <- function (dfw)
{
    dfw %>%
        filter(Application == TRUE) %>%
        group_by(Iteration) %>%
        summarize(Start = min(Start), End=max(End), Duration=End-Start) %>%
        ggplot() +
        theme_bw(base_size=12) +
        xlab("Time [ms]") +
        ylab("Iteration") +
        scale_y_reverse() +
        geom_errorbarh(aes(x=(Start+((End-Start)/2)), xmin=Start, xmax=End, y=Iteration)) +
        theme (
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.margin = unit(0, "mm"),
            panel.grid = element_blank(),
            legend.position = "top",
            legend.title = element_blank()) -> goijk;
    return(goijk);
}
k_chart <- function (dfw = NULL)
{
    if (is.null(dfw)) stop("dfw provided to k_chart is NULL");

    dfw <- dfw %>% filter(Application == TRUE);

    # Prepare for colors
    dfw %>% select(Value, Color) %>% unique %>% .$Color -> choleskyColors
    choleskyColors %>% setNames(dfw %>% select(Value, Color) %>% unique %>% .$Value) -> choleskyColors;

    # Prepare for borders
    dfborders <- dfw %>%
        group_by(Iteration) %>%
        summarize(Start = min(Start), End=max(End)) %>%
        mutate(IterationB = lead(Iteration), StartB = lead(Start)) %>%
        mutate(IterationE = lead(Iteration), EndB = lead(End)) %>%
        na.omit;

    # Height of each bar
    height = 0.8;

    dfw %>% ggplot() +
        guides(fill = guide_legend(nrow = 1)) +
        scale_fill_manual(values = choleskyColors) +
        theme_bw(base_size=12) +
        xlab("Time [ms]") +
        ylab("Cholesky\nIteration") +
        default_theme() +
        # Keep the alpha = 1 even if we use an alpha below
        guides(fill = guide_legend(override.aes = list(alpha=1))) +
        scale_y_reverse() +
        # The start border
        geom_curve(data=dfborders, aes(x=Start, xend=StartB, y=Iteration+height, yend=IterationB+height), curvature=0.1, angle=20) +
        # The end border
        geom_curve(data=dfborders, aes(x=End, xend=EndB, y=Iteration, yend=IterationB), curvature=-0.1, angle=20) +
        # The state
        geom_rect(aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Iteration,
                      ymax=Iteration+height), alpha=.5) -> goijk;
    return(goijk);
}
library(ggplot2);
var_chart <- function (dfv = NULL, ylabel = NA)
{
    if (is.null(dfv)) return(NULL);

    variable <- dfv %>% select(Type) %>% .$Type %>% unique;
    if (is.na(ylabel)) ylabel = variable;

    k <- dfv %>% rename(x=Start, xend=End, y=Value) %>% mutate(yend=y) %>% select(-Duration);
    v <- k %>% group_by(ResourceId, Type) %>% mutate(xend=x, y=y, yend=lag(y));# %>% na.omit();
    k %>%
        ggplot(aes(x=x, xend=xend, y=y, yend=yend, color=ResourceId)) +
        default_theme() +
        geom_segment() +
        geom_segment(data=v) +
        geom_point(size=.1) +
        coord_cartesian(xlim=c(0, max(dfv$End))) +
        ylim (0, NA) +
        ylab (ylabel) +
        scale_colour_brewer(palette = "Dark2");
}
library(ggplot2);
var_cumulative_chart <- function (dfv = NULL)
{
    if (is.null(dfv)) return(NULL);

    variable <- dfv %>% select(Type) %>% .$Type %>% unique;

    dfv %>%
        ggplot(aes(x=Start, y=Value, fill=Node)) +
        geom_area() +
        xlab ("Time [ms]") +
        ylab (variable) +
        theme_bw(base_size = 12) +
        theme (
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.margin = unit(.2, "line"),
            panel.grid = element_blank(),
            legend.position = "top",
            legend.title = element_blank()
        )
}
var_simple_chart <- function (dfv = NULL, ylabel = NA)
{
    if (is.null(dfv)) return(NULL);

    variable <- dfv %>% select(Type) %>% .$Type %>% unique;
    if (is.na(ylabel)) ylabel = variable;

    dfv %>%
        ggplot(aes(x=Start, y=Value, color=ResourceId)) +
        geom_line() +
        geom_point(size=.1) +
        xlab ("Time [ms]") +
        coord_cartesian(xlim=c(0, max(dfv$End))) +
        ylim (0, NA) +
        ylab (ylabel) +
        theme_bw(base_size = 12) +
        scale_fill_brewer(palette = "Set1") +
        theme (
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.margin = unit(.2, "line"),
            panel.grid = element_blank(),
            legend.position = "top",
            legend.title = element_blank()
        );
}
var_integration_chart <- function (dfv = NULL, ylabel = NA, step = 250, facetting = FALSE)
{
    if (is.null(dfv)) return(NULL);

    variable <- dfv %>% select(Type) %>% .$Type %>% unique;
    if (is.na(ylabel)) ylabel = variable;

    dfv %>%
        group_by(Node, ResourceType) %>%
        arrange(Node, Start) %>%
        do(remyTimeIntegrationPrep(., myStep = step)) %>%
        ggplot(aes(x=Slice, y=Value, color=Node)) +
        default_theme() +
        geom_point(size=1) +
        geom_line() +
        coord_cartesian(xlim=c(0, max(dfv$End))) +
        ylim (0, NA) +
        ylab (ylabel) +
        scale_fill_brewer(palette = "Set1") -> result;
    if (facetting){
        result <- result +
            facet_wrap(~ResourceType, ncol=1, scales="free_y") +
            theme(
                strip.background=element_rect(fill="white"),
                strip.placement="inside",
                panel.spacing=unit(1, "mm")
            );
    }
    return(result);
}
var_integration_segment_chart <- function (dfv = NULL, ylabel = NA, step = 250, facetting = FALSE)
{
    if (is.null(dfv)) return(NULL);
    if (nrow(dfv) == 0) return(NULL);

    variable <- dfv %>% select(Type) %>% .$Type %>% unique;
    if (is.na(ylabel)) ylabel = variable;

    dfv %>%
        group_by(Type, Node, ResourceId, ResourceType) %>%
        do(remyTimeIntegrationPrep(., myStep = step)) %>%
        mutate(Start = Slice, End = lead(Slice), Duration = End-Start) %>%
        ungroup() %>%
        filter(!is.na(End)) %>%
        group_by(Type, Node, ResourceType, Start, End, Duration) %>%
        summarize(Value = sum(Value), N=n()) %>%
        rename(ResourceId = Node) %>%
        var_chart(., ylabel=ylabel) -> result;
    if (facetting){
        result <- result +
            facet_wrap(~ResourceType, ncol=1, scales="free_y") + #, strip.position="right") + # cowplot can't align this
            theme(
                strip.background=element_rect(fill="white"),
                strip.placement="inside",
                panel.spacing=unit(1, "mm")
            );
    }
    return(result);
}
suppressMessages(library(readr));
suppressMessages(library(feather));
suppressMessages(library(logging));
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
library(tidyr);
hl_per_node_ABE <- function (dfw = NULL)
{
    if(is.null(dfw)) stop("Input data frame is NULL");

    print("hl_per_node_ABE starts");

    dftemp <- dfw %>%
        filter(Application == TRUE) %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Position, Height);
    pernodeABE <- dftemp %>%
        group_by(Node) %>%
        do(abe_cpu_cuda(.));
    # Y position
    pernodeABE <- dftemp %>%
        group_by(Node) %>%
        summarize(MinPosition = min(Position), MaxPosition = max(Position)+min(Height)/2) %>%
        left_join(pernodeABE, by="Node");

    print("hl_per_node_ABE ends");

    return(pernodeABE);
}

hl_per_node_ABE_details <- function (data = NULL)
{
    if(is.null(data)) stop("Input data is NULL");
    if(is.null(data$State)) stop("Input data state is NULL");

    data$State %>%
        filter(Application == TRUE) %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Color, Position, Height) %>%
        group_by(Node) %>%
        do(abe_cpu_cuda_details(.)) %>%
        ungroup();
}
hl_global_cpb <- function (data = NULL)
{
    if(is.null(data)) return(NULL);

    dfdag <- data$DAG;

    # Create unique _integer_ identifiers
    identifiers <- c((dfdag %>% .$JobId), (dfdag %>% .$Dependent)) %>%
        unique %>%
        tibble(JobId = .) %>%
        mutate(JobIdInt = 1:n());

    # Create the structure necessary for calling the Rcpp CPB function
    dfdag %>%
        # Select only the necessary information
        select(JobId, Dependent, Start, Cost, Value) %>%
        # Change to the appropriate data type to enable left_join
        mutate(JobId = as.character(JobId)) %>%
        # Merge with identifiers so the JobId gets an unique id
        left_join(identifiers, by=c("JobId" = "JobId")) %>%
        # Rename the new column
        rename(JobIdIntU = JobIdInt) %>%
        # Merge with identifiers _again_ so the Dependent gets an unique id
        left_join(identifiers, by=c("Dependent" = "JobId")) %>%
        # Rename the new column _again_
        rename(DepIntU = JobIdInt) %>%
        # Re-ordering
        select(JobId, JobIdIntU, Dependent, DepIntU, Start, Cost, Value) %>%
        # Rename things
        rename(JobIdStr = JobId, DepStr = Dependent,
               JobId = JobIdIntU, Dependent = DepIntU) %>%
        as_tibble() -> appdagcost;

    # Define the origin (the first task in the trace)
    stask <- appdagcost %>% arrange(Start) %>% slice(1) %>% as.data.frame() %>% .$JobId;
    tasksOnCriticalPath <- sort(boost_shortest_path (stask, appdagcost));
    tasksOnCriticalPath <- identifiers %>% filter(JobIdInt %in% tasksOnCriticalPath) %>% .$JobId %>% unique

    # Gather the CPB, sum the durations, return
    data$State %>%
        filter(JobId %in% tasksOnCriticalPath) %>% pull(Duration) -> sel1;
    sel1 %>% sum -> sum1;
    if(!is.null(data$Link)){
        data$Link %>%
            filter(Key %in% tasksOnCriticalPath) %>% pull(Duration) -> sel2;
        sel2 %>% sum -> sum2;
        ret <- list(
            "CPBMPI" = sum1+sum2,
            "NMPI" = sel2 %>% length);
    }else{
        ret <- list();
    }
    ret <- c(ret, list("CPB" = sum1,
                       "tasks" = tasksOnCriticalPath));
    return(ret);
}
library(zoo);
library(tidyr);
library(pipeR);
microscopic_time_expanding <- function (df = NULL, variable = NULL)
{
    if (is.null(df)) return(NULL);
    if (is.null(variable)) return(NULL);

    df %>>%
    # Select only one variable
    filter(Type == variable) %>>%
    # Define all the unique breaks (as a side effect)
    (~ startUnique <- (.$Start %>% unique)) %>>%
    # Negative values might occur, if so set them to 0
    mutate (Value = ifelse(Value <= 0, 0, Value)) %>>%
    # Order by ResourceId and Start (it is already ordered, but let's make sure)
    arrange(ResourceId, Type, Start) %>%
    # Group by ResourceId and Type
    group_by(ResourceId, Type) %>>%
    # Complete with the missing rows using the unique Start timestamps as guideline
    complete(ResourceId, Start=startUnique) %>%   # This is the first most important line
    # Repeat the values for the created that have been created
    mutate(Value = na.locf(Value, na.rm = FALSE)) %>% # This is the second most important line
    # 
    ungroup() %>%
    # Repeat other columns (less important)
    mutate(
        P = na.locf(P, na.rm = FALSE),
        Pipeline = na.locf(Pipeline, na.rm = FALSE),
        Type = na.locf(Type, na.rm = FALSE),
        Nature = na.locf(Nature, na.rm = FALSE),
        End = lead(Start),
        Duration = End-Start) %>%
    # Omit missing values
    filter(complete.cases(.)) %>%
    #
    # Starting per-node aggregation
    #
    group_by(Node, Type, Start, End, Duration) %>%
    summarize(Value=sum(Value)) %>%
    ungroup();
}
library(Rcpp);
cppFunction("NumericVector integrateStepFunc(NumericVector x, NumericVector bounds, NumericVector values) {
  int nx=x.size()-1;
  int ix=0,ib=0;
  double xcur;
  NumericVector integ(nx);
  //enlève les intervalles à gauche
  for(;bounds[ib+1]<x[ix];ib++) {};ib++;
  for(;ix<nx;ix++) {
    xcur=x[ix];
    double intd=0;
    for(;bounds[ib]<=x[ix+1];ib++) {
      intd += values[ib-1]*(bounds[ib]-xcur);
      xcur=bounds[ib];
    }
    intd += values[ib-1]*(x[ix+1]-xcur);
    xcur=x[ix+1];
    integ[ix]=intd;
  }
  return integ;
}")
integrate.stepfunc.Rcpp <- function(inter,b,f) integrateStepFunc(inter,c(-Inf,b,Inf),c(0,f,0))

getBreaks <- function(dfv = NULL)
{
    if(is.null(dfv)) return(NULL);
    breaks = c(dfv$Start, dfv$End[length(dfv$End)]);
    return(breaks);
}

getSlices <- function(dfv = NULL, step = 100)
{
    tstart = dfv %>% .$Start %>% min;
    tend = (dfv %>% .$End %>% max);
    slices = seq(0, tend, step);
    return(slices);
}

remyTimeIntegration <- function(dfv = NULL, slices = NULL)
{
    if(is.null(dfv)) return(NULL);
    if(is.null(slices)) return(NULL);

    # Define breaks
    breaks = getBreaks(dfv);

    # Define values on breaks
    values = dfv$Value;

    result <- integrate.stepfunc.Rcpp(slices, breaks, values);

    return(result);
}

remyTimeIntegrationPrep <- function(dfv = NULL, myStep = 100)
{
    if (is.null(dfv)) return(NULL);
    if (nrow(dfv) == 0) return(NULL);
    mySlices <- getSlices(dfv, step = myStep);
    tibble(Slice = mySlices, Value = c(remyTimeIntegration(dfv, slices=mySlices), 0)/myStep);
}

suppressMessages(library(cowplot));

title_plot <- function(title = NULL)
{
    ggplot() +
        xlim(0,1) +
        ylim(0,1) +
        theme(axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.line=element_blank(),
              axis.text=element_blank()) +
        annotate("text", x = .5, y = .5, label=title, size=5);
}

pjr_value <- function (property, default)
{
  ifelse(is.null(property), default, property);
}

pjr <- function (property)
{
    ifelse(!is.null(property) && property, property, FALSE);
}

starpu_mpi_grid_arrange <- function(atree, st, starpu, ijk, lackready, ready, submitted, mpi, mpiconc, mpistate, gpu, memory, gflops, title = NULL)
{
    # The list that will contain the plots
    P <- list();
    # The list that will contain the proportinal height of each plot
    H <- list();

    # Prepare title
    if (pjr(pajer$title$active)){
        if (!is.null(title)){
            tplot <- title_plot(title);
            P[[length(P)+1]] <- tplot;
            H[[length(H)+1]] <- pjr_value(pajer$title$height, 0.3);
        }
    }

    # Customized legend position
    print("Customized legend position, plot list preparation");

    if (pjr(pajer$atree$active)){
        P[[length(P)+1]] <- atree;
        H[[length(H)+1]] <- pjr_value(pajer$atree$height, 3);
    }
    if (pjr(pajer$kiteration$active)){
        P[[length(P)+1]] <- ijk;
        H[[length(H)+1]] <- pjr_value(pajer$kiteration$height, 2);
    }
    if (pjr(pajer$st$active)){
        P[[length(P)+1]] <- st;
        H[[length(H)+1]] <- pjr_value(pajer$st$height, 4);
    }
   if (pjr(pajer$submitted$active)){
        P[[length(P)+1]] <- submitted;
        H[[length(H)+1]] <- pjr_value(pajer$submitted$height, 1);
    }
    if (pjr(pajer$starpu$active)){
        P[[length(P)+1]] <- starpu;
        H[[length(H)+1]] <- pjr_value(pajer$starpu$height, 4);
    }
    if (pjr(pajer$ready$active)){
        P[[length(P)+1]] <- ready;
        H[[length(H)+1]] <- pjr_value(pajer$ready$height, 1);
    }
    if (pjr(pajer$lackready$active)){
        P[[length(P)+1]] <- lackready;
        H[[length(H)+1]] <- pjr_value(pajer$lackready$height, .05);
    }
     if (pjr(pajer$gflops$active)){
        P[[length(P)+1]] <- gflops;
        H[[length(H)+1]] <- pjr_value(pajer$gflops$height, 2);
    }
    if (pjr(pajer$usedmemory$active)){
        P[[length(P)+1]] <- memory;
        H[[length(H)+1]] <- pjr_value(pajer$usedmemory$height, 1);
    }
    if (pjr(pajer$gpubandwidth$active)){
        P[[length(P)+1]] <- gpu;
        H[[length(H)+1]] <- pjr_value(pajer$gpubandwidth$height, 1);
    }
    if (pjr(pajer$mpibandwidth$active)){
        P[[length(P)+1]] <- mpi;
        H[[length(H)+1]] <- pjr_value(pajer$mpibandwidth$height, 1);
    }
    if (pjr(pajer$mpiconcurrent$active)){
        P[[length(P)+1]] <- mpiconc;
        H[[length(H)+1]] <- pjr_value(pajer$mpiconcurrent$height, 1);
    }
    if (pjr(pajer$mpistate$active)){
        P[[length(P)+1]] <- mpistate;
        H[[length(H)+1]] <- pjr_value(pajer$mpistate$height, 1);
    }

    # Add empty X and horizontal legend to all plots
    print("Add empty X and horizontal legend");
    # Empty the X axis of all + add horizontal direction for legends
    emptyx <- theme (axis.text.x = element_blank(), axis.title.x = element_blank());
    leghor <- theme (legend.direction = "horizontal", legend.background = element_rect(fill = "white"));
    P <- lapply(P, function(p) { p <- p + emptyx; });

    # Vanilla configuration
    if (pjr_value(pajer$vanilla$horizontal, FALSE) == FALSE){
        # Add time scale on last plot
        print("Add time scale on last plot");
        notemptyx <- theme (axis.text.x = element_text(), axis.title.x = element_text());
        P[[length(P)]] <- P[[length(P)]] + notemptyx;
    }

    if (pjr_value(pajer$vanilla$vertical, FALSE) == TRUE){
        # Remove Y scale title and text
        emptyy <- theme (axis.text.y = element_blank(), axis.title.y = element_blank());
        P <- lapply(P, function(p) { p <- p + emptyy; });
    }

    # Preparation for cowplot's plot_grid
    print("Call cowplot's plot_grid function");
    g <- plot_grid(plotlist = P, align="v", ncol = 1, rel_heights = unlist(H));
    return(g);
}
suppressMessages(library(logging));
the_reader_function <- function (directory = ".", app_states_fun = NULL, strict_state_filter = FALSE, whichApplication = NULL)
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
                           strict_state_filter=strict_state_filter,
                           whichApplication = whichApplication) %>%
        hl_y_coordinates(where = directory);

    # QRMumps case:
    # If the ATree is available and loaded, we create new columns for each task
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

    loginfo("Assembling the named list with the data from this case.");

    data <- list(Origin=directory, State=dfw, Variable=dfv, Link=dfl, DAG=dfdag, Y=dfhie, ATree=dfa);

    # Calculate the GAPS from the DAG
    if (whichApplication == "cholesky"){
        data$Gaps <- NULL;#gaps(data);
    }else{
        data$Gaps <- NULL;
    }

    return(data);
}
the_master_function <- function(data = NULL)
{
    if(is.null(data)) return(NULL);
    if(is.null(pajer)) return(NULL);

    # Get data
    directory <- data$Origin;
    dfw <- data$State;
    dfv <- data$Variable;

    loginfo("Starting the master function");

    if (!is.null(pajer$time)){
        stop("pajer: you are using a deprecated parameter.");
    }

    # Adjust temporal scale
    tstart <- pjr_value(pajer$limits$start, dfw %>% pull(Start) %>% min);
    tend <- pjr_value(pajer$limits$end, dfw %>% pull(End) %>% max)
    tScale <- list(
        coord_cartesian(xlim=c(tstart, tend))
    );
    # Define the global aggregation step as 0.1% of the total window
    globalAggStep = (tend - tstart) * .001;

    # Set all possible plots to NULL
    goatreet <- geom_blank();
    gow <- geom_blank();
    gstarpu <- geom_blank();
    goijk <- geom_blank();
    golrv <- geom_blank();
    gorv <- geom_blank();
    gosv <- geom_blank();
    gogfv <- geom_blank();
    goguv <- geom_blank();
    gomov <- geom_blank();
    gogov <- geom_blank();

    if (!is.null(data$ATree) && pjr(pajer$atree$active)){
        loginfo("Creating the temporal atree plot");
        goatreet <- atree_temporal_chart(data) + tScale;
        loginfo("Temporal atree plot completed");
    }

    # SpaceTime
    if (pjr(pajer$st$active)){
        loginfo("Creating the Space/Time");
        if (pjr(pajer$st$aggregation$active)){
            if (pjr_value(pajer$st$aggregation$method, "lucas") == "lucas"){
                loginfo("Will call st_time_aggregation");
                aggStep <- pjr_value(pajer$st$aggregation$step, globalAggStep);
                dfw_agg <- st_time_aggregation(dfw, step=aggStep);
                data %>% st_time_aggregation_plot (dfw_agg) + tScale -> gow;
                loginfo("st_time_aggregation completed");
            }else{
                loginfo("Call vinicius aggregation");
                data %>% st_time_aggregation_vinicius_plot() + tScale -> gow;
                loginfo("Finish vinicius aggregation");
            }
        }else{
            loginfo("Will call state_chart");
            data %>% state_chart (globalEndTime = tend, ST.Outliers = pjr(pajer$st$outliers), StarPU.View = FALSE) + tScale -> gow;
            loginfo("state_chart completed (no aggregation)");
        }

        # Without legend
        if (!pjr(pajer$st$legend)){
            gow <- gow + theme(legend.position="none");
        }
    }

    # StarPU SpaceTime
    if (pjr(pajer$starpu$active)){
        loginfo("Creating the StarPU Space/Time");
        data %>% state_chart (globalEndTime = tend, StarPU.View = TRUE) + tScale -> gstarpu;
        loginfo("state_chart for StarPU behavior completed (no aggregation)");

        if (!pjr(pajer$starpu$legend)){
            gstarpu <- gstarpu + theme(legend.position="none");
        }
    }

    # KIteration
    if (pjr(pajer$kiteration$active)){
        loginfo("Creating the KIteration");
        goijk <- k_chart(dfw) + tScale;

        if (!pjr(pajer$kiteration$legend)){
            goijk <- goijk + theme(legend.position="none");
        }
    }

    # Lack ready (companion for Ready Variable)
    if (pjr(pajer$lackready$active)){
        loginfo("Creating the Lack Ready Plot");
        golrv <- plot_lackready(data) + tScale;
    }

    # Ready
    if (pjr(pajer$ready$active)){
        loginfo("Creating the Ready plot");
        aggStep <- pjr_value(pajer$ready$step, globalAggStep);
        gorv <- dfv %>%
            filter(grepl("sched", ResourceId), grepl("Ready", Type)) %>%
            var_integration_segment_chart(step = aggStep) + tScale;
        if (!pjr(pajer$ready$legend)){
            gorv <- gorv + theme(legend.position="none");
        }else{
            gorv <- gorv +
                theme(legend.position=c(.99,.8),
                      legend.justification="right") +
                guides(color = guide_legend(nrow = 1))
        }
        gorv <- userYLimit(gorv, pajer$ready$limit, c(tstart, tend));
    }

    # Submitted
    if (pjr(pajer$submitted$active)){
        loginfo("Creating the Submitted plot");
        aggStep <- pjr_value(pajer$submitted$step, globalAggStep);
        gosv <- dfv %>%
            filter(grepl("sched", ResourceId), grepl("Submitted", Type)) %>%
            var_integration_segment_chart(step = aggStep) + tScale;
        if (!pjr(pajer$submitted$legend)){
            gosv <- gosv + theme(legend.position="none");
        }else{
            gosv <- gosv +
                theme(legend.position=c(.99,.8),
                      legend.justification="right") +
                guides(color = guide_legend(nrow = 1))
        }
        gosv <- userYLimit(gosv, pajer$submitted$limit, c(tstart, tend));
    }

    # GFlops
    if (pjr(pajer$gflops$active)){
        loginfo("Creating the GFlops plot");
        aggStep <- pjr_value(pajer$gflops$step, globalAggStep);
        facetted <- pjr_value(pajer$gflops$facet, TRUE);
        gogfv <- dfv %>%
            filter(Type == "GFlops") %>%
            var_integration_segment_chart (., ylabel="GFlops", step = aggStep, facetting = facetted) + tScale;

        # adjust GFlops scale
        if (pjr_value(pajer$gflops$limit, FALSE)){
            limit <- pjr_value(pajer$gflops$limit, 0); # TODO define a better default value
            gogfv <- gogfv + coord_cartesian(xlim=c(tstart, tend), ylim=c(0, limit));
        }
        if (!pjr(pajer$gflops$legend)){
            gogfv <- gogfv + theme(legend.position="none");
        }
        # TODO: user limit
    }

    # Used Memory
    if (pjr(pajer$usedmemory$active)){
        loginfo("Creating the Used Memory plot");
        goguv <- dfv %>%
            filter(grepl("MEMMANAGER", ResourceId), grepl("Used", Type)) %>%
            var_simple_chart(ylabel="Used Mem.\n(MB/s)") + tScale;
        if (!pjr(pajer$usedmemory$legend)){
            goguv <- goguv + theme(legend.position="none");
        }
        # TODO: user limit
    }

    # MPIBandwidth
    if (pjr(pajer$mpibandwidth$active)){
        loginfo("Creating the MPIBandwidth plot");
        aggStep <- pjr_value(pajer$mpibandwidth$step, globalAggStep);
        gomov <- dfv %>% filter(grepl("mpict", ResourceId), grepl("Out", Type)) %>%
            var_integration_segment_chart(., ylabel="MPI\n(MB/s)", step=aggStep) + tScale;
        if (!pjr(pajer$mpibandwidth$legend)){
            gomov <- gomov + theme(legend.position="none");
        }
        gomov <- userYLimit(gomov, pajer$mpibandwidth$limit, c(tstart, tend));
    }

    # MPI Concurrent
    if (pjr(pajer$mpiconcurrent$active)){
        loginfo("Creating the MPI concurrent ops plot");
        aggStep <- pjr_value(pajer$mpiconcurrent$step, globalAggStep);
        gompiconc <- data %>% concurrent_mpi() %>%
            var_integration_segment_chart(., ylabel="Concurrent\nMPI Tasks", step=aggStep) + tScale;
        if (!pjr(pajer$mpiconcurrent$legend)){
            gompiconc <- gompiconc + theme(legend.position="none");
        }
        gompiconc <- userYLimit(gompiconc, pajer$mpiconcurrent$limit, c(tstart, tend));
    }

    # MPI State
    if (pjr(pajer$mpistate$active)){
        loginfo("Creating the MPI state");
        gompistate <- data %>% state_mpi_chart() + tScale;
        if (!pjr(pajer$mpistate$legend)){
            gompistate <- gompistate + theme(legend.position="none");
        }
    }

    # GPUBandwidth
    if (pjr(pajer$gpubandwidth$active)){
        loginfo("Creating the GPU Bandwidth plot");
        aggStep <- pjr_value(pajer$gpubandwidth$step, globalAggStep);
        if(aggStep < 0){
            gogov <- dfv %>%
                # Get only GPU memory banwidth (out)
                filter(grepl("MEMMANAGER", ResourceId), grepl("Out", Type)) %>%
                # Remove the MANAGER0, which is CPU-only
                # TODO: After the logical OR there is a support for single-node StarPU traces
                filter(!grepl("MANAGER0", Resource)) %>%
                group_by(Type, Node, ResourceType, Start, End, Duration) %>%
                summarize(Value = sum(Value), N=n()) %>%
                rename(ResourceId = Node) %>%
                var_chart(ylabel = "GPU\n(MB/s)") + tScale;
        }else{
            gogov <- dfv %>%
                # Get only GPU memory banwidth (out)
                filter(grepl("MEMMANAGER", ResourceId), grepl("Out", Type)) %>%
                # Remove the MANAGER0, which is CPU-only
                # TODO: After the logical OR there is a support for single-node StarPU traces
                filter(Resource != "MEMMANAGER0" | Node != "MEMMANAGER0") %>%
                var_integration_segment_chart(., ylabel="GPU\n(MB/s)", step=aggStep) + tScale;
        }
        if (!pjr(pajer$gpubandwidth$legend)){
            gogov <- gogov + theme(legend.position="none");
        }
        # Fixed upper bound
        if (pjr(pajer$gpubandwidth$bound)) {
            lbound <- pjr_value(pajer$gpubandwidth$bound, 0); # TODO: define a better default value
            gogov <- gogov + coord_cartesian(ylim=c(0, lbound),
                                             xlim=c(tstart, tend));
        }
    }
  
    loginfo("Assembling the plot");

    # assembling
    g <- starpu_mpi_grid_arrange(atree = goatreet,
                                 st = gow,
                                 starpu = gstarpu,
                                 ijk = goijk,
                                 lackready = golrv,
                                 ready = gorv,
                                 submitted = gosv,
                                 mpi = gomov,
                                 mpiconc = gompiconc,
                                 mpistate = gompistate,
                                 gpu = gogov,
                                 memory = goguv,
                                 gflops = gogfv,
                                 title = directory);

    loginfo("Assembling concluded.");
    return(g);
}

userYLimit <- function(obj, configuration, xlimits)
{
    if (pjr(configuration)){
        # if there is an user vertical scale defined, use it
        tpScale <- list(
            coord_cartesian(xlim=xlimits,
                            ylim=c(0, pjr(configuration)))
        );
        obj <- obj + tpScale;
    }
    return(obj);
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
    bind_rows (tibble(Kernel = c("init_block", "clean_block"),
                      Color = c("#FFFF33", "#984EA3")));
}
geom_atree <- function (data=NULL, Offset=1.02, Flip = TRUE)
{
    if(is.null(data)) stop("input data for geom_atree is NULL");

    makespan = data$State %>% filter(Application == TRUE) %>% .$End %>% max;

    ffactor <- ifelse(Flip, +1, -1);
    dfactor <- makespan * 0.04;
    doffset <- makespan * Offset;

    data <- data$ATree %>%
        # Get graphical properties of each parent for each row
        left_join(data$ATree, by=c("Parent" = "ANode"), suffix=c(".Node", ".Parent")) %>%
        rename(Height = Height.Node, Position = Position.Node, Depth = Depth.Node, Intermediary = Intermediary.Node) %>%
        select(-Parent.Parent) %>%
        # Keep only intermediary nodes
        filter(Intermediary == TRUE) %>%
        # Calculate coordinates for Labels
        mutate(Label.X = doffset - (Depth * dfactor) * ffactor,
               Label.Y = Position + Height/2) %>%
        # Calculate coordinates for lines connecting child with parent
        mutate(Edge.X = doffset - ((Depth-0.3) * dfactor) * ffactor,
               Edge.Y = Position + Height/2,
               Edge.Xend = doffset - ((Depth.Parent+0.3) * dfactor) * ffactor,
               Edge.Yend = Position.Parent + Height.Parent/2);

    ret <-
        list(
            # Horizontal lines
#            geom_segment(data=(data %>% mutate(XOrigin = max(Depth)+0.6)), aes(yend=Position, x=Depth+0.5, xend=XOrigin, y=Position), color="lightgray"),
            # Lines connecting child with parent
            geom_segment(data=data, aes(x=Edge.X, yend=Edge.Yend, xend=Edge.Xend, y=Edge.Y), color="gray"),
            # The Label
            geom_text(data=data, size=3, aes(y=Label.Y, label=ANode, x=Label.X)),
            # Fix time coordinates
            coord_cartesian(xlim=c(0, makespan))
        );
    return(ret);
}
library(tidyverse);
library(ggplot2);
atree_temporal_chart <- function(data = NULL, globalEndTime = NULL)
{
    if (is.null(data)) stop("a NULL data has been provided to atree_temporal_chart");

    loginfo("Entry of atree_temporal_chart");

    dfw <- data$State;
    dfa <- data$ATree;
    # Prepare for colors
    namedcolors <- extract_colors(dfw);
    atreeplot <- dfw %>%
        filter(Application == TRUE) %>%
        # Remove all tasks that do not have ANode
        filter(!is.na(Height.ANode)) %>%
        # Plot
        ggplot() +
        default_theme() +
        ylab("Task Location") +
        scale_y_continuous(breaks=NULL, labels=NULL) +
        scale_fill_manual(values = namedcolors) +
        geom_rect(aes(fill=as.factor(Value),
                      xmin=Start,
                      xmax=End,
                      ymin=Position.ANode,
                      ymax=Position.ANode+Height.ANode), alpha=.5) +
        # Add the atree representation on top
        geom_atree(data, Offset = 1.05, Flip = TRUE);

    loginfo("Exit of atree_temporal_chart");

    return(atreeplot);
}
default_theme <- function()
{
    ret <- list();

    bsize = pjr_value(pajer$base_size, 22);

    ret[[length(ret)+1]] <- theme_bw(base_size=bsize);
    ret[[length(ret)+1]] <- theme (
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.spacing = unit(1, "mm"),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.box.spacing = unit(0, "pt"),
        legend.box.margin = margin(0,0,0,0),
        legend.title = element_blank());
    ret[[length(ret)+1]] <- xlab("Time [ms]");
    ret[[length(ret)+1]] <- scale_x_continuous(expand=c(pjr_value(pajer$expand, 0.05),0));
    return(ret);
}

vanilla_theme <- function()
{
    ret <- list();

    bsize = pjr_value(pajer$base_size, 22);

    ret[[length(ret)+1]] <- theme_bw(base_size=bsize);
    ret[[length(ret)+1]] <- theme (
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.spacing = unit(1, "mm"),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.justification = "left",
        legend.box.spacing = unit(0, "pt"),
        legend.box.margin = margin(0,0,0,0),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()
        );

    return(ret);
}

default_theme_non_temporal <- function ()
{
    ret <- list();

    bsize = pjr_value(pajer$base_size, 18);

    ret[[length(ret)+1]] <- theme_bw(base_size=bsize);
    ret[[length(ret)+1]] <- theme (
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.spacing = unit(1, "mm"),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.box.spacing = unit(0, "pt"),
        legend.box.margin = margin(0,0,0,0),
        legend.title = element_blank());
    return(ret);
}
geom_states <- function (data = NULL, Show.Outliers = FALSE, StarPU = FALSE)
{
    if (is.null(data)) stop("data is NULL when given to geom_states");
    if (StarPU){
        dfw <- data$State %>%
            # Non application
            filter(Application == FALSE) %>%
            # And only Starpu Worker State
            filter(Type == "Worker State");
    }else{
        dfw <- data$State %>% filter(Application == TRUE);
    }

    loginfo("Starting geom_states");

    ret <- list();

    # Color mapping
    ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab(ifelse(StarPU, "StarPU Workers", "Application Workers"));

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+Height-0.4), alpha=ifelse(Show.Outliers && !StarPU, 0.5, 1.0));

    # Add outliers conditionally
    if (Show.Outliers && !StarPU){
        ret[[length(ret)+1]] <-
            geom_rect(data=(dfw %>% filter(Outlier == TRUE)),
                      aes(fill=Value,
                          xmin=Start,
                          xmax=End,
                          ymin=Position,
                          ymax=Position+Height-0.4), alpha=1);
    }

    loginfo("Finishing geom_states");

    return(ret);
}

geom_mpistates <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_mpistates");
    dfw <- data$State;
    if (is.null(dfw)) stop("dfw is NULL when given to geom_mpistates");

    # Get only MPI states
    dfw <- dfw %>% filter(Type == "Communication Thread State");
    if (nrow(dfw) == 0) stop("there is no data after filtering for MPI states");

    # Check if there is MPI data
    loginfo("Starting geom_mpistates");

    ret <- list();

    # Calculate Y position
    ypos <- tibble(ResourceId = (dfw %>% pull(ResourceId) %>% unique)) %>%
        mutate(Height = 1) %>%
        mutate(Position = cumsum(Height));

    dfw <- dfw %>%
        # Remove existing position
        select(-Position, -Height) %>%
        # Establish new position
        left_join(ypos);

    # Color mapping
    ret[[length(ret)+1]] <- scale_fill_brewer(palette = "Dark2");

    # Y label
    ret[[length(ret)+1]] <- ylab("MPI\nThread");

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+0.6));

    loginfo("Finishing geom_mpistates");

    return(ret);
}

calculate_resource_idleness <- function(dfw = NULL)
{
    if(is.null(dfw)) stop("Input data frame is NULL");

    # Get only application states
    dfw <- dfw %>% filter(Application == TRUE);

    # Obtain time interval
    tstart <- dfw %>% .$Start %>% min;
    tend <- dfw %>% .$End %>% max;

    #Calculate resources idleness
    total_time <- tend - tstart;
    dfw %>%
        group_by (ResourceType, ResourceId, Node, Position, Height) %>%
        summarize(Idleness = round((1-(sum(End-Start)/total_time))*100,2),
                  End = max(End)) %>%
        group_by(Node, ResourceType) %>%
        filter(Idleness %in% c(max(Idleness))) %>% #, min(Idleness))) %>%
        ungroup();
}

geom_idleness <- function(data = NULL)
{
    if(is.null(data)) stop("data provided for geom_idleness is NULL");

    dfidle <- calculate_resource_idleness(data$State);

    bsize = pjr_value(pajer$base_size, 22);
    expand = pjr_value(pajer$expand, 0.05);
    idleness_factor = pjr_value(pajer$idleness_factor, 5.5);

    globalEndTime <- dfidle %>% pull(End) %>% na.omit %>% max;
    ret <- NULL;
    ret <- geom_text(data=dfidle,
                     # The size of the idle values for each resource
                     size=bsize/idleness_factor,
                     # The X position of each one
                     x=(-1) * globalEndTime/100*2.5, # 2.5% before 0.0
                     # The Y position (depends on the Resource, so use "aes"
                     aes(y=Position+(Height/2.5), # vertical
                         # The idleness number followed by % as text
                         label=gsub("$", "%", Idleness)));
    return(ret);
}
geom_makespan <- function(data = NULL)
{
    if(is.null(data)) stop("data provided for geom_makespan is NULL");
    dfw <- data$State;

    bsize = pjr_value(pajer$base_size, 22);

    tend = dfw %>% filter(Application == TRUE) %>% pull(End) %>% max;
    print(paste("makespan is", tend));
    height = dfw %>% select(Position) %>% na.omit %>% pull(Position) %>% max;
    print(paste("max height for makespan is", height));
    ret <- geom_text(data=data.frame(), x=tend, y=height*.5, aes(label=round(tend,0)), angle=90, size=bsize/4);
    return(ret);
}
geom_cpb <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_cpb");
    if (is.null(data$DAG)) return(list());

    # Calculate the global CPB
    cpbs <- hl_global_cpb(data);

    ret <- list();
    if (pajer$st$cpb){
        ret <- c(ret, geom_cpb_internal(data, cpbs$CPB, "CPB:"));
    }
    if (pjr_value(pajer$st$cpb_mpi$active)){
        tile_size = pajer$st$cpb_mpi$tile_size;
        bandwidth = pajer$st$cpb_mpi$bandwidth;
        cpbmpit = cpbs$CPB + cpbs$NMPI * (tile_size*tile_size*8) / bandwidth / 1000000;
        print(paste(data$Origin, cpbs$NMPI, cpbmpit));
        ret <- c(ret, geom_cpb_internal(data, cpbs$CPBMPI, "CPB-MPI:"));
        if (pjr_value(pajer$st$cpb_mpi$theoretical)){
            ret <- c(ret, geom_cpb_internal(data, cpbmpit, "CPB-MPI*:"));
        }
    }
    return(ret);
}

geom_cpb_internal <- function(data = NULL, value = NULL, desc = NULL)
{

    if (!is.null(value) && !is.null(desc)){
        dfw <- data$State;

        bsize = pjr_value(pajer$base_size, 22);

        minPos = dfw %>% select(Position) %>% na.omit %>% pull(Position) %>% min;
        maxPos = dfw %>% select(Position) %>% na.omit %>% pull(Position) %>% max;
        corr = dfw %>% select(Height) %>% na.omit %>% pull(Height) %>% min / 2;
        ret <- list(
            # the gray band
            geom_segment(data=data.frame(x=value,
                                         xend=value,
                                         y=minPos,
                                         yend=maxPos-corr),
                         aes(x=x,
                             xend=xend,
                             y=y,
                             yend=yend),
                         size=5,
                         alpha=.7,
                         color="gray"),
            # the text on top of the gray brand
            geom_text(data=data.frame(x=value,
                                      y=minPos+(maxPos-minPos)/2),
                      aes(x=x, y=y),
                      label=paste0(desc, " ", round(value, 0)),
                      angle=90,
                      color="black",
                      size=bsize/4)
        );
        return(ret);
    }
    return(list());
}
geom_abe <- function(data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_abe");

    # states and k
    pernodeABEdf <- hl_per_node_ABE(data$State);
    print(pernodeABEdf);

    bsize = pjr_value(pajer$base_size, 22)/5;

    if (!is.null(pernodeABEdf)){
        ret <- list(
            geom_segment(data=pernodeABEdf, aes(x = Result+tstart, xend=Result+tstart, y = MinPosition, yend=MaxPosition), size=5, alpha=.7, color="gray"),
            geom_text (data=pernodeABEdf, aes(x = Result+tstart, y = MinPosition+(MaxPosition-MinPosition)/2, label=paste0(ifelse(pjr_value(pajer$st$abe$label, TRUE), "ABE: ", ""), round(Result, 0))), angle=90, color="black", size=bsize)
        );
        return(ret);
    }
    return(list());
}
suppressMessages(library(logging));
suppressMessages(library(feather));
the_fast_reader_function <- function (directory = ".")
{
    names <- c("State", "Variable", "Link", "DAG", "Y", "ATree", "Gaps");
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
gaps.f_backward <- function (data)
{
    # Create the seed chain
    data$DAG %>%
        filter(grepl("mpicom", JobId)) %>%
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
    data$DAG %>%
        filter(grepl("mpicom", Dependent)) %>%
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

gaps <- function (data)
{
    loginfo("Starting the gaps calculation.");

    if(is.null(data$DAG)) return(NULL);
    if(is.null(data$State)) return(NULL);
    if(is.null(data$Link)) return(NULL);

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
    dfl <- data$Link %>%
        filter(grepl("mpicom", Key)) %>%
        mutate(Value = NA, ResourceId = Origin, Node = NA) %>%
        rename(JobId = Key) %>%
        select(JobId, Value, ResourceId, Node, Start, End);
    data.z %>%
        left_join(dfw, by=c("JobId" = "JobId")) %>%
        left_join(dfw, by=c("Dependent" = "JobId")) -> data.z.dag;
    data.b %>%
        left_join(dfl, by=c("JobId" = "JobId")) %>%
        left_join(dfw, by=c("Dependent" = "JobId")) -> data.b.dag;
    data.f %>%
        left_join(dfw, by=c("JobId" = "JobId")) %>%
        left_join(dfl, by=c("Dependent" = "JobId")) -> data.f.dag;

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
                        progress=TRUE,
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
                            Key = col_character()
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
                          progress=TRUE,
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
plot_lackready <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_lackready");
    if (is.null(data$State)) stop("state is NULL when given to geom_lackready");
    if (is.null(data$Variable)) stop("variable is NULL when given to geom_lackready");

    data$State %>%
        select(Node, Resource) %>%
        unique %>%
        group_by(Node) %>%
        summarize(N=n()) %>%
        .$N %>%
        min -> minResources;

    aggStep <- pjr_value(pajer$lackready$aggregation, 200);
    loginfo(paste("lack ready aggregation is", aggStep));
    threshold <- pjr_value(pajer$lackready$threshold, minResources);
    loginfo(paste("lack ready threshold is", threshold));

    data$Variable %>%
        filter(Type == "Ready") %>%
        group_by(Type, Node, ResourceId, ResourceType) %>%
        do(remyTimeIntegrationPrep(., myStep = aggStep)) %>%
        mutate(Start = Slice, End = lead(Slice), Duration = End-Start) %>%
        ungroup() %>%
        filter(!is.na(End)) %>%
        group_by(Type, Node, ResourceType, Start, End, Duration) %>%
        summarize(Value = sum(Value), N=n()) %>%
        ungroup() %>%
        rename(ResourceId = Node) %>%
        filter(Value < threshold) %>%
        group_by(Type, Start, End) %>%
        summarize(Value=n()) %>%
        ungroup() %>%
        ggplot() + geom_lackready();
}

geom_lackready <- function (data = NULL)
{

    if (!pjr(pajer$lackready$active)){
        return(NULL);
    }

    loginfo("Starting geom_lackready");

    ret <- list();

    ret[[length(ret)+1]] <- default_theme();
    ret[[length(ret)+1]] <- scale_fill_gradient(low="lightsalmon", high="red1");
    ret[[length(ret)+1]] <- geom_rect(aes(fill=Value,
                                          xmin=Start,
                                          xmax=End,
                                          ymin=0,
                                          ymax=1), alpha=1);
    ret[[length(ret)+1]] <- theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank());

    loginfo("Finishing geom_lackready");
    return(ret);
}
library(tidyverse);
library(ggplot2);
state_mpi_chart <- function (data = NULL)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    loginfo("Entry of state_mpi_chart");

    # Obtain time interval
    tstart <- data$State %>% pull(Start) %>% min;
    tend <- data$State %>% pull(End) %>% max;

    #Plot
    gow <- ggplot() +
        default_theme() +
        coord_cartesian(xlim=c(tstart, tend)) +
        # Add states and outliers if requested
        geom_mpistates(data);

    print("Exit of state_mpi_chart");
    return(gow);
}
concurrent_mpi <- function(data = NULL)
{
    if (is.null(data)) return(NULL);

    data$Link %>%
        filter(grepl("mpicom", Key)) %>%
        select(-Nature, -Container, -Type, -Duration) -> dflink;

    dflink %>%
        select(-End) %>%
        rename(Timestamp = Start) %>%
        mutate(Start = TRUE) -> dfstart;
    dflink %>%
        select(-Start) %>%
        rename(Timestamp = End) %>%
        mutate(Start = FALSE) %>%
        bind_rows (dfstart) %>%
        arrange(Timestamp) %>%
        group_by(Origin, Dest) %>%
        mutate(Value = cumsum(as.integer(
                   case_when(
                       Start == TRUE ~ 1,
                       Start == FALSE ~ -1,
                       TRUE ~ 0)))) %>%
        arrange(Origin, Timestamp) %>%
        select(-Start) %>%
        rename(Start = Timestamp) %>%
        group_by(Origin) %>% mutate(End = lead(Start)) %>% na.omit %>% mutate(Duration = End-Start) %>% ungroup() %>%
        mutate(Type = "MPI Concurrent") %>%
        rename(ResourceId = Origin) %>%
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        mutate(Node = as.factor(Node)) %>%
        mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource))) %>%
        select(Start, End, Duration, Node, ResourceId, ResourceType, Type, Value)
}

# 2nd phase task dependencies based on Gaps
gaps_backward_deps <- function (data = NULL, tasks = NULL, levels = 1)
{
    if (is.null(data)) stop("data is NULL when given to gaps_backward_deps");
    if (is.null(tasks)) stop("task is NULL when given to gaps_backward_deps");
    if (levels < 1) stop("level lt 1 when given to  gaps_backward_deps");

    tibble(Path = tasks) %>%
        unique %>%
        group_by(Path) %>%
        do(gaps_backward_deps_one (data = data, task = .$Path, levels = levels)) %>%
        ungroup()
}

gaps_backward_deps_one <- function(data = NULL, task = NULL, levels = 1)
{
    ret <- gaps_backward_deps_rec (data = data, path = task, task = task, levels = levels);

    # Enrich states
    dfw <- data$State;
    ret %>%
        filter(!grepl("mpi", JobId)) %>%
        left_join(dfw, by=c("JobId" = "JobId")) -> retw;

    # Enrich links
    dfl <- data$Link
    ret %>%
        filter(grepl("mpi", JobId)) %>%
        left_join(dfl, by=c("JobId" = "Key")) %>%
        # Post-processing
        # Keep only the destination of the link
        rename(ResourceId = Dest) %>%
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        select(-Origin, -Container) %>%
        # Enrich ResourceId with Height, Position
        left_join((data$Y %>% select(-Type, -Nature)), by=c("ResourceId" = "Parent")) %>%
        # Post-processing to ease row binding
        mutate(Size = as.character(Size)) -> retl;

    # Merge
    return(retw %>% bind_rows(retl) %>% arrange(Start));
}

gaps_backward_deps_rec <- function(data = NULL, path = NULL, task = NULL, levels = 1)
{
    if (is.null(data)) stop("data is NULL when given to gaps_backward_deps_rec");
    if (is.null(task)) stop("task is NULL when given to gaps_backward_deps_rec");
    if (is.null(path)) stop("path is NULL when given to gaps_backward_deps_rec");

    data$Gaps %>%
        # get only the job id for which we have an interest
        filter(JobId == task) %>%
        mutate(Path = path) %>%
        # the group_by is really not interessant, since we have just one JobId
        group_by(JobId) %>%
        # reverse order by the end of its dependents
        arrange(-End.y) %>%
        # get the last one to finish
        slice(1) %>%
        ungroup() %>%
        # get only what is useful
        select(Path, JobId, Dependent) -> dep;

    # check if dep has something
    if (nrow(dep) == 0) {
        return(NULL);
    }

    # recurse, if levels > 1
    if (levels > 1){
        dep %>%
            bind_rows(gaps_backward_deps_rec(data = data,
                                             path = path,
                                             task = dep$Dependent,
                                             levels=(levels-1)));
    }else{
        return(dep);
    }
}
geom_path_highlight <- function (paths = NULL)
{
    if (is.null(paths)) stop("paths is NULL when given to gaps_backward_deps");

    # paths is identical to data$State, but with an additional column called Path

    ret <- list();

    # highlight the tasks involved in the path
    ret[[length(ret)+1]] <- geom_rect(data=paths,
                                      size=1,
                                      aes(color=Path,
                                          xmin=Start,
                                          xmax=End,
                                          ymin=Position,
                                          ymax=Position+Height-0.4), alpha=0);

    # let's draw lines connecting tasks in the path
    
    # collect each JobId coordinates
    paths %>% select(JobId, Start, End, Position, Height) %>% unique -> x1;
    # gather coordinates for the lines
    paths %>%
        select(Path, JobId, Dependent) %>%
        left_join(x1, by=c("JobId" = "JobId")) %>%
        left_join(x1, by=c("Dependent" = "JobId")) %>%
        na.omit -> pathlines;

    ret[[length(ret)+1]] <- geom_segment(data=pathlines,
                                         aes(x=Start.x,
                                             xend=End.y,
                                             y=Position.x+(Height.x/2),
                                             yend=Position.y+(Height.y/2),
                                             color=Path));
    return(ret);
}

# Vinicius agg

# This function computes chunks by ResourceId, tasks in the same chunk will be aggregated. This function is used by aggregate_trace function.
# Input: Start,End,Duration,Value,JobId columns from a common data table with ResourceId, Start, End, Duration, Value, JobId columns
#        excludeIds - list of JobIds to do not aggregate 
#        min_time_pure - states longer than this value should be kept pure.
#        states - list of states to be aggregated
# Output: a list of Chunks with the same size of the input columns (Start,End,Duration,Value,JobId)
compute_chunk <- function(Start,End,Duration,Value,JobId,excludeIds="",states,min_time_pure) {
    chunk <- rep(0,length(Duration))
    if(states %in% c("all", "All")){
        v <- Duration>min_time_pure # pure states
    } else {
        v <- Duration>min_time_pure | !(Value %in% c(unlist(strsplit(states, ",")))) | (JobId %in% c(unlist(strsplit(excludeIds, ",")))) # pure states: duration > threshold OR is not present in the list of states for aggregation OR is present in excludeIds list
    }
    v2 <- c(FALSE,Start[-1]>End[0:(length(v)-1)]+min_time_pure) # "idle" states (actually white spaces once normal idle states are already present in the trace )
    chunk[v | c(FALSE,v[0:(length(v)-1)]) | v2] <-1
    cumsum(chunk)
}

# This function creates an aggregated version of the trace. Using the computed chunks, it calculates the Start/End values of each chunk. It also calculates a proportion (Activity) that represents the time spent by each different state. 
# Input: df_native - a common data table with ResourceId, Start, End, Duration, Value, JobId columns
#        states - list of states to be aggregated
#        excludeIds - list of JobIds to do not aggregate 
#        min_time_pure - states longer than this value should be kept pure.
# Output: a modified data table with new columns indicating the Chunk, the Number of tasks in this chunk (by state), Activity representing the proportion of time spent in this state, Aggregated that shows if a state is pure or not.
aggregate_trace <- function(df_native, states, excludeIds, min_time_pure) {
    df_native <- df_native %>% group_by(ResourceId) %>% mutate(Chunk = compute_chunk(Start, End, Duration, Value, JobId, excludeIds, states, min_time_pure))
    df_native2 <- df_native %>% group_by(ResourceId, Chunk) %>% mutate(Start=head(Start,1), End=tail(End, 1))
    df_aggregate <- df_native2 %>% group_by(ResourceId, Chunk, Value) %>%
        summarize(Duration = sum(Duration), Start=head(Start,1), End=head(End,1), Number=n()) # n() is used to get the number of lines (former length(ResourceId))
    df_aggregate <- df_aggregate %>% group_by(ResourceId, Chunk) %>% mutate(Activity = Duration/(End-Start))  
    df_aggregate <- df_aggregate %>% group_by(ResourceId, Chunk) %>% mutate(aggregated=ifelse((abs(Activity-1) <= 0.00001) & Number==1, FALSE, TRUE)) 
    df_aggregate
}

# This function is used to compute the relative height of different states in the chunk. After, this is used to create a stacked view of each chunk.
# Input: a data table computed using the aggreate_trace function.
# Output: a data table with PosY column, representing the height of each state in the chunk. This column is computed using the Activity proportion.
compute_aggregate_coord <- function(df){
    df$Activity <- df$Activity * 0.8  # this is used to reduce the size of the rectangle (a smaller rectangle is better for visualization as we already tested in another gantt charts)
    df %>% group_by(ResourceId, Chunk) %>% mutate(PosY=(as.numeric(ResourceId)-0.4)+(cumsum(Activity))) # -0.4 is needed to put the rectangle in the middle of the y-axi
}
geom_aggregated_states <- function (data = NULL, Show.Outliers = FALSE, min_time_pure = 1000, states = NA)
{
    if (is.null(data)) stop("data is NULL when given to geom_aggregated_states");
    if (is.na(states)) stop("states is NA when given to geom_aggregated_states");

    loginfo("Starting geom_aggregated_states");

    # Define the exclude ids based on the Show.Outliers parameter
    if (Show.Outliers){
        data$State %>%
            filter(Application == TRUE) %>%
            filter(Outlier == TRUE) %>%
            pull(JobId) -> excludeIds;
    }else{
        excludeIds <- c("");
    }

    # Prepare Y coordinates for left_join
    data$Y %>%
        rename(ResourceId = Parent) %>%
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        mutate(Node = as.factor(Node)) %>%
        mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource))) -> ydf;

    # Do the aggregation
    data$State %>%
        filter(Application == TRUE) %>%
        select(ResourceId, Start, End, Duration, Value, JobId) %>%
        aggregate_trace(states, excludeIds, min_time_pure) %>%
        left_join(
            ydf %>% select(ResourceId, Resource, Node, ResourceType, Height, Position),
            by=c("ResourceId" = "ResourceId")
        ) %>%
        mutate(Color = case_when(
                   grepl("potrf", Value) ~ "#e41a1c",
                   grepl("trsm", Value) ~ "#377eb8",
                   grepl("syrk", Value) ~ "#984ea3",
                   grepl("gemm", Value) ~ "#4daf4a",
                   TRUE ~ "unspecified")) -> dfw;

    # The list of geoms
    ret <- list();

    # Add the default theme
    ret[[length(ret)+1]] <- default_theme();

    # Y axis breaks and their labels
    yconfm <- yconf(dfw);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));

    ret[[length(ret)+1]] <- geom_rect(data=dfw, aes(fill=Value,
                                                    xmin=Start,
                                                    xmax=End,
                                                    ymin=Position,
                                                    ymax=Position+(Height-0.4)*Activity,
                                                    alpha=aggregated));
    ret[[length(ret)+1]] <- guides(alpha=FALSE);
    ret[[length(ret)+1]] <- scale_alpha_discrete(range=c(1, .6));
    ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    loginfo("Finishing geom_aggregated_states");

    return(ret);
}
library(ggplot2);
st_time_aggregation_vinicius_plot <- function (data = NULL)
{
    if (is.null(data)) return(NULL);

    loginfo("Vinicius Entry Agg");

    # Parameters
    with.outliers = pjr_value(pajer$st$outliers, TRUE);
    with.states = pjr_value(pajer$st$aggregation$states, c("dgemm"));
    # The longer outlier
    longer.outlier = data$State %>% filter(Application == TRUE, Outlier == TRUE) %>% pull(Duration) %>% max;
    loginfo(paste("Longer outlier for min_time_pure definition is", longer.outlier));
    with.min_time_pure = pjr_value(pajer$st$aggregation$step, longer.outlier);

    # Considering only application states
    dfw <- data$State %>% filter(Application == TRUE);

    # Obtain time interval
    tstart <- dfw %>% .$Start %>% min;
    tend <- dfw %>% .$End %>% max;

    loginfo("Vinicius Plotting Agg");

    #Plot
    gow <- ggplot() +
        geom_aggregated_states(data=data,
                               Show.Outliers = with.outliers,
                               states = with.states,
                               min_time_pure = with.min_time_pure) +
        xlab("Time [ms]");
 
    # Y Label
    gow <- gow + ylab("Application Workers");

    # The per-node ABE
    if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

    # add makespan
    if (pjr(pajer$st$makespan)) gow = gow + geom_makespan(data);

    # add idleness
    if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

    # add Global CPB
    if (pjr(pajer$st$cpb)) gow = gow + geom_cpb(data);

    # check if task dependencies should be added
    if (pjr(pajer$st$tasks$active)){
        tasklist <- pajer$st$tasks$list;
        levels <- pjr_value(pajer$st$tasks$levels);

        tasksel <- gaps_backward_deps (data = data,
                                       tasks = tasklist,
                                       levels = levels);

        gow = gow + geom_path_highlight(tasksel);
    }else{
        loginfo("DO NOT add dependencies");
    }

    loginfo("ViniciusExit Agg");
    return(gow);
}
