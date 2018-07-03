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

time_aggregation_prep <- function(dfw = NULL)
{
    if (is.null(dfw)) return(NULL);

    dfw_initial <- dfw %>%
        rename(Task = Value) %>%
        group_by (ResourceId, Task) %>%
        mutate(Value = 1) %>%
        select(-Duration, -Color, -Nature, -Type,
               -Size, -Depth, -Params, -JobId, -Footprint, -Tag,
               -GFlop, -X, -Y, -Iteration, -Subiteration,
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
    }else{
        dfw <- dfw %>% filter(Application == FALSE);
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
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(1) %>%
            ungroup;
    }else if(pjr_value(pajer$st$labels, "1") == "1GPU_per_NODE"){
        # One GPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(n()) %>%
            ungroup;
    }else if(pjr_value(pajer$st$labels, "1") == "ALL"){
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            ungroup;
    }else{
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(c(1, n())) %>%
            ungroup;
    }
}
outlier_definition <- function(x) {
    (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}

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
starpu_states <- function()
{
    c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task", "Waiting all tasks", "Building task", "Deinitializing");
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

        # add pmtool bound
        if (pjr(pajer$pmtool$bounds$active)) gow = gow + geom_pmtools_bounds(data);

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
        # Move to geom_states, as it was doing 2 times...
        # gow = gow + scale_fill_manual(values = starpu_colors());
    }

    loginfo("Exit of state_chart");
    return(gow);
}

events_memory_chart <- function (data = NULL, globalEndTime = NULL, combined = FALSE, tstart = NULL, tend = NULL)
{
    if (is.null(data)) stop("data provided to memory_chart is NULL");

    # Get traces
    dfw <- data$Events;

    loginfo("Entry of events_memory_chart");

    memory_states = c("Allocating Async Start", "Allocating Async End", "Allocating Start", "Allocating End", "WritingBack Start", "WritingBack End");
    memory_states_start = c("Allocating Async Start", "Allocating Start", "WritingBack Start");

    # Filter
    dfwapp = data$Events %>%
          filter(Type %in% memory_states) %>%
          group_by(Container, Handle, Tid, Src) %>% arrange(Start) %>%
          mutate(End = lead(Start)) %>%
          filter(Type %in% memory_states_start) %>%
          mutate(Duration = End-Start)

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_events(data, dfwapp, combined=combined, tstart=tstart, tend=tend);
    if(combined){
      gow <- gow + geom_links(data, combined=TRUE, tstart=tstart, tend=tend);
    }

    loginfo("Exit of events_memory_chart");
    return(gow);
}

memory_chart <- function (data = NULL, globalEndTime = NULL, combined = FALSE, tstart = NULL, tend = NULL)
{
    if (is.null(data)) stop("data provided to memory_chart is NULL");

    # Get traces
    dfw <- data$State;

    loginfo("Entry of memory_chart");

    # Filter
    dfwapp = dfw %>%
        # Considering only application data
        filter(Application == TRUE) %>%
        # Considering only Worker State
        filter(Type == "Memory Node State");

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_memory(data, combined=combined, tstart=tstart, tend=tend);
    if(combined){
      gow <- gow + geom_links(data, combined=TRUE, tstart=tstart, tend=tend);
    }

    #gow = gow + scale_fill_manual(values = starpu_colors());

    loginfo("Exit of memory_chart");
    return(gow);
}


state_pmtool_chart <- function (data = NULL)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    # Get traces
    dfw <- data$pmtool_states;

    loginfo("Entry of state_pmtool_chart");

    # Filter
    dfwapp = dfw %>%
        filter(sched == pajer$pmtool$state$sched);

    # Obtain time interval
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_pmtool_states(data);


    #if (pjr(pajer$st$abe$active)) gow = gow + geom_abe(data);

    #if (pjr(pajer$pmtool$bounds$active)) gow = gow + geom_pmtools_bounds(data);

    # add makespan
    if (pjr(pajer$st$makespan)) gow = gow + geom_makespan_pmtool(data);

    # add idleness
    #if (pjr(pajer$st$idleness)) gow = gow + geom_idleness(data);

    # add Global CPB
    #if (pjr(pajer$st$cpb) || pjr(pajer$st$cpb_mpi$active)) gow = gow + geom_cpb(data);

    loginfo("Exit of state_pmtool_chart");
    return(gow);
}


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
        gow = gow + scale_fill_manual(values = starpu_colors());
    }

    loginfo("Exit Agg");
    return(gow);
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
        geom_curve(data=dfborders, aes(x=Start, xend=StartB, y=Iteration+height-height/2, yend=IterationB+height-height/2), curvature=0.1, angle=20) +
        # The end border
        geom_curve(data=dfborders, aes(x=End, xend=EndB, y=Iteration-height/2, yend=IterationB-height/2), curvature=-0.1, angle=20) +
        # The state
        geom_rect(aes(fill=Value,
                      xmin=Start,
                      xmax=End,
                      ymin=Iteration-height/2,
                      ymax=Iteration+height/2), alpha=.5) -> goijk;
    return(goijk);
}

k_chart_pmtool <- function (dfw = NULL)
{
    if (is.null(dfw)) stop("dfw provided to k_chart is NULL");

    dfw <- dfw %>%
        filter(sched == pajer$pmtool$state$sched);

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
        ylab("PMTool\nIteration") +
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

var_chart <- function (dfv = NULL, ylabel = NA)
{
    if (is.null(dfv)) return(NULL);

    variable <- dfv %>% select(Type) %>% .$Type %>% unique;
    if (is.na(ylabel)) ylabel = variable;

    k <- dfv %>% rename(x=Start, xend=End, y=Value) %>% mutate(yend=y) %>% select(-Duration);
    v <- k %>% group_by(ResourceId, Type) %>% mutate(xend=x, y=y, yend=lag(y));# %>% na.omit();
    k %>%
        ggplot() +
        default_theme() +
        geom_segment(aes(x=x, xend=xend, y=y, yend=yend, color=ResourceId)) +
        geom_segment(data=v, aes(x=x, xend=xend, y=y, yend=yend, color=ResourceId)) +
        geom_point(size=.1, aes(x=x, y=y, color=ResourceId)) +
        coord_cartesian(xlim=c(0, max(dfv$End))) +
        ylim (0, NA) +
        ylab (ylabel) +
        scale_colour_brewer(palette = "Dark2");
}

var_chart_text <- function (dfv = NULL, tstart = NULL, tend = NULL, y_end = NULL)
{
    if (is.null(dfv)) return(NULL);
    max_value <- y_end;
    ms <- dfv %>% filter(Start < tend & End > tstart);
    ret <- list();
    #Calculate selected state % in time
    total_time <- tend - tstart;
    ms <- ms %>%
        group_by (ResourceId) %>%
        summarize(xvar = round(sum(Value * (Duration/1000) / 1024),2));


    if(nrow(ms) != 0){
        globalEndTime <- tend * 1.01;
        ms <- ms %>% tibble::rowid_to_column("Position")
        ms$Position <- max_value*0.9 - (ms$Position-1) * (max_value/nrow(ms))
        ms$xvar <- paste0(ms$xvar, " GB");
        ret[[length(ret)+1]] <- geom_label(data=ms, x = globalEndTime, colour = "white", fontface = "bold", aes(y = Position, label=xvar, fill = ResourceId), alpha=1.0, show.legend = FALSE);
    }
    return(ret);
}

link_chart <- function (data = NULL, tstart = NULL, tend = NULL)
{
  if (is.null(data)) stop("data provided to memory_chart is NULL");

  loginfo("Entry of link_chart");

  #Plot
  gow <- ggplot() + default_theme();

  # Add states and outliers if requested
  gow <- gow + geom_links(data, tstart=tstart, tend=tend);

  #gow = gow + scale_fill_manual(values = starpu_colors());

  loginfo("Exit of link_chart");
  return(gow);
}

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
        ungroup() %>%
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

starpu_mpi_grid_arrange <- function(atree, st, st_pm, st_mm, transf, starpu, ijk, ijk_pm, lackready, ready, submitted, mpi, mpiconc, mpistate, gpu, memory, gflops, title = NULL)
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
    if (pjr(pajer$pmtool$kiteration$active)){
        P[[length(P)+1]] <- ijk_pm;
        H[[length(H)+1]] <- pjr_value(pajer$pmtool$kiteration$height, 2);
    }
    if (pjr(pajer$pmtool$state$active)){
        P[[length(P)+1]] <- st_pm;
        H[[length(H)+1]] <- pjr_value(pajer$pmtool$state$height, 4);
    }
    if (pjr(pajer$memory$state$active)){
        P[[length(P)+1]] <- st_mm;
        H[[length(H)+1]] <- pjr_value(pajer$memory$state$height, 3);
    }
    if (pjr(pajer$memory$transfers$active) && !pjr(pajer$memory$combined)){
        P[[length(P)+1]] <- transf;
        H[[length(H)+1]] <- pjr_value(pajer$memory$transfers$height, 2);
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
        H[[length(H)+1]] <- pjr_value(pajer$usedmemory$height, 2);
    }
    if (pjr(pajer$gpubandwidth$active)){
        P[[length(P)+1]] <- gpu;
        H[[length(H)+1]] <- pjr_value(pajer$gpubandwidth$height, 2);
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

the_master_function <- function(data = NULL)
{
    if(is.null(data)) return(NULL);
    if(is.null(pajer)) return(NULL);

    # Activate logs
    if(pjr(pajer$log)){
        addHandler(writeToConsole)
    }

    # Get data
    directory <- data$Origin;
    dfw <- data$State;
    dfv <- data$Variable;

    if(is.null(dfw)){
       stop("The State data was not loaded, check if the feather files exists.");
    }

    loginfo("Starting the master function");

    # Fail Checking
    if((pjr(pajer$pmtool$state$active) || pjr(pajer$pmtool$kiteration$active)) && is.null(data$pmtool_states)){
      print("Pmtool states config is active but the data is NULL")
      pajer$pmtool$state$active <<- FALSE;
      pajer$pmtool$kiteration$active <<- FALSE;
    }

    if(pjr(pajer$pmtool$bounds$active) && is.null(data$pmtool)){
      print("Pmtool bounds config is active but the data is NULL")
      pajer$pmtool$bounds$active <<- FALSE;
    }

    if(is.null(data$Link)){
      print("This dataset dont have links, disabling some options")
      pajer$memory$transfers$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

    dfevents = dfw %>% filter(Type == "Memory Node State")

    if((dfevents %>% nrow) == 0 && ( pjr(pajer$memory$new_data) && (data$Events %>% nrow) == 0) ){
      print("This dataset dont have memory node states")
      pajer$memory$state$active <<- FALSE;
      pajer$memory$combined <<- FALSE;
    }

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
    gow_pm <- geom_blank();
    gow_mm <- geom_blank();
    gow_tf <- geom_blank();
    gstarpu <- geom_blank();
    goijk <- geom_blank();
    goijk_pm <- geom_blank();
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

    if (pjr(pajer$pmtool$state$active)){
        data %>% state_pmtool_chart () + tScale -> gow_pm;
    }

    memory_combined <- pjr(pajer$memory$combined) & pjr(pajer$memory$transfers$active);

    if (pjr(pajer$memory$state$active)){
        if(pjr(pajer$memory$new_data)){
            loginfo("Memory Chart using Events")
            data %>% events_memory_chart(combined = memory_combined, tstart=tstart, tend=tend) + tScale -> gow_mm;
        }else{
            loginfo("Memory Chart using States")
            data %>% memory_chart(combined = memory_combined, tstart=tstart, tend=tend) + tScale -> gow_mm;
        }
    }

    if (pjr(pajer$memory$transfers$active) & !memory_combined){
        data %>% link_chart(tstart=tstart, tend=tend) + tScale -> gow_tf;
    }



    # StarPU SpaceTime
    if (pjr(pajer$starpu$active)){
        loginfo("Creating the StarPU Space/Time");
        if (pjr(pajer$starpu$aggregation$active)){
          loginfo("Will call st_time_aggregation");
          aggStep <- pjr_value(pajer$starpu$aggregation$step, globalAggStep);
          dfw_agg <- st_time_aggregation(dfw, StarPU.View=TRUE, step=aggStep);
          data %>% st_time_aggregation_plot (dfw_agg, StarPU.View=TRUE) + tScale -> gstarpu;
          loginfo("st_time_aggregation completed");
        }else{
          data %>% state_chart (globalEndTime = tend, StarPU.View = TRUE) + tScale -> gstarpu;
          loginfo("state_chart for StarPU behavior completed (no aggregation)");
        }
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

    # KIteration PMTOOL
    if (pjr(pajer$pmtool$kiteration$active)){
        loginfo("Creating the KIteration for PMTool");
        goijk_pm <- k_chart_pmtool(data$pmtool_states) + tScale;

        if (!pjr(pajer$pmtool$kiteration$legend)){
            goijk_pm <- goijk_pm + theme(legend.position="none");
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
            var_chart(ylabel="Used Mem.\n(MB)") + tScale;
        if (!pjr(pajer$usedmemory$legend)){
            goguv <- goguv + theme(legend.position="none");
        }
        # TODO: user limit
    }

    # MPIBandwidth
    if (pjr(pajer$mpibandwidth$active)){
        loginfo("Creating the MPIBandwidth plot");
        aggStep <- pjr_value(pajer$mpibandwidth$step, globalAggStep);
        mpi_out <- dfv %>% filter(grepl("mpict", ResourceId), grepl("Out", Type));
        if ((mpi_out %>% nrow) == 0){
            print("There aren't any information on MPIBandwidth, ignoring it.");
            pajer$mpibandwidth$active <<- FALSE;
        }else{
            gomov <- mpi_out %>%
                var_integration_segment_chart(., ylabel="MPI\n(MB/s)", step=aggStep) + tScale;
            if (!pjr(pajer$mpibandwidth$legend)){
                gomov <- gomov + theme(legend.position="none");
            }
            gomov <- userYLimit(gomov, pajer$mpibandwidth$limit, c(tstart, tend));
        }
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
        if(pjr(pajer$gpubandwidth$total)){
           ms <- dfv %>%
              filter(grepl("MEMMANAGER", ResourceId), grepl("Out", Type)) %>%
              filter(Resource != "MEMMANAGER0" | Node != "MEMMANAGER0") %>%
              group_by(Type, Node, ResourceType, Start, End, Duration) %>%
              summarize(Value = sum(Value), N=n()) %>%
              rename(ResourceId = Node)
          y_size <- layer_scales(gogov)$y$range$range[2];
          gogov <- gogov + ms %>% var_chart_text(tstart=tstart, tend=tend, y_end = y_size);
        }
    }

    loginfo("Assembling the plot");

    # assembling
    g <- starpu_mpi_grid_arrange(atree = goatreet,
                                 st = gow,
                                 st_pm = gow_pm,
                                 st_mm = gow_mm,
                                 transf = gow_tf,
                                 starpu = gstarpu,
                                 ijk = goijk,
                                 ijk_pm = goijk_pm,
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
    if (StarPU){
      ret[[length(ret)+1]] <- scale_fill_manual(values = starpu_colors());
    }else{
      ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));
    }

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

geom_events <- function (main_data = NULL, data = NULL, combined = FALSE, tstart=NULL, tend=NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_events");


    loginfo("Starting geom_events");

    dfw <- data

    dfl <- main_data$Link;

    loginfo("Starting geom_links");


    col_pos <- data.frame(Container=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position")
    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);
    dfw <- dfw %>% left_join(col_pos, by=c("Container" = "Container"))
    ret <- list();

    dfw$Height = 1;


    # Color mapping
    #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    # Hardcoded here because yconf is specific to Resources Workers
    yconfm <- dfw %>% ungroup %>%
        select(Container, Position, Height) %>%
        distinct() %>%
        group_by(Container) %>%
        arrange(Container) %>%
        ungroup;

    #yconfm$Height = pjr_value(pajer$memory$state$height, 2);
    #yconfm$Position = yconfm$Position * yconfm$Height;
    #dfw$Height = pjr_value(pajer$memory$state$height, 2);
    #dfw$Position = dfw$Position * dfw$Height;

    yconfm$Container <- lapply(yconfm$Container, function(x) gsub("MEMMANAGER", "MM", x));

    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$Container, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab("Memory State");


    border <- 0
    if(pjr(pajer$memory$state$border)){
        border <- 1
    }
    # Add states
    ret[[length(ret)+1]] <- geom_rect(data=dfw, aes(fill=Type, xmin=Start, xmax=End, ymin=Position, ymax=Position+(2.0-0.4-Height)), color= "black", linetype=border, size=0.4, alpha=0.5);
    dx <- dfw %>% filter(Type == "Allocating Start")

    if(pjr(pajer$memory$state$text)){
      ret[[length(ret)+1]] <- geom_text(data=dx, colour = "black", fontface = "bold", aes(x = Start+Duration/2, y = Position+(2.0-0.4-Height)/2, angle=90, label=substr(Handle, start = 6, stop = 12)), size = 3, alpha=1.0, show.legend = FALSE);
    }

    loginfo("Finishing geom_events");

    return(ret);
}


geom_memory <- function (data = NULL, combined = FALSE, tstart=NULL, tend=NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_memory");

    dfw <- data$State %>%
        # Memory State
        filter(Type == "Memory Node State");


    loginfo("Starting geom_memory");



    col_pos <- data.frame(ResourceId=unique(dfw$ResourceId)) %>% tibble::rowid_to_column("Position")
    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);
    dfw <- dfw %>% select(-Position) %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"))
    ret <- list();

    # Color mapping
    #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    # Hardcoded here because yconf is specific to Resources Workers
    yconfm <- dfw %>%
        select(Node, ResourceId, Position, Height) %>%
        distinct() %>%
        group_by(Node) %>%
        arrange(Node, ResourceId) %>%
        ungroup;
    yconfm$Height = pjr_value(pajer$memory$state$height, 2);
    yconfm$Position = yconfm$Position * yconfm$Height;
    dfw$Height = pjr_value(pajer$memory$state$height, 2);
    dfw$Position = dfw$Position * dfw$Height;

    yconfm$ResourceId <- lapply(yconfm$ResourceId, function(x) gsub("MEMMANAGER", "MM", x));

    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab("Memory State");

    border <- NA;

    if(pjr(pajer$memory$state$border)){
        border <- "black";
    }
    const_depth <- 0;

    if(pjr(pajer$memory$state$depth$active)){
        const_depth <- pjr_value(pajer$memory$state$depth$height, 0.15);
    }

    dfw$Height = dfw$Depth * const_depth;

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+(2.0-0.1-Height)), linetype=1, color=border, size=0.4, alpha=0.5);


    if(pjr(pajer$memory$state$total)){
      select <- pjr_value(pajer$memory$state$select, "DriverCopy");
      ms <- dfw %>% filter(Value == select);

      #Calculate selected state % in time
      total_time <- tend - tstart;
      ms <- ms %>%
          group_by (ResourceId, Node, Position) %>%
          summarize(percent_time = round((sum(End-Start)/total_time)*100,2));

      #ms <- data.frame(with(ms, table(Node, ResourceId)));
      if(nrow(ms) != 0){
          #ms[2] <- data.frame(lapply(ms[2], as.character), stringsAsFactors=FALSE);
          #ms <- ms %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
          ms$Value <- select;
          globalEndTime <- tend * 1.01;
          ms$Position = ms$Position + 0.8;
          ms$percent_time <- paste0(ms$percent_time, "%");
          ret[[length(ret)+1]] <- geom_label(data=ms, x = globalEndTime, colour = "white", fontface = "bold", aes(y = Position, label=percent_time, fill = Value), alpha=1.0, show.legend = FALSE);
      }
    }

    loginfo("Finishing geom_memory");

    return(ret);
}


geom_links <- function (data = NULL, combined = FALSE, tstart=NULL, tend=NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_links");

    #Get the start info on states because link dont have nodes & Position

    dfl <- data$Link;

    loginfo("Starting geom_links");

    col_pos <- as.tibble(data.frame(ResourceId=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position"));
    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);

    if(combined){
      col_pos$Position = col_pos$Position * pjr_value(pajer$memory$state$height, 2);;
    }

    ret <- list();

    dfl <- dfl %>% left_join(col_pos, by=c("Origin" = "ResourceId")) %>%
                                  rename(O_Position = Position) %>%
                                  left_join(col_pos, by=c("Dest" = "ResourceId")) %>%
                                  rename(D_Position = Position);
    stride <- 0.3;

    dfl$Height <- 1;

    if(combined){
      stride = stride*pjr_value(pajer$memory$state$height, 2);;
    }

    if(!combined){

      #dfw <- dfw %>% select(-Position) %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
      # Hardcoded here because yconf is specific to Resource Workers

      yconfm <- dfl %>%
          select(Dest, D_Position, Height) %>%
          distinct() %>%
          group_by(Dest) %>%
          arrange(Dest) %>%
          ungroup;

      yconfm$Height <- 1;
      yconfm$Dest <- lapply(yconfm$Dest, function(x) gsub("MEMMANAGER", "MM", x));
      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$D_Position, labels=yconfm$Dest, expand=c(0.10,0.5));

      # Color mapping
      #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

      # Y label
      ret[[length(ret)+1]] <- ylab("Transfers");
      stride <- 0.0;
    }
    dfl$O_Position = dfl$O_Position + stride;
    dfl$D_Position = dfl$D_Position + stride;
    arrow_g <- NULL;
    if(pjr(pajer$memory$transfers$arrow)){
        arrow_g <- arrow(length = unit(0.15, "cm"));
    }
    if(pjr(pajer$memory$transfers$border)){
        ret[[length(ret)+1]] <- geom_segment(data=dfl, aes(x = Start, xend = End, y = O_Position, yend = D_Position), arrow = arrow_g, alpha=0.5, size=1.5, color = "black");
    }


    ret[[length(ret)+1]] <- geom_segment(data=dfl, aes(x = Start, xend = End, y = O_Position, yend = D_Position, color = Origin), arrow = arrow_g, alpha=1.0);
    selected_dfl <- dfl %>% filter(End > tstart) %>%  filter(Start < tend);

    #ret[[length(ret)+1]] <- geom_text(data=dfl, colour = "black", fontface = "bold", aes(x = Start, y = O_Position, label=Key), size = 3, alpha=1.0, show.legend = FALSE);
    if(pjr(pajer$memory$transfers$total)){
      total_links <- data.frame(with(selected_dfl, table(Origin)))
      if(nrow(total_links) != 0 & !combined){
          total_links[1] <- data.frame(lapply(total_links[1], as.character), stringsAsFactors=FALSE);
          total_links <- total_links %>% left_join(col_pos, by=c("Origin" = "ResourceId"));

          globalEndTime <- tend * 1.01;

          ret[[length(ret)+1]] <- geom_label(data=total_links, x = globalEndTime, colour = "white", fontface = "bold", aes(y = Position, label=Freq, fill = Origin), alpha=1.0, show.legend = FALSE);
      }
    }
    loginfo("Finishing geom_links");

    return(ret);
}

geom_pmtool_states <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_pmtool_states");


    dfw <- data$pmtool_states %>% filter(sched == pajer$pmtool$state$sched);


    loginfo("Starting geom_pmtool_states");

    ret <- list();

    # Color mapping
    ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    gg <- data$State %>% filter(Application == TRUE);
    yconfm <- yconf(gg);
    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab("Pmtool Workers");

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+Height-0.4), alpha=0.5);


    loginfo("Finishing geom_pmtool_states");

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

calculate_resource_idleness <- function(dfw = NULL, max_only = TRUE)
{
    if(is.null(dfw)) stop("Input data frame is NULL");

    # Get only application states
    dfw <- dfw %>% filter(Application == TRUE);

    # Obtain time interval
    tstart <- dfw %>% .$Start %>% min;
    tend <- dfw %>% .$End %>% max;

    #Calculate resources idleness
    total_time <- tend - tstart;
    dfw <- dfw %>%
        group_by (ResourceType, ResourceId, Node, Position, Height) %>%
        summarize(Idleness = round((1-(sum(End-Start)/total_time))*100,2),
                  End = max(End));
    if(max_only){
        dfw <- dfw %>% group_by(Node, ResourceType) %>%
            filter(Idleness %in% c(max(Idleness))) #%>% #, min(Idleness))) %>%
    }
    dfw %>% ungroup();
}

geom_idleness <- function(data = NULL)
{
    if(is.null(data)) stop("data provided for geom_idleness is NULL");

    dfidle <- calculate_resource_idleness(data$State, !pjr_value(pajer$idleness_all, FALSE));

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

geom_makespan_pmtool <- function(data = NULL)
{
    if(is.null(data)) stop("data provided for geom_makespan_pmtool is NULL");
    dfw <- data$pmtool_states;

    bsize = pjr_value(pajer$base_size, 22);

    tend = dfw %>% filter(sched == pajer$pmtool$state$sched) %>% pull(End) %>% max;
    print(paste("makespan pm tool is", tend));
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

    bsize = pjr_value(pajer$base_size, 22)/5;

    # Obtain time interval
    dfwapp <- data$State %>% filter(Application == TRUE) %>%
        filter(Type == "Worker State");
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    if (!is.null(pernodeABEdf)){
        ret <- list(
            geom_segment(data=pernodeABEdf, aes(x = Result+tstart, xend=Result+tstart, y = MinPosition, yend=MaxPosition), size=5, alpha=.7, color="gray"),
            geom_text (data=pernodeABEdf, aes(x = Result+tstart, y = MinPosition+(MaxPosition-MinPosition)/2, label=paste0(ifelse(pjr_value(pajer$st$abe$label, TRUE), "ABE: ", ""), round(Result, 0))), angle=90, color="black", size=bsize)
        );
        return(ret);
    }
    return(list());
}

geom_pmtools_bounds <- function(data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_pmtools_bounds");

    dftemp <- data$State %>%
        filter(Application == TRUE) %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Position, Height);
    # Y position
    minPos = dftemp %>% pull(Position) %>% min;
    minHeight = dftemp %>% pull(Height) %>% min;
    maxPos = dftemp %>% pull(Position) %>% max + minHeight/2;

    data$pmtool %>%
      mutate(MinPosition = minPos,
            MaxPosition = maxPos) -> df.pmtool;

    bsize = pjr_value(pajer$base_size, 22)/5;

    for(i in 1:nrow(df.pmtool)) {
      bound <- df.pmtool[i,];
      if (!is.null(bound)){
          ret <- list(
              geom_segment(data=bound, aes(x = Time+tstart, xend=Time+tstart, y = MinPosition, yend=MaxPosition), size=5, alpha=.7, color="gray"),
              geom_text (data=bound, aes(x = Time+tstart, y = MinPosition+(MaxPosition-MinPosition)/2, label=paste0(ifelse(pjr_value(pajer$pmtool$bounds$label, TRUE), paste0(Alg, ": "), ""), round(Time, 0))), angle=90, color="black", size=bsize)
          );
          return(ret);
      }
    }
    return(list());
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

    if(is.null(ret)){
        return(data.frame());
    }

    # Enrich states
    dfw <- data$State;
    ret %>%
        filter(!grepl("mpi", JobId)) %>%
        left_join(dfw, by=c("JobId" = "JobId")) -> retw;

    # Enrich links
    if(is.null(data$Link)){
        dfl <- data.frame()
    } else {
        dfl <- data$Link
    }
    if(TRUE %in% grepl("mpi", ret$JobId)){
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
    } else {
        data.frame() -> retl;
    }

    # Merge
    return(retw %>% bind_rows(retl) %>% arrange(Start));
}

gaps_backward_deps_rec <- function(data = NULL, path = NULL, task = NULL, levels = 1)
{
    if (is.null(data)) stop("data is NULL when given to gaps_backward_deps_rec");
    if (is.null(task)) stop("task is NULL when given to gaps_backward_deps_rec");
    if (is.null(path)) stop("path is NULL when given to gaps_backward_deps_rec");

    dta <- data$Gaps %>%
        # get only the job id for which we have an interest
        filter(JobId == task)

    if ((dta %>% nrow) == 0){
        print(paste0("The selected task on config$st$tasks$list is invalid (skipping it):", task));
        return(NULL);
    }

    dta %>%
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
    if ((paths %>% nrow) == 0){
        return(list());
    }

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
