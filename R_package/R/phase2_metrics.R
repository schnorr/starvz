
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

    # This fixes some problems on recent versions of tidyverse
    # Check: https://github.com/tidyverse/tidyr/issues/751
    # Check: https://github.com/tidyverse/tidyr/issues/694
    if(exists("unnest_legacy")){
      unnest <- unnest_legacy
    }

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
  df1.res_quantity <- dfl %>%
      select(Node, Resource, ResourceType) %>%
      unique %>%
      group_by(ResourceType) %>%
      summarize(Quantity=n())

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
  nnames <- length(names);
  nvalues <- length(values);
  ntypes <- length(types);
  size = nvalues*ntypes;

  if (debug){
      print("ABE: Inicial metrics (v2)");
      print(paste("values:", values));
      print(paste("types:", types));
      print(paste("names:", names));
      print(paste("nnames:", names));
      print(paste("nvalues:", nvalues));
      print(paste("ntypes:", ntypes));
      print(paste("size:", size));
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
  M = matrix(data=rep(0, (ntypes*nvalues)), nrow=ntypes)
  for (i in 1:ntypes) { # for each ResourceType
      for (j in 1:nvalues) { # for each Kernel
          M[i,nvalues*(i-1)+j] = m[i,j]
      }
  }
  m.con2 <- cbind(M, df1.res_quantity %>%
                     arrange(ResourceType) %>%
                     mutate(Quantity=as.numeric(Quantity)*-1) %>%
                     .$Quantity); # %>% set_colnames(names);
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

hl_per_node_ABE <- function (dfw = NULL)
{
    if(is.null(dfw)) stop("Input data frame is NULL");

    loginfo("hl_per_node_ABE starts");

    dftemp <- dfw %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Position, Height);
    pernodeABE <- dftemp %>%
        group_by(Node) %>%
        do(abe_cpu_cuda(.));
    # Y position
    pernodeABE <- dftemp %>%
        group_by(Node) %>%
        summarize(MinPosition = min(Position), MaxPosition = max(Position)+min(Height)/1.25) %>%
        left_join(pernodeABE, by="Node");

    loginfo("hl_per_node_ABE ends");

    return(pernodeABE);
}

hl_per_node_ABE_details <- function (data = NULL)
{
    if(is.null(data$Application)) stop("Input data is NULL");

    data$Application %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Color, Position, Height) %>%
        group_by(Node) %>%
        do(abe_cpu_cuda_details(.)) %>%
        ungroup();
}
hl_global_cpb <- function (data = NULL)
{
    if(is.null(data)) return(NULL);

    dfdag <- data$Dag;

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
    States <- bind_rows(data$Application, data$Starpu)
    States %>%
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

hl_global_abe <- function (dfw = NULL)
{
    if(is.null(dfw)) stop("Input data frame is NULL");

    loginfo("hl_global_abe starts");
    dfw %>%
        filter(grepl("CPU|CUDA", ResourceId)) %>%
        select(Node, Resource, ResourceType, Duration, Value, Position, Height) %>%
        do(abe_cpu_cuda(.)) -> globalABE
    loginfo("hl_global_abe ends");

    return(globalABE);
}

calculate_resource_idleness <- function(dfw = NULL, max_only = TRUE)
{
    if(is.null(dfw)) stop("Input data frame is NULL");

    # Get only application states
    dfw <- dfw %>%
            distinct(ResourceType, ResourceId, Node, Position, Height, JobId,
                     Value, Duration, .keep_all=TRUE);

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

    dfidle <- calculate_resource_idleness(data$Application, !pjr_value(pajer$idleness_all, FALSE));

    bsize = pjr_value(pajer$base_size, 22);
    expand = pjr_value(pajer$expand, 0.05);
    idleness_factor = pjr_value(pajer$idleness_factor, 5.5);

    globalEndTime <- dfidle %>% pull(End) %>% na.omit %>% max;
    ret <- NULL;
    ret <- geom_label(data=dfidle,
                     # The size of the idle values for each resource
                     size=bsize/idleness_factor,
                     # The X position of each one
                     x=0, # 2.5% before 0.0
                     hjust=0,
                     fill="white",
                     fontface = "bold",
                     # The Y position (depends on the Resource, so use "aes"
                     aes(y=Position+(Height/2.5), # vertical
                         # The idleness number followed by % as text
                         label=gsub("$", "%", Idleness)));
    return(ret);
}
geom_makespan <- function(data = NULL)
{
    if(is.null(data)) stop("data provided for geom_makespan is NULL");
    dfw <- data$Application;

    bsize = pjr_value(pajer$base_size, 22);

    tend = dfw %>% pull(End) %>% max;
    loginfo(paste("makespan is", tend));
    height = dfw %>% select(Position) %>% na.omit %>% pull(Position) %>% max;
    loginfo(paste("max height for makespan is", height));
    ret <- geom_text(data=data.frame(), x=tend, y=height*.5, aes(label=round(tend,0)), angle=90, size=bsize/4);
    return(ret);
}



geom_cpb <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_cpb");
    if (is.null(data$Dag)){
      logwarn("CPB is active but data$Dag is NULL")
      return(list());
    }

    # Calculate the global CPB
    cpbs <- hl_global_cpb(data);

    ret <- list();
    if (pajer$st$cpb){
        ret <- c(ret, geom_cpb_internal(data, cpbs$CPB, "CPB:"));
    }
    if (pjr(pajer$st$cpb_mpi$active)){
        if(is.na(pajer$st$cpb_mpi$tile_size)){
          logwarn("CPB_MPI is active and st$cpb_mpi$tile_size is NULL")
        }
        if(is.na(pajer$st$cpb_mpi$bandwidth)){
          logwarn("CPB_MPI is active and st$cpb_mpi$bandwidth is NULL")
        }
        tile_size = pajer$st$cpb_mpi$tile_size;
        bandwidth = pajer$st$cpb_mpi$bandwidth;
        cpbmpit = cpbs$CPB + cpbs$NMPI * (tile_size*tile_size*8) / bandwidth / 1000000;
        ret <- c(ret, geom_cpb_internal(data, cpbs$CPBMPI, "CPB-MPI:"));
        if (pjr(pajer$st$cpb_mpi$theoretical)){
            ret <- c(ret, geom_cpb_internal(data, cpbmpit, "CPB-MPI*:"));
        }
    }
    return(ret);
}

geom_cpb_internal <- function(data = NULL, value = NULL, desc = NULL)
{

    if (!is.null(value) && !is.null(desc)){
        dfw <- data$Application;

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
    pernodeABEdf <- hl_per_node_ABE(data$Application);

    bsize = pjr_value(pajer$base_size, 22)/5;

    # Obtain time interval
    dfwapp <- data$Application %>%
        filter(Type == "Worker State");
    tstart <- dfwapp %>% .$Start %>% min;
    tend <- dfwapp %>% .$End %>% max;

    abesize <- pjr_value(pajer$st$abe$size, 5)

    if (!is.null(pernodeABEdf)){
        ret <- list(
            geom_segment(data=pernodeABEdf, aes(x = Result+tstart, xend=Result+tstart, y = MinPosition, yend=MaxPosition), size=abesize, alpha=.7, color="gray")
        )
        if(pjr_value(pajer$st$abe$text, TRUE)){
            ret <- list(ret,
                        geom_text (data=pernodeABEdf, aes(x = Result+tstart, y = MinPosition+(MaxPosition-MinPosition)/2, label=paste0(ifelse(pjr_value(pajer$st$abe$label, TRUE), "ABE: ", ""), round(Result, 0))), angle=90, color="black", size=bsize)
                        )
        }
        return(ret);
    }
    return(list());
}
