library(logging);
library(feather);

resolve_io_function <- function(targetDf, directory = ".") {
    switch(targetDf,
        "dfw" = read_state(directory),
        "dfe" = read_entities(directory),
        "dfa" = read_atree(directory),
        "dfv" = read_variables(directory),
        "dfl" = read_links_io(directory),
        "dfdag" = read_dag_io(directory),
        "dpmtb" = read_pmtools_bounds(directory),
        "dpmts" = read_pmtools_states(directory),
        "ddh" = read_data_handles(directory),
        "task_handles" = read_tasks_handles(directory),
        "tasks" = read_tasks(directory)
    );
}

read_state <- function (directory = '.') {
    state.csv <- paste0(directory, '/paje.state.csv.gz');
    if (!file.exists(state.csv)) {
        stop('States CSV file does not exist');
    }
    loginfo(paste('Reading', state.csv));
    dfw <- fread(input=paste0('zcat < ',state.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
    dfw <- dfw %>% mutate(
        Nature = as.character(Nature),
        ResourceId = as.character(ResourceId),
        Type = as.character(Type),
        Start = as.double(Start),
        End = as.double(End),
        Duration = as.double(Duration),
        Depth = as.double(Depth),
        Value = as.factor(Value),
        Size = as.character(Size),
        Params = as.character(Params),
        Footprint = as.character(Footprint),
        Tag = as.character(Tag),
        JobId = as.character(JobId),
        GFlop = as.numeric(GFlop),
        SubmitOrder = as.character(SubmitOrder),
        X = as.integer(X),
        Y = as.integer(Y),
        Iteration = as.integer(Iteration),
        Subiteration = as.integer(Subiteration)
    );
  if (nrow(dfw) == 0) stop('After reading states CSV, number of rows is zero.');
  return(dfw);
};

read_entities <- function(directory = '.') {
    entities.feather <- paste0(directory, "/entities.feather");
    entities.csv <- paste0(directory, "/entities.csv.gz");

    if (file.exists(entities.feather)){
        loginfo(paste("Reading ", entities.feather));
        dfe <- read_feather(entities.feather);
        loginfo(paste("Read of", entities.feather, "completed"));
    }else if (file.exists(entities.csv)){
        loginfo(paste("Reading ", entities.csv));
        dfe <- fread(input=paste0('zcat < ',entities.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        dfe <- dfe %>% mutate(
            Parent = as.character(Parent),
            Name = as.character(Name),
            Type = as.character(Type),
            Nature = as.character(Nature)
        );
        loginfo(paste("Read of", entities.csv, "completed"));
    }else{
        loginfo(paste("Files", entities.feather, "or", entities.csv, "do not exist."));
        return(NULL);
    }
    if ((dfe %>% nrow) == 0) stop(paste("After reading the entities file, the number of rows is zero"));
    return (dfe);
}

read_atree <- function(directory = '.') {
    atree.feather = paste0(directory, "/atree.feather");
    atree.csv <- paste0(directory, "/atree.csv.gz");

    if (file.exists(atree.feather)){
        loginfo(paste("Reading ", atree.feather));
        dfa <- read_feather(atree.feather);
    }else if (file.exists(atree.csv)){
        loginfo(paste("Reading ", atree.csv));
        dfa <- fread(input=paste0('zcat < ',atree.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        dfa <- dfa %>% mutate(
            Node = as.integer(Node),
            DependsOn = as.integer(DependsOn)
        );
    }else{
        loginfo(paste("Files", atree.feather, "or", atree.csv, "do not exist."));
        return(NULL);
    }
    return(dfa);
}

read_variables <- function(directory = '.') {
    variable.feather = paste0(directory, "/paje.variable.feather");
    variable.csv <- paste0(directory, "/paje.variable.csv.gz");
    if(file.exists(variable.feather)){
        loginfo(paste("Reading ", variable.feather));
        dfv <- read_feather(variable.feather);
        loginfo(paste("Read of", variable.feather, "completed"));
    }else if(file.exists(variable.csv)){
        loginfo(paste("Reading ", variable.csv));
        dfv <- fread(input=paste0('zcat < ',variable.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        dfv <- dfv %>% mutate(
            Nature = as.character(Nature),
            ResourceId = as.character(ResourceId),
            Type = as.character(Type),
            Start = as.double(Start),
            End = as.double(End),
            Duration = as.double(Duration),
            Value = as.double(Value)
        );
        loginfo(paste("Read of", variable.csv, "completed"));
    }else{
        stop(paste("Files", variable.feather, "or", variable.csv, "do not exist"));
    }
    return(dfv);
}

read_links_io <- function(directory = '.') {
    link.feather = paste0(directory, "/paje.link.feather");
    link.csv <- paste0(directory, "/paje.link.csv.gz");
    if(file.exists(link.feather)){
        loginfo(paste("Reading ", link.feather));
        dfl <- read_feather(link.feather) %>%
            mutate(Size = as.integer(Size));
        loginfo(paste("Read of", link.feather, "completed"));
    }else if(file.exists(link.csv)){
        loginfo(paste("Reading ", link.csv));
        dfl <- fread(input=paste0('zcat < ',link.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        dfl <- dfl %>% mutate(
            Nature = as.character(Nature),
            Container = as.character(Container),
            Type = as.character(Type),
            Start = as.double(Start),
            End = as.double(End),
            Duration = as.double(Duration),
            Size = as.integer(Size),
            Origin = as.character(Origin),
            Dest = as.character(Dest),
            Key = as.character(Key)
        );
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
    return(dfl);
}

read_dag_io <- function(directory = '.') {
    dag.feather = paste0(directory, "/dag.feather");
    dag.csv <- paste0(directory, "/dag.csv.gz");
    if(file.exists(dag.feather)){
        loginfo(paste("Reading ", dag.feather));
        dfdag <- read_feather(dag.feather);
        loginfo(paste("Read of", dag.feather, "completed"));
    }else if(file.exists(dag.csv)){
        loginfo(paste("Reading ", dag.csv));
        dfdag <- fread(input=paste0('zcat < ',dag.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        # TODO: This does not seem to work. Check with Lucas why the header names are different in the CSV file.
        # dfdag <- dfdag %>% mutate(
        #     Node = as.integer(Node),
        #     DependsOn = as.integer(DependsOn)
        # );
        loginfo(paste("Read of", dag.csv, "completed"));
    }else{
        logwarn(paste("Files", dag.feather, "or", dag.csv, "do not exist"));
        return(NULL);
    }
    return(dfdag);
}

read_pmtools_bounds <- function(directory = '.') {
    pmtools_bounds.feather = paste0(directory, "/pmtool.feather");
    pmtools_bounds.csv <- paste0(directory, "/pmtool.csv.gz");

    if (file.exists(pmtools_bounds.feather)){
        loginfo(paste("Reading ", pmtools_bounds.feather));
        dpmtb<- read_feather(pmtools_bounds.feather);
        loginfo(paste("Read of", pmtools_bounds.feather, "completed"));
    }else if (file.exists(pmtools_bounds.csv)){
        loginfo(paste("Reading ", pmtools_bounds.csv));
        dpmtb <- fread(input=paste0('zcat < ',pmtools_bounds.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        dpmtb <- dpmtb %>% mutate(
            Alg = as.character(Alg),
            Time = as.double(Time)
        );
        loginfo(paste("Read of", pmtools_bounds.csv, "completed"));
    }else{
        loginfo(paste("Files", pmtools_bounds.feather, "or", pmtools_bounds.csv, "do not exist."));
        return(NULL);
    }
    return(dpmtb);
}

read_pmtools_states <- function(directory = '.') {
    pmtools_states.feather = paste0(directory, "/pmtool_states.feather");
    pmtools_states.csv <- paste0(directory, "/pmtool_states.csv.gz");

    if (file.exists(pmtools_states.feather)){
        loginfo(paste("Reading ", pmtools_states.feather));
        dpmts <- read_feather(pmtools_states.feather);
        loginfo(paste("Read of", pmtools_states.feather, "completed"));
    }else if (file.exists(pmtools_states.csv)){
        loginfo(paste("Reading ", pmtools_states.csv));

        dpmts <- fread(input=paste0('zcat < ',pmtools_states.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        dpmts <- dpmts %>% mutate(
            shced = as.character(sced),
            Tid = as.integer(Tid),
            worker = as.integer(worker),
            taskType = as.character(taskType),
            JobId = as.character(JobId),
            start = as.double(start),
            duration = as.double(duration),
            end = as.double(end)
        );
        #pmtools states gives time in miliseconds
        loginfo(paste("Read of", pmtools_states.csv, "completed"));
    }else {
        logwarn(paste("Files", pmtools_states.feather, "or", pmtools_states.csv, "do not exist"));
        return(NULL);
    }
    return(dpmts)
}

read_data_handles <- function(directory = '.') {
    data_handles.feather = paste0(directory, "/data_handles.feather");
    data_handles.csv <- paste0(directory, "/rec.data_handles.csv.gz");

    if (file.exists(data_handles.feather)){
        loginfo(paste("Reading ", data_handles.feather));
        ddh <- read_feather(data_handles.feather);
        loginfo(paste("Read of", data_handles.feather, "completed"));
    }else if (file.exists(data_handles.csv)){
        loginfo(paste("Reading ", data_handles.csv));
        ddh <- fread(input=paste0('zcat < ',data_handles.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        ddh <- ddh %>% mutate(
            Handle = as.character(Handle),
            HomeNode = as.integer(HomeNode),
            Size = as.integer(Size),
            Coordinates = as.character(Coordinates)
        );
        # Not supported in feather
        # pm$Coordinates <- lapply(strsplit(pm$Coordinates, " "), as.integer);
        loginfo(paste("Read of", data_handles.csv, "completed"));
    }else{
        loginfo(paste("Files", data_handles.feather, "or", data_handles.csv, "do not exist."));
        return(NULL);
    }
    return(ddh);
}

read_tasks_handles <- function(directory = '.') {
    task_handles.feather = paste0(directory, "/task_handles.feather");

    if (!file.exists(task_handles.feather)){
        return(NULL)
    }
    loginfo(paste("Reading ", task_handles.feather));
    task_handles <- read_feather(task_handles.feather);
    loginfo(paste("Read of", task_handles.feather, "completed"));
    return(task_handles);
}

read_tasks <- function(directory = '.') {
    tasks.feather = paste0(directory, "/tasks.feather");
    tasks.csv <- paste0(directory, "/rec.tasks.csv.gz");

    if (file.exists(tasks.feather)){
        loginfo(paste("Reading ", tasks.feather));
        tasks <- read_feather(tasks.feather);
        loginfo(paste("Read of", tasks.feather, "completed"));
    }else if (file.exists(tasks.csv)){
        loginfo(paste("Reading ", tasks.csv));
        tasks <- fread(input=paste0('zcat < ',tasks.csv), sep=",", header=TRUE, fill=TRUE, data.table=FALSE);
        tasks <- tasks %>% mutate(
            Control = as.character(Control),
            JobId = as.integer(JobId),
            SubmitOrder = as.integer(SubmitOrder),
            SubmitTime = as.double(SubmitTime),
            Handles = as.character(Handles),
            MPIRank = as.integer(MPIRank),
            DependsOn = as.character(DependsOn),
            Tag = as.character(Tag),
            Footprint = as.character(Footprint),
            Iteration = as.integer(Iteration),
            Name = as.character(Name),
            Model = as.character(Model),
            Priority = as.integer(Priority),
            WorkerId = as.integer(WorkerId),
            MemoryNode = as.integer(MemoryNode),
            StartTime = as.double(StartTime),
            EndTime = as.double(EndTime),
            Parameters = as.character(Parameters),
            Modes = as.character(Modes),
            Sizes = as.character(Sizes)
        );
    } else {
        loginfo(paste("Files", tasks.feather, "or", tasks.csv, "do not exist."));
        return(NULL);
    }
    return(tasks);
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