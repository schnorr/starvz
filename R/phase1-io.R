library(tidyverse);

read_state <- function (directory) {
    state.csv <- paste0(directory, '/paje.state.csv');
    if (!file.exists(state.csv)) {
        stop('States CSV file does not exist');
    }
    loginfo(paste('Reading', state.csv));
    dfw <- read_csv(file = state.csv,
                  trim_ws = TRUE,
                  progress = TRUE,
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
                  ));
  if (nrow(dfw) == 0) stop('After reading states CSV, number of rows is zero.');
  return(dfw);
};

read_entities <- function(directory = '.') {
    entities.feather = paste0(directory, "/entities.feather");
    entities.csv = paste0(directory, "/entities.csv");

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
    if ((dfe %>% nrow) == 0) stop(paste("After reading the entities file, the number of rows is zero"));
    return (dfe);
}

read_atree <- function(directory = '.') {
    atree.feather = paste0(directory, "/atree.feather");
    atree.csv = paste0(directory, "/atree.csv");

    if (file.exists(atree.feather)){
        loginfo(paste("Reading ", atree.feather));
        dfa <- read_feather(atree.feather);
    }else if (file.exists(atree.csv)){
        loginfo(paste("Reading ", atree.csv));
        dfa <- read_csv(file=atree.csv,
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
    return(dfa);
}

read_variables <- function(directory = '.') {
    variable.feather = paste0(directory, "/paje.variable.feather");
    variable.csv = paste0(directory, "/paje.variable.csv");
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
    return(dfv);
}

read_links_io <- function(directory = '.') {
    link.feather = paste0(directory, "/paje.link.feather");
    link.csv = paste0(directory, "/paje.link.csv");
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
    return(dfl);
}

read_dag_io <- function(directory = '.') {
    dag.feather = paste0(directory, "/dag.feather");
    dag.csv = paste0(directory, "/dag.csv");
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
    return(dfdag);
}

read_pmtools_states <- function(directory = '.') {
    pmtools_states.feather = paste0(directory, "/pmtool_states.feather");
    pmtools_states.csv = paste0(directory, "/pmtool_states.csv");

    if (file.exists(pmtools_states.feather)){
        loginfo(paste("Reading ", pmtools_states.feather));
        dpmts <- read_feather(pmtools_states.feather);
        loginfo(paste("Read of", pmtools_states.feather, "completed"));
    }else if (file.exists(pmtools_states.csv)){
        loginfo(paste("Reading ", pmtools_states.csv));

        #sched Tid   worker taskType JobId start duration end

        dpmts <- read_csv(pmtools_states.csv,
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
        #pmtools states gives time in miliseconds
        loginfo(paste("Read of", pmtools_states.csv, "completed"));
    }else {
        logwarn(paste("Files", pmtools_states.feather, "or", pmtools_states.csv, "do not exist"));
        return(NULL);
    }
    return(dpmts)
}

read_pmtools_bounds <- function(directory = '.') {
    pmtools_bounds.feather = paste0(where, "/pmtool.feather");
    pmtools_bounds.csv = paste0(where, "/pmtool.csv");

    if (file.exists(pmtools_bounds.feather)){
        loginfo(paste("Reading ", pmtools_bounds.feather));
        dpmtb<- read_feather(pmtools_bounds.feather);
        loginfo(paste("Read of", pmtools_bounds.feather, "completed"));
    }else if (file.exists(pmtools_bounds.csv)){
        loginfo(paste("Reading ", pmtools_bounds.csv));
        dpmtb<- read_csv(pmtools_bounds.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Alg = col_character(),
                            Time = col_double()
                        ));
        loginfo(paste("Read of", pmtools_bounds.csv, "completed"));
    }else{
        loginfo(paste("Files", pmtools_bounds.feather, "or", pmtools_bounds.csv, "do not exist."));
        return(NULL);
    }

    filename <- 'pre.pmtool.feather';
    write_feather(pm, filename);
    return(filename);
}

read_data_handles <- function(directory = '.') {
    data_handles.feather = paste0(directory, "/data_handles.feather");
    data_handles.csv = paste0(directory, "/rec.data_handles.csv");

    if (file.exists(data_handles.feather)){
        loginfo(paste("Reading ", data_handles.feather));
        ddh <- read_feather(data_handles.feather);
        loginfo(paste("Read of", data_handles.feather, "completed"));
    }else if (file.exists(data_handles.csv)){
        loginfo(paste("Reading ", data_handles.csv));
        ddh <- read_csv(data_handles.csv,
                        trim_ws=TRUE,
                        col_types=cols(
                            Handle = col_character(),
                            HomeNode = col_integer(),
                            Size = col_integer(),
                            Coordinates = col_character()
                        ));

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
    task_handles.feather = paste0(where, "/task_handles.feather");

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
    tasks.csv = paste0(directory, "/rec.tasks.csv");

    if (file.exists(tasks.feather)){
        loginfo(paste("Reading ", tasks.feather));
        tasks <- read_feather(tasks.feather);
        loginfo(paste("Read of", tasks.feather, "completed"));
    }else if (file.exists(tasks.csv)){
        loginfo(paste("Reading ", tasks.csv));
        tasks <- read_csv(tasks.csv,
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
    } else {
        loginfo(paste("Files", tasks.feather, "or", tasks.csv, "do not exist."));
        return(NULL);
    }
    return(tasks);
}