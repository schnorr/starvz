
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
