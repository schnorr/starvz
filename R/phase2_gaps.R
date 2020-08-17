#' @include starvz_data.R
NULL

# 2nd phase task dependencies based on Gaps
gaps_backward_deps <- function(data = NULL, tasks = NULL, levels = 1) {
  if (is.null(data)) stop("data is NULL when given to gaps_backward_deps")
  if (is.null(tasks)) stop("task is NULL when given to gaps_backward_deps")
  if (levels < 1) stop("level lt 1 when given to  gaps_backward_deps")

  tibble(Path = tasks) %>%
    unique() %>%
    group_by(.data$Path) %>%
    do(gaps_backward_deps_one(data = data, task = .data$Path, levels = levels)) %>%
    ungroup()
}

gaps_backward_deps_one <- function(data = NULL, task = NULL, levels = 1) {
  ret <- gaps_backward_deps_rec(data = data, path = task, task = task, levels = levels)

  if (is.null(ret)) {
    return(data.frame())
  }

  # Enrich states
  dfw <- bind_rows(data$Starpu, data$Application)
  ret %>%
    filter(!grepl("mpi", .data$JobId)) %>%
    left_join(dfw, by = c("JobId" = "JobId")) -> retw

  # Enrich links
  if (is.null(data$Link)) {
    dfl <- data.frame()
  } else {
    dfl <- data$Link
  }
  if (TRUE %in% grepl("mpi", ret$JobId)) {
    ret %>%
      filter(grepl("mpi", .data$JobId)) %>%
      left_join(dfl, by = c("JobId" = "Key")) %>%
      # Post-processing
      # Keep only the destination of the link
      rename(ResourceId = .data$Dest) %>%
      separate(.data$ResourceId, into = c("Node", "Resource"), remove = FALSE) %>%
      select(-.data$Origin, -.data$Container) %>%
      # Enrich ResourceId with Height, Position
      left_join((data$Y %>% select(-.data$Type) %>% mutate(Parent = as.character(.data$Parent))), by = c("ResourceId" = "Parent")) -> retl
  } else {
    data.frame() -> retl
  }

  # Merge
  return(retw %>% bind_rows(retl) %>% arrange(.data$Start))
}

gaps_backward_deps_rec <- function(data = NULL, path = NULL, task = NULL, levels = 1) {
  if (is.null(data)) stop("data is NULL when given to gaps_backward_deps_rec")
  if (is.null(task)) stop("task is NULL when given to gaps_backward_deps_rec")
  if (is.null(path)) stop("path is NULL when given to gaps_backward_deps_rec")

  dta <- data$Gaps %>%
    # get only the job id for which we have an interest
    filter(.data$JobId == task)

  if ((dta %>% nrow()) == 0) {
    starvz_log(paste0("The selected task on config$st$tasks$list is invalid (skipping it):", task))
    return(NULL)
  }

  dta %>%
    filter(.data$JobId == task) %>%
    mutate(Path = path) %>%
    # the group_by is really not interessant, since we have just one JobId
    group_by(.data$JobId) %>%
    # reverse order by the end of its dependents
    arrange(-.data$End.y) %>%
    # get the last one to finish
    slice(1) %>%
    ungroup() %>%
    # get only what is useful
    select(.data$Path, .data$JobId, .data$Dependent) -> dep

  # check if dep has something
  if (nrow(dep) == 0) {
    return(NULL)
  }

  # recurse, if levels > 1
  if (levels > 1) {
    dep %>%
      bind_rows(gaps_backward_deps_rec(
        data = data,
        path = path,
        task = dep$Dependent,
        levels = (levels - 1)
      ))
  } else {
    return(dep)
  }
}
