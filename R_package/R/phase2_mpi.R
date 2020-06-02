
geom_mpistates <- function (dfw = NULL)
{
    if (is.null(dfw)) stop("dfw is NULL when given to geom_mpistates");

    if (nrow(dfw) == 0) stop("there is no data on MPI states");

    ret <- list();

    # Calculate Y position
    ypos <- tibble(ResourceId = (dfw %>% arrange(as.integer(as.character(Node))) %>% pull(ResourceId) %>% unique())) %>%
        mutate(Height = 1) %>%
        mutate(Position = cumsum(Height));

    dfw <- dfw %>%
        # Establish new position
        left_join(ypos, by = c("ResourceId"));

    mycolors <- c(brewer.pal(8, "Dark2"), "blue")

    # Color mapping
    ret[[length(ret)+1]] <- scale_fill_manual(values = mycolors);

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

    return(ret);
}

state_mpi_chart <- function (data = NULL)
{
    if (is.null(data)) stop("data provided to state_chart is NULL");

    #Plot
    gow <- ggplot() +
        default_theme() +
        # Add states and outliers if requested
        geom_mpistates(data);

    return(gow);
}
concurrent_mpi <- function(data = NULL)
{
    if (is.null(data)) return(NULL);

    data$Link %>%
        filter(grepl("mpicom", Key)) %>%
        select(-Container, -Type, -Duration) -> dflink;

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

concurrent_mpi_out <- function(data = NULL)
{
    if (is.null(data)) return(NULL);

    data$Link %>%
        filter(grepl("mpicom", Key)) %>%
        select(-Container, -Type, -Duration) -> dflink;

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
        arrange(Dest, Timestamp) %>%
        select(-Start) %>%
        rename(Start = Timestamp) %>%
        group_by(Dest) %>% mutate(End = lead(Start)) %>% na.omit %>% mutate(Duration = End-Start) %>% ungroup() %>%
        mutate(Type = "MPI Concurrent") %>%
        rename(ResourceId = Dest) %>%
        separate(ResourceId, into=c("Node", "Resource"), remove=FALSE) %>%
        mutate(Node = as.factor(Node)) %>%
        mutate(ResourceType = as.factor(gsub('[[:digit:]]+', '', Resource))) %>%
        select(Start, End, Duration, Node, ResourceId, ResourceType, Type, Value)

}
