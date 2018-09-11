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
