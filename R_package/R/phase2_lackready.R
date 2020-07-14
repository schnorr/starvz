#' @include starvz_data.R

plot_lackready <- function (data = NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_lackready");
    if (is.null(data$Starpu)) stop("state is NULL when given to geom_lackready");
    if (is.null(data$Variable)) stop("variable is NULL when given to geom_lackready");

    data$Starpu %>%
        select(.data$Node, .data$Resource) %>%
        unique %>%
        group_by(.data$Node) %>%
        summarize(N=n()) %>%
        .$N %>%
        min -> minResources;

    aggStep <- data$config$lackready$aggregation
    #loginfo(paste("lack ready aggregation is", aggStep));
    threshold <- config_value(data$config$lackready$threshold, minResources);
    #loginfo(paste("lack ready threshold is", threshold));

    data$Variable %>%
        filter(.data$Type == "Ready") %>%
        group_by(.data$Type, .data$Node, .data$ResourceId, .data$ResourceType) %>%
        do(remyTimeIntegrationPrep(., myStep = aggStep)) %>%
        mutate(Start = .data$Slice, End = lead(.data$Slice), Duration = .data$End-.data$Start) %>%
        ungroup() %>%
        filter(!is.na(.data$End)) %>%
        group_by(.data$Type, .data$Node, .data$ResourceType, .data$Start, .data$End, .data$Duration) %>%
        summarize(Value = sum(.data$Value), N=n()) %>%
        ungroup() %>%
        rename(ResourceId = .data$Node) %>%
        filter(.data$Value < threshold) %>%
        group_by(.data$Type, .data$Start, .data$End) %>%
        summarize(Value=n()) %>%
        ungroup() %>%
        ggplot() + default_theme(data$config$base_size, data$config$expand) + geom_lackready();
}

geom_lackready <- function ()
{
    ret <- list();

    ret[[length(ret)+1]] <- scale_fill_gradient(low="lightsalmon", high="red1");
    ret[[length(ret)+1]] <- geom_rect(aes(fill=.data$Value,
                                          xmin=.data$Start,
                                          xmax=.data$End,
                                          ymin=0,
                                          ymax=1), alpha=1);
    ret[[length(ret)+1]] <- theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank());

    return(ret);
}
