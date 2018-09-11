
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
        ret[[length(ret)+1]] <- geom_label(data=ms, x = globalEndTime, colour = "white", fontface = "bold", aes(y = Position, label=xvar, fill = ResourceId), alpha=1.0, show.legend = FALSE)
        ret[[length(ret)+1]] <- scale_fill_brewer(palette = "Dark2")
    }
    return(ret);
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
