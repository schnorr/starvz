
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
