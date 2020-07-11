
check_arrow <- function(){
  if(!arrow_available()){
    logwarn("Arrow was not property installed, use: install_arrow()")
  }
}

extract_colors <- function(dfw = NULL, colors = NULL)
{
    if(is.null(dfw)) return(NULL);
    if(is.null(colors)) return(NULL);

    dfw <- dfw %>% ungroup;

    dfw %>%
        select(Value) %>%
        unique %>%
        left_join(colors, by=c("Value")) %>%
        .$Color %>%
        setNames(dfw %>% select(Value) %>% unique %>% .$Value);
}

yconf <- function (dfw = NULL, option = "ALL")
{
    if(is.null(dfw)) return(NULL);

    # Currently being ignored
    # step <- pjr_value(pajer$st$labels, 6);
    dfw %>% mutate(Node = as.integer(as.character(Node))) -> dfw
    dfw %>% mutate(ResourceId = factor(ResourceId)) %>%
            mutate(ResourceId = factor(ResourceId, levels=mixedsort(levels(ResourceId)))) -> dfw
    if(option == "1CPU_per_NODE"){ #First
        # One CPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(1) %>%
            ungroup;
    }else if(option == "1GPU_per_NODE"){ #Last
        # One GPU per node
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(n()) %>%
            ungroup;
    }else if(option == "NODES_only"){ #First
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(ResourceId, ResourceType) %>%
            slice(1) %>%
            mutate(ResourceId = Node) %>%
            ungroup;
    }else if(option == "NODES_1_in_10"){ #First
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node) %>%
            arrange(ResourceId, ResourceType) %>%
            slice(1) %>%
            mutate(ResourceId = Node) %>%
            ungroup %>%
            mutate(Node = as.integer(as.character(Node))) %>%
            arrange(Node) %>%
            slice(seq(1, n(), 10))
    }else if(option == "ALL"){
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            ungroup;
    }else{ #First and Last
        dfw %>%
            select(Node, ResourceId, ResourceType, Position, Height) %>%
            distinct() %>%
            group_by(Node, ResourceType) %>%
            arrange(Node, ResourceId, ResourceType) %>%
            slice(c(1, n())) %>%
            ungroup;
    }
}

userYLimit <- function(obj, configuration, xlimits)
{
    if (pjr(configuration)){
        # if there is an user vertical scale defined, use it
        tpScale <- list(
            coord_cartesian(xlim=xlimits,
                            ylim=c(0, pjr(configuration)))
        );
        obj <- obj + tpScale;
    }
    return(obj);
}

outlier_definition <- function(x) {
    (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}

title_plot <- function(title = NULL)
{
    ggplot() +
        xlim(0,1) +
        ylim(0,1) +
        theme(axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.line=element_blank(),
              axis.text=element_blank()) +
        annotate("text", x = .5, y = .5, label=title, size=5);
}
