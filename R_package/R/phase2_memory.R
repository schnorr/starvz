
events_memory_chart <- function (data = NULL, globalEndTime = NULL, combined = FALSE, tstart = NULL, tend = NULL)
{
    if (is.null(data)) stop("data provided to memory_chart is NULL");

    # Get traces
    dfw <- data$Events;

    loginfo("Entry of events_memory_chart");

    memory_states = c("Allocating Async Start", "Allocating Async End", "Allocating Start", "Allocating End", "WritingBack Start", "WritingBack End", "Free Start", "Free End");
    memory_states_start = c("Allocating Async Start", "Allocating Start", "WritingBack Start", "Free Start");

    # Filter
    dfwapp = data$Events %>%
          filter(Type %in% memory_states) %>%
          group_by(Container, Handle, Tid, Src) %>% arrange(Start) %>%
          mutate(End = lead(Start)) %>%
          filter(Type %in% memory_states_start) %>%
          mutate(Duration = End-Start) %>%
          mutate(Type = gsub(" Start", "", Type))

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_events(data, dfwapp, combined=combined, tstart=tstart, tend=tend);
    if(combined){
      gow <- gow + geom_links(data, combined=TRUE, tstart=tstart, tend=tend);
    }

    loginfo("Exit of events_memory_chart");
    return(gow);
}

memory_chart <- function (data = NULL, globalEndTime = NULL, combined = FALSE, tstart = NULL, tend = NULL)
{
    if (is.null(data)) stop("data provided to memory_chart is NULL");

    # Get traces
    dfw <- data$State;

    loginfo("Entry of memory_chart");

    # Filter
    dfwapp = dfw %>%
        # Considering only application data
        filter(Application == TRUE) %>%
        # Considering only Worker State
        filter(Type == "Memory Node State");

    #Plot
    gow <- ggplot() + default_theme();

    # Add states and outliers if requested
    gow <- gow + geom_memory(data, combined=combined, tstart=tstart, tend=tend);
    if(combined){
      gow <- gow + geom_links(data, combined=TRUE, tstart=tstart, tend=tend);
    }

    #gow = gow + scale_fill_manual(values = starpu_colors());

    loginfo("Exit of memory_chart");
    return(gow);
}


link_chart <- function (data = NULL, tstart = NULL, tend = NULL)
{
  if (is.null(data)) stop("data provided to memory_chart is NULL");

  loginfo("Entry of link_chart");

  #Plot
  gow <- ggplot() + default_theme();

  # Add states and outliers if requested
  gow <- gow + geom_links(data, tstart=tstart, tend=tend);

  #gow = gow + scale_fill_manual(values = starpu_colors());

  loginfo("Exit of link_chart");
  return(gow);
}


geom_events <- function (main_data = NULL, data = NULL, combined = FALSE, tstart=NULL, tend=NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_events");


    loginfo("Starting geom_events");

    dfw <- data

    dfl <- main_data$Link;

    loginfo("Starting geom_events");

    col_pos_1 <- data.frame(Container=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position");

    col_pos_2 <- data.frame(Container=unique(dfw$Container)) %>% tibble::rowid_to_column("Position")

    if(nrow(col_pos_1)>nrow(col_pos_2)){
      col_pos <- col_pos_1
    }else{
      col_pos <- col_pos_2
      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$Container, expand=c(pjr_value(pajer$expand, 0.05),0));
    }

    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);
    dfw <- dfw %>% left_join(col_pos, by=c("Container" = "Container"))

    #col_pos <- data.frame(Container=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position")

    ret <- list();

    dfw$Height = 1;

    # Color mapping
    #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    # Hardcoded here because yconf is specific to Resources Workers
    yconfm <- dfw %>% ungroup %>%
        select(Container, Position, Height) %>%
        distinct() %>%
        group_by(Container) %>%
        arrange(Container) %>%
        ungroup;

    #yconfm$Height = pjr_value(pajer$memory$state$height, 2);
    #yconfm$Position = yconfm$Position * yconfm$Height;
    #dfw$Height = pjr_value(pajer$memory$state$height, 2);


    yconfm$Container <- lapply(yconfm$Container, function(x) gsub("MEMMANAGER", "MM", x));

    if(!combined){
      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$Container, expand=c(pjr_value(pajer$expand, 0.05),0));
    }else{
        dfw$Position = dfw$Position - 0.3;
    }

    # Y label
    ret[[length(ret)+1]] <- ylab("Memory State");


    border <- 0
    if(pjr(pajer$memory$state$border)){
        border <- 1
    }
    # Add states
    ret[[length(ret)+1]] <- geom_rect(data=dfw, aes(fill=Type, xmin=Start, xmax=End, ymin=Position, ymax=Position+(2.0-0.4-Height)), color= "black", linetype=border, size=0.4, alpha=0.5);

    dx <- dfw %>% filter(Type == "Allocating")

    if(pjr(pajer$memory$state$text)){
      ret[[length(ret)+1]] <- geom_text(data=dx, colour = "black", fontface = "bold", aes(x = Start+Duration/2, y = Position+(2.0-0.4-Height)/2, angle=90, label=substr(Handle, start = 6, stop = 12)), size = 3, alpha=1.0, show.legend = FALSE);
    }

    if(pjr(pajer$memory$state$total)){
      select <- pjr_value(pajer$memory$state$select, "Allocating");
      ms <- dfw %>% filter(Type == select, Start<tend, End>tstart) %>%
                    mutate(Start = ifelse(Start<tstart, tstart, Start)) %>%
                    mutate(End = ifelse(End>tend, tend, End))

        #Calculate selected state % in time
      total_time <- tend - tstart;

      ms <- ms %>%
          group_by (Container, Position) %>%
          summarize(percent_time = round((sum(End-Start)/total_time)*100,2));
      if(nrow(ms) != 0){
          #ms[2] <- data.frame(lapply(ms[2], as.character), stringsAsFactors=FALSE);
          #ms <- ms %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
          ms$Value <- select;
          globalEndTime <- tend - (tend-tstart) * 0.05;
          ms$percent_time <- paste0(ms$percent_time, "%");
          ret[[length(ret)+1]] <- geom_label(data=ms, x = globalEndTime, colour = "black", fontface = "bold", aes(y = Position+0.3, label=percent_time, fill = Value), alpha=1.0, show.legend = FALSE);
      }
    }

    loginfo("Finishing geom_events");

    return(ret);
}


geom_memory <- function (data = NULL, combined = FALSE, tstart=NULL, tend=NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_memory");

    dfw <- data$State %>%
        # Memory State
        filter(Type == "Memory Node State");


    loginfo("Starting geom_memory");

    col_pos_1 <- data.frame(ResourceId=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position");

    col_pos_2 <- data.frame(ResourceId=unique(dfw$ResourceId)) %>% tibble::rowid_to_column("Position")

    nrow(ms) != 0

    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);
    dfw <- dfw %>% select(-Position) %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"))
    ret <- list();

    # Color mapping
    #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    # Hardcoded here because yconf is specific to Resources Workers
    yconfm <- dfw %>%
        select(Node, ResourceId, Position, Height) %>%
        distinct() %>%
        group_by(Node) %>%
        arrange(Node, ResourceId) %>%
        ungroup;
    yconfm$Height = pjr_value(pajer$memory$state$height, 2);
    yconfm$Position = yconfm$Position * yconfm$Height;
    dfw$Height = pjr_value(pajer$memory$state$height, 2);
    dfw$Position = dfw$Position * dfw$Height;

    yconfm$ResourceId <- lapply(yconfm$ResourceId, function(x) gsub("MEMMANAGER", "MM", x));

    ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$ResourceId, expand=c(pjr_value(pajer$expand, 0.05),0));
    # Y label
    ret[[length(ret)+1]] <- ylab("Memory State");

    border <- NA;

    if(pjr(pajer$memory$state$border)){
        border <- "black";
    }
    const_depth <- 0;

    if(pjr(pajer$memory$state$depth$active)){
        const_depth <- pjr_value(pajer$memory$state$depth$height, 0.15);
    }

    dfw$Height = dfw$Depth * const_depth;

    # Add states
    ret[[length(ret)+1]] <-
        geom_rect(data=dfw, aes(fill=Value,
                                xmin=Start,
                                xmax=End,
                                ymin=Position,
                                ymax=Position+(2.0-0.1-Height)), linetype=1, color=border, size=0.4, alpha=0.5);


    if(pjr(pajer$memory$state$total)){
      select <- pjr_value(pajer$memory$state$select, "DriverCopy");
      ms <- dfw %>% filter(Value == select);

      #Calculate selected state % in time
      total_time <- tend - tstart;
      ms <- ms %>%
          group_by (ResourceId, Node, Position) %>%
          summarize(percent_time = round((sum(End-Start)/total_time)*100,2));

      #ms <- data.frame(with(ms, table(Node, ResourceId)));
      if(nrow(ms) != 0){
          #ms[2] <- data.frame(lapply(ms[2], as.character), stringsAsFactors=FALSE);
          #ms <- ms %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
          ms$Value <- select;
          globalEndTime <- tend * 1.01;
          ms$Position = ms$Position + 0.8;
          ms$percent_time <- paste0(ms$percent_time, "%");
          ret[[length(ret)+1]] <- geom_label(data=ms, x = globalEndTime, colour = "white", fontface = "bold", aes(y = Position, label=percent_time, fill = Value), alpha=1.0, show.legend = FALSE);
      }
    }

    loginfo("Finishing geom_memory");

    return(ret);
}


geom_links <- function (data = NULL, combined = FALSE, tstart=NULL, tend=NULL)
{
    if (is.null(data)) stop("data is NULL when given to geom_links");

    #Get the start info on states because link dont have nodes & Position

    dfl <- data$Link;

    loginfo("Starting geom_links");

    col_pos <- as.tibble(data.frame(ResourceId=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position"));
    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);

    if(combined){
      col_pos$Position = col_pos$Position * pjr_value(pajer$memory$state$height, 1);;
    }

    ret <- list();

    dfl <- dfl %>% left_join(col_pos, by=c("Origin" = "ResourceId")) %>%
                                  rename(O_Position = Position) %>%
                                  left_join(col_pos, by=c("Dest" = "ResourceId")) %>%
                                  rename(D_Position = Position);
    stride <- 0.3;

    dfl$Height <- 1;

    yconfm <- dfl %>%
        select(Dest, D_Position, Height) %>%
        distinct() %>%
        group_by(Dest) %>%
        arrange(Dest) %>%
        ungroup;

    yconfm$Height <- 1;
    yconfm$Dest <- lapply(yconfm$Dest, function(x) gsub("MEMMANAGER", "MM", x));

    if(combined){
      stride = stride*pjr_value(pajer$memory$state$height, 1);
      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$D_Position, labels=yconfm$Dest, expand=c(0.10, 0.1));
      stride <- 0.0;
    }

    if(!combined){

      #dfw <- dfw %>% select(-Position) %>% left_join(col_pos, by=c("ResourceId" = "ResourceId"));
      # Hardcoded here because yconf is specific to Resource Workers

      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$D_Position, labels=yconfm$Dest, expand=c(0.10,0.5));

      # Color mapping
      #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

      # Y label
      ret[[length(ret)+1]] <- ylab("Transfers");
      stride <- 0.0;
    }
    dfl$O_Position = dfl$O_Position + stride;
    dfl$D_Position = dfl$D_Position + stride;
    arrow_g <- NULL;
    if(pjr(pajer$memory$transfers$arrow)){
        arrow_g <- arrow(length = unit(0.15, "cm"));
    }
    if(pjr(pajer$memory$transfers$border)){
        ret[[length(ret)+1]] <- geom_segment(data=dfl, aes(x = Start, xend = End, y = O_Position, yend = D_Position), arrow = arrow_g, alpha=0.5, size=1.5, color = "black");
    }

    ret[[length(ret)+1]] <- geom_segment(data=dfl, aes(x = Start, xend = End, y = O_Position, yend = D_Position, color = Origin), arrow = arrow_g, alpha=1.0);
    selected_dfl <- dfl %>% filter(End > tstart) %>%  filter(Start < tend);

    #ret[[length(ret)+1]] <- geom_text(data=dfl, colour = "black", fontface = "bold", aes(x = Start, y = O_Position, label=Key), size = 3, alpha=1.0, show.legend = FALSE);
    if(pjr(pajer$memory$transfers$total)){
      total_links <- data.frame(with(selected_dfl, table(Origin)))
      if(nrow(total_links) != 0 & !combined){
          total_links[1] <- data.frame(lapply(total_links[1], as.character), stringsAsFactors=FALSE);
          total_links <- total_links %>% left_join(col_pos, by=c("Origin" = "ResourceId"));
          print(total_links)

          globalEndTime <- tend - (tend-tstart) * 0.05;

          ret[[length(ret)+1]] <- geom_label(data=total_links, x = globalEndTime, colour = "white", fontface = "bold", aes(y = Position, label=Freq, fill = Origin), alpha=1.0, show.legend = FALSE);
      }
    }
    loginfo("Finishing geom_links");

    return(ret);
}
