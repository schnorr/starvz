
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

    loginfo("Entry of memory_chart");

    # Filter
    dfwapp = data$Application %>%
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

    col_pos_1 <- data.frame(Container=unique(dfl$Dest)) %>% arrange(Container) %>% tibble::rowid_to_column("Position");

    col_pos_2 <- data.frame(Container=unique(dfw$Container)) %>% arrange(Container) %>% tibble::rowid_to_column("Position")

    if(nrow(col_pos_1)>nrow(col_pos_2)){
      col_pos <- col_pos_1
    }else{
      col_pos <- col_pos_2
      #ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$Container, expand=c(pjr_value(pajer$expand, 0.05),0));
    }

    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);
    dfw <- dfw %>% left_join(col_pos, by=c("Container" = "Container"))

    #col_pos <- data.frame(Container=unique(dfl$Dest)) %>% tibble::rowid_to_column("Position")

    ret <- list();

    dfw$Height = 1;

    # This fixes some problems on recent versions of tidyverse
    # Check: https://github.com/tidyverse/tidyr/issues/751
    # Check: https://github.com/tidyverse/tidyr/issues/694
    if(exists("unnest_legacy")){
      unnest <- unnest_legacy
    }

    # Color mapping
    #ret[[length(ret)+1]] <- scale_fill_manual(values = extract_colors(dfw));

    # Y axis breaks and their labels
    # Hardcoded here because yconf is specific to Resources Workers
    yconfm <- dfw %>% unnest() %>% ungroup %>%
        select(Container, Position, Height) %>%
        distinct() %>%
        group_by(Container) %>%
        arrange(Container) %>%
        ungroup;

    #yconfm$Height = pjr_value(pajer$memory$state$height, 2);
    #yconfm$Position = yconfm$Position * yconfm$Height;
    #dfw$Height = pjr_value(pajer$memory$state$height, 2);


    #yconfm$Container <- lapply(yconfm$Container, function(x) gsub("MEMMANAGER", "MM", x));
    #yconfm$Container <- c("RAM", "GPU1", "GPU2", "DISK")
    yconfm <- yconfm %>% unnest() %>% arrange(Container)

    if(!combined){
      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$Container, expand=c(pjr_value(pajer$expand, 0.05),0));
    }else{
        dfw$Position = dfw$Position - 0.3;
    }

    # Y label
    ret[[length(ret)+1]] <- ylab("Mem Managers");


    border <- 0
    if(pjr(pajer$memory$state$border)){
        border <- 1
    }
    # Add states
    ret[[length(ret)+1]] <- geom_rect(data=dfw, aes(fill=Type, xmin=Start, xmax=End, ymin=Position, ymax=Position+(2.0-0.2-Height)), color= "black", linetype=border, size=0.4, alpha=0.5);



    if(pjr(pajer$memory$state$text)){

	dx <- dfw %>% filter(Type == "Allocating") %>%
                  left_join(main_data$Data_handles, by=c("Handle"="Handle") ) %>%
                  select(-Tid, -Src, -Value)
	dx$Coordinates <- gsub(" ", "x", dx$Coordinates)

      ret[[length(ret)+1]] <- geom_text(data=dx, colour = "black", fontface = "bold", aes(x = Start+Duration/2, y = Position+(2.0-0.2-Height)/2, label=Coordinates), size = 5, alpha=1.0, angle=pjr_value(pajer$memory$state$angle, 90), show.legend = FALSE);
    }

    ret[[length(ret)+1]] <- theme (
            legend.spacing.x = unit(2, 'mm'))

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
          ret[[length(ret)+1]] <- geom_label(data=ms, x = globalEndTime, colour = "black", fontface = "bold", aes(y = Position+0.4, label=percent_time, fill = Value), alpha=1.0, show.legend = FALSE, size = 5);
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


    loginfo("Starting geom_memory2");

    dfl <- data$Link;

    col_pos_1 <- data.frame(Container=unique(dfl$Dest)) %>% arrange(Container) %>% tibble::rowid_to_column("Position");

    col_pos_2 <- data.frame(Container=unique(dfw$Container)) %>% arrange(Container) %>% tibble::rowid_to_column("Position")

    if(nrow(col_pos_1)>nrow(col_pos_2)){
      col_pos <- col_pos_1
    }else{
      col_pos <- col_pos_2
      #ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$Position+(yconfm$Height/3), labels=yconfm$Container, expand=c(pjr_value(pajer$expand, 0.05),0));
    }


    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);
    dfw <- dfw %>% select(-Position) %>% left_join(col_pos, by=c("ResourceId" = "Container"))
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

    #Calculate selected state % in time

    if(is.null(tend) || is.null(tstart)){
        total_time <- 0;
    }else{
        total_time <- tend - tstart;
    }

    if(pjr_value(pajer$memory$state$total, FALSE) & (total_time>0)){
      select <- pjr_value(pajer$memory$state$select, "DriverCopy");
      ms <- dfw %>% filter(Value == select);
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

    col_pos <- as.tibble(data.frame(ResourceId=unique(dfl$Dest)) %>% arrange(ResourceId) %>% tibble::rowid_to_column("Position"));
    col_pos[2] <- data.frame(lapply(col_pos[2], as.character), stringsAsFactors=FALSE);

    print(col_pos)

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
        select(Origin, O_Position, Height) %>%
        distinct() %>%
        group_by(Origin) %>%
        arrange(Origin) %>%
        ungroup;

    yconfm$Height <- 1;
    yconfm$Origin <- lapply(yconfm$Origin, function(x) gsub("MEMMANAGER", "MM", x));

    if(combined){
      stride = stride*pjr_value(pajer$memory$state$height, 1);
      ret[[length(ret)+1]] <- scale_y_continuous(breaks = yconfm$O_Position, labels=yconfm$Origin, expand=c(0.10, 0.1));
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



handles_presence_states <- function(data){
	# Selecting only the data state events
	data$Events %>% filter(Type=="data state invalid" |
		               Type=="data state owner"   |
		               Type=="data state shared") -> data_states

	# Getting the data handles by its coordinates
	data$Data_handles %>% separate(Coordinates, c("X", "Y")) -> cords

	# Getting all events of the handles
	data_states %>% filter(Value %in% cords$Handle) %>% select(Container, Start, Type, Value) -> data_state_events

	end <- max(data$State$End)

	fini_end <- unlist(end)

	data_state_events %>% group_by(Value, Container) %>%
		          mutate(rep = case_when(Type=="data state owner" ~ 1,
		                                 Type=="data state invalid" ~ 2,
		                                 TRUE ~ 5)) %>%
		          mutate(flow = c(1, diff(rep)), t_diff = c(diff(Start), 1)) %>%
		          mutate(need = flow!=0 & t_diff>0.001) %>%
		          filter(need == TRUE) %>%
		          mutate(flow = c(1, diff(rep))) %>%
		          mutate(need = flow!=0) %>%
		          filter(need == TRUE) %>%
		          mutate(End = lead(Start, default=unlist(fini_end))) %>%
		          filter(Type != "data state invalid") %>%
		          select(-rep, -flow, -t_diff, -need) %>%
		          ungroup() %>%
		          group_by(Value, Container) -> f_data

	return(f_data)
}

pre_handle_gantt <- function(data){

	data$Events <- data$Events %>% mutate(Type=case_when(Type=="Allocating Start" ~ "Allocation Request",
                                    Type=="Request Created" ~ "Transfer Request",
                                    TRUE ~ Type))

	if(is.null(data$handle_states)){
		data$handle_states <- handles_presence_states(data)
	}

	# Processing the States: Handles & Tasks
	data$Data_handle$Coordinates <- gsub(" ", "x", data$Data_handle$Coordinates)

	position <- data$handle_states %>% ungroup() %>%
                    select(Container) %>%
                    distinct() %>%
                    arrange(Container) %>%
	            mutate(y1 = 1:n())

	p_data <- data$handle_states %>%
		  mutate(Colour = ifelse(Type=="data state owner", "Owner", "Shared") ) %>%
                  inner_join(position, by=c("Container"="Container")) %>%
                  select(Container, Start, End, Value, y1, Colour)


	p_data %>% select(Container, Value, y1) %>% distinct() -> pre_p_data

	data$Task_handles %>% filter(!is.na(JobId)) %>%
                  inner_join(data$Tasks %>% filter(!is.na(JobId)), by=c("JobId"="JobId")) %>%
		  select(JobId, Handles, MPIRank, MemoryNode) %>%
                  mutate(sContainer = paste0(MPIRank,"_MEMMANAGER",MemoryNode) ) -> job_handles

	job_handles %>% inner_join(pre_p_data, by=c("Handles"="Value")) %>%
		              select(Container, sContainer, JobId, Handles, y1, MemoryNode) %>%
		              filter(sContainer==Container) %>%
		              mutate(JobId=as.character(JobId)) %>%
		              inner_join(data$State, by=c("JobId"="JobId")) %>%
		              select(Container, JobId, Handles, Start, End, y1, Value) %>%
		              rename(Colour=Value) %>%
		              rename(Value=Handles) -> jobs_p_data
	p_data$size <- 0.8
	jobs_p_data$size <- 0.6

	all_st_m_data <- bind_rows(p_data, jobs_p_data) %>%
		         inner_join(data$Data_handle, by=c("Value" = "Handle") ) %>%
		         ungroup() %>%
		         mutate(Value = paste0("Memory Block ", Coordinates, "")) %>%
		         select(Container, Start, End, Value, y1, Colour, size, JobId) %>%
		         group_by(Value, Container)

	# Processing the Events: Request & Allocation
	data$Events %>%  filter(Type=="Transfer Request") %>%
		         mutate(P = substring(Tid, 5)) %>%
                     mutate(G = substr(Container,1,nchar(Container)-1)) %>%
		         mutate(Container=paste0(G, P)) %>% select(-P, -G) %>%
		         select(-Nature, -Tid) %>%
		         inner_join(data$Data_handle, by=c("Handle" = "Handle") ) %>%
		         inner_join(position, by=c("Container"="Container")) %>%
		         mutate(Value = paste0("Memory Block ", Coordinates, "")) %>%
		         select(Container, Type, Start, Value, Info, y1) %>%
		         filter(Type=="Transfer Request") %>%
		         mutate(Pre = as.character(Info)) -> request_events

	data$Task_handles %>% select(Handles) %>% distinct() %>% .$Handles -> h_used
	data$Events %>%  filter(Handle %in% h_used) %>% select(-Nature, -Tid) %>%
		         inner_join(data$Data_handle, by=c("Handle" = "Handle") ) %>%
		         inner_join(position, by=c("Container"="Container")) %>%
		         mutate(Value = paste0("Memory Block ", Coordinates, "")) %>%
		         select(Container, Type, Start, Value, Info, y1) %>%
		         filter(Type=="Allocation Request") -> allocation_events

	allocation_events  %>% group_by(Value, Container, Type) %>%
		           mutate(Old = lag(Start, default=-5), R=abs(Start-Old)) %>%
		           filter(R>1) %>% select(-Old,-R)  -> allocation_events_filtered

	allocation_events_filtered$Pre <- "0"

	events_points <- bind_rows(request_events, allocation_events_filtered)

	# Processing Links (Transfers)
	data$Events %>% filter(Type=="DriverCopy Start") %>%
		        select(Handle, Info, Container) -> links_handles

	mpi_links <- data$Link %>% filter(Type=="MPI communication") %>%
			select(-Nature, -Container, -Size) %>%
                 	mutate(Origin=str_replace(Origin, "mpict", "MEMMANAGER0")) %>%
                 	mutate(Dest=str_replace(Dest, "mpict", "MEMMANAGER0")) %>%
			inner_join(position, by=c("Origin"="Container")) %>%
			rename(origin_y = y1) %>%
			inner_join(position, by=c("Dest"="Container")) %>%
			rename(dest_y = y1) %>%
                    mutate(Tag = as.numeric(Tag)) %>%
			inner_join(data$Data_handle, by=c("Tag" = "MPITag")) %>%
			mutate(Value = paste0("Memory Block ", Coordinates, "")) %>%
			select(Type, Start, End, Value, origin_y, dest_y) %>%
			rename(Transfer = Type)

	links <- data$Link %>% filter(Type=="Intra-node data Fetch" |
		                      Type=="Intra-node data PreFetch") %>%
		      select(-Nature, -Container, -Size) %>%
		      mutate(Con = as.integer(substring(Key, 5))) %>%
		      select(-Key)

	final_links <- links %>% inner_join(links_handles, by=c("Con"="Info", "Dest"="Container")) %>%
		 inner_join(position, by=c("Origin"="Container")) %>%
		 rename(origin_y = y1) %>%
		 inner_join(position, by=c("Dest"="Container")) %>%
		 rename(dest_y = y1) %>%
		 inner_join(data$Data_handle, by=c("Handle" = "Handle") ) %>%
		 mutate(Value = paste0("Memory Block ", Coordinates, "")) %>%
		 select(Type, Start, End, Value, origin_y, dest_y) %>%
		 rename(Transfer = Type)

        all_links <- bind_rows(mpi_links, final_links)

	return(list(all_st_m_data = all_st_m_data,
		    events_points = events_points,
		    final_links = all_links,
                    position = position))

}

handles_gantt <- function(data, JobId=NA, lines=NA, lHandle=NA){

	if(is.null(data$handle_gantt_data)){
		data$handle_gantt_data <- pre_handle_gantt(data)
	}

	if(is.na(JobId) && is.na(lHandle)){
	     final_st_data <- data$handle_gantt_data$all_st_m_data
	     final_events_data <- data$handle_gantt_data$events_points
	     final_links_data <- data$handle_gantt_data$final_links
	}else if(!is.na(lHandle)){
	     final_st_data <- data$handle_gantt_data$all_st_m_data %>% filter(Value %in% lHandle)
	     print(final_st_data)
	     final_events_data <- data$handle_gantt_data$events_points  %>% filter(Value %in% lHandle)
	     final_links_data <- data$handle_gantt_data$final_links %>% filter(Value %in% lHandle)
	}else{
	      data$Data_handle$Coordinates <- gsub(" ", "x", data$Data_handle$Coordinates)
	      myjobid = JobId
	      data$Task_handles %>% filter(JobId==myjobid) %>%
			  inner_join(data$Data_handle, by=c("Handles" = "Handle") ) %>%
			  ungroup() %>%
			  mutate(Value = paste0("Memory Block ", Coordinates, "")) %>%
			  .$Value -> selected_handles

	     final_st_data <- data$handle_gantt_data$all_st_m_data %>% filter(Value %in% selected_handles)
	     final_events_data <- data$handle_gantt_data$events_points  %>% filter(Value %in% selected_handles)
	     final_links_data <- data$handle_gantt_data$final_links %>% filter(Value %in% selected_handles)
	}

	events_colors <- brewer.pal(n = 6, name = "Dark2")

	fills <- c("Owner" = "darksalmon",
		    "Shared" = "steelblue1",
		    " " = "white",
		    "dpotrf" = "#e41a1c",
		    "dtrsm" = "#377eb8",
		    "dsyrk" = "#984ea3",
		    "dgemm" = "#4daf4a",
		    "dplgsy" = "yellow")

	colors <- c("Allocation Request" = events_colors[[1]],
		    "Transfer Request" = events_colors[[2]],
		    "Intra-node data Fetch" = events_colors[[3]],
		    "Intra-node data PreFetch" = events_colors[[4]],
		    "MPI communication" = events_colors[[5]],
		    "Last Job on same Worker" = events_colors[[6]]
		    )

	arrow_g <- arrow(length = unit(0.1, "cm"));

	p <- ggplot(data = final_st_data) + theme_bw(base_size=16) +
	     geom_point(data = final_events_data,
		        aes(x=Start,
		            y=y1+0.4,
		            colour=Type,
		            shape=Pre),
		        size=2.5, stroke = 1) +
	     geom_rect(aes(xmin=Start,
		           xmax=End,
		           fill=Colour,
		           ymin=y1+ifelse(is.na(JobId), 0, 0.2),
		           ymax=y1+size
		           ),
		       colour="black",
		       size=0.1
		       ) +
	     scale_fill_manual(name = "State         Task", values = fills,
		               drop = FALSE,
		               limits=c( "Owner", "Shared", " ", "dpotrf", "dtrsm", "dsyrk", "dgemm", "dplgsy" ),
		               guide = guide_legend(nrow=3, title.position = "top", order=1,
		                       override.aes =
		                        list(shape = NA, colour=NA)
		               )) +
	     scale_colour_manual(name = "Event", values = colors,
		                 drop = FALSE,
		                 breaks=c("Allocation Request",
		                          "Transfer Request",
		                          "Intra-node data Fetch",
		                          "Intra-node data PreFetch",
		                          "MPI communication"),
		                 limits=c("Allocation Request",
		                          "Transfer Request",
		                          "Intra-node data Fetch",
		                          "Intra-node data PreFetch",
		                          "MPI communication"),
		               guide = guide_legend(nrow=5,title.position = "top", order=0,
		                       override.aes =
		                        list(arrow = NA, linetype = 0, shape=c(19, 19, 15, 15, 15),
		                             yintercept=NA )
		               )) +

	     scale_shape_manual(name = "Event Type", labels=c("Fetch", "Prefetch", "Idle Fetch"), values=c(19, 21, 23),
		                guide = guide_legend(nrow=3,title.position = "top") )+
	     #Arrow Border
	     geom_segment(data = final_links_data,
		          aes(x = Start,
		              xend = End,
		              y = origin_y+0.4,
		              yend = dest_y+0.4),
		          arrow = arrow_g,
		          colour="black",
		          alpha=0.8,
		          size=1.2) +

	     geom_segment(data = final_links_data,
		          aes(x = Start,
		              xend = End,
		              y = origin_y+0.4,
		              yend = dest_y+0.4,
		              colour=Transfer),
		          arrow = arrow_g,
		          size=0.6, show.legend = FALSE) +
	    geom_segment(data = final_links_data,
		          aes(x = Start,
		              xend = End,
		              y = origin_y+0.4,
		              yend = dest_y+0.4,
		              colour=Transfer),
		          size=0.6) +
	     scale_y_continuous(breaks=data$handle_gantt_data$position$y1 + 0.4, labels=data$handle_gantt_data$position$Container) +
	     #geom_segment(data=handle_end_m,
	     #             aes(x = End, y = MemoryNode+1, xend = End, yend = MemoryNode+1.8), color = "red") +
	     facet_wrap(Value ~ ., strip.position="top", ncol=1 ) +
	     scale_x_continuous(expand=c(0,0),
	     #breaks = c(5000, 5185, 5486, 5600, 5676, 5900),
	     labels = function(x) format(x, big.mark = "",  scientific = FALSE)) +
	     #coord_cartesian(xlim=c(5000, 6000)) +
	     #scale_color_manual(values=c("red"="red", "blue"="blue")) +
	     #scale_colour_identity() +
	     theme(strip.text.y = element_text(angle = 0),
		   legend.box.margin=margin(-10,-10,-16,-10),
		   legend.background = element_rect(fill="transparent"),
		   legend.position="top") +
	     labs(x="Time [ms]", y="Memory Manager")


	if(!is.na(lines)){
	   p <- p + geom_vline(data=lines, aes(xintercept=x, color=colors), alpha=0.7, size=1)
	}
	#if(!is.na(JobId)){
	#   my_job <- JobId
	#   data$State %>% filter(JobId==my_job) %>% .$Start -> job_start
	#   data$State %>% filter(JobId==my_job) %>% .$Duration -> job_dur
	#   data$Tasks %>% filter(JobId==my_job) %>% .$ MemoryNode -> job_node
	#   text <- data.frame(x=c(job_start+job_dur/2), y=c(job_node+1.4), text=c(my_job))
	#   p <- p + geom_text(data=text, aes(x=x, y=y, label=my_job), color="black", size=2,
        #		  fontface="bold",
	#	  alpha=0.8)
	#}
	return(p)

}


pre_snap <- function(data, f_data){

	# This fixes some problems on recent versions of tidyverse
	# Check: https://github.com/tidyverse/tidyr/issues/751
	# Check: https://github.com/tidyverse/tidyr/issues/694
	if(exists("unnest_legacy")){
	  unnest <- unnest_legacy
	}

	data$Data_handles %>%
		  separate(Coordinates, c("Y", "X")) %>%
		  mutate(X=as.numeric(X), Y=as.numeric(Y)) -> new_handles


	new_handles %>% select(Handle, X, Y) -> hand

	f_data %>% ungroup() %>% select(Container) %>% distinct() %>% .$Container -> cont
	hand <- hand %>% mutate(Container=list(cont))
	hand %>% unnest() -> hand

	f_data %>% mutate(st = ifelse(Type=="data state owner", "Owner", "Shared")) -> d_presence

	data$State %>% mutate(JobId=JobId) %>%
		       inner_join(data$Tasks, by=c("JobId"="JobId")) %>%
		       select(Start, End, Value, JobId, MemoryNode, MPIRank, Color) %>%
		       inner_join(data$Task_handles, by=c("JobId"="JobId")) %>%
		       mutate(Container = ifelse(MPIRank>=0, paste0(MPIRank, "_MEMMANAGER", MemoryNode), paste0("MEMMANAGER", MemoryNode))) %>%
		       select(Handles, Modes, Start, End, Value, JobId, Container, Color) -> tasks

	return(list(d_presence, hand, tasks))
}

memory_snap <- function(data, selected_time, step, tasks_size=30){

	data[[3]] %>% select(Value, Color) %>% distinct() -> c_info

	colors <- c("darksalmon","steelblue1")

	colors <- c(colors, c_info$Color)

	c_names <- c("Owner", "Shared")
	c_names <- c(c_names, c_info$Value)

	data[[1]] %>% filter(Start < selected_time, End > selected_time) %>%
		      right_join(data[[2]], by=c("Value" = "Handle", "Container"="Container")) -> d_presence

	task_presence <- data[[3]] %>% filter(Start <= selected_time, End >= selected_time) %>%
		         inner_join(data[[2]], by=c("Handles" = "Handle", "Container"="Container"))

	task_presence_alpha <- data[[3]] %>% filter(Start > selected_time-step, End <= selected_time) %>%
		         inner_join(data[[2]], by=c("Handles" = "Handle", "Container"="Container"))

	max_x <- data[[2]] %>% arrange(-X) %>% slice(1) %>% .$X %>% unlist()

	p <- ggplot(d_presence, aes(Y, X)) +
	    geom_tile(aes(fill = st),
		      colour = "white") +
	    geom_point(data = task_presence_alpha,
		      aes(fill = Value,
		          x=Y,
		          y=X,
		          shape=Modes),
		      colour = "black",
		      size=(tasks_size/max_x), stroke = 0.2, alpha=0.3) +
	    geom_point(data = task_presence,
		      aes(fill = Value,
		          x=Y,
		          y=X,
		          shape=Modes),
		      colour = "black",
		      size=(tasks_size/max_x), stroke = 0.2) +

	    scale_shape_manual(values=c("R"=21, "W"=22, "RW"=22), drop = FALSE,
		               limits = c("R", "RW"),
		               guide = guide_legend(title.position = "top") ) +
	    scale_fill_manual(name = "State", values = colors, drop = FALSE,
		              limits = c_names,
		              guide = guide_legend(title.position = "top", override.aes =
		                      list(shape = NA, stroke=1
		                          )) ) +
	    scale_y_reverse(limits=c(max_x+0.6, -0.6), expand=c(0,0) ) +
	    scale_x_continuous(limits=c(-0.6, max_x+0.6), expand=c(0,0) ) +
	    facet_wrap(~ Container) +
	    labs(x="Block X Coordinate", y="Block Y Coordinate") +
	    theme_bw(base_size=16) +
	    theme(legend.position="top",
		  plot.margin = unit(c(0, 10, 0, 0), "mm"),
		  legend.box.margin=margin(-5, 0, -16, 0),
		  strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
		  legend.background = element_rect(fill="transparent"),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank(),
		  panel.spacing = unit(1, "mm"))

	return(p)
}

multiple_snaps <- function(snap_data, start, end, step, path){
	  se <- seq(start, end, step)
	  se <- c(se, end)
	  i <- 1
	  for(time in se) {
	    p <- memory_snap(snap_data, time, step, tasks_size=40)
	    p <- p + ggtitle(paste0("Time: ",as.character(time)))
	    ggsave(paste0(path, i, ".png"), plot = p, scale=4, width=4, height=3, unit="cm")
	    i <- i+1
	  }
}

handles_help <- function(){
	print("To accelerate the process:")
	print("data$handle_states <- handles_presence_states(data)")
	print("data$handle_gantt_data <- pre_handle_gantt(data)")
	print("To Select time:")
	print("handles_gantt(data, JobId=c(jobid)) + coord_cartesian(xlim=c(start, end))")
	print("snap_data <- pre_snap(data, data$handle_states)")
	print("memory_snap(snap_data, 1000, tasks_size=200, step=1)")
}
