
metric_percent_imbalance <- function(times, max=NULL){
    max_util <- max(times)
    mean_util <- mean(times)
    pi <- ((max_util / mean_util) - 1)
    if(!is.null(max)){
      pi <- pi/((max / mean_util))
    }
    return(pi)
}

metric_imbalance_percentage  <- function(times){
    max_util <- max(times)
    mean_util <- mean(times)
    n = length(times)
    ip = ((max_util - mean_util) / max_util ) * (n/(n-1))
    return(ip)
}

metric_imbalance_time  <- function(times, max=NULL){
    max_util <- max(times)
    mean_util <- mean(times)
    n = length(times)
    it = max_util - mean_util
    if(!is.null(max)){
      it <- it/max
    }
    return(it)
}

metric_imbalance_std  <- function(times, max=NULL){
    std = sd(times)
    if(!is.null(max)){
      std <- std/max
    }
    return(std)
}

metric_imbalance_norm  <- function(times, max=NULL){
   norm <- mean(abs(times))
   if(!is.null(max)){
      norm <- norm/max
    }
   return(norm)
}

utilization_per_step <- function(data_app, step){
   if(exists("unnest_legacy")){
      unnest <- unnest_legacy
   }
   min_time <- min(data_app$Start)
   max_time <- max(data_app$End)

   data_app %>% select(JobId, Duration, Node, ResourceId, ResourceType, Start, End) %>%
            mutate(SStep = as.integer(floor(Start/step)),
                   EStep = as.integer(floor(End/step)),
                   UtilFirst = ifelse(SStep!=EStep, step-Start%%step, Duration),
                   UtilLast = End%%step) %>%
            mutate(FullUtil = mapply(function(x, y) seq(x, y, by=1), SStep, EStep) ) %>%
            unnest() %>%
            mutate(Util = case_when( (FullUtil==SStep) ~ UtilFirst,
                                     (FullUtil==EStep) ~ UtilLast,
                                     TRUE ~ step)) %>%
            rename(Step=FullUtil) %>% select(-SStep, -EStep, -UtilFirst, -UtilLast) %>%
            group_by(ResourceId, Node, ResourceType, Step) %>%
            summarize(Utilization = sum(Util)/step) %>%
            complete(ResourceId, Step=0:(max_time/step), fill = list(Utilization = 0)) %>%
            mutate(UtilizationTime = Utilization*step)
}

var_imbalance <- function(data_app, step)
{
  utilization_per_step(data_app, step) %>% ungroup() %>%
      group_by(Step) %>%
      summarize("Per. Imb."=metric_percent_imbalance(UtilizationTime, step),
          "Imb. Per."=metric_imbalance_percentage(UtilizationTime),
          "Imb. Time"=metric_imbalance_time(UtilizationTime, step),
          "Std"=metric_imbalance_std(UtilizationTime, step),
          "Norm"=metric_imbalance_norm(UtilizationTime, step)) %>%
      pivot_longer(-Step, names_to = "metric", values_to = "value") %>%
      mutate(Time = Step*step+step/2) -> to_plot

      to_plot %>% select(Step) %>% unique() %>% mutate(Step = Step*step) -> steps

      to_plot %>%
      ggplot(aes(x=Time, y=value, colour=metric)) +
      default_theme() +
      geom_point() +
      geom_vline(data=steps, aes(xintercept=Step), alpha=0.2) +
      geom_line() +
      labs(y="Imb Metric", x = "Time")
}

utilization_heatmap <- function(data_app, Y, step)
{
  utilization_per_step(data_app, step) %>%
      ungroup() %>% left_join(Y, by=c("ResourceId"="Parent")) -> to_plot

  yconfv <- yconf(to_plot %>% ungroup())

  to_plot %>% mutate(Time = Step*step+step/2) %>%
           ggplot(aes(y=Position, x=Time, fill=Utilization)) +
           geom_raster() +
           default_theme() +
           scale_y_continuous(breaks = yconfv$Position, labels=yconfv$ResourceId) +
           labs(y="Resource", x = "Time") +
           scale_fill_gradient(low = "darkred", high = "cornflowerblue")
}
