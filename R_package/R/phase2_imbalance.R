
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

metric_power_percent_imbalance <- function(times, max=NULL){
    max_util <- max(times)
    mean_util <- mean(times)
    pi <- ((max_util / mean_util) - 1)
    if(!is.null(max)){
      pi <- pi/((max / mean_util))
    }
    return(pi)
}

metric_power_imbalance_percentage  <- function(utilization, power){
     max_util <- max(utilization)
     tp <- sum(power)
     max_r <- which(utilization==max(utilization))
     norm <- sum(abs(utilization*power))
     norm <- norm/sum(power)
     it <- max_util - norm
     c1 <- it/max_util
     c2 <- tp/(tp-power[max_r])
    ip <- c1 * c2
    return(ip)
}

metric_power_imbalance_time  <- function(Utilization, power){
   max_util <- max(Utilization)

   norm <- sum(abs(Utilization*power))
   norm <- norm/sum(power)
   it <- max_util - norm
    return(it)
}

metric_power_imbalance_std  <- function(utilization){
    std = sd(utilization)
    return(std)
}

metric_power_imbalance_norm  <- function(utilization, power){
   norm <- sum(abs(utilization*power))
   norm <- norm/sum(power)
   return(norm)
}

metric_abe_imbalance_percentage  <- function(utilization, ABE, nmABE, step){
    max_util <- max(utilization)
    norm <- ABE/step
    it <- max_util - norm
    c1 <- it/max_util
    c2 <- nmABE/ABE
    ip <- c1 * c2
    return(ip)
}

metric_abe_imbalance_time  <- function(Utilization, ABE, step){
   max_util <- max(Utilization)

   norm <- ABE/step

   it <- max_util - norm
    return(it)
}

metric_abe_imbalance_std  <- function(utilization){
    std = sd(utilization)
    return(std)
}

metric_abe_imbalance_norm  <- function(ABE, step){
   norm <- ABE/step
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
            summarize(Utilization = sum(Util)/step, .groups="drop") %>%
            complete(ResourceId, Step=0:(max_time/step), fill = list(Utilization = 0)) %>%
            mutate(UtilizationTime = Utilization*step)
}

var_imbalance <- function(data_app, step)
{
  utilization_per_step(data_app, step) %>% ungroup() %>%
      group_by(Step) %>%
      summarize(
          "Imb. Per."=metric_imbalance_percentage(UtilizationTime),
          "Imb. Time"=metric_imbalance_time(UtilizationTime, step),
          "Std"=metric_imbalance_std(UtilizationTime, step),
          "Norm"=metric_imbalance_norm(UtilizationTime, step)) %>%
      pivot_longer(-Step, names_to = "metric", values_to = "value") %>%
      mutate(Time = Step*step+step/2) -> to_plot

      to_plot %>% var_imbalance_plot("Imb Metric", step)
}

var_imbalance_power <- function(data_app, step)
{
  # Compute POWER
  task <- pjr_value(pajer$power_imbalance$task, NULL)
  if(is.null(task)){
    logwarn("Task is not available for imbalance power")
    return(NULL)
  }
  data_app %>% filter(Value==task) %>%
       group_by(Node, ResourceId) %>%
       summarize(power=1/mean(Duration), .groups="drop") %>%
       .$power -> power

  if(length(power) !=
     data_app %>% select(Node, ResourceId) %>% unique() %>% nrow())
  {
       logwarn("Power could not be computed for all resource in imbalance power")
       return(NULL)
  }

  utilization_per_step(data_app, step) %>% ungroup() %>%
      group_by(Step) %>%
      summarize(
            "Imb. Per."=metric_power_imbalance_percentage(Utilization, power),
            "Imb. Time"=metric_power_imbalance_time(Utilization, power),
            "Std"=metric_power_imbalance_std(Utilization),
            "Norm"=metric_power_imbalance_norm(Utilization, power)) %>%
      pivot_longer(-Step, names_to = "metric", values_to = "value") %>%
      mutate(Time = Step*step+step/2) -> to_plot

      to_plot %>% var_imbalance_plot("Imb Metric Power", step)
}

var_imbalance_double_hetero <- function(data_app, step)
{
  utilization_per_step_double_hetero(step, data_app) %>%
      group_by(Step) %>%
      summarize(#"Per. Imb."=metric_percent_imbalance(UtilizationTime, step),
          "Imb. Per."=metric_abe_imbalance_percentage(Utilization, ABE, nmABE, step),
          "Imb. Time"=metric_abe_imbalance_time(Utilization, ABE, step),
          "Std"=metric_abe_imbalance_std(Utilization),
          "Norm"=metric_abe_imbalance_norm(ABE, step)) %>%
      pivot_longer(-Step, names_to = "metric", values_to = "value") %>%
      mutate(Time = Step*step+step/2) -> to_plot

      to_plot %>% var_imbalance_plot("Imb Metric Hete", step)
}

var_imbalance_plot <- function(data, name, step)
{
  data %>% select(Step) %>% unique() %>% mutate(Step = Step*step) -> steps
  data %>% ggplot(aes(x=Time, y=value, colour=metric)) +
  default_theme() +
  geom_point() +
  geom_vline(data=steps, aes(xintercept=Step), alpha=0.2) +
  geom_line() +
  theme(panel.grid.major.y = element_line(color = "grey80")) +
  labs(y=name, x = "Step") +
  ylim(0, 1)
}

# Consider heterogenery tasks and resources
utilization_per_step_double_hetero <- function(step, df){
    if(exists("unnest_legacy")){
       unnest <- unnest_legacy
    }
    max_time <- max(df$End)

    df %>%
        select(JobId, Value, Duration, Node, ResourceId, ResourceType, Start, End) %>%
        mutate(SStep = as.integer(floor(Start/step)),
               EStep = as.integer(floor(End/step)),
               UtilFirst = ifelse(SStep!=EStep, step-Start%%step, Duration),
               UtilLast = End%%step) %>%
        mutate(FullUtil = mapply(function(x, y) seq(x, y, by=1), SStep, EStep) ) %>%
        unnest() %>%
        mutate(Util = case_when( (FullUtil==SStep) ~ UtilFirst,
        (FullUtil==EStep) ~ UtilLast,
        TRUE ~ step)) %>%
        rename(Step=FullUtil) %>% select(-SStep, -EStep, -UtilFirst, -UtilLast) -> temp

    temp %>%
        group_by(ResourceId, Node, ResourceType, Step) %>%
        mutate(PTask = Util/Duration) %>%
        ungroup() %>%
        group_by(Node, Value, Step) %>%
        summarize(NTasks = sum(PTask), .groups="drop") -> tasks_per_slice

    temp %>%
        group_by(ResourceId, Node, ResourceType, Step) %>%
        summarize(Utilization = sum(Util)/step, .groups="drop") %>%
        complete(ResourceId, Step=0:(max_time/step), fill = list(Utilization = 0)) %>%
        mutate(UtilizationTime = Utilization*step) -> util

    util %>% group_by(Step) %>% arrange(-Utilization) %>%
        slice(1) %>% select(Step, ResourceType) -> max_res

    tasks_per_slice %>% rename(freq=NTasks) -> ts
    df %>% select(ResourceType, ResourceId) %>%
        distinct() %>%
        group_by(ResourceType) %>%
        mutate(n=n()) %>%
        select(ResourceType, n) %>%
        distinct() -> n_resources

    df %>% select(Value, ResourceType, Duration, Outlier) %>%
        ungroup() %>% rename(codelet=Value) %>%
        filter(Outlier==FALSE) %>%
        group_by(ResourceType, codelet) %>%
        summarize(mean=mean(Duration), .groups="drop") %>%
        left_join(n_resources, by=c("ResourceType")) -> ri

    ts %>% select(Step) %>%
        unique() %>% rowwise() %>%
        mutate(ABE = starpu_apply_abe_per_slice(Step, ri, ts)) %>%
        mutate(nmABE = starpu_apply_abe_per_slice(Step, ri, ts, max_res)) %>%
        ungroup() -> ABE_steps

    util %>% left_join(ABE_steps, by=c("Step")) -> ret
    return(ret)
}

utilization_heatmap <- function(data_app, Y, step)
{
  utilization_per_step(data_app, step) %>%
      ungroup() %>% left_join(Y, by=c("ResourceId"="Parent")) -> to_plot

  yconfv <- yconf(to_plot %>% ungroup(), pjr_value(pajer$st$labels, "1"))

  to_plot %>% mutate(Time = Step*step+step/2) %>%
           ggplot(aes(y=Position, x=Time, fill=Utilization)) +
           geom_raster() +
           default_theme() +
           scale_y_continuous(breaks = yconfv$Position, labels=yconfv$ResourceId, expand=c(pjr_value(pajer$st$expand, 0.05),0)) +
           labs(y="Resource Utilization", x = "Time") +
           scale_fill_gradient(low = "cornflowerblue", high = "darkred")
}
