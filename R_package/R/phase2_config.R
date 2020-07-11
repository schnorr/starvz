pjr_value <- function (property, default)
{
  ifelse(is.null(property), default, property);
}

pjr <- function (property)
{
    ifelse(!is.null(property) && property, property, FALSE);
}

starvz_default_config <- function(){
  config <- list()
  config$base_size <- 22
  config$st <- list()
  config$st$active <- TRUE
  config$st$labels <- "ALL"
  config$st$legend <- TRUE
  config$st$makespan <- TRUE
  return(config)
}

starvz_read_config <- function(file=NULL){
  if(is.null(file)){
    loginfo("Using default config")
    return(starvz_default_config())
  }else if(!file.exists(file)){
    logwarn("Config file dont exist, using defaut")
    return(starvz_default_config())
  }else{
    config <- read_yaml(file)
    # Retrocompatible with default classes
    if(isTRUE(names(config)=="default")){
      config <- config$default
    }
    return(config)
  }
}
