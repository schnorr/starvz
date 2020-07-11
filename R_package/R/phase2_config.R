pjr_value <- function (property, default)
{
  ifelse(is.null(property), default, property);
}

pjr <- function (property)
{
    ifelse(!is.null(property) && property, property, FALSE);
}

default_config <- function(){
  print("TODO")
}
