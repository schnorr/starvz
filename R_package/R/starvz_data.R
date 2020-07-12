# This follows:
# https://www.datamentor.io/r-programming/s3-class/
# https://adv-r.hadley.nz/s3.html#s3-classes
new_starvz_data <- function(data = list()) {
  structure(data, class = "starvz_data")
}

validate_starvz_data <- function(data){
  data_list <- unclass(data)
  if (!is.list(data_list)) {
    stop(
      "Invalid starvz_data",
      call. = FALSE
    )
  }
  data
}

starvz_data <- function(data = list()){
  validate_starvz_data(new_starvz_data(data))
}

summary.starvz_data <- function(x){
  cat("StarVZ data\n")
  cat("Elements:\n")
  cat(paste0("\t", names(x), "\n"))
}

print.starvz_data <- function(x){
  cat("TODO");
}
