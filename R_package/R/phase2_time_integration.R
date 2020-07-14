#' @include starvz_data.R

integrate.stepfunc.Rcpp <- function(inter, b, f) integrateStepFunc(inter, c(-Inf, b, Inf), c(0, f, 0))

getBreaks <- function(dfv = NULL) {
  if (is.null(dfv)) {
    return(NULL)
  }
  breaks <- c(dfv$Start, dfv$End[length(dfv$End)])
  return(breaks)
}

getSlices <- function(dfv = NULL, step = 100) {
  tstart <- dfv %>%
    .$Start %>%
    min()
  tend <- (dfv %>% .$End %>% max())
  # TODO: Explain how we ignore the last slice if tend is not multiple
  # of step. This will make us ignore all behavior from the beginning
  # of that last slice until tend. This is unimportant for visualization
  # purposes but it can be important for stats. On that case, replace
  # the second argument by =tend+step= to make sure all data is considered.
  slices <- c(seq(0, tend, step), tend)
  return(slices)
}

remyTimeIntegration <- function(dfv = NULL, slices = NULL) {
  if (is.null(dfv)) {
    return(NULL)
  }
  if (is.null(slices)) {
    return(NULL)
  }

  # Define breaks
  breaks <- getBreaks(dfv)

  # Define values on breaks
  values <- dfv$Value

  result <- integrate.stepfunc.Rcpp(slices, breaks, values)

  return(result)
}

remyTimeIntegrationPrep <- function(dfv = NULL, myStep = 100) {
  if (is.null(dfv)) {
    return(NULL)
  }
  if (nrow(dfv) == 0) {
    return(NULL)
  }
  mySlices <- getSlices(dfv, step = myStep)
  tibble(Slice = mySlices, Value = c(remyTimeIntegration(dfv, slices = mySlices), 0) / myStep)
}
