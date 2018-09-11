
microscopic_time_expanding <- function (df = NULL, variable = NULL)
{
    if (is.null(df)) return(NULL);
    if (is.null(variable)) return(NULL);

    df %>>%
    # Select only one variable
    filter(Type == variable) %>>%
    # Define all the unique breaks (as a side effect)
    (~ startUnique <- (.$Start %>% unique)) %>>%
    # Negative values might occur, if so set them to 0
    mutate (Value = ifelse(Value <= 0, 0, Value)) %>>%
    # Order by ResourceId and Start (it is already ordered, but let's make sure)
    arrange(ResourceId, Type, Start) %>%
    # Group by ResourceId and Type
    group_by(ResourceId, Type) %>>%
    # Complete with the missing rows using the unique Start timestamps as guideline
    complete(ResourceId, Start=startUnique) %>%   # This is the first most important line
    # Repeat the values for the created that have been created
    mutate(Value = na.locf(Value, na.rm = FALSE)) %>% # This is the second most important line
    #
    ungroup() %>%
    # Repeat other columns (less important)
    mutate(
        P = na.locf(P, na.rm = FALSE),
        Pipeline = na.locf(Pipeline, na.rm = FALSE),
        Type = na.locf(Type, na.rm = FALSE),
        Nature = na.locf(Nature, na.rm = FALSE),
        End = lead(Start),
        Duration = End-Start) %>%
    # Omit missing values
    filter(complete.cases(.)) %>%
    #
    # Starting per-node aggregation
    #
    group_by(Node, Type, Start, End, Duration) %>%
    summarize(Value=sum(Value)) %>%
    ungroup();
}

integrate.stepfunc.Rcpp <- function(inter,b,f) integrateStepFunc(inter,c(-Inf,b,Inf),c(0,f,0))

getBreaks <- function(dfv = NULL)
{
    if(is.null(dfv)) return(NULL);
    breaks = c(dfv$Start, dfv$End[length(dfv$End)]);
    return(breaks);
}

getSlices <- function(dfv = NULL, step = 100)
{
    tstart = dfv %>% .$Start %>% min;
    tend = (dfv %>% .$End %>% max);
    # TODO: Explain how we ignore the last slice if tend is not multiple
    # of step. This will make us ignore all behavior from the beginning
    # of that last slice until tend. This is unimportant for visualization
    # purposes but it can be important for stats. On that case, replace
    # the second argument by =tend+step= to make sure all data is considered.
    slices = seq(0, tend, step);
    return(slices);
}

remyTimeIntegration <- function(dfv = NULL, slices = NULL)
{
    if(is.null(dfv)) return(NULL);
    if(is.null(slices)) return(NULL);

    # Define breaks
    breaks = getBreaks(dfv);

    # Define values on breaks
    values = dfv$Value;

    result <- integrate.stepfunc.Rcpp(slices, breaks, values);

    return(result);
}

remyTimeIntegrationPrep <- function(dfv = NULL, myStep = 100)
{
    if (is.null(dfv)) return(NULL);
    if (nrow(dfv) == 0) return(NULL);
    mySlices <- getSlices(dfv, step = myStep);
    tibble(Slice = mySlices, Value = c(remyTimeIntegration(dfv, slices=mySlices), 0)/myStep);
}
