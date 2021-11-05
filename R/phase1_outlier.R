#' @include starvz_data.R
NULL

outlier_definition <- function(x) {
  (quantile(x)["75%"] + (quantile(x)["75%"] - quantile(x)["25%"]) * 1.5)
}

#' @importFrom rlang quo_name
#' @importFrom rlang :=
bonferroni_regression_based_outlier_detection <- function(Application, task_model, column_name) {
  if (!requireNamespace("car", quietly = TRUE)) {
    starvz_warn("The bonferroni regression-based outlier test needs the suggested package car (that is not installed). Falling back to normal regression")
    return(regression_based_outlier_detection(Application, task_model, column_name))
  }
  column_name <- paste0("Outlier", column_name)

  # Step 1: apply the model to each task, considering the ResourceType
  df.pre.outliers <- Application %>%
    filter(grepl("qrt", .data$Value)) %>%
    # cannot have zero gflops
    filter(.data$GFlop > 0) %>%
    unique() %>%
    group_by(.data$ResourceType, .data$Value, .data$Cluster) %>%
    nest() %>%
    mutate(model = map(.data$data, task_model)) %>%
    mutate(Residual = map(.data$model, resid)) %>%
    mutate(outliers = map(.data$model, function(m) {
      tibble(Row = names(car::outlierTest(m, n.max = Inf)$rstudent))
    }))

  # Step 1.1: Check if any anomaly was detected
  if (df.pre.outliers %>% nrow() > 0) {

    # Step 2: identify outliers rows
    df.pre.outliers %>%
      select(-.data$Residual) %>%
      unnest(cols = c(.data$outliers)) %>%
      mutate(!!quo_name(column_name) := TRUE, Row = as.integer(.data$Row)) %>%
      ungroup() -> df.pos.outliers

    # Step 3: unnest all data and tag create the Outiler field according to the Row value
    df.pre.outliers %>%
      unnest(cols = c(.data$data, .data$Residual)) %>%
      # this must be identical to the grouping used in the step 1
      group_by(.data$Value, .data$ResourceType, .data$Cluster) %>%
      mutate(Row = 1:n()) %>%
      ungroup() %>%
      # the left join must be by exactly the same as the grouping + Row
      left_join(df.pos.outliers, by = c("Value", "Row", "ResourceType")) %>%
      # same as mutate(Outlier = ifelse(is.na(Outlier), FALSE, Outlier))
      mutate(!!quo_name(column_name) := ifelse(is.na(!!sym(column_name)), FALSE, !!sym(column_name))) %>%
      # remove outliers that are below the regression line
      mutate(!!quo_name(column_name) := ifelse(.data$Residual < 0, FALSE, !!sym(column_name))) %>%
      select(-.data$Row) %>%
      ungroup() -> df.outliers

    # Step 4: regroup the Outlier data to the original Application
    Application <- Application %>%
      left_join(df.outliers %>%
        select(.data$JobId, !!quo_name(column_name)), by = c("JobId"))
  } else {
    starvz_log("No anomalies were detected.")
    Application <- Application %>%
      mutate(!!quo_name(column_name) := FALSE)
  }

  return(Application)
}

#' @importFrom rlang quo_name
#' @importFrom rlang :=
regression_based_outlier_detection <- function(Application, task_model, column_name, level = 0.95) {
  column_name <- paste0("Outlier", column_name)

  # Step 1: apply the model to each task, considering the ResourceType
  df.model.outliers <- Application %>%
    filter(grepl("qrt", .data$Value)) %>%
    # cannot have zero gflops
    filter(.data$GFlop > 0) %>%
    unique() %>%
    group_by(.data$ResourceType, .data$Value, .data$Cluster) %>%
    nest() %>%
    mutate(model = map(.data$data, task_model))

  # check if we need to transform log value with the exponential function after prediction
  if (grepl("LOG", column_name) | grepl("FLEXMIX", column_name)) {
    df.model.outliers <- df.model.outliers %>%
      mutate(Prediction = map(.data$model, function(model) {
        data_predict <- suppressWarnings(predict(model, interval = "prediction", level = level))
        data_predict %>%
          tibble(fit = exp(.[, 1]), lwr = exp(.[, 2]), upr = exp(.[, 3])) %>%
          select(.data$fit, .data$upr, .data$lwr)
      }))
  } else {
    df.model.outliers <- df.model.outliers %>%
      mutate(Prediction = map(.data$model, function(model) {
        data_predict <- suppressWarnings(predict(model, interval = "prediction", level = level))
        data_predict %>%
          tibble(fit = .[, 1], lwr = .[, 2], upr = .[, 3]) %>%
          select(.data$fit, .data$upr, .data$lwr)
      }))
  }

  df.model.outliers <- df.model.outliers %>%
    unnest(cols = c(.data$data, .data$Prediction)) %>%
    # Test if the Duration is bigger than the upper prediction interval value for that cluster
    mutate(DummyOutlier = ifelse(.data$Duration > .data$upr, TRUE, FALSE)) %>%
    ungroup()

  # Step 2: regroup the Outlier data to the original Application, and thats it
  Application <- Application %>%
    left_join(df.model.outliers %>%
      select(.data$JobId, .data$DummyOutlier, .data$fit, .data$lwr, .data$upr), by = c("JobId")) %>%
    mutate(DummyOutlier = ifelse(is.na(.data$DummyOutlier), FALSE, .data$DummyOutlier))

  # Step 3: consider the tasks that seems strange for both models, assuming that we have more than 1 cluster
  # Now we can check if there are any tasks in the "anomalous area" ex: it
  # is out of both models prediction interval, being smaller than the max(lwr) and higher than the min(upr)
  if (grepl("FLEXMIX", column_name)) {
    anomalous.region.tasks <- Application %>%
      filter((.data$Duration < .data$lwr) & !.data$DummyOutlier) %>%
      select(.data$Value, .data$GFlop, .data$ResourceType, .data$Duration, .data$JobId, .data$DummyOutlier)

    # get the models per task type, resource type and cluster
    models <- df.model.outliers %>%
      select(.data$model, .data$Value, .data$ResourceType, .data$Cluster) %>%
      unique()

    # predict the upr value for these tasks using both models
    region.outliers <- anomalous.region.tasks %>%
      left_join(models, by = c("Value", "ResourceType")) %>%
      spread(.data$Cluster, .data$model) %>%
      rename(Cluster1_model = .data$`1`, Cluster2_model = .data$`2`) %>%
      mutate(upr1 = map2(.data$Cluster1_model, .data$GFlop, function(m, gflop) {
        data_predict <- suppressWarnings(predict(m, interval = "prediction", newdata = tibble(GFlop = gflop), level = level))
        data_predict %>%
          tibble(upr1 = exp(.[, 3])) %>%
          select(.data$upr1)
      })) %>%
      mutate(upr2 = map2(.data$Cluster2_model, .data$GFlop, function(m, gflop) {
        # check if Flexmix has choosen the 2 clusters model for that task
        if (!is.null(m)) {
          data_predict <- suppressWarnings(predict(m, interval = "prediction", newdata = tibble(GFlop = gflop), level = level)) %>%
            tibble(upr2 = exp(.[, 3])) %>%
            select(.data$upr2)
        } else {
          data_predict <- tibble(GFlop = gflop, upr2 = 0) %>%
            select(.data$upr2)
        }
        data_predict
      })) %>%
      unnest(cols = c(.data$upr1, .data$upr2)) %>%
      # if the task duration is higher than at least one upr value it is an anomalous task
      mutate(DummyOutlier = ifelse(.data$Duration > .data$upr1 | .data$Duration > .data$upr2, TRUE, FALSE))

    # Step 4: regroup the Outlier data to the original Application, and thats it
    Application <- Application %>%
      mutate(DummyOutlier = ifelse(.data$JobId %in% c(region.outliers %>% filter(.data$DummyOutlier) %>% .$JobId), TRUE, .data$DummyOutlier))
  } else {
    Application <- Application %>% select(-.data$fit, -.data$lwr, -.data$upr)
  }

  Application <- Application %>%
    rename(!!quo_name(column_name) := .data$DummyOutlier)

  return(Application)
}
