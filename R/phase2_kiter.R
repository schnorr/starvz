#' Internal transformation for kiteration data
#'
#' @param df starvz_data$Application
#' @param factor_val Numeric factor value to step iterations
#' @param slice_size Numeric aggregation step size for time slicing
#' @param levels Character vector specifying the order of tasks
#' @return A transformed data frame ready for geom_kiter
#' @keywords internal
kiter_transform <- function(df, factor_val, slice_size, levels = NULL) {
  max_power <- df %>%
    dplyr::distinct(.data$Node, .data$ResourceId) %>%
    nrow()

  if (!is.null(levels)) {
    df <- df %>%
      dplyr::mutate(Value = factor(.data$Value, levels = levels))
  }

  df %>%
    dplyr::mutate(
      Iteration = as.integer(.data$Iteration / factor_val)
    ) %>%
    time_aggregation_prep() %>%
    dplyr::group_by(.data$ResourceId, .data$Iteration, .data$Task) %>%
    time_aggregation_do(step = slice_size) %>%
    dplyr::filter(.data$Value != 0) %>%
    dplyr::mutate(Load.Core = .data$Value * .data$Duration) %>%
    dplyr::arrange(.data$Task) %>%
    dplyr::group_by(.data$Iteration, .data$Slice, .data$Task) %>%
    dplyr::summarize(Load = sum(.data$Load.Core), .groups = "drop") %>%
    dplyr::mutate(P.Global.Load = .data$Load / (max_power * slice_size)) %>%
    dplyr::group_by(.data$Iteration, .data$Slice) %>%
    dplyr::mutate(
      Load.P.cumsum = cumsum(.data$P.Global.Load),
      X.min = .data$Slice,
      X.max = .data$Slice + slice_size,
      Y.min = (.data$Iteration + (.data$P.Global.Load - .data$Load.P.cumsum)) * factor_val,
      Y.max = (.data$Iteration - .data$Load.P.cumsum) * factor_val
    )
}

#' Kiteration tracking rectangle layer
#'
#' @param factor Numeric factor value to step iterations
#' @param slice_size Numeric aggregation step size for time slicing
#' @param levels Character vector specifying the order of tasks
#' @param ... Additional arguments passed to geom_rect
#' @return A ggplot2 layer
#' @export
geom_kiter <- function(factor = 1, slice_size = 100, levels = NULL, ...) {
  ggplot2::geom_rect(
    mapping = ggplot2::aes(
      xmin = .data$X.min, xmax = .data$X.max,
      ymin = .data$Y.min, ymax = .data$Y.max,
      fill = .data$Task
    ),
    data = function(df) kiter_transform(df, factor, slice_size, levels),
    inherit.aes = FALSE,
    ...
  )
}

#' Kiteration makespan label layer
#'
#' @param size Numeric font size for the label
#' @return A ggplot2 layer
#' @export
geom_kiter_makespan <- function(size = 5) {
  ggplot2::geom_label(
    mapping = ggplot2::aes(x = .data$Start, y = 30, label = .data$Label),
    data = function(df) {
      data.frame(
        Start = min(df$Start, na.rm = TRUE) / 1000,
        Label = paste(round(max(df$End, na.rm = TRUE) / 1000, 2), "seconds")
      )
    },
    hjust = 0, vjust = 0, fill = "#cce5ff", color = "black",
    size = size, fontface = "bold", label.size = size / 10,
    inherit.aes = FALSE
  )
}

#' Standard scales and labels for kiteration panels
#'
#' @param colors A named character vector of colors
#' @return A list of ggplot2 scales and labels
#' @export
scale_kiter <- function(colors = NULL) {
  scales <- list(
    ggplot2::scale_y_reverse(),
    ggplot2::scale_x_continuous(labels = function(x) x / 1000),
    ggplot2::labs(x = "Time [seconds]", y = "Iteration")
  )

  if (!is.null(colors)) {
    scales <- c(list(ggplot2::scale_fill_manual(values = colors)), scales)
  } else {
    scales <- c(list(ggplot2::scale_fill_discrete()), scales)
  }

  return(scales)
}

#' Create a standard kiteration panel for application traces
#'
#' @param data starvz_data with trace data
#' @param factor factor value to step iterations
#' @param slice_size aggregation step size for time slicing
#' @param makespan_size font size for the total execution label
#' @param levels Character vector specifying the order of tasks
#' @return A ggplot object
#' @export
panel_kiter <- function(data = NULL,
                        factor = data$config$kiter$factor,
                        slice_size = data$config$kiter$slice_size,
                        makespan_size = data$config$kiter$makespan_size,
                        levels = data$config$kiter$levels) {
  starvz_check_data(data, tables = list(
    "Application" = c("Node", "ResourceId", "Value", "Iteration", "Start", "End"),
    "Colors" = c("Value", "Color")
  ))

  if (is.null(factor) || !is.numeric(factor)) { factor <- 1 }
  if (is.null(slice_size) || !is.numeric(slice_size)) { slice_size <- 100 }
  if (is.null(makespan_size) || !is.numeric(makespan_size)) { makespan_size <- 5 }

  data$Colors %>%
    dplyr::select("Value", "Color") %>%
    unique() -> color_df

  appColors <- color_df$Color
  names(appColors) <- color_df$Value

  goijk <- ggplot2::ggplot(data$Application) +
    geom_kiter(factor = factor, slice_size = slice_size, levels = levels) +
    geom_kiter_makespan(size = makespan_size) +
    scale_kiter(colors = appColors) +
    ggplot2::theme_bw(base_size = data$config$base_size) +
    default_theme(data$config$base_size, data$config$expand)

  return(goijk)
}

