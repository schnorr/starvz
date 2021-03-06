% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phase2_events.R
\name{panel_node_events}
\alias{panel_node_events}
\title{Shows nodes events}
\usage{
panel_node_events(
  data = NULL,
  legend = data$config$node_events$legend,
  base_size = data$config$base_size,
  expand_x = data$config$expand,
  x_start = data$config$limits$start,
  x_end = data$config$limits$end
)
}
\arguments{
\item{data}{starvz_data with trace data}

\item{legend}{enable/disable legends}

\item{base_size}{base_size base font size}

\item{expand_x}{expand size for scale_x_continuous padding}

\item{x_start}{X-axis start value}

\item{x_end}{X-axis end value}
}
\value{
A ggplot object
}
\description{
Plot a Gantt chart for all nodes where program events are states
An example of event is the fxt_flush
}
\examples{
panel_node_events(data = starvz_sample_lu)
}
