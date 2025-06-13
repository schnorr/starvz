context("Test with LU")
library(starvz)

test_that("starvz_plot works", {
  expect_equal(class(starvz_sample_lu), "starvz_data")

  pl <- starvz_plot(starvz_sample_lu)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  skip_on_cran()
  skip_if_not_installed("arrow")

  other_data <- starvz_sample_lu

  other_data$config <- starvz_read_config(system.file("extdata", "config.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data$config <- starvz_read_config(system.file("extdata", "agg.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data$config <- starvz_read_config(system.file("extdata", "selected.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data$config <- starvz_read_config(system.file("extdata", "imb.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data$config <- starvz_read_config(system.file("extdata", "agg_dynamic.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data$config <- starvz_read_config(system.file("extdata", "agg_static.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data$config <- starvz_read_config(system.file("extdata", "pmtool.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_true(is_ggplot(pl) && inherits(pl, "patchwork"))

  other_data <- starvz_phase1(system.file("extdata", "lu_trace", package = "starvz"), lu_colors, state_filter = 2, whichApplication = "lu")
  other_data <- starvz_read(system.file("extdata", "lu_trace", package = "starvz"), system.file("extdata", "config.yaml", package = "starvz"))
  result <- all.equal(nrow(other_data$Application), nrow(starvz_sample_lu$Application))
  if (arrow::codec_is_available("gzip")) {
    expect_equal(result, TRUE)
  }

  pl <- NULL
  pl <- panel_handles(starvz_sample_lu, JobId = "0_1")
  expect_true(is_ggplot(pl))

  pl <- NULL
  pl <- panel_memory_snap(starvz_sample_lu, 100, 10)
  expect_true(is_ggplot(pl))

  pl <- NULL
  pl <- panel_memory_heatmap(starvz_sample_lu)
  expect_true(is_ggplot(pl))

  pl <- NULL
  pl <- panel_dist2d(starvz_sample_lu)
  expect_true(is_ggplot(pl))

  pl <- NULL
  pl <- panel_node_events(starvz_sample_lu)
  expect_true(is_ggplot(pl))
})
