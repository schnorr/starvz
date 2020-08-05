context("Test with LU")
library(starvz)

test_that("starvz_plot works", {
  expect_equal(class(starvz_sample_lu), "starvz_data")

 pl <- starvz_plot(starvz_sample_lu)
 expect_equal(class(pl),c("patchwork", "gg", "ggplot"))

 other_data <- starvz_sample_lu
 other_data$config <- starvz_read_config(system.file("extdata", "agg.yaml", package = "starvz"))
 pl <- starvz_plot(other_data)
 expect_equal(class(pl),c("patchwork", "gg", "ggplot"))

 other_data$config <- starvz_read_config(system.file("extdata", "selected.yaml", package = "starvz"))
 pl <- starvz_plot(other_data)
 expect_equal(class(pl),c("patchwork", "gg", "ggplot"))

 other_data <- starvz_phase1_read_write(system.file("extdata", "lu_trace", package = "starvz"), lu_colors, state_filter=2, whichApplication="lu")
 other_data <- starvz_read(system.file("extdata", "lu_trace", package = "starvz"), system.file("extdata", "config.yaml", package = "starvz"))
 result <- all.equal(nrow(other_data$Application), nrow(starvz_sample_lu$Application))
 expect_equal(result, TRUE)
})
