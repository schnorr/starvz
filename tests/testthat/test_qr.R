context("Test with QR")
library(starvz)

test_that("starvz with QR", {
  skip_on_cran()

  skip_if(system.file("extdata", "qr_trace", package = "starvz") == "")

  other_data <- starvz_phase1(system.file("extdata", "qr_trace", package = "starvz"), qrmumps_colors, state_filter = 1, whichApplication = "qrmumps")
  other_data <- starvz_read(system.file("extdata", "qr_trace", package = "starvz"), system.file("extdata", "qr.yaml", package = "starvz"))
  pl <- NULL
  pl <- starvz_plot(other_data)
  expect_equal(class(pl), c("patchwork", "gg", "ggplot"))
})
