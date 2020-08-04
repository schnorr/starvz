context("Test with LU")
library(starvz)

test_that("starvz_plot works", {
  expect_equal(class(starvz_sample_lu), "starvz_data")

 pl <- starvz_plot(starvz_sample_lu)
 expect_equal(class(pl),c("patchwork", "gg", "ggplot"))
})
