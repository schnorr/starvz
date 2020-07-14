context("Test with LU")
library(starvz)

test_that("starvz_plot works", {
  dt <- starvz_selective_read("../../../test/lu_test/fxt/", "../../../test/lu_test/config.yaml")
  expect_equal(class(dt), "starvz_data")

 pl <- starvz_plot(dt)
 expect_equal(class(pl),c("patchwork", "gg", "ggplot"))
})
