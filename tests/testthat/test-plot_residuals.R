context("plot_residuals works")

set.seed(2323)

test_that("residual_plots works", {
  residual_plots(small)
  residual_plots(small, small_mis)
})