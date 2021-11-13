context("plot_residuals works")

set.seed(2323)

test_that("residual_plots works", {
  vdiffr::expect_doppelganger("residual_plot", residual_plots(small))
  vdiffr::expect_doppelganger("residual_plot two models", residual_plots(small, small_mis))
})