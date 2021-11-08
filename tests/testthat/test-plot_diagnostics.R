context("plot_diagnostics tests")

test_that("diagnostics_trace works", {
  vdiffr::expect_doppelganger("disturbance_trace two models", diagnostics_trace(small, small_mis, aes(x1, x2)))
  vdiffr::expect_doppelganger("disturbance_trace one model" , diagnostics_trace(small,    mapping=aes(y1, x2)))
})

test_that("diagnostics_disturbance works", {
  vdiffr::expect_doppelganger("diagnostics_disturbance two models", diagnostics_disturbance(small, small_mis, aes(x1, x2)))
  vdiffr::expect_doppelganger("diagnostics_disturbance one model" , diagnostics_disturbance(small,    mapping=aes(y1, x2)))
})

test_that("diagnostics_histogram works", {
  vdiffr::expect_doppelganger("diagnostics_histogram", diagnostics_histogram(small, aes(x1)))
})

test_that("viz_diagnostics works", {
  vdiffr::expect_doppelganger("viz_diagnostics trace", viz_diagnostics(small_data, aes(x1, x2), small, small_mis, "trace"))
  vdiffr::expect_doppelganger("viz_diagnostics hist" , viz_diagnostics(small_data, aes(x1, x2), small, small_mis, "histogram"))
  vdiffr::expect_doppelganger("viz_diagnostics ddp"  , viz_diagnostics(small_data, aes(x1, x2), small, small_mis, "disturbance"))
})