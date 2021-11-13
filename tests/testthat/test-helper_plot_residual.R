context("helper_plot_residuals works")

test_that("return_residual_dataset works", {
  returned_ds = return_residual_dataset(small)
  expect_true(nrow(returned_ds) ==14)
  expect_true(all(abs(returned_ds$Residual)>.01))
  expect_true(all(abs(return_residual_dataset(small, .03)$Residual)>.03))
})

test_that("combine_residual_datasets works", {
  expect_equal(nrow(combine_residual_datasets(small)), 14)
  expect_equal(combine_residual_datasets(small, small_mis)$Model[1], "Residual.x")
  expect_true(all(combine_residual_datasets(small, small_mis, .03)$average_residual>.03))
})
