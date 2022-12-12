context("data_functions tests")

test_that("sort_dataset works", {
  expect_true(names(sort_dataset(fit_bollen$lavaan, T))[1] == "y5")
})

test_that("get_lav_data works", {
  expect_true(is.data.frame(get_lav_data(fit_bollen$lavaan)))
  expect_true(names(get_lav_data(fit_bollen$lavaan))[3] == "y3")
})

test_that("get_all_data works", {
  expect_true("Eta1" %in% names(get_all_data(fit_bollen$lavaan)))
})
  