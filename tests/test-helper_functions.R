context("helper_functions tests")

test_that("sort_variables works", {
  expect_true(sort_variables(fit_bollen$lavaan, T)[1] == 8)
})

test_that("get_subset works", {
  expect_equal(letters[1:5], get_subset(letters[1:5], NULL))
  expect_error(get_subset(letters[1:5], 1:6))
  expect_error(get_subset(letters[1:5], c("a", "b", "q")))
  expect_equal(get_subset(letters[1:5], 1:3), letters[1:3])
  expect_equal(get_subset(letters[1:5], letters[1:3]), letters[1:3])
})

test_that("residual_from_latents works", {
  expect_equal(as.numeric(residual_from_latents(3, flexplavaan_to_lavaan(fit_twofactor_2))[1]), -8.617, tolerance=0.01)
  expect_equal(as.numeric(residual_from_latents(1, flexplavaan_to_lavaan(fit_twofactor_2))[1]), 2.18, tolerance=0.01)
})

test_that("flexplavaan_to_lavaan works", {
  expect_true(class(flexplavaan_to_lavaan(fit_bollen))[1], "lavaan")
})