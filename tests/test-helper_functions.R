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

test_that("check_formula_in_data works", {
  expect_null( check_formula_in_data(hogwarts_survival, potions~survived + magic_skills))
  expect_error(check_formula_in_data(hogwarts_survival, potions~survived + magix_skills))  
})

test_that("get_legend works", {
  expect_null(get_legend(NULL))
  expect_equal(get_legend(1), c(1,2))
})

test_that("get_names works", {
  expect_true(length(get_names(flexplavaan_to_lavaan(fit_bollen)))==2)
  expect_true(get_names(flexplavaan_to_lavaan(fit_bollen))[[1]][1] == "y1")
  expect_true(get_names(flexplavaan_to_lavaan(fit_bollen))[[2]][1] == "Eta1")
})

test_that("check_models works", {
  expect_null(check_models(flexplavaan_to_lavaan(force_fit), flexplavaan_to_lavaan(force_cross)))
  expect_error(check_models(flexplavaan_to_lavaan(fit_twofactor), flexplavaan_to_lavaan(fit_bollen)))
  expect_null(check_models(fit_twofactor))
  expect_error(check_models(flexplavaan_to_lavaan(force_fit), flexplavaan_to_lavaan(force_exp)))
  expect_error(check_models(flexplavaan_to_lavaan(force_exp), flexplavaan_to_lavaan(force_fit)))
})

test_that("get_subset works", {
  expect_true(length(get_subset(letters[1:10], NULL))==10)
  expect_error(get_subset(letters[1:10], 11))
  expect_true(get_subset(letters[1:10], 3)=="c")
  expect_error(get_subset(letters[1:10], "q"))
  expect_true(length(get_subset(letters[1:10], c("a", "b")))==2)
})
