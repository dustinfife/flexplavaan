context("Test hidden functions")
require(testthat)

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

test_that("find_latents_for_observed works", {
  expect_true(find_latents_for_observed(1, flexplavaan_to_lavaan(fit_bollen))=="Eta1")
  expect_true(length(find_latents_for_observed(3, flexplavaan_to_lavaan(fit_twofactor_2)))==2)
})

test_that("residual_from_latents works", {
  expect_equal(as.numeric(residual_from_latents(3, flexplavaan_to_lavaan(fit_twofactor_2))[1]), -8.617, tolerance=0.01)
  expect_equal(as.numeric(residual_from_latents(1, flexplavaan_to_lavaan(fit_twofactor_2))[1]), 2.18, tolerance=0.01)
})


test_that("block_model_residuals works", {
  expect_true(block_model_residuals(flexplavaan_to_lavaan(fit_bollen))[1]==8)
})

test_that("vechs works", {
  m = matrix(1:9, nrow=3)
  m[upper.tri(m)] = m[lower.tri(m)]
  expect_true(all(vechs(m) %in% c(2,3,6)))
  expect_true(vech2full(1:3)[2,2] == 3)
})

test_that("random variable naming works", {
  set.seed(23232)
  expect_true(random_var_name()=="huhpz")
  expect_true(random_var_name_check(varnames=letters[1:5])=="dlzoi")
})

test_that("get_names works", {
  expect_true(get_names(flexplavaan_to_lavaan(fit_bollen))[[1]][1] == "y1")
  expect_true(get_names(flexplavaan_to_lavaan(fit_bollen))[[2]][1] == "Eta1")
})

