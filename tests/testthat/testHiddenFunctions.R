context("Test hidden functions")
require(testthat)





test_that("find_latents_for_observed works", {
  expect_true(find_latents_for_observed(1, flexplavaan_to_lavaan(fit_bollen))=="Eta1")
  expect_true(length(find_latents_for_observed(3, flexplavaan_to_lavaan(fit_twofactor_2)))==2)
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


