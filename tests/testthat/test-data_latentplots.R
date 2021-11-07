context("data_latentplots works")

test_that("check_for_sd_true works", {
  expect_true(sum(check_for_sd_true(F, flexplavaan_to_lavaan(fit_bollen), "Eta1"))==0)
  expect_true(sum(check_for_sd_true(F, flexplavaan_to_lavaan(fit_bollen), c("Eta1", "Eta2")))==0)
  expect_true(ncol(suppressMessages(check_for_sd_true(T, flexplavaan_to_lavaan(fit_bollen), "Eta1")))==3)
})

test_that("create_ci_limits works", {
  data = data.frame(a=1:3, b=1:3, c=1:3, se_a = .2, se_b=.3)
  expect_equal(create_ci_limits(data, a~b)[1,"upper_pi_yvar"], 1.2)
  expect_error(create_ci_limits(data, a~c))  
})

test_that("create_se_for_endogenous works", {
  d = data.frame(lavPredict(flexplavaan_to_lavaan(health)))
  # estimate se for only health variable
  se_data = estimate_standard_errors(1, flexplavaan_to_lavaan(health))$sd_imp
  d_new = cbind(d, se_data)
  expect_true(sum(create_se_for_endogenous(d_new, CESD~internet, flexplavaan_to_lavaan(health))$se_CESD)==0)
  expect_true(ncol(create_se_for_endogenous(d_new, internet~health_physical, flexplavaan_to_lavaan(health)))==4)
})