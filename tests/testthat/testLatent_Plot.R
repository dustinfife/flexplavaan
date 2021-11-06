context("Latent plots function")
set.seed(2323)
options(warn=-1)
require(lavaan)




test_that("check_for_sd_true works", {
  expect_true(sum(check_for_sd_true(F, flexplavaan_to_lavaan(fit_bollen), "Eta1"))==0)
  expect_true(sum(check_for_sd_true(F, flexplavaan_to_lavaan(fit_bollen), c("Eta1", "Eta2")))==0)
})

test_that("beta_to_flexplot works", {
  expect_true(paste(beta_to_flexplot(flexplavaan_to_lavaan(fit_bollen), data.frame(lavPredict(flexplavaan_to_lavaan(fit_bollen))))[[2]])[2] == "Eta2")
  expect_true(beta_to_flexplot(flexplavaan_to_lavaan(fit_bollen), data.frame(lavPredict(flexplavaan_to_lavaan(fit_bollen))), return_dvs = T)[1] == 1)
  #expect_true(paste(beta_to_flexplot(sem_a, data.frame(lavPredict(sem_a))))[2] == "latent_x")
  #expect_true(beta_to_flexplot(sem_a, data.frame(lavPredict(sem_a)), return_dvs = T)[1] == 1)  
})

test_that("get_dv_iv works", {
  expect_true(get_dv_iv(1, flexplavaan_to_lavaan(fit_bollen)@Model@GLIST$beta)==3)
  # for negative loadings
  expect_true(length(get_dv_iv(3, flexplavaan_to_lavaan(health)@Model@GLIST$beta))==0)
})

# test_that("check_data_has_observed works", {
#   d = data.frame(lavPredict(flexplavaan_to_lavaan(health)))
#   se_data = estimate_standard_errors(1, flexplavaan_to_lavaan(health))$sd_imp
#   d_new = cbind(d, se_data)
#   head(check_data_has_observed(d_new, "internet", "health", flexplavaan_to_lavaan(health)))
#   head(d_new)
#   #expect_true(ncol(check_data_has_observed(d_new, "internet", "health", flexplavaan_to_lavaan(health)))==3)
#   #expect_true(ncol(check_data_has_observed(d_new, "internet", "CESD", flexplavaan_to_lavaan(health)))==17)
# })

test_that("get_endogenous_names works", {
  expect_true(get_endogenous_names(flexplavaan_to_lavaan(fit_bollen))[1] == "Eta1")
  expect_true(get_endogenous_names(sem_a)[1] == "latent_x")  
})
options(warn=0)