context("Latent plots function")
set.seed(2323)
options(warn=-1)
require(lavaan)


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



test_that("get_endogenous_names works", {
  expect_true(get_endogenous_names(flexplavaan_to_lavaan(fit_bollen))[1] == "Eta1")
  expect_true(get_endogenous_names(sem_a)[1] == "latent_x")  
})
options(warn=0)