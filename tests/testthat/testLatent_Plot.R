context("Latent plots function")
set.seed(2323)

require(lavaan)

test_that("latent_plot works", {
  vdiffr::expect_doppelganger("simple latent_plot",latent_plot(fit_bollen, formula = Eta2 ~ Eta1))
  vdiffr::expect_doppelganger("multiple latent_plots",latent_plot(fit_bollen)[[2]])
})  

test_that("return_alpha works", {
  vdiffr::expect_doppelganger("alpha adjustment latent_plot",
                              latent_plot(fit_bollen, formula = Eta2 ~ Eta1, alpha = .6))
  vdiffr::expect_doppelganger("color adjustment latent_plot",
                              latent_plot(fit_bollen, formula = Eta2 ~ Eta1, color = "blue"))  
})  

test_that("beta_to_flexplot works", {
  expect_true(paste(beta_to_flexplot(fit_bollen, data.frame(lavPredict(fit_bollen)))[[2]])[2] == "Eta2")
  expect_true(beta_to_flexplot(fit_bollen, data.frame(lavPredict(fit_bollen)), return_dvs = T)[1] == 1)
  beta = fit_bollen@Model@GLIST$beta
  expect_true(get_dv_iv(2, beta)[2] == 3)
})

test_that("get_dv_iv works", {
  expect_true(get_dv_iv(1, fit_bollen@Model@GLIST$beta)==3)
  # for negative loadings
  expect_true(length(get_dv_iv(3, health@Model@GLIST$beta))==2)
})

test_that("check_data_has_observed works", {
  d = data.frame(lavPredict(health))
  se_data = estimate_standard_errors(1, health)$sd_imp
  d_new = cbind(d, se_data)
  expect_true(ncol(check_data_has_observed(d_new, "internet", "health", health))==3)
  expect_true(ncol(check_data_has_observed(d_new, "internet", "CESD", health))==17)
})