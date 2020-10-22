context("Latent plots function")
set.seed(2323)

require(lavaan)

test_that("latent_plot works", {
  vdiffr::expect_doppelganger("simple latent_plot",latent_plot(fit_bollen, formula = Eta2 ~ Eta1))
  vdiffr::expect_doppelganger("multiple latent_plots",latent_plot(fit_bollen)[[2]])
})  

test_that("beta_to_flexplot works", {
  expect_true(paste(beta_to_flexplot(fit_bollen, data.frame(lavPredict(fit_bollen)))[[2]])[2] == "Eta2")
  expect_true(beta_to_flexplot(fit_bollen, data.frame(lavPredict(fit_bollen)), return_dvs = T)[1] == 1)
  beta = fit_bollen@Model@GLIST$beta
  expect_true(get_dv_iv(2, beta)[2] == 3)
})