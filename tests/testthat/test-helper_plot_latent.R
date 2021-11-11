context("helper_plot_latent works")
set.seed(2323)
options(warn=-1)
require(lavaan)


test_that("beta_to_flexplot works", {
  expect_equal(beta_to_flexplot(small)[[1]], formula(z~f1 | f2))
  expect_equal(beta_to_flexplot(small, return_dvs = T), 3)
  expect_equal(beta_to_flexplot(small_fa), formula(f1~f2))
  expect_equal(beta_to_flexplot(small_fa, T), c(1,2))
  
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