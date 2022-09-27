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

test_that("get_endogenous_names works", {
  expect_true(get_endogenous_names(small)[3] == "z")
  expect_equal(get_endogenous_names(small_fa), c("f1", "f2"))  
})


test_that("get_dv_iv works", {
  beta = small@Model@GLIST$beta
  expect_equal(get_dv_iv(3,beta), c(1,2))
})



options(warn=0)