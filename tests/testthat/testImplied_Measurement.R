context("Implied Measurement Functions")
require(testthat)

set.seed(1212)
test_that("latent_observed_implied works", {
  expect_equal(latent_observed_implied(fit_bollen)[1,1]*100 %>% round, 18.3371, tolerance = .001, scale = 1)
})

test_that("get_slopes and get_intercepts works", {
  names = get_names(fit_bollen)
  slopes = get_slopes(fit_bollen, names[[1]], names[[2]]) %>% transmute_all(function(x) x*100) %>% round
  intercepts = get_intercepts(slopes, get_all_data(fit_bollen), names[[2]], names[[1]]) %>% round
  expect_true(slopes[1,1]==24)
  expect_true(slopes[2,1]==23)
  expect_true(intercepts[1,1]==-131)
  expect_true(intercepts[1,2]==-126)  
})

test_that("standardize_observed works", {
  standardize_observed(fit_bollen) %>% head
  names = get_names(fit_bollen)
  slopes = get_slopes(lav_data = get_all_data(model)fit_bollen, 
  expect_true(slopes[1,1]==24)
  expect_true(slopes[2,1]==23)
})