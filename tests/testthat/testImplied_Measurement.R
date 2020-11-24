context("Implied Measurement Functions")
require(testthat)

set.seed(1212)


test_that("implied_measurement works", {
  vdiffr::expect_doppelganger("implied_measurement plot works", implied_measurement(fit_bollen, "Eta1")[[1]])
  vdiffr::expect_doppelganger("implied_measurement with diff limit works", implied_measurement(fit_bollen, "Eta1", limit=3)[[1]])
})

test_that("prepare_measurement_data and latent_flexplot works", {
  b_data = prepare_measurement_data(fit_bollen)
  # make sure all variables are standardized
  column_means = colMeans(b_data %>% select(-Variable)) %>% round(2)
  expect_true(sum(column_means[1:4])==0)
  vdiffr::expect_doppelganger("latent_flexplot works", latent_flexplot(b_data, "Eta1"))
})

test_that("standardize_observed works", {
  
  names = get_names(fit_bollen)
  slopes = get_slopes(fit_bollen,
                      names[[1]], names[[2]]) 
  expect_true(abs(slopes[1,1]-.85)<.01)
})
  
test_that("latent_observed_implied works", {
  ob = latent_observed_implied(noloadings)
  expect_equal(ob[1,1], 0)
})

test_that("return_actual_slope works", {
  flex_data = prepare_measurement_data(fit_bollen)
  return_actual_slope("y1", "Eta1", flex_data)
})

test_that("check_for_latent", {
  expect_error(check_for_latent(fit_bollen, "EEEE"))
  expect_null(check_for_latent(fit_bollen, "Eta1"))
})

test_that("get_slopes and get_intercepts works", {
  names = get_names(fit_bollen)
  slopes = get_slopes(fit_bollen, names[[1]], names[[2]]) %>% transmute_all(function(x) x*100) %>% round
  intercepts = get_intercepts(slopes, get_all_data(fit_bollen), names[[2]], names[[1]]) %>% round
  expect_true(slopes[1,1]==85)
  expect_true(slopes[2,1]==72)
  expect_true(intercepts[1,1]==-464)
  expect_true(intercepts[1,2]==-448)  
})


