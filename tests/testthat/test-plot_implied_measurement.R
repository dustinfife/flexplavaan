context("plot_implied_measurement works")

require(testthat)
options(warn=-1)
set.seed(1212)


test_that("implied_measurement works", {
  vdiffr::expect_doppelganger("implied_measurement plot works", implied_measurement(small, sort_slopes = F))
  vdiffr::expect_doppelganger("implied_measurement with diff limit works", implied_measurement(small, latent="f1", limit=3))
  vdiffr::expect_doppelganger("implied_measurement for two models", 
                              implied_measurement(small, 
                                                  small_mis))
  vdiffr::expect_doppelganger("implied_measurement for non-nested models", implied_measurement(small, small_diflat))  
  vdiffr::expect_doppelganger("implied_measurement for non-nested models with no common data", 
                              implied_measurement(force_fit, force_exp, "Jedi"))  
})

test_that("latent_flexplot works", {
  b_data = prepare_measurement_data(small)
  vdiffr::expect_doppelganger("latent_flexplot works", latent_flexplot(b_data, "f1"))
  b_data = prepare_measurement_data(small, small_mis)
  vdiffr::expect_doppelganger("latent_flexplot (two models) works", latent_flexplot(b_data, "f2"))
})



options(warn=0)