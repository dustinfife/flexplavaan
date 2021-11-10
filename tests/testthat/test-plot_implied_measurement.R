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

test_that("prepare_measurement_data and latent_flexplot works", {
  b_data = prepare_measurement_data(flexplavaan_to_lavaan(fit_bollen))
  # make sure all variables are standardized
  column_means = colMeans(b_data %>% select(-Variable)) %>% round(2)
  vdiffr::expect_doppelganger("latent_flexplot works", latent_flexplot(b_data, "Eta1"))
})



options(warn=0)