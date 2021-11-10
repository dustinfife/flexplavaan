context("Implied Measurement Functions")
require(testthat)
options(warn=-1)
set.seed(1212)


test_that("implied_measurement works", {
  vdiffr::expect_doppelganger("implied_measurement plot works", implied_measurement(fit_bollen, latent="Eta1"))
  vdiffr::expect_doppelganger("implied_measurement with diff limit works", implied_measurement(fit_bollen, latent="Eta1", limit=3))
  vdiffr::expect_doppelganger("implied_measurement for two models", 
                      implied_measurement(force_fit, 
                                          force_cross, "Force"))
  #vdiffr::expect_doppelganger("implied_measurement for two non-nested", 
  #                    implied_measurement(force_fit, force_exp, "Jedi"))
})
lavNames(force_fit$lavaan)



options(warn=0)