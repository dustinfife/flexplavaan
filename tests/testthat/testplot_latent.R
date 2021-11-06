context("testplot_latent plots")
set.seed(2323)
options(warn=-1)
require(lavaan)

test_that("latent_plot works", {
  vdiffr::expect_doppelganger("simple latent_plot",latent_plot(fit_bollen))
})  

test_that("return_alpha works", {
  vdiffr::expect_doppelganger("alpha adjustment latent_plot",
                              latent_plot(fit_bollen, alpha = .6))
  vdiffr::expect_doppelganger("color adjustment latent_plot",
                              latent_plot(fit_bollen, color = "blue"))  
})  