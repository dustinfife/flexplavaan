context("Visualize function")
set.seed(2323)

require(lavaan)
data("correct_small")

model_2 = "
f1 =~ x1 + x2 + x3
f2 =~ x3 + y1 + y2 + y3
f1 ~ f2
"
fit.lavaan_2 = cfa(model_2, data=correct_small)
test_that("regular lavaan works", {
  vdiffr::expect_doppelganger("simple lavaan graph",visualize(flexplavaan_to_lavaan(fit_twofactor), subset=1:3))
  vdiffr::expect_doppelganger("simple lavaan graph without sorting",visualize(flexplavaan_to_lavaan(fit_twofactor), subset=1:3, sort_plots = F))
  vdiffr::expect_doppelganger("lavaan graph ddp",visualize(flexplavaan_to_lavaan(fit_twofactor), subset=1:3, plot="disturbance"))
  vdiffr::expect_doppelganger("lavaan graph model only",visualize(flexplavaan_to_lavaan(fit_twofactor), subset=1:3, plot="model"))
  vdiffr::expect_doppelganger("two lavaan models",
                              visualize(fit_twofactor, 
                                        fit.lavaan_2, subset=1:3))
  vdiffr::expect_doppelganger("er:observed with no latents",visualize(health,subset=1:4))
  
})

test_that("measurement works in visualize", {
  vdiffr::expect_doppelganger("measurement plot in visualize",visualize(fit_twofactor, subset=1, plot="measurement"))
})

test_that("latent variables work in visualize", {
  vdiffr::expect_doppelganger("latent in visualize",visualize(fit_twofactor, plot="latent"))
  expect_error(visualize(stats_jedi_fit, plot="latent"))
})