context("Visualize function")
set.seed(2323)

require(lavaan)
data("correct_small")

model_1 = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
f1 ~ f2
"

model_2 = "
f1 =~ x1 + x2 + x3
f2 =~ x3 + y1 + y2 + y3
f1 ~ f2
"
  
test_that("regular lavaan works", {
  fit.lavaan_1 = cfa(model_1, data=correct_small)
  fit.lavaan_2 = cfa(model_2, data=correct_small)
  vdiffr::expect_doppelganger("simple lavaan graph",visualize(fit.lavaan_1, subset=1:3))
  vdiffr::expect_doppelganger("lavaan graph ddp",visualize(fit.lavaan_1, subset=1:3, plot="residuals"))
  vdiffr::expect_doppelganger("lavaan graph model only",visualize(fit.lavaan_1, subset=1:3, plot="model"))
  vdiffr::expect_doppelganger("two lavaan models",visualize(fit.lavaan_1, fit.lavaan_2, subset=1:3))
})