context("Plot Modifiers")

set.seed(2323)
options(warn=-1)

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

fit1 = cfa(model_1, data=correct_small)
fit2 = cfa(model_2, data=correct_small)


test_that("modify_model_names works", {
  vdiffr::expect_doppelganger("modify_model_names one plot ", plot_scatter_matrix(fit1,       subset=1:3, plot="all") %>% modify_model_names(c("a", "b")))
  vdiffr::expect_doppelganger("modify_model_names two plots", plot_scatter_matrix(fit1, fit2, subset=1:3, plot="all") %>% modify_model_names(c("a", "b")))
  expect_error(modify_model_names(1, "a"))
})

p = implied_measurement(small, small_mis)
test_that("modify_latent works", {
  vdiffr::expect_doppelganger("modify_latent for implied measurement", p %>% modify_latent("f2"))
})

test_that("modify_smooth works", {
  vdiffr::expect_doppelganger("modify_smooth for implied measurement", p %>% modify_smooth(method="quadratic"))
  visualize(small, plot="trail", subset=1:3) %>% modify_smooth(method="lm")
                              
})