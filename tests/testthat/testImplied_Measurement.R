context("Implied Measurement Functions")
require(testthat)

set.seed(1212)
test_that("latent_observed_implied works", {
  expect_equal(latent_observed_implied(fit_bollen)[1,1]*100 %>% round, 18.3371, tolerance = .001, scale = 1)
})