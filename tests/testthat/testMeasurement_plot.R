context("Measurement plots function")
set.seed(2323)

require(lavaan)
data("correct_small")


test_that("create_latent_dataset works", {
  d1 = suppressWarnings(create_latent_dataset(1, fit_twofactor))
  d2 = suppressWarnings(create_latent_dataset(2, fit_twofactor))
  expect_true(d1[1,1] == "x1")
  expect_true(d2[1,1] == "y1")
  expect_true(names(d1)[3] == "f1")
  expect_true(nrow(d2) == 1500)
})  

test_that("estimate_standard_errors works", {
  d1 = estimate_standard_errors(1,fit_twofactor)
  d2 = estimate_standard_errors(2,fit_twofactor)
  expect_true(round(d1[1,2], digits=2) == .590)
  expect_true(round(d2[1,2], digits=2) == .33)
})

test_that("return_latent_index works", {
  expect_error(return_latent_index(fit_twofactor, 3))
  expect_true(return_latent_index(fit_twofactor, 2)==2)
  expect_true(length(return_latent_index(fit_twofactor, 1:2))==2)
  expect_error(return_latent_index(fit_twofactor, "2"))
  expect_true(return_latent_index(fit_twofactor, "f2")==2)
  expect_true(length(return_latent_index(fit_twofactor))==2)
})

test_that("find nth works", {
  testthat::expect_true(find_nth(1)=="st")
  testthat::expect_true(find_nth(2)=="nd")
  testthat::expect_true(find_nth(3)=="rd")
  testthat::expect_true(find_nth(4)=="th")
})

test_that("regular measurement plot", {
  vdiffr::expect_doppelganger("simple measurement plot",measurement_plot(fit_twofactor, 1))
  vdiffr::expect_doppelganger("simple measurement plot with two plots",measurement_plot(fit_twofactor, 1:2)[[2]])
})