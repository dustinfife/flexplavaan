context("Measurement plots function")
set.seed(2323)
options(warn=-1)
require(lavaan)
data("correct_small")


test_that("create_latent_dataset works", {
  d1 = suppressWarnings(create_latent_dataset(1, flexplavaan_to_lavaan(fit_twofactor)))
  d2 = suppressWarnings(create_latent_dataset(2, flexplavaan_to_lavaan(fit_twofactor)))
  # this will fail...need to fix it
  #d3 = create_latent_dataset(1:2, fit_twofactor)
  expect_true(d1[1,1] == "x1")
  expect_true(d2[1,1] == "y1")
  expect_true(names(d1)[3] == "f1")
  expect_true(nrow(d2) == 1500)
})  

test_that("estimate_standard_errors works", {
  d1 = estimate_standard_errors(1,flexplavaan_to_lavaan(fit_twofactor))
  d2 = estimate_standard_errors(2,flexplavaan_to_lavaan(fit_twofactor))
  expect_true(round(d1[1,2], digits=2) == .590)
  expect_true(round(d2[1,2], digits=2) == .33)
})

test_that("return_latent_index works", {
  expect_error(return_latent_index(flexplavaan_to_lavaan(fit_twofactor), 3))
  expect_true(return_latent_index(flexplavaan_to_lavaan(fit_twofactor), 2)==2)
  expect_true(length(return_latent_index(flexplavaan_to_lavaan(fit_twofactor), 1:2))==2)
  expect_error(return_latent_index(flexplavaan_to_lavaan(fit_twofactor), "2"))
  expect_true(return_latent_index(flexplavaan_to_lavaan(fit_twofactor), "f2")==2)
  expect_true(length(return_latent_index(flexplavaan_to_lavaan(fit_twofactor)))==2)
})

test_that("find nth works", {
  testthat::expect_true(find_nth(1)=="st")
  testthat::expect_true(find_nth(2)=="nd")
  testthat::expect_true(find_nth(3)=="rd")
  testthat::expect_true(find_nth(4)=="th")
})

test_that("regular measurement plot", {
  vdiffr::expect_doppelganger("simple measurement plot",suppressMessages(measurement_plot(fit_twofactor, 1)))
  vdiffr::expect_doppelganger("simple measurement plot with two plots",suppressMessages(measurement_plot(fit_twofactor, 1:2)[[2]]))
  vdiffr::expect_doppelganger("sampling with measurement plot works",suppressMessages(measurement_plot(force_fit, 1, sample=100)))
})

test_that("random_sample_from_data works", {
  expect_true(nrow(random_sample_from_data(jedi_jedi, sample=100))==100)
})

test_that("put_geom_last works", {
  p = flexplot(fitness~saber, data=jedi_jedi)
  p = put_geom_last(p, name="GeomPoint")
  vdiffr::expect_doppelganger("moving geom last works",p)
})
options(warn=0)