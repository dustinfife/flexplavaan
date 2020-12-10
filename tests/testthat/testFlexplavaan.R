context("Flexplavaan functions")
require(testthat)

data("correct_small")
model_1 = "
  f1 =~ x1 + x2 + x3
  f2 =~ y1 + y2 + y3
  f1 ~ f2
  "
fitted_object = flexplavaan(model_1, data=correct_small)

test_that("flexplavaan works", {
  expect_true(class(fitted_object)=="flexplavaan")
  expect_error(flexplavaan(model_1))
})  

test_that("check_for_standard_errors works", {
  expect_message(check_for_standard_errors(fitted_object), regexp=NA)  
  #expect_message(check_for_standard_errors(fit_twofactor_2))
})


