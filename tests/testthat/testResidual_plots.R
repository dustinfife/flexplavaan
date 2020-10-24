
context("Residual plots function")
set.seed(2323)

require(lavaan)

test_that("name_vechs works", {
  testthat::expect_true(name_vechs(letters[1:3])[3]=="b:c")
})

test_that("return_residual_dataset works", {
  expect_true(return_residual_dataset(fit_bollen)$Correlation[1] == "y5:x1")
  expect_true(return_residual_dataset(fit_bollen, max_val = .1) %>% nrow == 2)
})

test_that("combine_residual_datasets works", {
  expect_true(combine_residual_datasets(fit_twofactor)[1,2] == "x2:y2")
  expect_true(is.null(combine_residual_datasets(fit_twofactor)$Model))
  expect_true(!is.null(combine_residual_datasets(fit_twofactor, fit_twofactor_2)$Model))
})

test_that("residual_plots works", {
  vdiffr::expect_doppelganger("single residual_plots",
                              residual_plots(fit_bollen, max_val = .05))
  vdiffr::expect_doppelganger("multiple residual_plots",
                              residual_plots(fit_twofactor, fit_twofactor_2))
})  