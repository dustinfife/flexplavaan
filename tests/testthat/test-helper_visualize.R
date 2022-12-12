context("helper_visualize functions work")

test_that("invert_aes_mapping works", {
  expect_equal(invert_aes_mapping(aes(x,y), TRUE), c("y", "x"))
  expect_equal(invert_aes_mapping(aes(x,y), FALSE), c("x", "y"))
})

test_that("viz_diagnostics_error_check works", {
  expect_error(viz_diagnostics_error_check(c("a", "b"), small))
  expect_null(viz_diagnostics_error_check(c("x1", "x2"), small))
})

test_that("viz_diagnostics_get_data works", {
  expect_null(viz_diagnostics_get_data(small, variables = c("x1", "x2"))$y2_name)
  expect_equal(names(viz_diagnostics_get_data(small, variables = c("x1", "x2"))), 
               c("data", "new_data", "y2_name", "resid_name"))
  expect_equal(ncol(viz_diagnostics_get_data(small, variables = c("x1", "x2"))$new_data), 2)
})

test_that("extract_subset works", {
  expect_error(extract_subset(small, 1:10))
  expect_error(extract_subset(small, 810))
  expect_equal(extract_subset(small, 1:3), c("x2", "y2", "y3"))
  expect_equal(extract_subset(small, "x1"), c("x1"))  
  expect_error(extract_subset(small, "xasdf1"))  
})