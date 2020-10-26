context("Test hidden functions")
require(testthat)

test_that("get_subset works", {
  expect_true(length(get_subset(NULL, letters[1:10]))==10)
  expect_error(get_subset(11, letters[1:10]))
  expect_true(get_subset(3, letters[1:10])=="c")
  expect_error(get_subset("q", letters[1:10]))
  expect_true(length(get_subset(c("a", "b"), letters[1:10]))==2)
})

test_that("block_model_residuals works", {
  expect_true(block_model_residuals(fit_bollen)[1]==8)
})

test_that("vechs works", {
  m = matrix(1:9, nrow=3)
  m[upper.tri(m)] = m[lower.tri(m)]
  expect_true(all(vechs(m) %in% c(2,3,6)))
  expect_true(vech2full(1:3)[2,2] == 3)
})

test_that("random variable naming works", {
  set.seed(23232)
  expect_true(random_var_name()=="huhpz")
  expect_true(random_var_name_check(varnames=letters[1:5])=="dlzoi")
})