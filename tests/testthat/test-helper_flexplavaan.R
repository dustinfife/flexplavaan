context("helper_flexplavaan works")

test_that("flexplavaan works", {
  flex_small = flexplavaan(small_syntax, small_data)
  expect_equal(names(flex_small), c("lavaan", "data", "standard_errors")  )
  expect_true(class(flex_small$lavaan)[1] == "lavaan")
})  

test_that("flexplavaan_check_errors works", {
  expect_error(flexplavaan_check_errors(small_syntax))
  expect_null(flexplavaan_check_errors(small_syntax, small_data))
})

# calculate standard errors
test_that("check_for_standard_errors works", {
  expect_message(check_for_standard_errors(small))
  expect_equal(ncol(check_for_standard_errors(small_flexplavaan)), 2)
})

test_that("get_standard_errors works", {
  expect_equal(names(get_standard_errors(small)), c("se_f1", "se_f2"))
})