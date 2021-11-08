context("Create Prediction Sequence")
require(testthat)

data("mugglevwizard")
set.seed(1212)
# test_that("create_sequence generates correct output", {
#   #expect_equal(create_sequence("wingardium", mugglevwizard, sequence=TRUE, round=TRUE)[2], 2)
#   #expect_equal(create_sequence("wingardium", mugglevwizard, sequence=TRUE, round=FALSE)[2], 1.736842, tolerance = .001)
#   #expect_equal(create_sequence("wingardium", mugglevwizard, sequence=FALSE, round=FALSE)[2], 15.45281, tolerance = .001)
#   #expect_true(all(create_sequence("wingardium", mugglevwizard, sequence=FALSE, round=TRUE)==15))
#   #expect_identical(create_sequence("mugglevwizard", mugglevwizard, sequence=FALSE), "wizard")
#   #expect_true(length(create_sequence("mugglevwizard", mugglevwizard, sequence=TRUE))==2)
# })


test_that("create_new_data generates correct output", {
  nd = create_new_data(mugglevwizard, condition.vars = c("wingardium", "strange"))
  expect_true(nrow(nd)==400)
  nd = create_new_data(mugglevwizard)
  expect_true(nrow(nd)==20)
})