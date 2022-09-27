context("helper_calculations works")

test_that("vechs works", {
  m = matrix(1:9, nrow=3)
  m[upper.tri(m)] = m[lower.tri(m)]
  expect_true(all(vechs(m) %in% c(2,3,6)))
  expect_true(vech2full(1:3)[2,2] == 3)
})
