context("Test layout matrix calculations")

test_that("Expected error in case of erroneous input", {
  expect_error(layoutmat("a"))
  expect_error(layoutmat(0))
  expect_error(layoutmat(c(1,-1)))
  expect_error(layoutmat(c(0,1)))
})

test_that("Expected return in case of missing input", {
  expect_equal(layoutmat(), matrix(1))
})

test_that("Expected return in case of length 1 input", {
  expect_equal(layoutmat(1), matrix(1))
  expect_equal(layoutmat(10), matrix(1:10, nrow=1, byrow=T))
})
