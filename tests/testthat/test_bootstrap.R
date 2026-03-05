library(testthat)

test_that("sleep duration basic case works", {
  expect_equal(compute_sleep_duration(22, 30), 8)
})

test_that("sleep duration handles midnight crossing", {
  expect_equal(compute_sleep_duration(23, 7), 8)
})

test_that("bootstrap_mean returns correct length", {
  set.seed(380)
  x <- c(1,2,3,4,5)
  boot <- bootstrap_mean(x, B = 200)
  expect_equal(length(boot), 200)
})

test_that("bootstrap_median returns correct length", {
  set.seed(380)
  x <- c(1,2,3,4,5)
  boot <- bootstrap_median(x, B = 200)
  expect_equal(length(boot), 200)
})

test_that("bootstrap_ci returns two values", {
  set.seed(380)
  x <- rnorm(100)
  boot <- bootstrap_mean(x, B = 500)
  ci <- bootstrap_ci(boot)
  expect_equal(length(ci), 2)
})

