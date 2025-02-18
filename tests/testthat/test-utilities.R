test_that("is_integer_value works", {

  testthat::expect_true(is_integer_value(1))

  testthat::expect_false(is_integer_value(1.1))

  testthat::expect_false(is_integer_value("1"))

  testthat::expect_false(is_integer_value(NA, allow_na = FALSE))

  testthat::expect_true(is_integer_value(NA, allow_na = TRUE))

})

test_that("check_if_numeric gives error for character inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_numeric("5")
  )
})

test_that("check_if_numeric gives error for NULL inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_numeric(NULL)
  )
})

test_that("check_if_numeric gives error for NA inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_numeric(NA, allow_na = FALSE)
  )
})

test_that("check_if_positive gives error for invalid inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_positive(-5)
  )
})

test_that("check_if_positive gives error for invalid inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_positive(0)
  )
})

test_that("check_if_non_negative gives error for -5", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_non_negative(-5)
  )
})

test_that("check_if_non_negative gives no error for 0", {
  testthat::expect_silent(
    check_if_non_negative(0)
  )
})

test_that("check_if_integer gives no error for 5.5", {
  testthat::expect_snapshot(
    error = TRUE,
    check_if_integer(5.5)
  )
})

test_that("check_if_integer gives no error for 0", {
  testthat::expect_silent(
    check_if_integer(0)
  )
})

test_that("arg_match0_allow_na gives error for invalid inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_allow_na("M", values = c("female","male"))
  )
})

test_that("arg_match0_allow_na gives no error for valid inputs", {
  testthat::expect_equal(
    arg_match0_allow_na("male", values = c("female","male")),
    "male"
  )

  testthat::expect_equal(
    is.na(arg_match0_allow_na(NA, values = c("female","male"))),
    TRUE
  )

})

test_that("arg_match0_integer gives error for invalid inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(6, values = c(0:5))
  )
})

test_that("arg_match0_integer gives error for NULL inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(NULL, values = c(0:5))
  )
})

test_that("arg_match0_integer gives error for NA inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(NA, values = c(0:5), allow_na = FALSE)
  )
})

test_that("arg_match0_true_or_false gives error for invalid inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(0)
  )

  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(1)
  )
})

test_that("arg_match0_true_or_false gives error for NULL inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(NULL)
  )
})

test_that("arg_match0_true_or_false gives error for NA inputs", {
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(NA, allow_na = FALSE)
  )
})
