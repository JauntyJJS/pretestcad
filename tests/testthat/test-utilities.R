test_that("is_integer_value works", {

  testthat::expect_true(is_integer_value(1))

  testthat::expect_false(is_integer_value(1.1))

  testthat::expect_false(is_integer_value("1"))

  testthat::expect_false(is_integer_value(NA, allow_na = FALSE))

  testthat::expect_true(is_integer_value(NA, allow_na = TRUE))

})

test_that("check_if_numeric gives error for character inputs", {
  input = "5"
  testthat::expect_snapshot(
    error = TRUE,
    check_if_numeric(input)
  )
})

test_that("check_if_numeric gives error for NULL inputs", {
  input = NULL
  testthat::expect_snapshot(
    error = TRUE,
    check_if_numeric(input)
  )
})

test_that("check_if_numeric gives error for NA inputs", {
  input = NA
  testthat::expect_snapshot(
    error = TRUE,
    check_if_numeric(input, allow_na = FALSE)
  )
})

test_that("check_if_positive gives error for invalid inputs", {
  input = -5
  testthat::expect_snapshot(
    error = TRUE,
    check_if_positive(input)
  )
})

test_that("check_if_positive gives error for invalid inputs", {
  input = 0
  testthat::expect_snapshot(
    error = TRUE,
    check_if_positive(input)
  )
})

test_that("check_if_non_negative gives error for -5", {
  input = -5
  testthat::expect_snapshot(
    error = TRUE,
    check_if_non_negative(input)
  )
})

test_that("check_if_non_negative gives no error for 0", {
  input = 0
  testthat::expect_silent(
    check_if_non_negative(input)
  )
})

test_that("check_if_integer gives no error for 5.5", {
  input = 5.5
  testthat::expect_snapshot(
    error = TRUE,
    check_if_integer(input)
  )
})

test_that("check_if_integer gives no error for 0", {
  input = 0
  testthat::expect_silent(
    check_if_integer(input)
  )
})

test_that("arg_match0_allow_na gives error for invalid inputs", {
  input = "emale"
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_allow_na(input, values = c("female","male"))
  )
})

test_that("arg_match0_allow_na gives no error for valid inputs", {
  input = "male"
  testthat::expect_equal(
    arg_match0_allow_na(input, values = c("female","male")),
    "male"
  )

  input = NA
  testthat::expect_equal(
    is.na(arg_match0_allow_na(input, values = c("female","male"))),
    TRUE
  )

})

test_that("arg_match0_integer gives error for invalid inputs", {
  input = 6
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(input, values = c(0:5))
  )
})

test_that("arg_match0_integer gives error for NULL inputs with allow_na set to TRUE", {
  input = NULL
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(input, values = c(0:5))
  )
})

test_that("arg_match0_integer gives error for NULL inputs with allow_na set to TRUE", {
  input = NULL
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(input, values = c(0:5), allow_na = FALSE)
  )
})

test_that("arg_match0_integer gives error for NA inputs", {
  input = NA
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_integer(input, values = c(0:5), allow_na = FALSE)
  )
})

test_that("arg_match0_true_or_false gives error for invalid inputs", {
  input = 0
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(input)
  )

  input = 1
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(input)
  )
})

test_that("arg_match0_true_or_false gives error for NULL inputs with allow_na set to TRUE", {
  input = NULL
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(input)
  )
})

test_that("arg_match0_true_or_false gives error for NULL inputs with allow_na set to FALSE", {
  input = NULL
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(input, allow_na = FALSE)
  )
})

test_that("arg_match0_true_or_false gives error for NA inputs", {
  input = NA
  testthat::expect_snapshot(
    error = TRUE,
    arg_match0_true_or_false(input, allow_na = FALSE)
  )
})
