#' @title Character Vector To List Phrase With Oxford Comma
#' @description A function that converts a character vector into a list phrase that
#' uses the Oxford comma.
#' @param chr A character vector to turn into a list phrase (e.g. c("a", "b", "c")).
#' @param sep Separator symbols used to separate the elements in the
#' character vector, Default: ', '
#' @param final String to use in place of the final separator when we have
#' at least two elements in the character vector, Default: 'or'.
#' @return A string in the form of a list that has a comma if
#' there are at least three elements in the list (e.g. "a, b, or c")
#' @examples
#' oxford_comma(c("James", "John", "Jeremy"))
#'
#' oxford_comma(c("James", "John", "Jeremy"), final = "and")
#'
#' oxford_comma(c("James", "John"))
#'
#' oxford_comma(c("James"))
#' @rdname oxford_comma
#' @export
oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  head <- chr[seq_len(n - 1)]
  last <- chr[n]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}

#' @title Quoted Characters
#' @description Add a quote around characters.
#' @param chr A character vector to add quotes in each element.
#' (e.g. c("a", "b", "c")).
#' @param type Character to be used as a quote.
#' Default: '`'
#' @return A character vector with quotes added in each element.
#' (e.g. c("`a`", "`b`", "`c`")).
#' @details DETAILS
#' @examples
#' chr_quoted(c("a","b", "c"))
#' @rdname chr_quoted
#' @export
chr_quoted <- function(chr, type = "`") {
  paste0(type, chr, type)
}

#' @title Is Integer Value
#' @description Function to check if the input
#' value is an integer.
#' @param input_value The input value
#' @param allow_na If true, NA values
#' are ignored and output is considered TRUE.
#' Default: FALSE
#' @return A boolean indicating TRUE
#' when the input value is an integer and
#' FALSE otherwise.
#' @examples
#'
#' # An integer
#' is_integer_value(1)
#'
#' # Not an integer
#' is_integer_value(1.1)
#'
#' # Not numeric
#' is_integer_value("1")
#'
#' # NA cases
#' is_integer_value(NA, allow_na = FALSE)
#' is_integer_value(NA, allow_na = TRUE)
#'
#' @rdname is_integer_value
#' @export
is_integer_value <- function(input_value,
                             allow_na = FALSE) {

  boolean_result <- FALSE

  # When input value is NA
  if (is.na(input_value)) {
    if (isTRUE(allow_na)) {
      boolean_result <- TRUE
      return(boolean_result)
    } else {
      return(boolean_result)
    }
  }

  # When input value is not numeric
  if (isTRUE(!is.numeric(input_value))) {
    return(boolean_result)
  }

  # When input value is numeric
  boolean_result <- isTRUE(input_value %% 1 == 0)

  return(boolean_result)
}




#' @title Match an argument to a character vector but skip \code{NA}
#' @description This is equivalent to \code{\link[rlang]{arg_match}} but skip \code{NA}
#' @inheritParams rlang::arg_match
#' @return The string supplied to \code{arg}.
#' @examples
#' # No error
#' input = "male"
#' arg_match0_allow_na(input, values = c("female","male"))
#'
#' # Allow NA
#' input = NA
#' arg_match0_allow_na(input, values = c("female","male"))
#'
#' # Error as M is not female or male
#' input = "emale"
#' try(arg_match0_allow_na(input, values = c("female","male")))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}, \code{\link[rlang]{arg_match}}
#' @rdname arg_match0_allow_na
#' @export
arg_match0_allow_na <- function(
    arg,
    values,
    arg_nm = rlang::caller_arg(arg),
    error_call = rlang::caller_env()
  )
{
  if (!is.na(arg)) {
    arg <- arg |>
      rlang::arg_match0(values = values, arg_nm = arg_nm, error_call = error_call)
  }

  return(arg)
}

#' @title Error Message For \code{NA} Argument For Non-missing List
#' @description Provides an error message if the argument provided is \code{NA}
#' if a non-missing list is provided
#' @inheritParams rlang::arg_match
#' @return An error message if the argument provided is \code{NA}
#' if a non-missing list is provided. Else if will return \code{NULL} invisibly,
#' regardless if \code{arg} has a match with the elements in \code{values} or not.
#' @examples
#' # Error as input is NA but value list provided has no NA
#' input = NA
#' try(arg_match0_no_na_error_message(input, values = c("female","male")))
#'
#' # No error as value list provided has NA
#' input = NA
#' arg_match0_allow_na(input, values = c("female","male", NA))
#'
#' # No error as input is not NA
#' input = "male"
#' arg_match0_allow_na(input, values = c("female","male", NA))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_abort}}
#' @rdname arg_match0_no_na_error_message
#' @export
arg_match0_no_na_error_message <- function(
    arg,
    values,
    arg_nm = rlang::caller_arg(arg),
    error_call = rlang::caller_env()
  )
{

  if (!any(is.na(values)) && is.na(arg)) {
    quoted_list <- oxford_comma(chr_quoted(values, "\""))
    cli::cli_abort(
      message = c(
        "{.arg {arg_nm}} must be one of {quoted_list} not {.val {arg}}."
      ),
      call = error_call
    )
  }

  return(invisible(NULL))

}

#' @title Match an argument to a \code{TRUE} or \code{FALSE} vector but skip \code{NA}
#' @description This is equivalent to \code{\link[rlang]{arg_match}} but a
#' boolean variable is needed and skip \code{NA}.
#' @param allow_na Input boolean to determine if \code{NA} or \code{NaN} is allowed.
#' Default: \code{TRUE}
#' @inheritParams rlang::arg_match
#' @examples
#' # No error
#' input = TRUE
#' arg_match0_true_or_false(input)
#'
#' # Allow NA
#' input = NA
#' arg_match0_true_or_false(input)
#'
#' # Error as 0 is not TRUE or FALSE
#' input = 0
#' try(arg_match0_true_or_false(input))
#'
#' # Error as 1 is not TRUE or FALSE
#' input = 1
#' try(arg_match0_true_or_false(input))
#'
#' # Error as NULL is not TRUE or FALSE
#' input = NULL
#' try(arg_match0_true_or_false(input))
#'
#' # Error as NA is not TRUE or FALSE and allow_na is FALSE
#' input = NA
#' try(arg_match0_true_or_false(input, allow_na = FALSE))
#' @return The \code{TRUE} or \code{FALSE} value supplied to \code{arg}.
#' @rdname arg_match0_true_or_false
#' @export
arg_match0_true_or_false <-  function(
    arg,
    allow_na = TRUE,
    arg_nm = rlang::caller_arg(arg),
    error_call = rlang::caller_env()
)
{

  if (is.null(arg) & isTRUE(allow_na)) {
    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be TRUE, FALSE, NA or NaN.
        It is currently of type {.cls {class(arg)}}"
      ),
      call = error_call
    )
  }

  if (is.null(arg) & isFALSE(allow_na)) {
    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be TRUE or FALSE.
        It is currently of type {.cls {class(arg)}}"
      ),
      call = error_call
    )
  }

  if (is.na(arg) & isFALSE(allow_na) ) {

    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be must be TRUE or FALSE.
        It is currently {.val {arg}}."
      ),
      call = error_call
    )
  }

  if (!isTRUE(arg) & !isFALSE(arg) & !is.na(arg)) {

    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be TRUE, FALSE, NA or NaN.
        It is currently {.val {arg}}."
      ),
      call = error_call
    )
  }

  return(arg)

}


#' @title Match an argument to a integer vector but skip \code{NA}
#' @description This is equivalent to \code{\link[rlang]{arg_match}} but an
#' integer variable is needed and skip \code{NA}.
#' @param allow_na Input boolean to determine if \code{NA} or \code{NaN} is allowed.
#' Default: \code{TRUE}
#' @inheritParams rlang::arg_match
#' @examples
#' # No error
#' input = 5
#' arg_match0_integer(input, values = c(0:5))
#'
#' # Allow NA
#' input = NA
#' arg_match0_integer(input, values = c(0:5))
#'
#' # Error as 0 is not within 0 and 5
#' input = 6
#' try(arg_match0_integer(input, values = c(0:5)))
#'
#' # Error as NULL is not within 0 and 5
#' input = NULL
#' try(arg_match0_integer(input, values = c(0:5)))
#'
#' # Error as NA is not within 0 and 5 and allow_na is FALSE
#' input = NA
#' try(arg_match0_integer(input, values = c(0:5), allow_na = FALSE))
#' @return The integer supplied to \code{arg}.
#' @rdname arg_match0_integer
#' @export
arg_match0_integer <-  function(
    arg,
    values,
    allow_na = TRUE,
    arg_nm = rlang::caller_arg(arg),
    error_call = rlang::caller_env()
)
{

  if (is.null(arg) & isFALSE(allow_na) ) {
    values_text <- cli::cli_vec(
      c(values),
      style = list("vec-last" = " or ")
    )

    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be {.val {values_text}}.
        It is currently of type {.cls {class(arg)}}"
      ),
      call = error_call
    )
  }

  if (is.null(arg) & isTRUE(allow_na) ) {
    values_text <- cli::cli_vec(
      c(values, NA, NaN),
      style = list("vec-last" = " or ")
    )

    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be {.val {values_text}}.
        It is currently of type {.cls {class(arg)}}"
      ),
      call = error_call
    )
  }

  if (is.na(arg) & isFALSE(allow_na) ) {
    values_text <- cli::cli_vec(
      values,
      style = list("vec-last" = " or ")
    )

    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be must be {.val {values_text}}.
        It is currently {.val {arg}}."
      ),
      call = error_call
    )
  }

  if (!(arg %in% values) & !is.na(arg)) {

    values_text <- cli::cli_vec(
      c(values, NA, NaN),
      style = list("vec-last" = " or ")
    )

    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be {.val {values_text}}.
        It is currently {.val {arg}}."
      ),
      call = error_call
    )
  }

  return(arg)

}

#' @title Check If Numeric
#' @description Check if the input variable is numeric
#' @inheritParams rlang::args_error_context
#' @param x Input variable to check if it is numeric
#' @param allow_na Input boolean to determine if \code{NA} or \code{NaN} is allowed.
#' Default: \code{TRUE}
#' @return The variable itself or an error message if variable is not numeric
#' @examples
#' # No error
#' input = 1
#' try(check_if_numeric(input))
#'
#' # Error as "5" is not numeric
#' input = "5"
#' try(check_if_numeric(input))
#'
#' # Error as NULL is not numeric
#' input = NULL
#' try(check_if_numeric(input))
#'
#' # Error as NA is not numeric and allow_na is FALSE
#' input = NA
#' try(check_if_numeric(input, allow_na = FALSE))
#'
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_abort}}
#' @rdname check_if_numeric
#' @export
check_if_numeric <- function(
    x,
    allow_na = TRUE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {

  if (is.null(x)) {
    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg}}, must be {.cls numeric}, `NA` or `NaN`.
        It is currently of type {.cls {class(x)}}"
      ),
      call = call
    )
  }

  if (is.na(x) & isFALSE(allow_na) ) {
    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg}}, must be {.cls numeric}.
        It is currently {.val {x}} of type {.cls {class(x)}}"
      ),
      call = call
    )
  }

  if (!is.numeric(x) & !is.na(x)) {
    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg}}, must be {.cls numeric}, `NA` or `NaN`.
        It is currently {.val {x}} of type {.cls {class(x)}}"
      ),
      call = call
    )
  }

}

#' @title Check If Positive
#' @description Check if the input variable is a positive number
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::abort
#' @param x Input variable to check if it is positive number
#' @param allow_na Input boolean to determine if \code{NA} or \code{NaN} is allowed.
#' Default: \code{TRUE}
#' @return The variable itself or an error message if variable is not positive
#' @examples
#' # No error
#' input = 1
#' try(check_if_positive(input))
#'
#' # Error as 0 is not a positive number
#' input = 0
#' try(check_if_positive(input))
#'
#' # Error as -5 is not a positive number
#' input = -5
#' try(check_if_positive(input))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_abort}}
#' @rdname check_if_positive
#' @export
check_if_positive <- function(
    x,
    allow_na = TRUE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {

  check_if_numeric(x, allow_na = allow_na)

  if (isTRUE(any(x <= 0))) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be positive, not {.val {x}}"
      ),
      call = call
    )
  }

}

#' @title Check If Non-Negative
#' @description Check if the input variable is a non-negative number
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::abort
#' @param x Input variable to check if it is non-negative number
#' @param allow_na Input boolean to determine if \code{NA} or \code{NaN} is allowed.
#' Default: \code{TRUE}
#' @return The variable itself or an error message if variable is not non-negative
#' @examples
#' # No error
#' input = 0
#' try(check_if_non_negative(input))
#'
#' # Error as -5 is not a non-neagtive number
#' input = -5
#' try(check_if_non_negative(input))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_abort}}
#' @rdname check_if_non_negative
#' @export
check_if_non_negative <- function(
    x,
    allow_na = TRUE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {

  check_if_numeric(x, allow_na = allow_na)

  if (isTRUE(any(x < 0))) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be non-negative, not {.val {x}}"
      ),
      call = call
    )
  }

}

#' @title Check If Integer
#' @description Check if the input variable is an integer
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::abort
#' @param x Input variable to check if it is an integer
#' @param allow_na Input boolean to determine if \code{NA} or \code{NaN} is allowed.
#' Default: \code{TRUE}
#' @return The variable itself or an error message if variable is not non-negative
#' @examples
#' # No error
#' input = 0
#' try(check_if_integer(input))
#'
#' # Error as 5.5 is not an integer
#' input = 5.5
#' try(check_if_integer(input))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_abort}}
#' @rdname check_if_integer
#' @export
check_if_integer <- function(
    x,
    allow_na = TRUE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {

  check_if_numeric(x, allow_na = allow_na)

  if (isFALSE(is_integer_value(input_value = x, allow_na = allow_na))) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be an integer, not {.val {x}}.
        Consider rounding the value to the nearest integer using
        {.href [janitor::round_half_up](https://sfirke.github.io/janitor/reference/round_half_up.html)}
        and convert the value to type {.cls integer} using
        {.href [base::as.integer](https://stat.ethz.ch/R-manual/R-devel/library/base/html/integer.html)}
        before using the function."
      ),
      call = call
    )
  }

}

#' @title Check If Four Categories Are Mutually Exclusive
#' @description Check if the four input categories are mutually exclusive from each other
#' @inheritParams rlang::args_error_context
#' @param label_cat_1 First vector to check for mutually exclusiveness with another vector.
#' @param label_cat_2 Second vector to check for mutually exclusiveness with another vector.
#' @param label_cat_3 Third vector to check for mutually exclusiveness with another vector
#' @param label_cat_4 Fourth vector to check for mutually exclusiveness with another vector
#' @param label_cat_missing Missing values vector to check for mutually exclusiveness with another vector
#' if needed.
#' Default: NULL
#' @param arg_cat_1 An argument name as a string for the first vector.
#' This argument will be mentioned in error messages as the input that is at the origin of a problem.
#' @param arg_cat_2 An argument name as a string for the second vector.
#' This argument will be mentioned in error messages as the input that is at the origin of a problem.
#' @param arg_cat_3 An argument name as a string for the third vector.
#' This argument will be mentioned in error messages as the input that is at the origin of a problem.
#' @param arg_cat_4 An argument name as a string for the fourth vector.
#' This argument will be mentioned in error messages as the input that is at the origin of a problem.
#' @param arg_cat_missing An argument name as a string for the missing values vector.
#' This argument will be mentioned in error messages as the input that is at the origin of a problem.
#' @return An error message if the four input categories are not mutually exclusive
#' @examples
#' # No error
#' cat_1 <- c("no chest pain")
#' cat_2 <- c("typical")
#' cat_3 <- c("atypical")
#' cat_4 <- c("nonanginal")
#' cat_missing <- c("NA")
#' check_if_four_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_4)
#' check_if_four_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_4, cat_missing)
#'
#' # Common labels found
#' cat_1 <- c("no chest pain","typical", "atypical", "nonanginal")
#' cat_2 <- c("no chest pain","typical", "atypical", "nonanginal")
#' cat_3 <- c("no chest pain","typical", "atypical", "nonanginal")
#' cat_4 <- c("no chest pain","typical", "atypical", "nonanginal")
#' cat_missing <- c("no chest pain","typical", "atypical", "nonanginal")
#'
#' try(check_if_four_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_4))
#'
#' try(check_if_four_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_4, cat_missing))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_vec}}, \code{\link[cli]{cli_abort}}
#' @rdname check_if_four_categories_are_mutually_exclusive
#' @export
check_if_four_categories_are_mutually_exclusive <- function(
    label_cat_1,
    label_cat_2,
    label_cat_3,
    label_cat_4,
    label_cat_missing = NULL,
    arg_cat_1 = rlang::caller_arg(label_cat_1),
    arg_cat_2 = rlang::caller_arg(label_cat_2),
    arg_cat_3 = rlang::caller_arg(label_cat_3),
    arg_cat_4 = rlang::caller_arg(label_cat_4),
    arg_cat_missing = rlang::caller_arg(label_cat_missing),
    call = rlang::caller_env()
) {

  # Check intersection of label_cat_1 and label_cat_2
  intersect_cat1_cat2 <- intersect(label_cat_1, label_cat_2)

  intersect_cat1_cat2_text <- cli::cli_vec(
    c(intersect_cat1_cat2),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_1 and label_cat_3
  intersect_cat1_cat3 <- intersect(label_cat_1, label_cat_3)

  intersect_cat1_cat3_text <- cli::cli_vec(
    c(intersect_cat1_cat3),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_1 and label_cat_4
  intersect_cat1_cat4 <- intersect(label_cat_1, label_cat_4)

  intersect_cat1_cat4_text <- cli::cli_vec(
    c(intersect_cat1_cat4),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_2 and label_cat_3
  intersect_cat2_cat3 <- intersect(label_cat_2, label_cat_3)

  intersect_cat2_cat3_text <- cli::cli_vec(
    c(intersect_cat2_cat3),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_2 and label_cat_4
  intersect_cat2_cat4 <- intersect(label_cat_2, label_cat_4)

  intersect_cat2_cat4_text <- cli::cli_vec(
    c(intersect_cat2_cat4),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_3 and label_cat_4
  intersect_cat3_cat4 <- intersect(label_cat_3, label_cat_4)

  intersect_cat3_cat4_text <- cli::cli_vec(
    c(intersect_cat3_cat4),
    style = list("vec-last" = " and ")
  )

  error_message <- c()

  if (isTRUE(length(intersect_cat1_cat2) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_2}}:
      {.val {intersect_cat1_cat2_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat3) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_3}}:
      {.val {intersect_cat1_cat3_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat4) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_4}}:
      {.val {intersect_cat1_cat4_text}}."
    )
  }

  if (isTRUE(length(intersect_cat2_cat3) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_2}} and {.arg {arg_cat_3}}:
      {.val {intersect_cat2_cat3_text}}."
    )
  }

  if (isTRUE(length(intersect_cat2_cat4) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_2}} and {.arg {arg_cat_4}}:
      {.val {intersect_cat2_cat4_text}}."
    )
  }

  if (isTRUE(length(intersect_cat3_cat4) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_3}} and {.arg {arg_cat_4}}:
      {.val {intersect_cat3_cat4_text}}."
    )
  }

  if ((isTRUE(length(intersect_cat1_cat2) != 0) ||
       isTRUE(length(intersect_cat1_cat3) != 0) ||
       isTRUE(length(intersect_cat1_cat4) != 0) ||
       isTRUE(length(intersect_cat2_cat3) != 0) ||
       isTRUE(length(intersect_cat2_cat4) != 0) ||
       isTRUE(length(intersect_cat3_cat4) != 0)
  ) &&
  is.null(label_cat_missing)) {

    cli::cli_abort(
      message = c(
        "{.arg {arg_cat_1}}, {.arg {arg_cat_2}}, {.arg {arg_cat_3}} and {.arg {arg_cat_4}} must be mutually exclusive.",
        error_message,
        "Please ensure {.arg {arg_cat_1}}, {.arg {arg_cat_2}}, {.arg {arg_cat_3}} and {.arg {arg_cat_4}} do not hold common values."
      ),
      call = call
    )
  }

  if (is.null(label_cat_missing)) {
    return(invisible(NULL))
  }

  # Check intersection of label_cat_1 and label_cat_missing
  intersect_cat1_missing <- intersect(label_cat_1, label_cat_missing)

  intersect_cat1_missing_text <- cli::cli_vec(
    c(intersect_cat1_missing),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_2 and label_cat_missing
  intersect_cat2_missing <- intersect(label_cat_2, label_cat_missing)

  intersect_cat2_missing_text <- cli::cli_vec(
    c(intersect_cat2_missing),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_3 and label_cat_missing
  intersect_cat3_missing <- intersect(label_cat_3, label_cat_missing)

  intersect_cat3_missing_text <- cli::cli_vec(
    c(intersect_cat3_missing),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_4 and label_cat_missing
  intersect_cat4_missing <- intersect(label_cat_4, label_cat_missing)

  intersect_cat4_missing_text <- cli::cli_vec(
    c(intersect_cat4_missing),
    style = list("vec-last" = " and ")
  )

  if (isTRUE(length(intersect_cat1_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat1_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat2_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_2}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat2_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat3_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_3}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat3_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat4_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_4}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat4_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat2) != 0) ||
      isTRUE(length(intersect_cat1_cat3) != 0) ||
      isTRUE(length(intersect_cat1_cat4) != 0) ||
      isTRUE(length(intersect_cat2_cat3) != 0) ||
      isTRUE(length(intersect_cat2_cat4) != 0) ||
      isTRUE(length(intersect_cat3_cat4) != 0) ||
      isTRUE(length(intersect_cat1_missing) != 0) ||
      isTRUE(length(intersect_cat2_missing) != 0) ||
      isTRUE(length(intersect_cat3_missing) != 0) ||
      isTRUE(length(intersect_cat4_missing) != 0)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg_cat_1}}, {.arg {arg_cat_2}}, {.arg {arg_cat_3}}, {.arg {arg_cat_4}} and {.arg {arg_cat_missing}} must be mutually exclusive.",
        error_message,
        "Please ensure {.arg {arg_cat_1}}, {.arg {arg_cat_2}}, {.arg {arg_cat_3}}, {.arg {arg_cat_4}} and {.arg {arg_cat_missing}} do not hold common values."
      ),
      call = call
    )
  }

  return(invisible(NULL))

}

#' @title Check If Three Categories Are Mutually Exclusive
#' @description Check if the three input categories are mutually exclusive from each other
#' @inheritParams rlang::args_error_context
#' @inheritParams check_if_four_categories_are_mutually_exclusive
#' @return An error message if the three input categories are not mutually exclusive
#' @examples
#' # No error
#' cat_1 <- c("typical")
#' cat_2 <- c("atypical")
#' cat_3 <- c("nonanginal")
#' cat_missing <- c("NA")
#' check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3)
#' check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_missing)
#'
#' # Common labels found
#' cat_1 <- c("typical", "atypical", "nonanginal", "NA")
#' cat_2 <- c("typical", "atypical", "nonanginal", "NA")
#' cat_3 <- c("typical", "atypical", "nonanginal", "NA")
#' cat_missing <- c("typical", "atypical", "nonanginal", "NA")
#'
#' try(check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3))
#'
#' try(check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_missing))
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_vec}}, \code{\link[cli]{cli_abort}}
#' @rdname check_if_three_categories_are_mutually_exclusive
#' @export
check_if_three_categories_are_mutually_exclusive <- function(
    label_cat_1,
    label_cat_2,
    label_cat_3,
    label_cat_missing = NULL,
    arg_cat_1 = rlang::caller_arg(label_cat_1),
    arg_cat_2 = rlang::caller_arg(label_cat_2),
    arg_cat_3 = rlang::caller_arg(label_cat_3),
    arg_cat_missing = rlang::caller_arg(label_cat_missing),
    call = rlang::caller_env()
) {

  # Check intersection of label_cat_1 and label_cat_2
  intersect_cat1_cat2 <- intersect(label_cat_1, label_cat_2)

  intersect_cat1_cat2_text <- cli::cli_vec(
    c(intersect_cat1_cat2),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_1 and label_cat_3
  intersect_cat1_cat3 <- intersect(label_cat_1, label_cat_3)

  intersect_cat1_cat3_text <- cli::cli_vec(
    c(intersect_cat1_cat3),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_2 and label_cat_3
  intersect_cat2_cat3 <- intersect(label_cat_2, label_cat_3)

  intersect_cat2_cat3_text <- cli::cli_vec(
    c(intersect_cat2_cat3),
    style = list("vec-last" = " and ")
  )

  error_message <- c()

  if (isTRUE(length(intersect_cat1_cat2) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_2}}:
      {.val {intersect_cat1_cat2_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat3) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_3}}:
      {.val {intersect_cat1_cat3_text}}."
    )
  }

  if (isTRUE(length(intersect_cat2_cat3) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_2}} and {.arg {arg_cat_3}}:
      {.val {intersect_cat2_cat3_text}}."
    )
  }

  if ((isTRUE(length(intersect_cat1_cat2) != 0) ||
       isTRUE(length(intersect_cat1_cat3) != 0) ||
       isTRUE(length(intersect_cat2_cat3) != 0)
  ) &&
  is.null(label_cat_missing)) {

    cli::cli_abort(
      message = c(
        "{.arg {arg_cat_1}}, {.arg {arg_cat_2}} and {.arg {arg_cat_3}} must be mutually exclusive.",
        error_message,
        "Please ensure {.arg {arg_cat_1}}, {.arg {arg_cat_2}} and {.arg {arg_cat_3}} do not hold common values."
      ),
      call = call
    )
  }

  if (is.null(label_cat_missing)) {
    return(invisible(NULL))
  }

  # Check intersection of label_cat_1 and label_cat_missing
  intersect_cat1_missing <- intersect(label_cat_1, label_cat_missing)

  intersect_cat1_missing_text <- cli::cli_vec(
    c(intersect_cat1_missing),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_2 and label_cat_missing
  intersect_cat2_missing <- intersect(label_cat_2, label_cat_missing)

  intersect_cat2_missing_text <- cli::cli_vec(
    c(intersect_cat2_missing),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_3 and label_cat_missing
  intersect_cat3_missing <- intersect(label_cat_3, label_cat_missing)

  intersect_cat3_missing_text <- cli::cli_vec(
    c(intersect_cat3_missing),
    style = list("vec-last" = " and ")
  )

  if (isTRUE(length(intersect_cat1_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat1_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat2_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_2}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat2_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat3_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_3}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat2_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat2) != 0) ||
      isTRUE(length(intersect_cat1_cat3) != 0) ||
      isTRUE(length(intersect_cat2_cat3) != 0) ||
      isTRUE(length(intersect_cat1_missing) != 0) ||
      isTRUE(length(intersect_cat2_missing) != 0) ||
      isTRUE(length(intersect_cat3_missing) != 0)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg_cat_1}}, {.arg {arg_cat_2}}, {.arg {arg_cat_3}} and {.arg {arg_cat_missing}} must be mutually exclusive.",
        error_message,
        "Please ensure {.arg {arg_cat_1}}, {.arg {arg_cat_2}}, {.arg {arg_cat_3}} and {.arg {arg_cat_missing}} do not hold common values."
      ),
      call = call
    )
  }

  return(invisible(NULL))

}

#' @title Check If Two Categories Are Mutually Exclusive
#' @description Check if the two input categories are mutually exclusive from each other
#' @inheritParams rlang::args_error_context
#' @inheritParams check_if_four_categories_are_mutually_exclusive
#' @return An error message if the two input categories are not mutually exclusive
#' @seealso
#'  \code{\link[rlang]{caller_arg}}, \code{\link[rlang]{stack}}
#'  \code{\link[cli]{cli_vec}}, \code{\link[cli]{cli_abort}}
#' @rdname check_if_two_categories_are_mutually_exclusive
#' @examples
#' # No error
#' cat_1 <- c("male")
#' cat_2 <- c("female")
#' cat_missing <- c("not saying")
#' check_if_two_categories_are_mutually_exclusive(cat_1, cat_2)
#' check_if_two_categories_are_mutually_exclusive(cat_1, cat_2, cat_missing)
#'
#' # Common labels found
#' cat_1 <- c("male", "female", "not saying")
#' cat_2 <- c("male", "female", "not saying")
#' cat_missing <- c("male", "female", "not saying")
#'
#' try(check_if_two_categories_are_mutually_exclusive(cat_1, cat_2))
#'
#' try(check_if_two_categories_are_mutually_exclusive(cat_1, cat_2, cat_missing))
#' @export
check_if_two_categories_are_mutually_exclusive <- function(
    label_cat_1,
    label_cat_2,
    label_cat_missing = NULL,
    arg_cat_1 = rlang::caller_arg(label_cat_1),
    arg_cat_2 = rlang::caller_arg(label_cat_2),
    arg_cat_missing = rlang::caller_arg(label_cat_missing),
    call = rlang::caller_env()
) {

  # Check intersection of label_cat_1 and label_cat_2
  intersect_cat1_cat2 <- intersect(label_cat_1, label_cat_2)

  intersect_cat1_cat2_text <- cli::cli_vec(
    c(intersect_cat1_cat2),
    style = list("vec-last" = " and ")
  )

  error_message <- c()

  if (isTRUE(length(intersect_cat1_cat2) != 0)) {

    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_2}}:
      {.val {intersect_cat1_cat2_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat2) != 0) && is.null(label_cat_missing)) {

    cli::cli_abort(
      message = c(
        "{.arg {arg_cat_1}} and {.arg {arg_cat_2}} must be mutually exclusive.",
        error_message,
        "Please ensure {.arg {arg_cat_1}} and {.arg {arg_cat_2}} do not hold common values."
      ),
      call = call
    )
  }

  if (is.null(label_cat_missing)) {
    return(invisible(NULL))
  }

  # Check intersection of label_cat_1 and label_cat_missing
  intersect_cat1_missing <- intersect(label_cat_1, label_cat_missing)

  intersect_cat1_missing_text <- cli::cli_vec(
    c(intersect_cat1_missing),
    style = list("vec-last" = " and ")
  )

  # Check intersection of label_cat_2 and label_cat_missing
  intersect_cat2_missing <- intersect(label_cat_2, label_cat_missing)

  intersect_cat2_missing_text <- cli::cli_vec(
    c(intersect_cat2_missing),
    style = list("vec-last" = " and ")
  )

  if (isTRUE(length(intersect_cat1_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_1}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat1_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat2_missing) != 0)) {
    error_message <- c(
      error_message,
      "Common values found in {.arg {arg_cat_2}} and {.arg {arg_cat_missing}}:
      {.val {intersect_cat2_missing_text}}."
    )
  }

  if (isTRUE(length(intersect_cat1_cat2) != 0) ||
      isTRUE(length(intersect_cat1_missing) != 0) ||
      isTRUE(length(intersect_cat2_missing) != 0)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg_cat_1}}, {.arg {arg_cat_2}} and {.arg {arg_cat_missing}} must be mutually exclusive.",
        error_message,
        "Please ensure {.arg {arg_cat_1}}, {.arg {arg_cat_2}} and {.arg {arg_cat_missing}} do not hold common values."
      ),
      call = call
    )
  }

  return(invisible(NULL))

}
