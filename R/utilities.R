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

  arg <- ifelse(
    test = is.na(arg),
    yes = arg,
    no = rlang::arg_match0(arg = arg, values = values, arg_nm = arg_nm, error_call = error_call)
  )

  return(arg)
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
#' @inheritParams rlang::abort
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


# sex_are_mutually_exclusive <- function(
#     label_sex_male,
#     label_sex_female,
#     label_sex_missing = NULL,
#     arg_sex_male = rlang::caller_arg(label_sex_male),
#     arg_sex_female = rlang::caller_arg(label_sex_female),
#     arg_sex_missing = rlang::caller_arg(label_sex_missing),
#     call = rlang::caller_env()
# ) {
#
#   # Check intersection of label_sex_male and label_sex_female
#   intersect_male_female <- intersect(label_sex_male, label_sex_female)
#
#   intersect_male_female_text <- cli::cli_vec(
#     c(intersect_male_female),
#     style = list("vec-last" = " and ")
#   )
#
#   if (isTRUE(length(intersect_male_female) != 0) && is.null(label_sex_missing)) {
#
#     cli::cli_abort(
#       message = c(
#         "{.arg {arg_sex_male}} and {.arg {arg_sex_female}} must be mutally exclusive.
#
#         Common values found: {.val {intersect_male_female_text}}.
#
#         Please ensure {.arg {arg_sex_male}} and {.arg {arg_sex_female}} do not hold common values."
#       ),
#       call = call
#     )
#   }
#
# }
#
# sex_male <- c("male", "female", "F")
# sex_female <- c("female", "male", "F")
#
# sex_are_mutually_exclusive(
#   label_sex_male = sex_male,
#   label_sex_female = sex_female
# )

