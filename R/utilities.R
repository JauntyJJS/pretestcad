#' @title Match an argument to a character vector but skip NA
#' @description This is equivalent to \code{\link[rlang]{arg_match}} but skip NA
#' @inheritParams rlang::arg_match
#' @return The string supplied to arg.
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

#' @title Match an argument to a TRUE or FALSE vector but skip NA
#' @description This is equivalent to \code{\link[rlang]{arg_match}} but an
#' integer vector is compared and skip NA.
#' @param allow_na Input boolean to determine if NA or NaN is allowed. Default: TRUE
#' @inheritParams rlang::arg_match
#' @return The TRUE or FALSE value supplied to arg.
#' @rdname arg_match0_true_or_false
#' @export
arg_match0_true_or_false <-  function(
    arg,
    allow_na = TRUE,
    arg_nm = rlang::caller_arg(arg),
    error_call = rlang::caller_env()
)
{

  if (is.null(arg)) {
    cli::cli_abort(
      message = c(
        "Provided input {.arg {arg_nm}}, must be TRUE, FALSE, NA or NaN.
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


#' @title Match an argument to a integer vector but skip NA
#' @description This is equivalent to \code{\link[rlang]{arg_match}} but an
#' integer vector is compared and skip NA.
#' @param allow_na Input boolean to determine if NA or NaN is allowed. Default: TRUE
#' @inheritParams rlang::arg_match
#' @return The integer supplied to arg.
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

  if (is.null(arg)) {
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
#' @param allow_na Input boolean to determine if NA or NaN is allowed. Default: TRUE
#' @return The variable itself or an error message if variable is not numeric
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
#' @param allow_na Input boolean to determine if NA or NaN is allowed. Default: TRUE
#' @return The variable itself or an error message if variable is not positive
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

#' @title Check If Non-negative
#' @description Check if the input variable is a non-negative number
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::abort
#' @param x Input variable to check if it is non-negative number
#' @param allow_na Input boolean to determine if NA or NaN is allowed. Default: TRUE
#' @return The variable itself or an error message if variable is not non-negative
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
