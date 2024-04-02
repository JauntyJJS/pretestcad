check_if_not_numeric <- function(x,
                                 arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      message = c(
        "Provided vector, {.arg {arg}}, must be {.cls numeric}, not {.cls {class(x)}}",
        "We see that {.run is.numeric({.arg {arg}})} returns {.cls {class(x)}}"
      ),
      call = call
    )
  }

}
