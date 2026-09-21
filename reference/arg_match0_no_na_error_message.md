# Error Message For `NA` Argument For Non-missing List

Provides an error message if the argument provided is `NA` if a
non-missing list is provided

## Usage

``` r
arg_match0_no_na_error_message(
  arg,
  values,
  arg_nm = rlang::caller_arg(arg),
  error_call = rlang::caller_env()
)
```

## Arguments

- arg:

  A symbol referring to an argument accepting strings.

- values:

  A character vector of possible values that `arg` can take.

- arg_nm:

  Same as `error_arg`.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An error message if the argument provided is `NA` if a non-missing list
is provided. Else if will return `NULL` invisibly, regardless if `arg`
has a match with the elements in `values` or not.

## See also

[`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.html),
[`stack`](https://rlang.r-lib.org/reference/stack.html)
[`cli_abort`](https://cli.r-lib.org/reference/cli_abort.html)

## Examples

``` r
# Error as input is NA but value list provided has no NA
input = NA
try(arg_match0_no_na_error_message(input, values = c("female","male")))
#> Error in eval(expr, envir) : 
#>   `input` must be one of "female" or "male" not NA.

# No error as value list provided has NA
input = NA
arg_match0_allow_na(input, values = c("female","male", NA))
#> [1] NA

# No error as input is not NA
input = "male"
arg_match0_allow_na(input, values = c("female","male", NA))
#> [1] "male"
```
