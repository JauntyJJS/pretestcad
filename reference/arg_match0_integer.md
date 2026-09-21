# Match an argument to a integer vector but skip `NA`

This is equivalent to
[`arg_match`](https://rlang.r-lib.org/reference/arg_match.html) but an
integer variable is needed and skip `NA`.

## Usage

``` r
arg_match0_integer(
  arg,
  values,
  allow_na = TRUE,
  arg_nm = rlang::caller_arg(arg),
  error_call = rlang::caller_env()
)
```

## Arguments

- arg:

  A symbol referring to an argument accepting strings.

- values:

  A character vector of possible values that `arg` can take.

- allow_na:

  Input boolean to determine if `NA` or `NaN` is allowed. Default:
  `TRUE`

- arg_nm:

  Same as `error_arg`.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

The integer supplied to `arg`.

## Examples

``` r
# No error
input = 5
arg_match0_integer(input, values = c(0:5))
#> [1] 5

# Allow NA
input = NA
arg_match0_integer(input, values = c(0:5))
#> [1] NA

# Error as 0 is not within 0 and 5
input = 6
try(arg_match0_integer(input, values = c(0:5)))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be 0, 1, 2, 3, 4, 5, NA or NaN. It is
#> currently 6.

# Error as NULL is not within 0 and 5
input = NULL
try(arg_match0_integer(input, values = c(0:5)))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be 0, 1, 2, 3, 4, 5, NA or NaN. It is
#> currently of type <NULL>

# Error as NA is not within 0 and 5 and allow_na is FALSE
input = NA
try(arg_match0_integer(input, values = c(0:5), allow_na = FALSE))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be must be 0, 1, 2, 3, 4 or 5. It is
#> currently NA.
```
