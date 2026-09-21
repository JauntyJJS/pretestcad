# Check If Positive

Check if the input variable is a positive number

## Usage

``` r
check_if_positive(
  x,
  allow_na = TRUE,
  arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
)
```

## Arguments

- x:

  Input variable to check if it is positive number

- allow_na:

  Input boolean to determine if `NA` or `NaN` is allowed. Default:
  `TRUE`

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

The variable itself or an error message if variable is not positive

## See also

[`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.html),
[`stack`](https://rlang.r-lib.org/reference/stack.html)
[`cli_abort`](https://cli.r-lib.org/reference/cli_abort.html)

## Examples

``` r
# No error
input = 1
try(check_if_positive(input))

# Error as 0 is not a positive number
input = 0
try(check_if_positive(input))
#> Error in eval(expr, envir) : 
#>   `input` must be positive, not 0

# Error as -5 is not a positive number
input = -5
try(check_if_positive(input))
#> Error in eval(expr, envir) : 
#>   `input` must be positive, not -5
```
