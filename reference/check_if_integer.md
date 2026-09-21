# Check If Integer

Check if the input variable is an integer

## Usage

``` r
check_if_integer(
  x,
  allow_na = TRUE,
  arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
)
```

## Arguments

- x:

  Input variable to check if it is an integer

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

The variable itself or an error message if variable is not non-negative

## See also

[`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.html),
[`stack`](https://rlang.r-lib.org/reference/stack.html)
[`cli_abort`](https://cli.r-lib.org/reference/cli_abort.html)

## Examples

``` r
# No error
input = 0
try(check_if_integer(input))

# Error as 5.5 is not an integer
input = 5.5
try(check_if_integer(input))
#> Error in eval(expr, envir) : 
#>   `input` must be an integer, not 5.5. Consider rounding the value to the
#> nearest integer using janitor::round_half_up
#> (<https://sfirke.github.io/janitor/reference/round_half_up.html>) and convert
#> the value to type <integer> using base::as.integer
#> (<https://stat.ethz.ch/R-manual/R-devel/library/base/html/integer.html>) before
#> using the function.
```
