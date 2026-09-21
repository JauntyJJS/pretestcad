# Check If Numeric

Check if the input variable is numeric

## Usage

``` r
check_if_numeric(
  x,
  allow_na = TRUE,
  arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
)
```

## Arguments

- x:

  Input variable to check if it is numeric

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

The variable itself or an error message if variable is not numeric

## See also

[`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.html),
[`stack`](https://rlang.r-lib.org/reference/stack.html)
[`cli_abort`](https://cli.r-lib.org/reference/cli_abort.html)

## Examples

``` r
# No error
input = 1
try(check_if_numeric(input))

# Error as "5" is not numeric
input = "5"
try(check_if_numeric(input))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be <numeric>, `NA` or `NaN`. It is
#> currently "5" of type <character>

# Error as NULL is not numeric
input = NULL
try(check_if_numeric(input))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be <numeric>, `NA` or `NaN`. It is
#> currently of type <NULL>

# Error as NA is not numeric and allow_na is FALSE
input = NA
try(check_if_numeric(input, allow_na = FALSE))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be <numeric>. It is currently NA of type
#> <logical>
```
