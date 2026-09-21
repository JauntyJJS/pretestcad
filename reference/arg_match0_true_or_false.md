# Match an argument to a `TRUE` or `FALSE` vector but skip `NA`

This is equivalent to
[`arg_match`](https://rlang.r-lib.org/reference/arg_match.html) but a
boolean variable is needed and skip `NA`.

## Usage

``` r
arg_match0_true_or_false(
  arg,
  allow_na = TRUE,
  arg_nm = rlang::caller_arg(arg),
  error_call = rlang::caller_env()
)
```

## Arguments

- arg:

  A symbol referring to an argument accepting strings.

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

The `TRUE` or `FALSE` value supplied to `arg`.

## Examples

``` r
# No error
input = TRUE
arg_match0_true_or_false(input)
#> [1] TRUE

# Allow NA
input = NA
arg_match0_true_or_false(input)
#> [1] NA

# Error as 0 is not TRUE or FALSE
input = 0
try(arg_match0_true_or_false(input))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be TRUE, FALSE, NA or NaN. It is currently
#> 0.

# Error as 1 is not TRUE or FALSE
input = 1
try(arg_match0_true_or_false(input))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be TRUE, FALSE, NA or NaN. It is currently
#> 1.

# Error as NULL is not TRUE or FALSE
input = NULL
try(arg_match0_true_or_false(input))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be TRUE, FALSE, NA or NaN. It is currently
#> of type <NULL>

# Error as NA is not TRUE or FALSE and allow_na is FALSE
input = NA
try(arg_match0_true_or_false(input, allow_na = FALSE))
#> Error in eval(expr, envir) : 
#>   Provided input `input`, must be must be TRUE or FALSE. It is currently
#> NA.
```
