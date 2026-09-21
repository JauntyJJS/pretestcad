# Match an argument to a character vector but skip `NA`

This is equivalent to
[`arg_match`](https://rlang.r-lib.org/reference/arg_match.html) but skip
`NA`

## Usage

``` r
arg_match0_allow_na(
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

The string supplied to `arg`.

## See also

[`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.html),
[`stack`](https://rlang.r-lib.org/reference/stack.html),
[`arg_match`](https://rlang.r-lib.org/reference/arg_match.html)

## Examples

``` r
# No error
input = "male"
arg_match0_allow_na(input, values = c("female","male"))
#> [1] "male"

# Allow NA
input = NA
arg_match0_allow_na(input, values = c("female","male"))
#> [1] NA

# Error as M is not female or male
input = "emale"
try(arg_match0_allow_na(input, values = c("female","male")))
#> Error in eval(expr, envir) : 
#>   `input` must be one of "female" or "male", not "emale".
#> ℹ Did you mean "female"?
```
