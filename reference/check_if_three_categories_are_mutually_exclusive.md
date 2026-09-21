# Check If Three Categories Are Mutually Exclusive

Check if the three input categories are mutually exclusive from each
other

## Usage

``` r
check_if_three_categories_are_mutually_exclusive(
  label_cat_1,
  label_cat_2,
  label_cat_3,
  label_cat_missing = NULL,
  arg_cat_1 = rlang::caller_arg(label_cat_1),
  arg_cat_2 = rlang::caller_arg(label_cat_2),
  arg_cat_3 = rlang::caller_arg(label_cat_3),
  arg_cat_missing = rlang::caller_arg(label_cat_missing),
  error_call = rlang::caller_env()
)
```

## Arguments

- label_cat_1:

  First vector to check for mutually exclusiveness with another vector.

- label_cat_2:

  Second vector to check for mutually exclusiveness with another vector.

- label_cat_3:

  Third vector to check for mutually exclusiveness with another vector

- label_cat_missing:

  Missing values vector to check for mutually exclusiveness with another
  vector if needed. Default: NULL

- arg_cat_1:

  An argument name as a string for the first vector. This argument will
  be mentioned in error messages as the input that is at the origin of a
  problem.

- arg_cat_2:

  An argument name as a string for the second vector. This argument will
  be mentioned in error messages as the input that is at the origin of a
  problem.

- arg_cat_3:

  An argument name as a string for the third vector. This argument will
  be mentioned in error messages as the input that is at the origin of a
  problem.

- arg_cat_missing:

  An argument name as a string for the missing values vector. This
  argument will be mentioned in error messages as the input that is at
  the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An error message if the three input categories are not mutually
exclusive

## See also

[`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.html),
[`stack`](https://rlang.r-lib.org/reference/stack.html)
[`cli_vec`](https://cli.r-lib.org/reference/cli_vec.html),
[`cli_abort`](https://cli.r-lib.org/reference/cli_abort.html)

## Examples

``` r
# No error
cat_1 <- c("typical")
cat_2 <- c("atypical")
cat_3 <- c("nonanginal")
cat_missing <- c("NA")
check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3)
check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_missing)

# Common labels found
cat_1 <- c("typical", "atypical", "nonanginal", "NA")
cat_2 <- c("typical", "atypical", "nonanginal", "NA")
cat_3 <- c("typical", "atypical", "nonanginal", "NA")
cat_missing <- c("typical", "atypical", "nonanginal", "NA")

try(check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3))
#> Error in eval(expr, envir) : 
#>   `cat_1`, `cat_2` and `cat_3` must be mutually exclusive.
#> Common values found in `cat_1` and `cat_2`: "typical", "atypical", "nonanginal"
#> and "NA".
#> Common values found in `cat_1` and `cat_3`: "typical", "atypical", "nonanginal"
#> and "NA".
#> Common values found in `cat_2` and `cat_3`: "typical", "atypical", "nonanginal"
#> and "NA".
#> Please ensure `cat_1`, `cat_2` and `cat_3` do not hold common values.

try(check_if_three_categories_are_mutually_exclusive(cat_1, cat_2, cat_3, cat_missing))
#> Error in eval(expr, envir) : 
#>   `cat_1`, `cat_2`, `cat_3` and `cat_missing` must be mutually exclusive.
#> Common values found in `cat_1` and `cat_2`: "typical", "atypical", "nonanginal"
#> and "NA".
#> Common values found in `cat_1` and `cat_3`: "typical", "atypical", "nonanginal"
#> and "NA".
#> Common values found in `cat_2` and `cat_3`: "typical", "atypical", "nonanginal"
#> and "NA".
#> Common values found in `cat_1` and `cat_missing`: "typical", "atypical",
#> "nonanginal" and "NA".
#> Common values found in `cat_2` and `cat_missing`: "typical", "atypical",
#> "nonanginal" and "NA".
#> Common values found in `cat_3` and `cat_missing`: "typical", "atypical",
#> "nonanginal" and "NA".
#> Please ensure `cat_1`, `cat_2`, `cat_3` and `cat_missing` do not hold common
#> values.
```
