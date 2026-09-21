# Is Integer Value

Function to check if the input value is an integer.

## Usage

``` r
is_integer_value(input_value, allow_na = FALSE)
```

## Arguments

- input_value:

  The input value

- allow_na:

  If true, NA values are ignored and output is considered TRUE. Default:
  FALSE

## Value

A boolean indicating TRUE when the input value is an integer and FALSE
otherwise.

## Examples

``` r

# An integer
is_integer_value(1)
#> [1] TRUE

# Not an integer
is_integer_value(1.1)
#> [1] FALSE

# Not numeric
is_integer_value("1")
#> [1] FALSE

# NA cases
is_integer_value(NA, allow_na = FALSE)
#> [1] FALSE
is_integer_value(NA, allow_na = TRUE)
#> [1] TRUE
```
