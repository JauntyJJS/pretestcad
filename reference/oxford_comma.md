# Character Vector To List Phrase With Oxford Comma

A function that converts a character vector into a list phrase that uses
the Oxford comma.

## Usage

``` r
oxford_comma(chr, sep = ", ", final = "or")
```

## Arguments

- chr:

  A character vector to turn into a list phrase (e.g. c("a", "b", "c")).

- sep:

  Separator symbols used to separate the elements in the character
  vector, Default: ', '

- final:

  String to use in place of the final separator when we have at least
  two elements in the character vector, Default: 'or'.

## Value

A string in the form of a list that has a comma if there are at least
three elements in the list (e.g. "a, b, or c")

## Examples

``` r
oxford_comma(c("James", "John", "Jeremy"))
#> [1] "James, John, or Jeremy"

oxford_comma(c("James", "John", "Jeremy"), final = "and")
#> [1] "James, John, and Jeremy"

oxford_comma(c("James", "John"))
#> [1] "James or John"

oxford_comma(c("James"))
#> [1] "James"
```
