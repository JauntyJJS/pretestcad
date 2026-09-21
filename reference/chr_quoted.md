# Quoted Characters

Add a quote around characters.

## Usage

``` r
chr_quoted(chr, type = "`")
```

## Arguments

- chr:

  A character vector to add quotes in each element. (e.g. c("a", "b",
  "c")).

- type:

  Character to be used as a quote. Default: '\`'

## Value

A character vector with quotes added in each element. (e.g. c("`a`",
"`b`", "`c`")).

## Details

DETAILS

## Examples

``` r
chr_quoted(c("a","b", "c"))
#> [1] "`a`" "`b`" "`c`"
```
