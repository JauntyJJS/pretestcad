# Harmonise Two Labels

Function to map an input from two different list into one of the two
standardise labels

## Usage

``` r
harmonise_two_labels(
  arg,
  label_one,
  label_two,
  label_unknown,
  harmonise_label_one = "no",
  harmonise_label_two = "yes",
  harmonise_label_unknown = NA,
  error_call = rlang::caller_env()
)
```

## Arguments

- arg:

  Input argument, in characters to be harmonised

- label_one:

  Input character vector representing the ways to  
  identify `harmonise_label_one`

- label_two:

  Input character vector representing the ways to  
  identify `harmonise_label_two`

- label_unknown:

  Input character vector representing the ways to  
  identify `harmonise_label_unknown`

- harmonise_label_one:

  Input character representing the harmonised label for `label_one`  
  Default: 'group_1'

- harmonise_label_two:

  Input character representing the harmonised label for `label_two`  
  Default: 'group_2'

- harmonise_label_unknown:

  Input character representing the harmonised label for
  `label_unknown`  
  Default: NA

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

Character representing one of the two standardise labels.

## Examples

``` r
label_have_dyspnoea_no <- c("no", "No")
label_have_dyspnoea_yes <- c("yes", "Yes")
label_have_dyspnoea_unknown <- c(NA, NaN)

# Gives harmonise_label_one if there is valid input of have_dyspnoea
have_dyspnoea <- "No"

harmonise_two_labels(
  arg = have_dyspnoea,
  label_one = label_have_dyspnoea_no,
  label_two = label_have_dyspnoea_yes,
  label_unknown = label_have_dyspnoea_unknown,
  harmonise_label_one = "no",
  harmonise_label_two = "yes",
  harmonise_label_unknown = NA
)
#> [1] "no"

# Gives harmonise_label_two if there is valid input of have_dyspnoea
have_dyspnoea <- "Yes"

harmonise_two_labels(
  arg = have_dyspnoea,
  label_one = label_have_dyspnoea_no,
  label_two = label_have_dyspnoea_yes,
  label_unknown = label_have_dyspnoea_unknown,
  harmonise_label_one = "no",
  harmonise_label_two = "yes",
  harmonise_label_unknown = NA
)
#> [1] "yes"

# Gives harmonise_label_unknown if there is valid missing input of have_dyspnoea
have_dyspnoea <- NaN

harmonise_two_labels(
  arg = have_dyspnoea,
  label_one = label_have_dyspnoea_no,
  label_two = label_have_dyspnoea_yes,
  label_unknown = label_have_dyspnoea_unknown,
  harmonise_label_one = "no",
  harmonise_label_two = "yes",
  harmonise_label_unknown = NA
)
#> [1] NA

# Gives harmonise_label_unknown if there is valid missing input of have_dyspnoea
have_dyspnoea <- "NIL"
label_have_dyspnoea_unknown <- c("NIL")

harmonise_two_labels(
  arg = have_dyspnoea,
  label_one = label_have_dyspnoea_no,
  label_two = label_have_dyspnoea_yes,
  label_unknown = label_have_dyspnoea_unknown,
  harmonise_label_one = "no",
  harmonise_label_two = "yes",
  harmonise_label_unknown = NA
)
#> [1] NA

# Gives error of invalid have_dyspnoea input with partial match
have_dyspnoea <- "Not"
label_have_dyspnoea_unknown <- c(NA, NaN)

try(harmonise_two_labels(
   arg = have_dyspnoea,
   label_one = label_have_dyspnoea_no,
   label_two = label_have_dyspnoea_yes,
   label_unknown = label_have_dyspnoea_unknown,
   harmonise_label_one = "no",
   harmonise_label_two = "yes",
   harmonise_label_unknown = NA
))
#> Error in eval(expr, envir) : 
#>   `have_dyspnoea` must be one of "no", "No", "yes", "Yes", "NA", or "NaN",
#> not "Not".
#> ℹ Did you mean "No"?

# Gives error of invalid have_dyspnoea input without partial match
have_dyspnoea <- "Something"
label_have_dyspnoea_unknown <- c(NA, NaN)

try(harmonise_two_labels(
   arg = have_dyspnoea,
   label_one = label_have_dyspnoea_no,
   label_two = label_have_dyspnoea_yes,
   label_unknown = label_have_dyspnoea_unknown,
   harmonise_label_one = "no",
   harmonise_label_two = "yes",
   harmonise_label_unknown = NA
))
#> Error in eval(expr, envir) : 
#>   `have_dyspnoea` must be one of "no", "No", "yes", "Yes", "NA", or "NaN",
#> not "Something".

# Gives error of invalid missing input of have_dyspnoea
have_dyspnoea <- NA
label_have_dyspnoea_unknown <- c("NIL")

try(harmonise_two_labels(
   arg = have_dyspnoea,
   label_one = label_have_dyspnoea_no,
   label_two = label_have_dyspnoea_yes,
   label_unknown = label_have_dyspnoea_unknown,
   harmonise_label_one = "no",
   harmonise_label_two = "yes",
   harmonise_label_unknown = NA
))
#> Error in eval(expr, envir) : 
#>   `have_dyspnoea` must be one of "no", "No", "yes", "Yes", or "NIL" not
#> NA.

```
