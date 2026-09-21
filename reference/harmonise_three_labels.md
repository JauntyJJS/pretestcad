# Harmonise Three Labels

Function to map an input from three different list into one of the three
standardise labels

## Usage

``` r
harmonise_three_labels(
  arg,
  label_one,
  label_two,
  label_three,
  label_unknown,
  harmonise_label_one = "group_1",
  harmonise_label_two = "group_2",
  harmonise_label_three = "group_3",
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

- label_three:

  Input character vector representing the ways to  
  identify `harmonise_label_three`

- label_unknown:

  Input character vector representing the ways to  
  identify `harmonise_label_unknown`

- harmonise_label_one:

  Input character representing the harmonised label for `label_one`  
  Default: 'group_1'

- harmonise_label_two:

  Input character representing the harmonised label for `label_two`  
  Default: 'group_2'

- harmonise_label_three:

  Input character representing the harmonised label for `label_three`  
  Default: 'group_3'

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

Character representing one of the three standardise labels.

## Examples

``` r
label_cpt_nonanginal <- c("nonanginal", "unspecified")
label_cpt_atypical <- c("atypical", "Atypical")
label_cpt_typical <- c("typical", "angina")
label_cpt_unknown <- c(NA, NaN)

# Gives harmonise_label_one if there is valid input of chest_pain_type
chest_pain_type <- "unspecified"

harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
)
#> [1] "nonanginal"

# Gives harmonise_label_two if there is valid input of chest_pain_type
chest_pain_type <- "Atypical"

harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
)
#> [1] "atypical"

# Gives harmonise_label_three if there is valid input of chest_pain_type
chest_pain_type <- "angina"

harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
)
#> [1] "typical"

# Gives harmonise_label_unknown if there is valid missing input of chest_pain_type
chest_pain_type <- NaN

harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
)
#> [1] NA

# Gives harmonise_label_unknown if there is valid missing input of chest_pain_type
chest_pain_type <- "NIL"
label_cpt_unknown <- c("NIL")

harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
)
#> [1] NA

# Gives error of invalid typical chest pain input with partial match
chest_pain_type <- "Typical"
label_cpt_unknown <- c(NA, NaN)

try(harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
))
#> Error in eval(expr, envir) : 
#>   `chest_pain_type` must be one of "nonanginal", "unspecified",
#> "atypical", "Atypical", "typical", "angina", "NA", or "NaN", not "Typical".
#> ℹ Did you mean "typical"?

# Gives error of invalid typical chest pain input without partial match
chest_pain_type <- "Something"
label_cpt_unknown <- c(NA, NaN)

try(harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
))
#> Error in eval(expr, envir) : 
#>   `chest_pain_type` must be one of "nonanginal", "unspecified",
#> "atypical", "Atypical", "typical", "angina", "NA", or "NaN", not "Something".

# Gives error of invalid missing input of chest_pain_type
chest_pain_type <- NA
label_cpt_unknown <- c("NIL")

try(harmonise_three_labels(
   arg = chest_pain_type,
   label_one = label_cpt_nonanginal,
   label_two = label_cpt_atypical,
   label_three = label_cpt_typical,
   label_unknown = label_cpt_unknown,
   harmonise_label_one = "nonanginal",
   harmonise_label_two = "atypical",
   harmonise_label_three = "typical",
   harmonise_label_unknown = NA
))
#> Error in eval(expr, envir) : 
#>   `chest_pain_type` must be one of "nonanginal", "unspecified",
#> "atypical", "Atypical", "typical", "angina", or "NIL" not NA.

```
