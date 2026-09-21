# Calculate Symptom Score (ESC 2024)

A function used to calculate the symptom score of the patient. This is
used to calculate the pretest probability of coronary artery disease
(CAD) based on the ESC 2024 guidelines.

## Usage

``` r
calculate_esc_2024_symptom_score(
  chest_pain_type,
  have_dyspnoea,
  allow_na = TRUE,
  label_have_dyspnoea_no = c("no"),
  label_have_dyspnoea_yes = c("yes"),
  label_have_dyspnoea_unknown = c(NA, NaN),
  label_cpt_no_chest_pain = c("no chest pain"),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_unknown = c(NA, NaN),
  error_call = rlang::caller_env()
)
```

## Arguments

- chest_pain_type:

  The value of variable in the parameters `label_cpt_no_chest_pain`,  
  `label_cpt_nonanginal`,  
  `label_cpt_atypical`,  
  `label_cpt_typical`,  
  and `label_cpt_unknown`.

- have_dyspnoea:

  The value of variable in the parameters `label_have_dyspnoea_no`,  
  `label_have_dyspnoea_yes` and `label_have_dyspnoea_unknown`.

- allow_na:

  A logical evaluating to `TRUE` or `FALSE`  
  indicating whether we can allow `chest_pain_type` or `have_dyspnoea`  
  to be `NA` when calculating the score. Default: `TRUE`

- label_have_dyspnoea_no:

  Label(s) for patient having no dyspnoea symptoms. Default: `c("no")`

- label_have_dyspnoea_yes:

  Label(s) for patient having dyspnoea symptoms. Default: `c("yes")`

- label_have_dyspnoea_unknown:

  Label(s) for patient having unknown dyspnoea symptoms. Default:
  `c(NA, NaN)`

- label_cpt_no_chest_pain:

  Label(s) for patient having no chest pain. Default:
  `c("no chest pain")`

- label_cpt_nonanginal:

  Label(s) for patient having nonanginal or non-specific chest pain.  
  Default: `c("nonanginal")`

- label_cpt_atypical:

  Label(s) for patient having atypical chest pain. Default:
  `c("atypical")`

- label_cpt_typical:

  Label(s) for patient having typical chest pain. Default:
  `c("typical")`

- label_cpt_unknown:

  Label(s) for patient having unknown chest pain type symptoms. Default:
  `c(NA, NaN)`

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer indicating the symptom score of the patient. It can also be
`NA` if both `chest_pain_type` and `have_dyspnoea` are `NA`. Patients
with both nonanginal chest pain and dyspnoea will be given a score of 2

## Examples

``` r
calculate_esc_2024_symptom_score(
  chest_pain_type = "nonanginal",
  have_dyspnoea = "yes",
  allow_na = TRUE
)
#> [1] 2

calculate_esc_2024_symptom_score(
  chest_pain_type = "nonanginal",
  have_dyspnoea = NA,
  allow_na = FALSE
)
#> [1] NA

calculate_esc_2024_symptom_score(
  chest_pain_type = "nonanginal",
  have_dyspnoea = NA,
  allow_na = TRUE
)
#> [1] 1
```
