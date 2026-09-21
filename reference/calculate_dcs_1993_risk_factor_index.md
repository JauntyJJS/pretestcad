# Calculate The Risk Factor Index For Duke Clinical Score 1993

A function used to calculate the patient's risk factor index. This is
used to calculate the likelihood of severe coronary artery disease in
the Duke Clinical Score 1993 paper.

## Usage

``` r
calculate_dcs_1993_risk_factor_index(
  have_hypertension,
  have_dyslipidemia,
  have_diabetes,
  max_na = 0,
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN),
  error_call = rlang::caller_env()
)
```

## Arguments

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- have_dyslipidemia:

  The value of variable in the parameters
  `label_have_dyslipidemia_no`,  
  `label_have_dyslipidemia_yes` and `label_have_dyslipidemia_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- max_na:

  Input integer 0 to 3 to indicate the maximum number of missing risk
  factors to tolerate before outputting an `NA`. Default: 0

- label_have_hypertension_no:

  Label(s) for patient with no hypertension. Default: `c("no")`

- label_have_hypertension_yes:

  Label(s) for patient having hypertension. Default: `c("yes")`

- label_have_hypertension_unknown:

  Label(s) for patient having unknown hypertension. Default:
  `c(NA, NaN)`

- label_have_dyslipidemia_no:

  Label(s) for patient with no dyslipidemia. Default: `c("no")`

- label_have_dyslipidemia_yes:

  Label(s) for patient having dyslipidemia. Default: `c("yes")`

- label_have_dyslipidemia_unknown:

  Label(s) for patient having unknown dyslipidemia. Default:
  `c(NA, NaN)`

- label_have_diabetes_no:

  Label(s) for patient with no diabetes. Default: `c("no")`

- label_have_diabetes_yes:

  Label(s) for patient having diabetes. Default: `c("yes")`

- label_have_diabetes_unknown:

  Label(s) for patient having unknown diabetes. Default: `c(NA, NaN)`

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer indicating the patient's risk factor index. It can also be
`NA` if the number of missing risk factors exceeds the `max_na` input
value.

## Examples

``` r
calculate_dcs_1993_risk_factor_index(
  have_hypertension = "yes",
  have_dyslipidemia = "yes",
  have_diabetes = "no"
)
#> [1] 2

calculate_dcs_1993_risk_factor_index(
  have_hypertension = NA,
  have_dyslipidemia = "yes",
  have_diabetes = "no",
  max_na = 0
)
#> [1] NA

calculate_dcs_1993_risk_factor_index(
  have_hypertension = NA,
  have_dyslipidemia = "yes",
  have_diabetes = "no",
  max_na = 1
)
#> [1] 1
```
