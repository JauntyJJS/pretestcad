# Calculate Number Of Risk Factors (CONFIRM 2015)

A function used to calculate the number of risk factors the patient has.
This is used to calculate the pretest probability of obstructive
coronary artery disease (CAD) based on the 2015 CONFIRM Risk Score.

## Usage

``` r
calculate_confirm_2015_num_of_rf(
  have_typical_chest_pain,
  have_diabetes,
  have_hypertension,
  have_family_history,
  is_current_smoker,
  max_na = 0,
  label_have_typical_chest_pain_no = c("no"),
  label_have_typical_chest_pain_yes = c("yes"),
  label_have_typical_chest_pain_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_family_history_no = c("no"),
  label_have_family_history_yes = c("yes"),
  label_have_family_history_unknown = c(NA, NaN),
  label_is_current_smoker_no = c("no"),
  label_is_current_smoker_yes = c("yes"),
  label_is_current_smoker_unknown = c(NA, NaN),
  error_call = rlang::caller_env()
)
```

## Arguments

- have_typical_chest_pain:

  The value of variable in the parameters  
  `label_have_typical_chest_pain_no`,  
  `label_have_typical_chest_pain_yes` and  
  `label_have_typical_chest_pain_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- have_family_history:

  The value of variable in the parameters
  `label_have_family_history_no`,  
  `label_have_family_history_yes`  
  and `label_have_family_history_unknown`.

- is_current_smoker:

  The value of variable in the parameters  
  `label_is_current_smoker_no`,  
  `label_is_current_smoker_yes`, and  
  `label_is_current_smoker_unknown`.

- max_na:

  Input integer 0 to 5 to indicate the maximum number of missing risk
  factors to tolerate before outputting an `NA`. Default: 0

- label_have_typical_chest_pain_no:

  Label(s) for patient not having typical chest pain symptom. Default:
  `c("no")`

- label_have_typical_chest_pain_yes:

  Label(s) for patient having typical chest pain symptom. Default:
  `c("yes")`

- label_have_typical_chest_pain_unknown:

  Label(s) for patient having unknown typical chest pain symptom.

- label_have_diabetes_no:

  Label(s) for patient with no diabetes. Default: `c("no")`

- label_have_diabetes_yes:

  Label(s) for patient having diabetes. Default: `c("yes")`

- label_have_diabetes_unknown:

  Label(s) for patient having unknown diabetes. Default: `c(NA, NaN)`

- label_have_hypertension_no:

  Label(s) for patient with no hypertension. Default: `c("no")`

- label_have_hypertension_yes:

  Label(s) for patient having hypertension. Default: `c("yes")`

- label_have_hypertension_unknown:

  Label(s) for patient having unknown hypertension. Default:
  `c(NA, NaN)`

- label_have_family_history_no:

  Label(s) for patient with no family history of CAD. Default: `c("no")`

- label_have_family_history_yes:

  Label(s) for patient having family history of CAD. Default: `c("yes")`

- label_have_family_history_unknown:

  Label(s) for patient having unknown family history of CAD. Default:
  `c(NA, NaN)`

- label_is_current_smoker_no:

  Label(s) for patients who are not current smokers. Default: `c("no")`

- label_is_current_smoker_yes:

  Label(s) for patients who are current smokers. Default: `c("yes")`

- label_is_current_smoker_unknown:

  Label(s) for patient with unknown smoking status.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer indicating the number of risk factors the patient has. It can
also be `NA` if the number of missing risk factors exceeds the `max_na`
input value

## Examples

``` r
calculate_confirm_2015_num_of_rf(
  have_typical_chest_pain = "yes",
  have_diabetes = "yes",
  have_hypertension = "yes",
  have_family_history = "yes",
  is_current_smoker = "no"
)
#> [1] 4

calculate_confirm_2015_num_of_rf(
  have_typical_chest_pain = "no",
  have_diabetes = "no",
  have_hypertension = "no",
  have_family_history = NA,
  is_current_smoker = "no",
  max_na = 0
)
#> [1] NA

calculate_confirm_2015_num_of_rf(
  have_typical_chest_pain = "no",
  have_diabetes = "no",
  have_hypertension = "no",
  have_family_history = NA,
  is_current_smoker = "no",
  max_na = 1
)
#> [1] 0
```
