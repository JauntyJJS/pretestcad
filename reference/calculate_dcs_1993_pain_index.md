# Calculate The Pain Index For Duke Clinical Score 1993

A function used to calculate the patient's pain index. This is used to
calculate the likelihood of severe coronary artery disease in the Duke
Clinical Score 1993 paper.

## Usage

``` r
calculate_dcs_1993_pain_index(
  have_typical_chest_pain,
  frequency_of_angina_pain_per_week,
  have_progressive_angina,
  have_nocturnal_angina,
  have_q_waves,
  have_st_t_changes,
  max_na = 0,
  max_frequency_of_angina_pain_per_week = 35,
  label_have_typical_chest_pain_no = c("no"),
  label_have_typical_chest_pain_yes = c("yes"),
  label_have_typical_chest_pain_unknown = c(NA, NaN),
  label_have_progressive_angina_no = c("no"),
  label_have_progressive_angina_yes = c("yes"),
  label_have_progressive_angina_unknown = c(NA, NaN),
  label_have_nocturnal_angina_no = c("no"),
  label_have_nocturnal_angina_yes = c("yes"),
  label_have_nocturnal_angina_unknown = c(NA, NaN),
  label_have_q_waves_no = c("no"),
  label_have_q_waves_yes = c("yes"),
  label_have_q_waves_unknown = c(NA, NaN),
  label_have_st_t_changes_no = c("no"),
  label_have_st_t_changes_yes = c("yes"),
  label_have_st_t_changes_unknown = c(NA, NaN),
  error_call = rlang::caller_env()
)
```

## Arguments

- have_typical_chest_pain:

  The value of variable in the parameters  
  `label_have_typical_chest_pain_no`,  
  `label_have_typical_chest_pain_yes` and  
  `label_have_typical_chest_pain_unknown`.

- frequency_of_angina_pain_per_week:

  Input integer to indicate the patient's frequency of angina per week.

- have_progressive_angina:

  The value of variable in the parameters
  `label_have_progressive_angina_no`,  
  `label_have_progressive_angina_yes`, and  
  `label_have_progressive_angina_unknown`.

- have_nocturnal_angina:

  The value of variable in the parameters
  `label_have_nocturnal_angina_no`,  
  `label_have_nocturnal_angina_yes`, and  
  `label_have_nocturnal_angina_unknown`.

- have_q_waves:

  The value of variable in the parameters  
  `label_have_q_waves_no`, `label_have_q_waves_yes`,  
  and `label_have_q_waves_unknown`.

- have_st_t_changes:

  The value of variable in the parameters  
  `label_have_st_t_changes_no`, `label_have_st_t_changes_yes`,  
  and `label_have_st_t_changes_unknown`.

- max_na:

  Input integer 0 to 6 to indicate the maximum number of missing
  symptoms to tolerate before outputting an `NA`. Default: 0

- max_frequency_of_angina_pain_per_week:

  Input non-negative integer to indicate the maximum frequency angina
  per week to tolerate before outputting an `NA`. In the Duke Clinical
  Score 1993 paper, the maximum value is set as 35. Default: 35

- label_have_typical_chest_pain_no:

  Label(s) for patient not having typical chest pain symptom. Default:
  `c("no")`

- label_have_typical_chest_pain_yes:

  Label(s) for patient having typical chest pain symptom. Default:
  `c("yes")`

- label_have_typical_chest_pain_unknown:

  Label(s) for patient having unknown typical chest pain symptom.

- label_have_progressive_angina_no:

  Label(s) for patient not having progressive angina. Default: `c("no")`

- label_have_progressive_angina_yes:

  Label(s) for patient having progressive angina. Default: `c("yes")`

- label_have_progressive_angina_unknown:

  Label(s) for patient having unknown progressive angina. Default:
  `c(NA, NaN)`

- label_have_nocturnal_angina_no:

  Label(s) for patient not having nocturnal angina. Default: `c("no")`

- label_have_nocturnal_angina_yes:

  Label(s) for patient having nocturnal angina. Default: `c("yes")`

- label_have_nocturnal_angina_unknown:

  Label(s) for patient having unknown nocturnal angina. Default:
  `c(NA, NaN)`

- label_have_q_waves_no:

  Label(s) for patient not having Q waves on ECG. Default: `c("no")`

- label_have_q_waves_yes:

  Label(s) for patient having Q waves on ECG. Default: `c("yes")`

- label_have_q_waves_unknown:

  Label(s) for patient with unknown Q waves on ECG. Default:
  `c(NA, NaN)`

- label_have_st_t_changes_no:

  Label(s) for patient not having ST-T changes on ECG. Default:
  `c("no")`

- label_have_st_t_changes_yes:

  Label(s) for patient having ST-T changes on ECG. Default: `c("yes")`

- label_have_st_t_changes_unknown:

  Label(s) for patient with unknown ST-T changes on ECG. Default:
  `c(NA, NaN)`

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer indicating the patient's pain index. It can also be `NA` if
the number of missing symptoms exceeds the `max_na` input value or the
frequency of angina per week exceed the  
`max_frequency_of_angina_pain_per_week` input value.

## Examples

``` r
calculate_dcs_1993_pain_index(
  have_typical_chest_pain = "yes",
  frequency_of_angina_pain_per_week = 10,
  have_progressive_angina = "yes",
  have_nocturnal_angina = "no",
  have_q_waves = "no",
  have_st_t_changes = "no",
  max_na = 0,
  max_frequency_of_angina_pain_per_week = 35
)
#> [1] 10

calculate_dcs_1993_pain_index(
  have_typical_chest_pain = "yes",
  frequency_of_angina_pain_per_week = 10,
  have_progressive_angina = "yes",
  have_nocturnal_angina = NA,
  have_q_waves = "no",
  have_st_t_changes = "no",
  max_na = 0,
  max_frequency_of_angina_pain_per_week = 35
)
#> [1] NA

calculate_dcs_1993_pain_index(
  have_typical_chest_pain = "yes",
  frequency_of_angina_pain_per_week = 10,
  have_progressive_angina = "yes",
  have_nocturnal_angina = NA,
  have_q_waves = "no",
  have_st_t_changes = "no",
  max_na = 1,
  max_frequency_of_angina_pain_per_week = 35
)
#> [1] 10

calculate_dcs_1993_pain_index(
  have_typical_chest_pain = "yes",
  frequency_of_angina_pain_per_week = 40,
  have_progressive_angina = "yes",
  have_nocturnal_angina = "no",
  have_q_waves = "no",
  have_st_t_changes = "no",
  max_na = 0,
  max_frequency_of_angina_pain_per_week = 35
)
#> [1] NA

calculate_dcs_1993_pain_index(
  have_typical_chest_pain = "yes",
  frequency_of_angina_pain_per_week = 40,
  have_progressive_angina = "yes",
  have_nocturnal_angina = "no",
  have_q_waves = "no",
  have_st_t_changes = "no",
  max_na = 0,
  max_frequency_of_angina_pain_per_week = NA
)
#> [1] 40
```
