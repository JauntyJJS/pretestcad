# Calculate ESC 2024 PTP for obstructive CAD

This function returns a patient's pre-test Probability (PTP) of
obstructive coronary artery disease (CAD) based on the European Society
of Cardiology (ESC) 2024 guidelines.

## Usage

``` r
calculate_esc_2024_fig_4_ptp_simplfied(
  age,
  sex,
  symptom_score,
  num_of_rf,
  output = c("grouping", "numeric", "percentage"),
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  error_call = rlang::caller_env()
)
```

## Arguments

- age:

  Input integer value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- symptom_score:

  An integer indicating the symptom score of the patient. This value can
  be calculated via the
  [`calculate_esc_2024_symptom_score`](https://jauntyjjs.github.io/pretestcad/reference/calculate_esc_2024_symptom_score.md)

- num_of_rf:

  An integer indicating the number of risk factors the patient has. This
  value can be calculated via the
  [`calculate_esc_2024_num_of_rf`](https://jauntyjjs.github.io/pretestcad/reference/calculate_esc_2024_num_of_rf.md)
  Risk factors are:

  - having a family history of CAD.

  - having a smoking history (current and past smoker).

  - having dyslipidemia.

  - having hypertension.

  - having diabetes.

- output:

  Input text to indicate the how pre-test probability results be
  expressed Default: c("grouping", "numeric", "percentage")

  - grouping means the PTP will be expressed as Very Low, Low, and
    Moderate.

    - very low if PTP is less than or equal to 5%.

    - low if PTP is in between 6% to 15%.

    - moderate if PTP is more than 15%.

  - numeric means the PTP will be expressed as an integer probability
    (0-100).

  - percentage means the PTP will be expressed as percentage text
    (0-100%).

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer, percentage or category representing the patient's PTP for
obstructive CAD based on the ESC 2024 guidelines. See parameter option
`output` for more information.

## Examples

``` r
# 30 female with symptom score of 0 and 0 risk factors
calculate_esc_2024_fig_4_ptp_simplfied(
  age = 30,
  sex = "female",
  symptom_score = 0,
  num_of_rf = 0,
  output = "percentage"
)
#> [1] "0%"
```
