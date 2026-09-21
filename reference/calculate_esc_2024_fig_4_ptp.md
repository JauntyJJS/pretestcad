# Calculate ESC 2024 PTP for obstructive CAD

This function returns a patient's pre-test Probability (PTP) of
obstructive coronary artery disease (CAD) based on the European Society
of Cardiology (ESC) 2024 guidelines. Obstructive CAD was defined as a
stenosis causing \\\geq\\ 50% diameter stenosis on coronary CTA.

## Usage

``` r
calculate_esc_2024_fig_4_ptp(
  age,
  sex,
  chest_pain_type,
  have_dyspnoea,
  have_family_history,
  have_smoking_history,
  have_dyslipidemia,
  have_hypertension,
  have_diabetes,
  allow_na_symptom_score = TRUE,
  max_na_num_of_rf = 0,
  output = c("grouping", "numeric", "percentage"),
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_have_dyspnoea_no = c("no"),
  label_have_dyspnoea_yes = c("yes"),
  label_have_dyspnoea_unknown = c(NA, NaN),
  label_cpt_no_chest_pain = c("no chest pain"),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_unknown = c(NA, NaN),
  label_have_family_history_no = c("no"),
  label_have_family_history_yes = c("yes"),
  label_have_family_history_unknown = c(NA, NaN),
  label_have_smoking_history_no = c("no"),
  label_have_smoking_history_yes = c("yes"),
  label_have_smoking_history_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input integer value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- chest_pain_type:

  The value of variable in the parameters `label_cpt_no_chest_pain`,  
  `label_cpt_nonanginal`,  
  `label_cpt_atypical`,  
  `label_cpt_typical`,  
  and `label_cpt_unknown`.

- have_dyspnoea:

  The value of variable in the parameters `label_have_dyspnoea_no`,  
  `label_have_dyspnoea_yes` and `label_have_dyspnoea_unknown`.

- have_family_history:

  The value of variable in the parameters
  `label_have_family_history_no`,  
  `label_have_family_history_yes`  
  and `label_have_family_history_unknown`.

- have_smoking_history:

  The value of variable in the parameters
  `label_have_smoking_history_no`,  
  `label_have_smoking_history_yes`,  
  and `label_have_smoking_history_unknown`.

- have_dyslipidemia:

  The value of variable in the parameters
  `label_have_dyslipidemia_no`,  
  `label_have_dyslipidemia_yes` and `label_have_dyslipidemia_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- allow_na_symptom_score:

  A logical evaluating to `TRUE` or `FALSE`  
  indicating whether we can allow `chest_pain_type`  
  or `have_dyspnoea` to be `NA` when calculating the score.

- max_na_num_of_rf:

  Input integer 0 to 5 to indicate the maximum number of missing risk
  factors to tolerate before outputting an `NA`. Default: 0

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

- label_have_family_history_no:

  Label(s) for patient with no family history of CAD. Default: `c("no")`

- label_have_family_history_yes:

  Label(s) for patient having family history of CAD. Default: `c("yes")`

- label_have_family_history_unknown:

  Label(s) for patient having unknown family history of CAD. Default:
  `c(NA, NaN)`

- label_have_smoking_history_no:

  Label(s) for patient with no smoking history (current or past).
  Default: `c("no")`

- label_have_smoking_history_yes:

  Label(s) for patient having smoking history (current or past).
  Default: `c("yes")`

- label_have_smoking_history_unknown:

  Label(s) for patient having unknown smoking history (current or past).
  Default: `c(NA, NaN)`

- label_have_dyslipidemia_no:

  Label(s) for patient with no dyslipidemia. Default: `c("no")`

- label_have_dyslipidemia_yes:

  Label(s) for patient having dyslipidemia. Default: `c("yes")`

- label_have_dyslipidemia_unknown:

  Label(s) for patient having unknown dyslipidemia. Default:
  `c(NA, NaN)`

- label_have_hypertension_no:

  Label(s) for patient with no hypertension. Default: `c("no")`

- label_have_hypertension_yes:

  Label(s) for patient having hypertension. Default: `c("yes")`

- label_have_hypertension_unknown:

  Label(s) for patient having unknown hypertension. Default:
  `c(NA, NaN)`

- label_have_diabetes_no:

  Label(s) for patient with no diabetes. Default: `c("no")`

- label_have_diabetes_yes:

  Label(s) for patient having diabetes. Default: `c("yes")`

- label_have_diabetes_unknown:

  Label(s) for patient having unknown diabetes. Default: `c(NA, NaN)`

## Value

An integer, percentage or category representing the patient's PTP for
obstructive CAD based on the ESC 2024 guidelines. See parameter option
`output` for more information.

## Examples

``` r
# 30 female with symptom score of 0 and 0 risk factors
calculate_esc_2024_fig_4_ptp(
  age = 30,
  sex = "female",
  chest_pain_type = "no chest pain",
  have_dyspnoea = "no",
  have_family_history = "no",
  have_smoking_history = "no",
  have_dyslipidemia = "no",
  have_hypertension = "no",
  have_diabetes = "no",
  allow_na_symptom_score = TRUE,
  max_na_num_of_rf = 0,
  output = "percentage"
)
#> [1] "0%"
```
