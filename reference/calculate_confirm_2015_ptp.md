# Calculate 2015 CONFIRM Risk Score for obstructive CAD

This function returns a patient's risk score for obstructive coronary
artery disease based on the 2015 CONFIRM Risk Score. Obstructive
coronary artery disease was defined by \\\geq\\ 50% luminal diameter
stenosis in any coronary artery \\\geq\\ 1.5 mm in diameter.

## Usage

``` r
calculate_confirm_2015_ptp(
  age,
  sex,
  have_typical_chest_pain,
  have_diabetes,
  have_hypertension,
  have_family_history,
  is_current_smoker,
  max_na_num_of_rf = 0,
  output = c("text", "percentage"),
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
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
  label_is_current_smoker_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input integer value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

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

- max_na_num_of_rf:

  Input integer 0 to 5 to indicate the maximum number of missing risk
  factors to tolerate before outputting an `NA`. Default: 0

- output:

  Input text to indicate the how pre-test probability results be
  expressed Default: c("text", "percentage")

  - text means the PTP will be expressed as a probability in text (0 to
    \> 82.4).

  - percentage means the PTP will be expressed as percentage text
    (0-100%).

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

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

## Value

A numeric value representing the patient's risk score for obstructive
CAD based on the 2015 CONFIRM Risk Score.

## Details

The predictive model is based on CCTA images from 9093 patients from
Phase I of the Coronary CT Angiography EvaluatioN For Clinical Outcomes:
An InteRnational Multicenter (CONFIRM) registry.

## Examples

``` r
# 30 years old male current smoker with typical chest pain
calculate_confirm_2015_ptp(
  age = 30,
  sex = "male",
  have_typical_chest_pain = "yes",
  have_diabetes = "no",
  have_hypertension = "no",
  have_family_history = "no",
  is_current_smoker = "yes",
  max_na_num_of_rf = 0,
  output = "percentage"
)
#> [1] "5.5%"
```
