# Calculate 2022 LAH Extended PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2022 Local
Assessment of the Heart (LAH) extended model. Obstructive CAD was
defined as a stenosis causing \\\geq\\ 50% diameter stenosis on coronary
CTA.

## Usage

``` r
calculate_lah_2022_extended_ptp(
  age,
  sex,
  chest_pain_type,
  have_diabetes,
  have_hypertension,
  have_dyslipidemia,
  have_smoking_history,
  coronary_calcium_score,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_smoking_history_no = c("no"),
  label_have_smoking_history_yes = c("yes"),
  label_have_smoking_history_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input numeric value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- chest_pain_type:

  The value of variable in the parameters,  
  `label_cpt_nonanginal`, `label_cpt_atypical`,  
  `label_cpt_typical` and `label_cpt_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- have_dyslipidemia:

  The value of variable in the parameters
  `label_have_dyslipidemia_no`,  
  `label_have_dyslipidemia_yes` and `label_have_dyslipidemia_unknown`.

- have_smoking_history:

  The value of variable in the parameters
  `label_have_smoking_history_no`,  
  `label_have_smoking_history_yes`,  
  and `label_have_smoking_history_unknown`.

- coronary_calcium_score:

  Input non-negative numeric to indicate the total coronary calcium
  score of the patient.

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

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

- label_have_dyslipidemia_no:

  Label(s) for patient with no dyslipidemia. Default: `c("no")`

- label_have_dyslipidemia_yes:

  Label(s) for patient having dyslipidemia. Default: `c("yes")`

- label_have_dyslipidemia_unknown:

  Label(s) for patient having unknown dyslipidemia. Default:
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

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2022 Local Assessment of the Heart (LAH) extended model.

## Details

The predictive model is based on patients a mixed Asian cohort within
Singapore with stable chest pain.

Model formula used is from Table 2.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -4.241\quad+ \\\\ (0 \* age)\quad+ \\\\
(0.544 \* sex\\is\\male)\quad+ \\\\ (0.139 \*
have\\typical\\chest\\pain)\quad+ \\\\ (-0.242 \*
have\\atypical\\chest\\pain)\quad+ \\\\ (-0.143 \*
have\\hypertension)\quad+ \\\\ (-0.002 \* have\\diabetes)\quad+ \\\\
(-0.157 \* have\\dyslipidemia)\quad+ \\\\ (-0.315 \*
have\\smoking\\history)\quad+ \\\\ (0.905 \* ln(CACS + 1)) \end{array}
\$\$

## Examples

``` r
# 40 year old female with typical chest pain,
# diabetes but no hypertension, dyslipidemia,
# a non-smoker and a coronary calcium score of 0
calculate_lah_2022_extended_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical",
    have_diabetes = "yes",
    have_hypertension = "no",
    have_dyslipidemia = "no",
    have_smoking_history = "no",
    coronary_calcium_score = 0

)
#> [1] 0.01623848
```
