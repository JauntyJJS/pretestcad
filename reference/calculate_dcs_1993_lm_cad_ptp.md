# Calculate 1993 Duke Clinical Score for Left Main Disease

This function returns a patient's pre-test probability (PTP) of severe
(\>75% luminal diameter narrowing of the left main coronary artery)
coronary artery disease based on the 1993 Duke Clinical Score.

## Usage

``` r
calculate_dcs_1993_lm_cad_ptp(
  age,
  sex,
  have_typical_chest_pain,
  have_peripheral_vascular_disease,
  have_cerebrovascular_disease,
  have_carotid_bruits,
  duration_of_cad_symptoms_year,
  max_na_vascular_disease_index = 0,
  max_age = 65,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_have_typical_chest_pain_no = c("no"),
  label_have_typical_chest_pain_yes = c("yes"),
  label_have_typical_chest_pain_unknown = c(NA, NaN),
  label_have_pvd_no = c("no"),
  label_have_pvd_yes = c("yes"),
  label_have_pvd_unknown = c(NA, NaN),
  label_have_cvd_no = c("no"),
  label_have_cvd_yes = c("yes"),
  label_have_cvd_unknown = c(NA, NaN),
  label_have_carotid_bruits_no = c("no"),
  label_have_carotid_bruits_yes = c("yes"),
  label_have_carotid_bruits_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input numeric value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- have_typical_chest_pain:

  The value of variable in the parameters  
  `label_have_typical_chest_pain_no`,  
  `label_have_typical_chest_pain_yes` and  
  `label_have_typical_chest_pain_unknown`.

- have_peripheral_vascular_disease:

  The value of variable in the parameters `label_have_pvd_no`,  
  `label_have_pvd_yes`, and `label_have_pvd_unknown`.

- have_cerebrovascular_disease:

  The value of variable in the parameters `label_have_cvd_no`,  
  `label_have_cvd_yes`, and `label_have_cvd_unknown`.

- have_carotid_bruits:

  The value of variable in the parameters
  `label_have_carotid_bruits_no`,  
  `label_have_carotid_bruits_yes`,  
  and `label_have_carotid_bruits_unknown`.

- duration_of_cad_symptoms_year:

  Input integer to indicate the duration of coronary artery disease
  symptoms in years.

- max_na_vascular_disease_index:

  Input integer 0 to 3 to indicate the maximum number of missing disease
  history to tolerate before outputting an `NA`. Default: 0

- max_age:

  Input positive integer to indicate the maximum age to tolerate before
  outputting an `NA`. In the Duke Clinical Score 1993 paper, the maximum
  value is set as 65. Default: 65

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

- label_have_pvd_no:

  Label(s) for patient not having peripheral vascular disease. Default:
  `c("no")`

- label_have_pvd_yes:

  Label(s) for patient having peripheral vascular disease. Default:
  `c("yes")`

- label_have_pvd_unknown:

  Label(s) for patient having unknown peripheral vascular disease.
  Default: `c(NA, NaN)`

- label_have_cvd_no:

  Label(s) for patient not having cerebrovascular disease. Default:
  `c("no")`

- label_have_cvd_yes:

  Label(s) for patient having cerebrovascular disease. Default:
  `c("yes")`

- label_have_cvd_unknown:

  Label(s) for patient having unknown cerebrovascular disease. Default:
  `c(NA, NaN)`

- label_have_carotid_bruits_no:

  Label(s) for patient not having carotid bruits. Default: `c("no")`

- label_have_carotid_bruits_yes:

  Label(s) for patient having carotid bruits. Default: `c("yes")`

- label_have_carotid_bruits_unknown:

  Label(s) for patient having unknown carotid bruits. Default:
  `c(NA, NaN)`

## Value

A numeric value representing the patient's PTP for left main disease
(\>75% luminal diameter narrowing of the left main coronary artery)
based on the 1993 Duke Clinical Score.

## Details

The predictive model is based on patients referred for cardiac
catheterisation between 1969 and 1983.

Model formula used is from Appendix: Study Models.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -6.7271\quad+ \\\\ (1.1252 \*
have\\typical\\chest\\pain)\quad+ \\\\ (0.0483 \* age)\quad+ \\\\
(-0.5770 \* sex\\is\\female)\quad+ \\\\ (0.5923 \*
vascular\\disease\\index)\quad+ \\\\ (0.4027 \*
log\_{10}(duration\\of\\CAD\\symptoms\\year + 1)) \end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain for one year,
# She has peripheral vascular and cerebrovascular disease.

calculate_dcs_1993_lm_cad_ptp(
    age = 40,
    sex = "female",
    have_typical_chest_pain = "yes",
    have_peripheral_vascular_disease = "yes",
    have_cerebrovascular_disease = "yes",
    have_carotid_bruits = "no",
    duration_of_cad_symptoms_year = 1,
)
#> [1] 0.05016002
```
