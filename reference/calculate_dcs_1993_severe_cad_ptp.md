# Calculate 1993 Duke Clinical Score for Severe CAD

This function returns a patient's pre-test probability (PTP) of severe
(\>75% luminal diameter narrowing of all three major coronary arteries
or of the left main coronary artery) coronary artery disease based on
the 1993 Duke Clinical Score.

## Usage

``` r
calculate_dcs_1993_severe_cad_ptp(
  age,
  sex,
  chest_pain_type,
  have_progressive_angina,
  have_nocturnal_angina,
  have_peripheral_vascular_disease,
  have_cerebrovascular_disease,
  have_carotid_bruits,
  have_hypertension,
  have_dyslipidemia,
  have_diabetes,
  have_q_waves,
  have_st_t_changes,
  frequency_of_angina_pain_per_week,
  duration_of_cad_symptoms_year,
  max_na_risk_factor_index = 0,
  max_na_pain_index = 0,
  max_na_vascular_disease_index = 0,
  max_frequency_of_angina_pain_per_week = 35,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_unknown = c(NA, NaN),
  label_have_progressive_angina_no = c("no"),
  label_have_progressive_angina_yes = c("yes"),
  label_have_progressive_angina_unknown = c(NA, NaN),
  label_have_nocturnal_angina_no = c("no"),
  label_have_nocturnal_angina_yes = c("yes"),
  label_have_nocturnal_angina_unknown = c(NA, NaN),
  label_have_pvd_no = c("no"),
  label_have_pvd_yes = c("yes"),
  label_have_pvd_unknown = c(NA, NaN),
  label_have_cvd_no = c("no"),
  label_have_cvd_yes = c("yes"),
  label_have_cvd_unknown = c(NA, NaN),
  label_have_carotid_bruits_no = c("no"),
  label_have_carotid_bruits_yes = c("yes"),
  label_have_carotid_bruits_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN),
  label_have_q_waves_no = c("no"),
  label_have_q_waves_yes = c("yes"),
  label_have_q_waves_unknown = c(NA, NaN),
  label_have_st_t_changes_no = c("no"),
  label_have_st_t_changes_yes = c("yes"),
  label_have_st_t_changes_unknown = c(NA, NaN)
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

- have_q_waves:

  The value of variable in the parameters  
  `label_have_q_waves_no`, `label_have_q_waves_yes`,  
  and `label_have_q_waves_unknown`.

- have_st_t_changes:

  The value of variable in the parameters  
  `label_have_st_t_changes_no`, `label_have_st_t_changes_yes`,  
  and `label_have_st_t_changes_unknown`.

- frequency_of_angina_pain_per_week:

  Input integer to indicate the patient's frequency of angina per week.

- duration_of_cad_symptoms_year:

  Input integer to indicate the duration of coronary artery disease
  symptoms in years.

- max_na_risk_factor_index:

  Input integer 0 to 3 to indicate the maximum number of missing risk
  factors to tolerate before outputting an `NA`. Default: 0

- max_na_pain_index:

  Input integer 0 to 5 to indicate the maximum number of missing
  symptoms to tolerate before outputting an `NA`. Default: 0

- max_na_vascular_disease_index:

  Input integer 0 to 3 to indicate the maximum number of missing disease
  history to tolerate before outputting an `NA`. Default: 0

- max_frequency_of_angina_pain_per_week:

  Input non-negative integer to indicate the maximum frequency angina
  per week to tolerate before outputting an `NA`. In the Duke Clinical
  Score 1993 paper, the maximum value is set as 35. Default: 35

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

## Value

A numeric value representing the patient's PTP for severe (\>75% luminal
diameter narrowing of all three major coronary arteries or of the left
main coronary artery) CAD based on the 1993 Duke Clinical Score.

## Details

The predictive model is based on patients referred for cardiac
catheterisation between 1969 and 1983.

Model formula used is from Appendix: Study Models.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -3.4732\quad+ \\\\ (0.3424 \*
log\_{10}(duration\\of\\CAD\\symptoms\\year + 1))\quad+ \\\\ (0.3014 \*
chest\\pain\\type)\quad+ \\\\ (0.1559 \*
log\_{10}(duration\\of\\CAD\\symptoms\\year + 1) \*
chest\\pain\\type)\quad+ \\\\ (0.0299 \* age)\quad+ \\\\ (0.3513 \*
have\\Q\\waves)\quad+ \\\\ (0.0054 \* pain\\index)\quad+ \\\\ (-0.3823
\* sex\\is\\female)\quad+ \\\\ (0.1734 \* risk\\factor\\index)\quad+
\\\\ (0.2402 \* vascular\\disease\\index) \end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain for one year,
# She has progressive angina but no nocturnal angina.
# Angina pain lasted at most five times a week.
# She has peripheral vascular and cerebrovascular disease.
# She has hypertension but has no dyslipidemia and not diabetic.
# She has Q waves and ST-T changes on ECG.

calculate_dcs_1993_severe_cad_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical",
    have_progressive_angina = "yes",
    have_nocturnal_angina = "no",
    have_peripheral_vascular_disease = "yes",
    have_cerebrovascular_disease = "yes",
    have_carotid_bruits = "no",
    have_hypertension = "yes",
    have_dyslipidemia = "no",
    have_diabetes = "no",
    have_q_waves = "yes",
    have_st_t_changes = "yes",
    frequency_of_angina_pain_per_week = 5,
    duration_of_cad_symptoms_year = 1,
)
#> [1] 0.3041388
```
