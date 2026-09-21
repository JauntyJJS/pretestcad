# Calculate 2020 Winther RF-CL PTP model for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2020 Winther et.
al. Risk Factor-Weighted Clinical Likelihood (RF-CL) model. Obstructive
CAD was defined as a stenosis causing \\\geq\\ 50% diameter stenosis on
coronary CTA.

## Usage

``` r
calculate_winther_2020_rf_cl_ptp(
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

  Input numeric value to indicate the age of the patient in years.

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

A numeric value representing the patient's PTP for obstructive CAD based
on the 2020 Winther et. al. Risk Factor-Weighted Clinical Likelihood
(RF-CL) model.

## Details

The predictive model is based on \> 40000 symptomatic patients from 2008
to 2017 from 13 hospitals in Western Denmark. These patients are
registered under the Western Denmark Heart Registry.

Model formula used is from Table S3.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -9.5260\quad+ \\\\ (1.6128 \*
sex\\is\\male)\quad+ \\\\ (0.0844 \* age)\quad+ \\\\ (2.7112 \*
have\\typical\\chest\\pain)\quad+ \\\\ (-0.4675 \*
have\\nonanginal\\chest\\pain)\quad+ \\\\ (1.4940 \*
risk\\factor\\subgroups)\quad+ \\\\ (-0.0187 \* age \*
have\\typical\\chest\\pain)\quad+ \\\\ (-0.0131 \* age \*
risk\\factor\\subgroups)\quad+ \\\\ (-0.2799 \*
have\\typical\\chest\\pain \* risk\\factor\\subgroups)\quad+ \\\\
(-0.2091 \* sex\\is\\male \* risk\\factor\\subgroups) \end{array} \$\$

## Examples

``` r
# 40 year old Male with nonanginal chest pain
calculate_winther_2020_rf_cl_ptp(
    age = 40,
    sex = "male",
    chest_pain_type = "no chest pain",
    have_dyspnoea = "no",
    have_family_history = "no",
    have_smoking_history = "no",
    have_dyslipidemia = "no",
    have_hypertension = "no",
    have_diabetes = "no",
    allow_na_symptom_score = TRUE,
    max_na_num_of_rf = 0
)
#> [1] 0.01414985
```
