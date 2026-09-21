# Calculate 2021 PRECISE Clinical PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2021 Predictive
Risk scorE for CAD In Southeast Asians with chEst pain (PRECISE)
clinical model. Obstructive CAD was defined as a stenosis causing
\\\geq\\ 50% diameter stenosis on coronary CTA.

## Usage

``` r
calculate_precise_2021_clinical_ptp(
  age,
  sex,
  chest_pain_type,
  have_neck_radiation,
  have_diabetes,
  have_hypertension,
  smoking_history_type,
  have_q_waves,
  have_st_t_changes,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_unknown = c(NA, NaN),
  label_have_neck_radiation_no = c("no"),
  label_have_neck_radiation_yes = c("yes"),
  label_have_neck_radiation_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_smoking_history_type_current = c("current"),
  label_smoking_history_type_past = c("past"),
  label_smoking_history_type_none = c("none"),
  label_smoking_history_type_unknown = c(NA, NaN),
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

- have_neck_radiation:

  The value of variable in the parameters  
  `label_have_neck_radiation_no`, `label_have_neck_radiation_yes`,  
  and `label_have_neck_radiation_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- smoking_history_type:

  The value of variable in the parameters  
  `label_smoking_history_type_current`,  
  `label_smoking_history_type_past`,  
  `label_smoking_history_type_none` and  
  `label_smoking_history_type_unknown`

- have_q_waves:

  The value of variable in the parameters  
  `label_have_q_waves_no`, `label_have_q_waves_yes`,  
  and `label_have_q_waves_unknown`.

- have_st_t_changes:

  The value of variable in the parameters  
  `label_have_st_t_changes_no`, `label_have_st_t_changes_yes`,  
  and `label_have_st_t_changes_unknown`.

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

- label_have_neck_radiation_no:

  Label(s) for patient without chest pain radiating to the neck.
  Default: `c("no")`

- label_have_neck_radiation_yes:

  Label(s) for patient with chest pain radiating to the neck. Default:
  `c("yes")`

- label_have_neck_radiation_unknown:

  Label(s) for patient with unknown chest pain radiating to the neck
  Default: `c(NA, NaN)`

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

- label_smoking_history_type_current:

  Label(s) for patient who is a current smoker. Default: `c("current")`

- label_smoking_history_type_past:

  Label(s) for patient who is a past smoker. Default: `c("past")`

- label_smoking_history_type_none:

  Label(s) for patient who is a non-smoker. Default: `c("none")`

- label_smoking_history_type_unknown:

  Label(s) for patient with unknown smoking history. Default:
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

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2021 Predictive Risk scorE for CAD In Southeast Asians with chEst
pain (PRECISE) clinical model.

## Details

The predictive model is based on patients a mixed Asian cohort within
Singapore with stable chest pain.

An online calculator is accessible at
<https://webapps.duke-nus.edu.sg/tools/PRECISE>.

Model formula is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where
\\F(x)\\ equals \$\$ \begin{array}{l} -6.714\quad+ \\\\ (0.033 \*
age)\quad+ \\\\ (1.75 \* sex\\is\\male)\quad+ \\\\ (0.597 \*
have\\diabetes)\quad+ \\\\ (0.497 \* have\\hypertension)\quad+ \\\\
(0.733 \* is\\current\\smoker)\quad+ \\\\ (0.07 \*
is\\past\\smoker)\quad+ \\\\ (1.374 \* have\\typical\\chest\\pain)\quad+
\\\\ (0.875 \* have\\atypical\\chest\\pain)\quad+ \\\\ (1.157 \*
pain\\radiating\\to\\neck)\quad+ \\\\ (1.020 \* have\\Q\\waves)\quad+
\\\\ (0.552 \* have\\ST-T\\changes) \end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain
# radiating to the neck, has diabetes
# but no hypertension and a non-smoker.
# She has Q waves but no ST-T changes on ECG.

calculate_precise_2021_clinical_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical",
    have_neck_radiation = "yes",
    have_diabetes = "yes",
    have_hypertension = "no",
    smoking_history_type = "none",
    have_q_waves = "yes",
    have_st_t_changes = "no"
)
#> [1] 0.2233933
```
