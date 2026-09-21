# Calculate 2019 Reeh Clinical PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2019 Reeh et. al.
clinical model. Obstructive CAD was defined from invasive angiography as
\>70% stenosis of any epicardial vessel or 50–70% stenosis with a
fractional flow reserve \<0.8 or any lesion that was revascularized.

## Usage

``` r
calculate_reeh_2019_clinical_ptp(
  age,
  sex,
  symptom_type,
  have_dyslipidemia,
  have_family_history,
  have_diabetes,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_symptom_type_typical = c("typical"),
  label_symptom_type_atypical = c("atypical"),
  label_symptom_type_nonanginal = c("nonanginal"),
  label_symptom_type_dyspnoea = c("dyspnoea"),
  label_symptom_type_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_family_history_no = c("no"),
  label_have_family_history_yes = c("yes"),
  label_have_family_history_unknown = c(NA, NaN),
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

- symptom_type:

  Input characters (typical, atypical, nonanginal, dyspnoea) to indicate
  the symptom characteristics of the patient.

  - typical stands for the patient having typical chest pain.

  - atypical stands for the patient having atypical chest pain.

  - nonanginal stands for the patient having nonanginal or non-specific
    chest pain.

  - dyspnoea stands for the patient having dyspnoea.

- have_dyslipidemia:

  The value of variable in the parameters
  `label_have_dyslipidemia_no`,  
  `label_have_dyslipidemia_yes` and `label_have_dyslipidemia_unknown`.

- have_family_history:

  The value of variable in the parameters
  `label_have_family_history_no`,  
  `label_have_family_history_yes`  
  and `label_have_family_history_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

- label_symptom_type_typical:

  Label(s) for patient having typical chest pain. Default:
  `c("typical")`

- label_symptom_type_atypical:

  Label(s) for patient having atypical chest pain. Default:
  `c("atypical")`

- label_symptom_type_nonanginal:

  Label(s) for patient having nonanginal or non-specific chest pain.  
  Default: `c("nonanginal")`

- label_symptom_type_dyspnoea:

  Label(s) for patient having dyspnoea. Default: `c("dyspnoea")`

- label_symptom_type_unknown:

  Label(s) for patient having unknown symptoms. Default: `c(NA, NaN)`

- label_have_dyslipidemia_no:

  Label(s) for patient with no dyslipidemia. Default: `c("no")`

- label_have_dyslipidemia_yes:

  Label(s) for patient having dyslipidemia. Default: `c("yes")`

- label_have_dyslipidemia_unknown:

  Label(s) for patient having unknown dyslipidemia. Default:
  `c(NA, NaN)`

- label_have_family_history_no:

  Label(s) for patient with no family history of CAD. Default: `c("no")`

- label_have_family_history_yes:

  Label(s) for patient having family history of CAD. Default: `c("yes")`

- label_have_family_history_unknown:

  Label(s) for patient having unknown family history of CAD. Default:
  `c(NA, NaN)`

- label_have_diabetes_no:

  Label(s) for patient with no diabetes. Default: `c("no")`

- label_have_diabetes_yes:

  Label(s) for patient having diabetes. Default: `c("yes")`

- label_have_diabetes_unknown:

  Label(s) for patient having unknown diabetes. Default: `c(NA, NaN)`

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2019 Reeh et. al. clinical model.

## Details

The predictive model is based on 3903 patients free of CAD and heart
failure and suspected of angina, who were referred to a single, large,
urban university hospital for assessment in 2012–15.

Model formula used is from Table 3.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -8.5499\quad+ \\\\ (1.4468 \*
sex\\is\\male)\quad+ \\\\ (0.5031 \* (age/10))\quad+ \\\\ (2.7699 \*
have\\typical\\chest\\pain)\quad+ \\\\ (1.7839 \*
have\\atypical\\chest\\pain)\quad+ \\\\ (0.8071 \* have\\dyspnoea)\quad+
\\\\ (0.9551 \* have\\dyslipidemia)\quad+ \\\\ (0.4394 \*
have\\family\\history\\of\\CAD)\quad+ \\\\ (0.3767 \* have\\diabetes)
\end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain
calculate_reeh_2019_clinical_ptp(
    age = 40,
    sex = "female",
    symptom_type = "typical",
    have_dyslipidemia = "no",
    have_family_history = "no",
    have_diabetes = "no"
)
#> [1] 0.02258556
```
