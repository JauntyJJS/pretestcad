# Calculate 1993 Duke Clinical Score for Significant CAD

This function returns a patient's pre-test probability (PTP) of
significant (\>75% luminal diameter narrowing of at least one major
coronary artery) coronary artery disease based on the 1993 Duke Clinical
Score.

## Usage

``` r
calculate_dcs_1993_sig_cad_ptp(
  age,
  sex,
  chest_pain_type,
  have_mi,
  have_smoking_history,
  have_dyslipidemia,
  have_diabetes,
  have_q_waves,
  have_st_t_changes,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_unknown = c(NA, NaN),
  label_have_mi_no = c("no"),
  label_have_mi_yes = c("yes"),
  label_have_mi_unknown = c(NA, NaN),
  label_have_smoking_history_no = c("no"),
  label_have_smoking_history_yes = c("yes"),
  label_have_smoking_history_unknown = c(NA, NaN),
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

- have_mi:

  The value of variable in the parameters `label_have_mi_no`,  
  `label_have_mi_yes` and `label_have_mi_unknown`.

- have_smoking_history:

  The value of variable in the parameters
  `label_have_smoking_history_no`,  
  `label_have_smoking_history_yes`,  
  and `label_have_smoking_history_unknown`.

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

- label_have_mi_no:

  Label(s) for patient not having a previous history of MI. Default:
  `c("no")`

- label_have_mi_yes:

  Label(s) for patient having a previous history of MI. Default:
  `c("yes")`

- label_have_mi_unknown:

  Label(s) for patient with unknown previous history of MI. Default:
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

A numeric value representing the patient's PTP for significant (\>75%
luminal diameter narrowing of at least one major coronary artery) CAD
based on the 1993 Duke Clinical Score.

## Details

The predictive model is based on patients referred for cardiac
catheterisation between 1969 and 1983.

Model formula used is from Appendix: Study Models.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -7.376\quad+ \\\\ (0.1126 \* age)\quad+
\\\\ (-0.328 \* sex\\is\\female)\quad+ \\\\ (-0.0301 \* age \*
sex\\is\\female)\quad+ \\\\ (2.581 \* have\\typical\\chest\\pain)\quad+
\\\\ (0.976 \* have\\atypical\\chest\\pain)\quad+ \\\\ (1.093 \*
have\\history\\of\\MI)\quad+ \\\\ (1.213 \* have\\Q\\waves)\quad+ \\\\
(0.741 \* have\\history\\of\\MI \* have\\Q\\waves)\quad+ \\\\ (2.596 \*
have\\smoking\\history)\quad+ \\\\ (1.845 \* have\\dyslipidemia)\quad+
\\\\ (0.694 \* have\\diabetes)\quad+ \\\\ (0.637 \*
have\\ST-T\\changes)\quad+ \\\\ (-0.0404 \* age \*
have\\smoking\\history)\quad+ \\\\ (-0.0251 \* age \*
have\_\\dyslipidemia)\quad+ \\\\ (0.550 \* sex\\is\\female \*
have\\smoking\\history) \end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain,
# previous history of MI,
# has diabetes but no dyslipidemia and a non-smoker.
# She has Q waves but no ST-T changes on ECG.

calculate_dcs_1993_sig_cad_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical",
    have_mi = "yes",
    have_smoking_history = "no",
    have_dyslipidemia = "no",
    have_diabetes = "yes",
    have_q_waves = "yes",
    have_st_t_changes = "no"
)
#> [1] 0.8719152
```
