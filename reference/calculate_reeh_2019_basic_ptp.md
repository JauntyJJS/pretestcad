# Calculate 2019 Reeh Basic PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2019 Reeh et. al.
basic model. Obstructive CAD was defined from invasive angiography as
\>70% stenosis of any epicardial vessel or 50–70% stenosis with a
fractional flow reserve \<0.8 or any lesion that was revascularized.

## Usage

``` r
calculate_reeh_2019_basic_ptp(
  age,
  sex,
  symptom_type,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_symptom_type_typical = c("typical"),
  label_symptom_type_atypical = c("atypical"),
  label_symptom_type_nonanginal = c("nonanginal"),
  label_symptom_type_dyspnoea = c("dyspnoea"),
  label_symptom_type_unknown = c(NA, NaN)
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

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2019 Reeh et. al. basic model.

## Details

The predictive model is based on 3903 patients free of CAD and heart
failure and suspected of angina, who were referred to a single, large,
urban university hospital for assessment in 2012–15.

Model formula used is from Table 3.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -7.6348\quad+ \\\\ (1.4067 \*
sex\\is\\male)\quad+ \\\\ (0.4820 \* (age/10))\quad+ \\\\ (2.8779 \*
have\\typical\\chest\\pain)\quad+ \\\\ (1.8690 \*
have\\atypical\\chest\\pain)\quad+ \\\\ (0.7916 \* have\\dyspnoea)
\end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain
calculate_reeh_2019_basic_ptp(
    age = 40,
    sex = "female",
    symptom_type = "typical"
)
#> [1] 0.05578231
```
