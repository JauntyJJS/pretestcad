# Calculate 2025 COME-CCT-PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2025
Collaborative Meta-Analysis of Cardiac CT (COME-CCT) PTP model.
Obstructive coronary artery disease was defined as at least one \\\geq\\
50% diameter stenosis by invasive coronary angiography (ICA).

## Usage

``` r
calculate_come_cct_2025_ptp(
  age,
  sex,
  chest_pain_type,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
  label_cpt_others = c("others"),
  label_cpt_unknown = c(NA, NaN)
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
  `label_cpt_typical`, `label_cpt_others`,  
  and `label_cpt_unknown`.

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

- label_cpt_others:

  Label(s) for patient having other forms of chest pain. Default:
  `c("others")`

- label_cpt_unknown:

  Label(s) for patient having unknown chest pain type symptoms. Default:
  `c(NA, NaN)`

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2025 Collaborative Meta-Analysis of Cardiac CT (COME-CCT) PTP
model as shown in Table 2.

## Details

The predictive model is based on 5332 stable chest pain patients with
clinically indicated ICA from 22 countries.

Model formula used is from Table 3 to obtain results as shown in Table
2.

It is of the form \$\$\int\_{-\infty}^{\infty} \frac{1}{(1 + e^{-(F(x) +
\zeta)})} \* \frac{1}{\sqrt{2\pi \* 0.667}} e^{\frac{-\zeta^2}{2 \*
0.667}} \\d\zeta\$\$ where \\F(x)\\ equals \$\$ \begin{array}{l}
-1.434\quad+ \\\\ (0.036 \* (age - 61.3))\quad+ \\\\ (0.986 \*
sex\\is\\male)\quad+ \\\\ (1.427 \* have\\typical\\chest\\pain)\quad+
\\\\ (0.307 \* have\\atypical\\chest\\pain)\quad+ \\\\ (0.119 \*
have\\nonanginal\\chest\\pain) \end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain,
calculate_come_cct_2025_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical"
)
#> [1] 0.3369027
```
