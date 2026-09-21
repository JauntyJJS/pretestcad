# Calculate 2012 CAD2 Basic PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2012 CAD
Consortium 2 (CAD2) basic model. Obstructive coronary artery disease was
defined as \\\geq\\50% diameter stenosis in at least one vessel found on
catheter based coronary angiography.

## Usage

``` r
calculate_cad2_2012_basic_ptp(
  age,
  sex,
  chest_pain_type,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_nonanginal = c("nonanginal"),
  label_cpt_atypical = c("atypical"),
  label_cpt_typical = c("typical"),
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
  `label_cpt_typical` and `label_cpt_unknown`.

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

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2012 CAD Consortium 2 (CAD2) basic model.

## Details

The predictive model is based on patients from 18 hospitals in Europe
and the United States.

A web-based calculator is
[accessible](https://qxmd.com/calculate/calculator_287/pre-test-probability-of-cad-cad-consortium).

Model formula used is from Appendix Table 4.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -6.917\quad+ \\\\ (0.063 \* age)\quad+ \\\\
(1.358 \* sex\\is\\male)\quad+ \\\\ (0.658 \*
have\\atypical\\chest\\pain)\quad+ \\\\ (1.975 \*
have\\typical\\chest\\pain) \end{array} \$\$

## Examples

``` r
# 40 year old female with typical chest pain
calculate_cad2_2012_basic_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical"
)
#> [1] 0.0815104
```
