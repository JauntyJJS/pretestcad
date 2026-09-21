# Calculate 2020 Winther Basic PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2020 Winther et.
al. basic model (Basic_PTP). Obstructive CAD was defined as a stenosis
causing \\\geq\\ 50% diameter stenosis on coronary CTA.

## Usage

``` r
calculate_winther_2020_basic_ptp(
  age,
  sex,
  chest_pain_type,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_cpt_no_chest_pain = c("no chest pain"),
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

  The value of variable in the parameters `label_cpt_no_chest_pain`,  
  `label_cpt_nonanginal`,  
  `label_cpt_atypical`,  
  `label_cpt_typical`,  
  and `label_cpt_unknown`.

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

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

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2020 Winther et. al. basic model (Basic_PTP).

## Details

The predictive model is based on \> 40000 symptomatic patients from 2008
to 2017 from 13 hospitals in Western Denmark. These patients are
registered under the Western Denmark Heart Registry.

Model formula used is from Table S3.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -7.0753\quad+ \\\\ (1.2308 \*
sex\\is\\male)\quad+ \\\\ (0.0642 \* age)\quad+ \\\\ (2.2501 \*
have\\typical\\chest\\pain)\quad+ \\\\ (-0.5095 \*
have\\nonanginal\\chest\\pain)\quad+ \\\\ (-0.0191 \* age \*
have\\typical\\chest\\pain) \end{array} \$\$

## Examples

``` r
# 40 year old Male with typical chest pain
calculate_winther_2020_basic_ptp(
    age = 40,
    sex = "male",
    chest_pain_type = "typical"
)
#> [1] 0.1430237

# 40 year old Male with nonanginal chest pain
calculate_winther_2020_basic_ptp(
    age = 40,
    sex = "male",
    chest_pain_type = "nonanginal"
)
#> [1] 0.02218292
```
