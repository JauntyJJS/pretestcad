# Calculate 2018 Chen MFS PTP (formula version) for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2018 Chen et. al.
modified Framingham scoring (MFS) model. Obstructive CAD was defined as
(\\\geq\\50% stenosis by diameter in at least one major coronary vessel
based on coronary angiography)

## Usage

``` r
calculate_chen_2018_mfs_formula_ptp(
  age,
  sex,
  have_hypertension,
  tc_mg_dl,
  hdl_mg_dl,
  lvef_percent,
  hsCRP_mg_dl,
  have_anemia,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_anemia_no = c("no"),
  label_have_anemia_yes = c("yes"),
  label_have_anemia_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input numeric value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- tc_mg_dl:

  Input positive numeric value to indicate the patient's total
  cholesterol in \\mg/dL\\.

- hdl_mg_dl:

  Input positive numeric value to indicate the patient's high-density
  lipoprotein (HDL) in \\mg/dL\\.

- lvef_percent:

  Input positive numeric value to indicate the patient's Left
  Ventricular Ejection Fraction (LVEF) in \\\\\\

- hsCRP_mg_dl:

  Input positive numeric value to indicate the patient's
  high-sensitivity C reactive protein (hs-CRP) in \\mg/dL\\.

- have_anemia:

  The value of variable in the parameters `label_have_anemia_no`,  
  `label_have_anemia_yes`, and  
  `label_have_anemia_unknown`.

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

- label_have_hypertension_no:

  Label(s) for patient with no hypertension. Default: `c("no")`

- label_have_hypertension_yes:

  Label(s) for patient having hypertension. Default: `c("yes")`

- label_have_hypertension_unknown:

  Label(s) for patient having unknown hypertension. Default:
  `c(NA, NaN)`

- label_have_anemia_no:

  Label(s) for patient without anemia. Default: `c("no")`

- label_have_anemia_yes:

  Label(s) for patient with anemia. The paper uses baseline hematocrit
  value \<39% for men and \<36% for women. Default: `c("yes")`

- label_have_anemia_unknown:

  Label(s) for patient with no information if they have anemia or not.
  Default: `c(NA, NaN)`

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2018 modified Framingham scoring (MFS) model.

## Details

The predictive model is based on patients from a prospective
observational study Predictive Value of Contrast Volume to Creatinine
Clearance Ratio (PRECOMIN) conducted primarily at Guangdong General
Hospital.

Model formula is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where
\\F(x)\\ equals \$\$ \begin{array}{l} -0.0468\quad+ \\\\ (0.0204 \*
age)\quad+ \\\\ (1.0961 \* sex\\is\\male)\quad+ \\\\ (0.5444 \*
have\\hypertension)\quad+ \\\\ (0.0055 \*
total\\cholesterol\\mg\\dl)\quad+ \\\\ (-0.0257 \* hdl\\mg\\dl)\quad+
\\\\ (-0.022 \* LVEF\\\\)\quad+ \\\\ (0.5677 \* have\\anemia)\quad+ \\\\
(0.0254 \* hs-CRP\\mg\\dl) \end{array} \$\$

## Examples

``` r
# 40 year old female with no hypertension
# and anemia. She has a
# total cholesterol level of 150 mg/dL
# high-density lipoprotein cholesterol level of 70 mg/dL and
# high-sensitivity C reactive protein level of 1 mg/dL with
# a left Ventricular Ejection Fraction reading of 60%
calculate_chen_2018_mfs_formula_ptp(
    age = 40,
    sex = "female",
    have_hypertension = "no",
    tc_mg_dl = 150,
    hdl_mg_dl = 70,
    lvef_percent = 60,
    hsCRP_mg_dl = 1,
    have_anemia = "no"
)
#> [1] 0.182515
```
