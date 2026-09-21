# Calculate 2018 Chen MFS PTP (figure 3 version) for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2018 Chen et. al.
modified Framingham scoring (MFS) model. Obstructive CAD was defined as
(\\\geq\\50% stenosis by diameter in at least one major coronary vessel
based on coronary angiography)

## Usage

``` r
calculate_chen_2018_mfs_fig_3_ptp(
  age,
  sex,
  have_hypertension,
  tc_mg_dl,
  hdl_mg_dl,
  lvef_percent,
  hsCRP_mg_dl,
  have_anemia,
  output = c("risk_group", "risk_numeric", "risk_percentage", "mfs_points"),
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

  Input integer value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- tc_mg_dl:

  Input positive integer value to indicate the patient's total
  cholesterol in \\mg/dL\\.

- hdl_mg_dl:

  Input positive numeric value to indicate the patient's high-density
  lipoprotein (HDL) in \\mg/dL\\.

- lvef_percent:

  Input positive integer value to indicate the patient's Left
  Ventricular Ejection Fraction (LVEF) in \\\\\\

- hsCRP_mg_dl:

  Input positive integer value to indicate the patient's
  high-sensitivity C reactive protein (hs-CRP) in \\mg/dL\\.

- have_anemia:

  The value of variable in the parameters `label_have_anemia_no`,  
  `label_have_anemia_yes`, and  
  `label_have_anemia_unknown`.

- output:

  Input text to indicate the how pre-test probability results be
  expressed Default: c("risk_group", "risk_numeric", "risk_percentage",
  "mfs_points")

  - risk_group means the PTP will be expressed as Low, Moderate, High
    and Very High.

    - low risk score if MFS is \\\leq\\ 17.

    - moderate risk score if MFS is in between 18 to 26.

    - high risk score if MFS is in between 27 to 41.

    - very high risk score if MFS is \\\geq\\ 42.

  - risk_numeric means the PTP will be expressed as an integer
    probability (0-100).

  - risk_percentage means the PTP will be expressed as percentage text
    (0-100%).

  - mfs_points means the total risk points as calculated in figure 3.
    This value will be later converted to risk of obstructive CAD
    `risk_numeric` based on the table presented in figure 3.

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

An integer, percentage or category representing the patient's risk
points or PTP for obstructive CAD based on 2018 Chen et. al. predicted
risk if obstructive CAD defined as stenosis \\\geq\\ 50% in any vessel..
See parameter option `output` for more information.

## Details

The predictive model is based on patients from a prospective
observational study Predictive Value of Contrast Volume to Creatinine
Clearance Ratio (PRECOMIN) conducted primarily at Guangdong General
Hospital.

## Examples

``` r
# 40 year old female with no hypertension
# and anemia. She has a
# total cholesterol level of 150 mg/dL
# high-density lipoprotein cholesterol level of 70 mg/dL and
# high-sensitivity C reactive protein level of 1 mg/dL with
# a left Ventricular Ejection Fraction reading of 60%
calculate_chen_2018_mfs_fig_3_ptp(
    age = 40,
    sex = "female",
    have_hypertension = "no",
    tc_mg_dl = 150,
    hdl_mg_dl = 70,
    lvef_percent = 60,
    hsCRP_mg_dl = 1,
    have_anemia = "no",
    output = "risk_percentage"
)
#> [1] "16%"
```
