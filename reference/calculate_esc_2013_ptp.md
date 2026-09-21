# Calculate ESC 2013 PTP for obstructive CAD

This function returns a patient's pre-test Probability (PTP) of
obstructive coronary artery disease (CAD) based on the European Society
of Cardiology (ESC) 2013 guidelines. Obstructive CAD was defined as a
stenosis causing \\\geq\\ 50% diameter stenosis on coronary CTA.

## Usage

``` r
calculate_esc_2013_ptp(
  age,
  sex,
  chest_pain_type,
  output = c("numeric", "percentage")
)
```

## Arguments

- age:

  Input integer value to indicate the age of the patient.

- sex:

  Input characters (female, male) to indicate the sex of the patient.

  - female

  - male

- chest_pain_type:

  Input characters (typical, atypical, nonanginal) to indicate the chest
  pain characteristics of the patient.

  - typical stands for the patient having typical chest pain.

  - atypical stands for the patient having atypical chest pain.

  - nonanginal stands for the patient having nonanginal or non-specific
    chest pain.

- output:

  Input text to indicate the how pre-test probability results be
  expressed Default: c("numeric", "percentage")

  - numeric means the PTP will be expressed as an integer probability
    (0-100).

  - percentage means the PTP will be expressed as percentage text
    (0-100%).

## Value

An integer or percentage representing the patient's PTP for obstructive
CAD based on the ESC 2013 guidelines.

## Details

The predictive model used to create the guidelines are based on the
journal A clinical prediction rule for the diagnosis of coronary artery
disease: validation, updating, and extension by 2011 Genders et. al.

## Examples

``` r
# 35 year old female with typical chest pain
calculate_esc_2013_ptp(
    age = 35,
    sex = "female",
    chest_pain_type = "typical",
    output = "percentage"
)
#> [1] "28%"

# 65 year old male with nonanginal chest pain
calculate_esc_2013_ptp(
    age = 65,
    sex = "male",
    chest_pain_type = "nonanginal",
    output = "percentage"
)
#> [1] "44%"
```
