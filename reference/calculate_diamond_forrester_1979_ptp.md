# Calculate Diamond-Forrester 1979 PTP for obstructive CAD

This function returns a patient's pre-test Probability (PTP) of
obstructive coronary artery disease (CAD) based on Diamond-Forrester
1979 model. Obstructive CAD was defined as a stenosis causing \\\geq\\
50% diameter stenosis on coronary CTA.

## Usage

``` r
calculate_diamond_forrester_1979_ptp(
  age,
  sex,
  chest_pain_type,
  output = c("numeric", "percentage"),
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

  Input integer value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- chest_pain_type:

  The value of variable in the parameters,  
  `label_cpt_nonanginal`, `label_cpt_atypical`,  
  `label_cpt_typical`, and `label_cpt_unknown`.

- output:

  Input text to indicate the how pre-test probability results be
  expressed Default: c("numeric", "percentage")

  - numeric means the PTP will be expressed as an integer probability
    (0-100).

  - percentage means the PTP will be expressed as percentage text
    (0-100%).

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

A numeric or percentage representing the patient's PTP for obstructive
CAD based on Diamond-Forrester 1979 model.

## Examples

``` r
# 35 year old female with typical chest pain
calculate_diamond_forrester_1979_ptp(
    age = 35,
    sex = "female",
    chest_pain_type = "typical",
    output = "percentage"
)
#> [1] "25.8%"

# 65 year old male with nonanginal chest pain
calculate_diamond_forrester_1979_ptp(
    age = 65,
    sex = "male",
    chest_pain_type = "nonanginal",
    output = "percentage"
)
#> [1] "28.1%"
```
