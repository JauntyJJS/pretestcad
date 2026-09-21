# Calculate ESC 2019 PTP for obstructive CAD

This function returns a patient's pre-test Probability (PTP) of
obstructive coronary artery disease (CAD) based on the European Society
of Cardiology (ESC) 2019 guidelines. Obstructive CAD was defined as a
stenosis causing \\\geq\\ 50% diameter stenosis on coronary CTA.

## Usage

``` r
calculate_esc_2019_ptp(
  age,
  sex,
  have_dyspnoea,
  chest_pain_type,
  output = c("grouping", "numeric", "percentage"),
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_have_dyspnoea_no = c("no"),
  label_have_dyspnoea_yes = c("yes"),
  label_have_dyspnoea_unknown = c(NA, NaN),
  label_cpt_no_chest_pain = c("no chest pain"),
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

- have_dyspnoea:

  The value of variable in the parameters `label_have_dyspnoea_no`,  
  `label_have_dyspnoea_yes` and `label_have_dyspnoea_unknown`.

- chest_pain_type:

  The value of variable in the parameters `label_cpt_no_chest_pain`,  
  `label_cpt_nonanginal`,  
  `label_cpt_atypical`,  
  `label_cpt_typical`,  
  and `label_cpt_unknown`.

- output:

  Input text to indicate the how pre-test probability results be
  expressed Default: c("grouping", "numeric", "percentage")

  - grouping means the PTP will be expressed as Low, Intermediate and
    High.

    - low if PTP is less than 5%.

    - intermediate if PTP is in between 5% to 15%.

    - high if PTP is more than 15%.

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

- label_have_dyspnoea_no:

  Label(s) for patient having no dyspnoea symptoms. Default: `c("no")`

- label_have_dyspnoea_yes:

  Label(s) for patient having dyspnoea symptoms. Default: `c("yes")`

- label_have_dyspnoea_unknown:

  Label(s) for patient having unknown dyspnoea symptoms. Default:
  `c(NA, NaN)`

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

An integer, percentage or category representing the patient's PTP for
obstructive CAD based on the ESC 2019 guidelines. See parameter option
`output` for more information.

## Details

The predictive model used to create the guidelines are based on patients
from European countries with low cardiovascular disease (CVD) risk.

If the patient has both dyspnoea and a particular chest pain type
(typical, atypical, nonanginal), The chest pain type will take
precedence over dyspnoea.

## Examples

``` r
# 35 year old female with typical chest pain
calculate_esc_2019_ptp(
    age = 35,
    sex = "female",
    have_dyspnoea = "no",
    chest_pain_type = "typical",
    output = "percentage"
)
#> [1] "5%"

# 75 year old male with only dyspnoea
calculate_esc_2019_ptp(
    age = 75,
    sex = "male",
    have_dyspnoea = "yes",
    chest_pain_type = "no chest pain",
    output = "percentage"
)
#> [1] "32%"
```
