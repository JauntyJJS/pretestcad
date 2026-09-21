# Calculate 2017 PROMISE Minimal-Risk Score for obstructive CAD

This function returns a symptomatic (have chest pain or dyspnoea)
patient's minimal risk score for obstructive coronary artery disease
(CAD) based on the 2017 PROMISE Minimal-Risk Score. Obstructive CAD was
defined as a stenosis causing \\\geq\\ 50% diameter stenosis on coronary
CTA.

## Usage

``` r
calculate_prms_2017_ptp(
  age,
  sex,
  hdl_mg_dl,
  is_minority_ethnicity,
  have_diabetes,
  have_hypertension,
  have_dyslipidemia,
  have_smoking_history,
  have_family_history,
  have_stress_symptoms = NA,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_is_minority_ethnicity_no = c("no"),
  label_is_minority_ethnicity_yes = c("yes"),
  label_is_minority_ethnicity_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN),
  label_have_hypertension_no = c("no"),
  label_have_hypertension_yes = c("yes"),
  label_have_hypertension_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_smoking_history_no = c("no"),
  label_have_smoking_history_yes = c("yes"),
  label_have_smoking_history_unknown = c(NA, NaN),
  label_have_family_history_no = c("no"),
  label_have_family_history_yes = c("yes"),
  label_have_family_history_unknown = c(NA, NaN),
  label_have_stress_symptoms_no = c("no"),
  label_have_stress_symptoms_yes = c("yes"),
  label_have_stress_symptoms_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input numeric value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- hdl_mg_dl:

  Input positive numeric value to indicate the patient's high-density
  lipoprotein (HDL) in \\mg/dL\\.

- is_minority_ethnicity:

  The value of variable in the parameters  
  `label_is_minority_ethnicity_no`, `label_is_minority_ethnicity_yes`,  
  and `label_is_minority_ethnicity_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- have_hypertension:

  The value of variable in the parameters
  `label_have_hypertension_no`,  
  `label_have_hypertension_yes`, and `label_have_hypertension_unknown`.

- have_dyslipidemia:

  The value of variable in the parameters
  `label_have_dyslipidemia_no`,  
  `label_have_dyslipidemia_yes` and `label_have_dyslipidemia_unknown`.

- have_smoking_history:

  The value of variable in the parameters
  `label_have_smoking_history_no`,  
  `label_have_smoking_history_yes`,  
  and `label_have_smoking_history_unknown`.

- have_family_history:

  The value of variable in the parameters
  `label_have_family_history_no`,  
  `label_have_family_history_yes`  
  and `label_have_family_history_unknown`.

- have_stress_symptoms:

  The value of variable in the parameters  
  `label_have_stress_symptoms_no`, `label_have_stress_symptoms_yes`,  
  and `label_have_stress_symptoms_unknown`. Default: `NA`

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

- label_is_minority_ethnicity_no:

  Label(s) for patient not from a racial or minority ethnicity (or
  patient is a non-Hispanic/Latino White). Default: `c("no")`

- label_is_minority_ethnicity_yes:

  Label(s) for patient from a racial or minority ethnicity (or patient
  is not a non-Hispanic/Latino White). E.g. Blacks, Asians, etc.
  Default: `c("yes")`

- label_is_minority_ethnicity_unknown:

  Label(s) for patient from an unknown ethnicity Default: `c(NA, NaN)`

- label_have_diabetes_no:

  Label(s) for patient with no diabetes. Default: `c("no")`

- label_have_diabetes_yes:

  Label(s) for patient having diabetes. Default: `c("yes")`

- label_have_diabetes_unknown:

  Label(s) for patient having unknown diabetes. Default: `c(NA, NaN)`

- label_have_hypertension_no:

  Label(s) for patient with no hypertension. Default: `c("no")`

- label_have_hypertension_yes:

  Label(s) for patient having hypertension. Default: `c("yes")`

- label_have_hypertension_unknown:

  Label(s) for patient having unknown hypertension. Default:
  `c(NA, NaN)`

- label_have_dyslipidemia_no:

  Label(s) for patient with no dyslipidemia. Default: `c("no")`

- label_have_dyslipidemia_yes:

  Label(s) for patient having dyslipidemia. Default: `c("yes")`

- label_have_dyslipidemia_unknown:

  Label(s) for patient having unknown dyslipidemia. Default:
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

- label_have_family_history_no:

  Label(s) for patient with no family history of CAD. Default: `c("no")`

- label_have_family_history_yes:

  Label(s) for patient having family history of CAD. Default: `c("yes")`

- label_have_family_history_unknown:

  Label(s) for patient having unknown family history of CAD. Default:
  `c(NA, NaN)`

- label_have_stress_symptoms_no:

  Label(s) for patient with no symptoms (negative results) related to
  physical or mental stress. Default: `c("no")`

- label_have_stress_symptoms_yes:

  Label(s) for patient with symptoms (positive results) related to
  physical or mental stress. Default: `c("yes")`

- label_have_stress_symptoms_unknown:

  Label(s) for patient with inconclusive results or patient has not
  taken any stress test Default: `c(NA, NaN)`

## Value

A numeric value representing the patient's minimal risk score for
obstructive CAD based on the 2017 PROMISE Minimal-Risk Score.

## Details

The predictive model is based on CCTA images from 4632 patients in the
Prospective Multicenter imaging Study for Evaluation of Chest Pain
(PROMISE) trial.

Model formula used is from Table 3.

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -1.783\quad+ \\\\ (0.084 \* age)\quad+ \\\\
(-1.026 \* sex\\is\\female)\quad+ \\\\ (-0.142 \*
is\\minority\\ethnicity)\quad+ \\\\ (-0.526 \* is\\non\\smoker)\quad+
\\\\ (-0.314 \* have\\no\\diabetes)\quad+ \\\\ (-0.412 \*
have\\no\\dyslipidemia)\quad+ \\\\ (-0.309 \*
have\\no\\family_history)\quad+ \\\\ (-0.408 \*
have\\no\\hypertension)\quad+ \\\\ (-0.309 \*
have\\no\\stress_symptoms)\quad+ \\\\ (-0.195 \*
have\\unknown\\stress\\symptoms)\quad+ \\\\ (-0.006 \* hdl\\mg\\dl)
\end{array} \$\$

## Examples

``` r
# 50 year old white female with chest pain
# a medical history of hypertension, and a
# high-density lipoprotein cholesterol level of 70 mg/dL
calculate_prms_2017_ptp(
    age = 50,
    sex = "female",
    hdl_mg_dl = 70,
    is_minority_ethnicity = "no",
    have_diabetes = "no",
    have_hypertension = "yes",
    have_dyslipidemia = "no",
    have_smoking_history = "no",
    have_family_history = "no",
    have_stress_symptoms = "no"
)
#> [1] 0.710744

# 40 year old non-white male with chest pain
# a medical history of diabetes, unknown stress symptoms and a
# high-density lipoprotein cholesterol level of 70 mg/dL
calculate_prms_2017_ptp(
    age = 40,
    sex = "male",
    hdl_mg_dl = 70,
    is_minority_ethnicity = "yes",
    have_diabetes = "yes",
    have_hypertension = "no",
    have_dyslipidemia = "no",
    have_smoking_history = "no",
    have_family_history = "no",
    have_stress_symptoms = NA
)
#> [1] 0.6974111
```
