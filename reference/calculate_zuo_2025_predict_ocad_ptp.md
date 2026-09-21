# Calculate 2025 Zuo PREDICT-OCAD PTP for obstructive CAD

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the 2025 Zuo et. al.
PREDICT-OCAD model. Obstructive CAD was defined as \\\geq\\50% stenosis
on CCTA.

## Usage

``` r
calculate_zuo_2025_predict_ocad_ptp(
  age,
  sex,
  have_dyslipidemia,
  have_diabetes,
  coronary_calcium_score,
  label_sex_male = c("male"),
  label_sex_female = c("female"),
  label_sex_unknown = c(NA, NaN),
  label_have_dyslipidemia_no = c("no"),
  label_have_dyslipidemia_yes = c("yes"),
  label_have_dyslipidemia_unknown = c(NA, NaN),
  label_have_diabetes_no = c("no"),
  label_have_diabetes_yes = c("yes"),
  label_have_diabetes_unknown = c(NA, NaN)
)
```

## Arguments

- age:

  Input numeric value to indicate the age of the patient in years.

- sex:

  The value of variable in the parameters `label_sex_male`,  
  `label_sex_female` and `label_sex_unknown`.

- have_dyslipidemia:

  The value of variable in the parameters
  `label_have_dyslipidemia_no`,  
  `label_have_dyslipidemia_yes` and `label_have_dyslipidemia_unknown`.

- have_diabetes:

  The value of variable in the parameters `label_have_diabetes_no`,  
  `label_have_diabetes_yes` and `label_have_diabetes_unknown`.

- coronary_calcium_score:

  Input non-negative integer to indicate the total coronary calcium
  score of the patient.

- label_sex_male:

  Label(s) for definition(s) of male sex. Default: `c("male")`

- label_sex_female:

  Label(s) for definition(s) of female sex. Default: `c("female")`

- label_sex_unknown:

  Label(s) for definition(s) of missing sex. Default: `c(NA, NaN)`

- label_have_dyslipidemia_no:

  Label(s) for patient with no dyslipidemia. Default: `c("no")`

- label_have_dyslipidemia_yes:

  Label(s) for patient having dyslipidemia. Default: `c("yes")`

- label_have_dyslipidemia_unknown:

  Label(s) for patient having unknown dyslipidemia. Default:
  `c(NA, NaN)`

- label_have_diabetes_no:

  Label(s) for patient with no diabetes. Default: `c("no")`

- label_have_diabetes_yes:

  Label(s) for patient having diabetes. Default: `c("yes")`

- label_have_diabetes_unknown:

  Label(s) for patient having unknown diabetes. Default: `c(NA, NaN)`

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the 2025 Zuo et. al. PREDICT-OCAD model.

## Details

The predictive model is based on 2649 patients suspected of CAD at
Jinling Hospital, Nanjing University (Jiangsu, China), spanning from
January 2010 to December 2020.

Model formula used is taken from the web-based
[calculator](https://predict-obstructive-cad.shinyapps.io/dynnomapp/).

It is of the form \$\$\frac{1}{(1 + e^{-F(x)})}\$\$ where \\F(x)\\
equals \$\$ \begin{array}{l} -4.336859\quad+ \\\\ (-0.422110 \*
sex\\is\\female)\quad+ \\\\ (0.029603 \* age)\quad+ \\\\ (-0.539890 \*
no\\dyslipidemia)\quad+ \\\\ (-0.210570 \* no\\diabetes)\quad+ \\\\
(1.925711 \* cacs\\1\\to\\100)\quad+ \\\\ (3.362762 \*
cacs\\101\\to\\400)\quad+ \\\\ (4.377606 \* cacs\\more\\than\\400)
\end{array} \$\$

## Examples

``` r
# 40 year old female without dyslipidemia and diabetes
# with coronary calcium score of 0
calculate_zuo_2025_predict_ocad_ptp(
    age = 40,
    sex = "female",
    have_dyslipidemia = "no",
    have_diabetes = "no",
    coronary_calcium_score = 0
)
#> [1] 0.01305673
```
