# Calculate PTP for obstructive CAD using CTA results from COME-CCT Consortium

This function returns a patient's pre-test probability (PTP) of
obstructive coronary artery disease (CAD) based on the model that uses
only CTA results from the COME-CCT Consortium cohort. Obstructive
coronary artery disease was defined as at least one \\\geq\\ 50%
diameter stenosis by invasive coronary angiography (ICA).

## Usage

``` r
calculate_come_cta_alone_2025_ptp(
  cta_result,
  label_cta_obstructive = c("obstructive"),
  label_cta_non_obstructive = c("non_obstructive"),
  label_cta_unknown = c(NA, NaN)
)
```

## Arguments

- cta_result:

  The value of variable in the parameters,  
  `label_cta_obstructive`, `label_cta_non_obstructive`,  
  and `label_cta_unknown`.

- label_cta_obstructive:

  Label(s) for patient having obstructive CAD defined as at least one
  \\\geq\\ 50% diameter stenosis in computed tomography angiography
  (CTA).  
  Default: `c("obstructive")`

- label_cta_non_obstructive:

  Label(s) for patient having non-obstructive CAD defined as at least
  one \< 50% diameter stenosis in computed tomography angiography
  (CTA).  
  Default: `c("non_obstructive")`

- label_cta_unknown:

  Label(s) for patient unknown CTA results. Default: `c(NA, NaN)`

## Value

A numeric value representing the patient's PTP for obstructive CAD based
on the model that uses only CTA results from the COME-CCT Consortium
cohort as shown in Table 2.

## Details

The predictive model is based on 5332 stable chest pain patients with
clinically indicated ICA from 22 countries.

Model formula used is from Table 3 to obtain results as shown in Table
2.

It is of the form \$\$\int\_{-\infty}^{\infty} \frac{1}{(1 + e^{-(F(x) +
\zeta)})} \* \frac{1}{\sqrt{2\pi \* 0.348}} e^{\frac{-\zeta^2}{2 \*
0.348}} \\d\zeta\$\$ where \\F(x)\\ equals \$\$ \begin{array}{l}
-1.893\quad+ \\\\ (2.973 \* have\\obstructive\\CAD\\in\\CTA) \end{array}
\$\$

## Examples

``` r
# A patient with obstructive CAD in CTA
calculate_come_cta_alone_2025_ptp(
    cta_result = "obstructive"
)
#> [1] 0.7318626

# A patient with non-obstructive CAD in CTA
calculate_come_cta_alone_2025_ptp(
    cta_result = "non_obstructive"
)
#> [1] 0.1449881
```
