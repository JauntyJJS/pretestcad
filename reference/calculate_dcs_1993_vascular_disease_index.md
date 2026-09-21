# Calculate The Vascular Disease Index For Duke Clinical Score 1993

A function used to calculate the patient's vascular disease index. This
is used to calculate the likelihood of severe coronary artery disease in
the Duke Clinical Score 1993 paper.

## Usage

``` r
calculate_dcs_1993_vascular_disease_index(
  have_peripheral_vascular_disease,
  have_cerebrovascular_disease,
  have_carotid_bruits,
  max_na = 0,
  label_have_pvd_no = c("no"),
  label_have_pvd_yes = c("yes"),
  label_have_pvd_unknown = c(NA, NaN),
  label_have_cvd_no = c("no"),
  label_have_cvd_yes = c("yes"),
  label_have_cvd_unknown = c(NA, NaN),
  label_have_carotid_bruits_no = c("no"),
  label_have_carotid_bruits_yes = c("yes"),
  label_have_carotid_bruits_unknown = c(NA, NaN),
  error_call = rlang::caller_env()
)
```

## Arguments

- have_peripheral_vascular_disease:

  The value of variable in the parameters `label_have_pvd_no`,  
  `label_have_pvd_yes`, and `label_have_pvd_unknown`.

- have_cerebrovascular_disease:

  The value of variable in the parameters `label_have_cvd_no`,  
  `label_have_cvd_yes`, and `label_have_cvd_unknown`.

- have_carotid_bruits:

  The value of variable in the parameters
  `label_have_carotid_bruits_no`,  
  `label_have_carotid_bruits_yes`,  
  and `label_have_carotid_bruits_unknown`.

- max_na:

  Input integer 0 to 3 to indicate the maximum number of missing disease
  history to tolerate before outputting an `NA`. Default: 0

- label_have_pvd_no:

  Label(s) for patient not having peripheral vascular disease. Default:
  `c("no")`

- label_have_pvd_yes:

  Label(s) for patient having peripheral vascular disease. Default:
  `c("yes")`

- label_have_pvd_unknown:

  Label(s) for patient having unknown peripheral vascular disease.
  Default: `c(NA, NaN)`

- label_have_cvd_no:

  Label(s) for patient not having cerebrovascular disease. Default:
  `c("no")`

- label_have_cvd_yes:

  Label(s) for patient having cerebrovascular disease. Default:
  `c("yes")`

- label_have_cvd_unknown:

  Label(s) for patient having unknown cerebrovascular disease. Default:
  `c(NA, NaN)`

- label_have_carotid_bruits_no:

  Label(s) for patient not having carotid bruits. Default: `c("no")`

- label_have_carotid_bruits_yes:

  Label(s) for patient having carotid bruits. Default: `c("yes")`

- label_have_carotid_bruits_unknown:

  Label(s) for patient having unknown carotid bruits. Default:
  `c(NA, NaN)`

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer indicating the patient's vascular disease index. It can also
be `NA` if the number of missing disease history exceeds the `max_na`
input value.

## Examples

``` r
calculate_dcs_1993_vascular_disease_index(
  have_peripheral_vascular_disease = "yes",
  have_cerebrovascular_disease = "yes",
  have_carotid_bruits = "no"
)
#> [1] 2

calculate_dcs_1993_vascular_disease_index(
  have_peripheral_vascular_disease = NA,
  have_cerebrovascular_disease = "yes",
  have_carotid_bruits = "no",
  max_na = 0
)
#> [1] NA

calculate_dcs_1993_vascular_disease_index(
  have_peripheral_vascular_disease = NA,
  have_cerebrovascular_disease = "yes",
  have_carotid_bruits = "no",
  max_na = 1
)
#> [1] 1
```
