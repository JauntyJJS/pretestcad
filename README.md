

<a name="top"></a>

# pretestcad <a href="jauntyjjs.github.io/pretestcad/"><img src="man/figures/logo.png" align="right" height="200" alt="Hex logo of R package pretestcad. Logo is a picture of a coronary artery with partial blockage." /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/JauntyJJS/pretestcad/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JauntyJJS/pretestcad?branch=main)
<!-- badges: end -->

R package used to calculate different **P**re**T**est **P**robability
(PTP) scores for obstructive **C**oronary **A**rtery **D**isease (CAD).

## Installation

Install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("JauntyJJS/pretestcad")
```

## Usage

### 2024 ESC Guidelines PTP Score

``` r
# 30 female with symptom score of 0 and 0 risk factors

calculate_esc_2024_fig_4_ptp(
 age = 30,
 sex = "female",
 chest_pain_type = "no chest pain",
 have_dyspnea = "no",
 have_family_history = "no",
 have_smoking_history = "no",
 have_dyslipidemia = "no",
 have_hypertension = "no",
 have_diabetes = "no",
 output = "numeric"
)
```

    [1] 0

``` r
calculate_esc_2024_fig_4_ptp(
 age = 30,
 sex = "female",
 chest_pain_type = "no chest pain",
 have_dyspnea = "no",
 have_family_history = "no",
 have_smoking_history = "no",
 have_dyslipidemia = "no",
 have_hypertension = "no",
 have_diabetes = "no",
 output = "grouping"
)
```

    [1] "very low"

## Currently available pretest probability scores

- 2024 ESC Guidelines PTP Score
  - <a href="https://doi.org/10.1093/eurheartj/ehae177" target="_blank">📘
    Journal</a>
- 2022 Local Assessment of the Heart (LAH) clinical and extended model
  - <a href="https://doi.org/10.1161/JAHA.121.022697" target="_blank">📘
    Journal</a>
- 2020 Winther et. al. Basic, RF-CL and CACS-CL PTP
  - <a href="https://doi.org/10.1016/j.jacc.2020.09.585" target="_blank">📘
    Journal</a>
- 2019 ESC Guidelines PTP Score
  - <a href="https://doi.org/10.1007/s00059-020-04935-x" target="_blank">📘
    Journal</a>
- 2017 PROMISE Minimal-Risk Score
  - <a href="https://doi.org/10.1001/jamacardio.2016.5501"
    target="_blank">📘 Journal</a>
- 2012 CAD Consortium 2 (CAD2) Basic, Clinical and Clinical with
  Coronary Calcium Score (CCS) PTP
  - <a href="https://doi.org/10.1136/bmj.e3485" target="_blank">📘
    Journal</a>
