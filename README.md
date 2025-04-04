

<a name="top"></a>

# pretestcad <a href="jauntyjjs.github.io/pretestcad/"><img src="man/figures/logo.png" align="right" width="25%" height="25%" alt="Hex logo of R package pretestcad. Logo is a picture of a coronary artery with partial blockage." /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/JauntyJJS/pretestcad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JauntyJJS/pretestcad/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/JauntyJJS/pretestcad/graph/badge.svg?token=lpJSgliKK3)](https://codecov.io/gh/JauntyJJS/pretestcad?branch=main)
<!-- badges: end -->

R package used to calculate different **P**re**T**est **P**robability
(PTP) scores for obstructive **C**oronary **A**rtery **D**isease (CAD).

## Table of Content

- [:arrow_down: Installation](#arrow_down-installation)
- [:anatomical_heart: Currently available pretest probability
  scores](#anatomical_heart-currently-available-pretest-probability-scores)
- [:computer: Getting Started](#computer-getting-started)

## :arrow_down: Installation

Install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("JauntyJJS/pretestcad")
```

<a href="#top">Back to top</a>

## :anatomical_heart: Currently available pretest probability scores

- 2024 ESC Guidelines PTP Score
  - <a href="https://doi.org/10.1093/eurheartj/ehae177" target="_blank">📘
    Journal</a>
- 2022 Local Assessment of the Heart (LAH) clinical and extended model
  - <a href="https://doi.org/10.1161/JAHA.121.022697" target="_blank">📘
    Journal</a>
- 2021 AHA/ACC Guidelines PTP Score
  - <a href="https://doi.org/10.1161/CIR.0000000000001029"
    target="_blank">📘 Journal</a>
- 2020 Winther et. al. Basic, RF-CL and CACS-CL PTP
  - <a href="https://doi.org/10.1016/j.jacc.2020.09.585" target="_blank">📘
    Journal</a>
- 2019 ESC Guidelines PTP Score
  - <a href="https://doi.org/10.1007/s00059-020-04935-x" target="_blank">📘
    Journal</a>
- 2019 Reeh et. al. basic and clinical model
  - <a href="https://doi.org/10.1093/eurheartj/ehy806" target="_blank">📘
    Journal</a>
- 2017 PROMISE Minimal-Risk Score
  - <a href="https://doi.org/10.1001/jamacardio.2016.5501"
    target="_blank">📘 Journal</a>
- 2015 CONFIRM Risk Score
  - <a href="https://doi.org/10.1016/j.amjmed.2014.10.031"
    target="_blank">📘 Journal</a>
- 2012 CAD Consortium 2 (CAD2) Basic, Clinical and Clinical with
  Coronary Calcium Score (CCS) PTP
  - <a href="https://doi.org/10.1136/bmj.e3485" target="_blank">📘
    Journal</a>
- 2012 ACCF/AHA/ACP/AATS/PCNA/SCAI/STS Guidelines PTP Score
  - <a href="https://doi.org/10.1161/CIR.0b013e318277d6a0"
    target="_blank">📘 Journal</a>

<a href="#top">Back to top</a>

## :computer: Getting Started

### 2024 ESC Guidelines PTP Score

Here is how you can calculate the score using a single patient.

``` r
# 30 female with symptom score of 0 and 0 risk factors

calculate_esc_2024_fig_4_ptp(
 age = 30,
 sex = "female",
 chest_pain_type = "no chest pain",
 have_dyspnoea = "no",
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
 have_dyspnoea = "no",
 have_family_history = "no",
 have_smoking_history = "no",
 have_dyslipidemia = "no",
 have_hypertension = "no",
 have_diabetes = "no",
 output = "grouping"
)
```

    [1] "very low"

Here is how you can calculate the score using for multiple patients.

``` r
patient_data <- tibble::tribble(
    ~unique_id,
    ~age,     ~sex, 
    ~chest_pain_type, ~have_dyspnoea, 
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes,
    "45 year old male with typical chest pain, no dyspnoea, hypertension and diabetes",
    45, "male", 
    "typical", "no",  
    "no", "no", "no", "yes", "yes",
    "70 year old female with no chest pain, dyspnoea, have smoking history (past or current smoker) and dyslipidemia",
    70, "female", 
    "no chest pain", "yes",  
    "no", "yes", "yes", "no", "no"
)

risk_data <- patient_data |>
    dplyr::mutate(
      esc_2024_ptp_group = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]], 
          have_dyslipidemia = .data[["have_dyslipidemia"]], 
          have_hypertension = .data[["have_hypertension"]], 
          have_diabetes = .data[["have_diabetes"]],
          output = "grouping"
        ),
        .f = pretestcad::calculate_esc_2024_fig_4_ptp,
      ),
      esc_2024_ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]],
          output = "numeric"
        ),
        .f = pretestcad::calculate_esc_2024_fig_4_ptp
      ),
      esc_2024_ptp_percent = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]],
          output = "percentage"
        ),
        .f = pretestcad::calculate_esc_2024_fig_4_ptp
      )
    ) |> 
   dplyr::select(
      c("unique_id", "esc_2024_ptp_group", 
        "esc_2024_ptp_numeric", "esc_2024_ptp_percent")
   )

print(risk_data)
```

    # A tibble: 2 × 4
      unique_id         esc_2024_ptp_group esc_2024_ptp_numeric esc_2024_ptp_percent
      <chr>             <chr>                             <int> <chr>               
    1 45 year old male… moderate                             20 20%                 
    2 70 year old fema… low                                  10 10%                 

<a href="#top">Back to top</a>
