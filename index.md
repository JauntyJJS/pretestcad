# pretestcad

R package used to calculate different **P**re**T**est **P**robability
(PTP) scores for obstructive **C**oronary **A**rtery **D**isease (CAD).

As diagnosis of CAD involves a costly and invasive coronary angiography
procedure for patients, having a reliable PTP for CAD helps doctors to
make better decisions during patient management. This ensures high risk
patients can be diagnosed and treated early for CAD while avoiding
unnecessary testing for low-risk patients.

## Table of Content

- [⬇️ Installation](#arrow_down-installation)
- [🫀 Currently available pretest probability
  scores](#anatomical_heart-currently-available-pretest-probability-scores)
- [💻 Getting Started](#computer-getting-started)

## ⬇️ Installation

Install the development version from GitHub with:

``` r

# install.packages("pak")
pak::pak("JauntyJJS/pretestcad")
```

[Back to top](#top)

## 🫀 Currently available pretest probability scores

- 2025 Rasmussen et. al. RF-CL_(CCTA) and CACS-CL_(CCTA) PTP
  - [📘 Journal](https://doi.org/10.1093/ehjci/jeaf049)
- 2025 Wieske et. al. COME-CCT PTP
  - [📘 Journal](https://doi.org/10.1016/j.jacadv.2025.102014)  
- 2025 Zuo et. al. PREDICT-OCAD PTP
  - [📘 Journal](https://doi.org/10.1007/s42058-025-00189-w)
- 2024 ESC Guidelines PTP Score
  - [📘 Journal](https://doi.org/10.1093/eurheartj/ehae177)
- 2023 Miller et. al. PTP Score (Likelihood Tables)
  - [📘 Journal](https://doi.org/10.1161/JAHA.123.031601)
- 2022 Local Assessment of the Heart (LAH) clinical and extended model
  - [📘 Journal](https://doi.org/10.1161/JAHA.121.022697)
- 2021 Predictive Risk scorE for CAD In Southeast Asians with chEst pain
  (PRECISE) simple and clinical model
  - [📘 Journal](https://doi.org/10.1007/s11606-021-06701-z)
- 2021 AHA/ACC Guidelines PTP Score
  - [📘 Journal](https://doi.org/10.1161/CIR.0000000000001029)
- 2020 Winther et. al. Basic, RF-CL and CACS-CL PTP
  - [📘 Journal](https://doi.org/10.1016/j.jacc.2020.09.585)
- 2019 ESC Guidelines PTP Score
  - [📘 Journal](https://doi.org/10.1007/s00059-020-04935-x)
- 2019 Reeh et. al. basic and clinical model
  - [📘 Journal](https://doi.org/10.1093/eurheartj/ehy806)
- 2018 Chen et. al. Modified Framingham Scoring
  - [📘 Journal](https://doi.org/10.1186/s12872-018-0745-0)  
- 2017 PROMISE Minimal-Risk Score
  - [📘 Journal](https://doi.org/10.1001/jamacardio.2016.5501)
- 2015 CONFIRM Risk Score
  - [📘 Journal](https://doi.org/10.1016/j.amjmed.2014.10.031)
- 2013 ESC Guidelines PTP Score
  - [📘 Journal](https://doi.org/10.1093/eurheartj/eht296)
- 2012 CAD Consortium 2 (CAD2) Basic, Clinical and Clinical with
  Coronary Calcium Score (CCS) PTP
  - [📘 Journal](https://doi.org/10.1136/bmj.e3485)
- 2012 ACCF/AHA/ACP/AATS/PCNA/SCAI/STS Guidelines PTP Score
  - [📘 Journal](https://doi.org/10.1161/CIR.0b013e318277d6a0)
- 2011 CAD Consortium 1 (CAD1) PTP (Updated Diamond-Forrester PTP Score)
  - [📘 Journal](https://doi.org/10.1093/eurheartj/ehr014)
- 1993 Duke Clinical Score for Significant and Severe CAD
  - [📘
    Journal](https://doi.org/10.7326/0003-4819-118-2-199301150-00001)
- 1979 Diamond-Forrester PTP
  - [📘 Journal](https://doi.org/10.1056/NEJM197906143002402)

[Back to top](#top)

## 💻 Getting Started

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

``` R
[1] 0
```

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

``` R
[1] "very low"
```

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

``` R
# A tibble: 2 × 4
  unique_id         esc_2024_ptp_group esc_2024_ptp_numeric esc_2024_ptp_percent
  <chr>             <chr>                             <int> <chr>               
1 45 year old male… moderate                             20 20%                 
2 70 year old fema… low                                  10 10%                 
```

[Back to top](#top)
