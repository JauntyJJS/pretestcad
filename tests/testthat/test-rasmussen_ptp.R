test_that("calculate_rasmussen_2025_rf_cl_ccta_ptp works", {

  # Verified with https://github.com/CardioLab/cadptp/blob/master/R/cadptp_cta.R

  medical_data <- tibble::tribble(
    ~age,    ~sex,
    ~chest_pain_type, ~have_dyspnoea,
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes,
    ~expected_score,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no",  "no",  "no", 0.009961304,
    39, "female",    "nonanginal",  "no", "yes",  "no",  "no",  "no",  "no", 0.0177169,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no", "yes", "yes", 0.02357935,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes",  "no",  "no", 0.03873366,
    30, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes",  "no", 0.05478438,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes", "yes", 0.08258557,

    30,   "male", "no chest pain",  "no",  "no",  "no",  "no",  "no",  "no", 0.02520178,
    39,   "male", "no chest pain",  "no",  "no", "yes",  "no",  "no",  "no", 0.04429232,
    30,   "male", "no chest pain",  "no", "yes",  "no",  "no",  "no", "yes", 0.04883306,
    39,   "male", "no chest pain",  "no",  "no", "yes", "yes", "yes",  "no", 0.07890616,
    30,   "male", "no chest pain",  "no", "yes", "yes", "yes",  "no", "yes", 0.09251989,
    39,   "male", "no chest pain",  "no", "yes", "yes", "yes", "yes", "yes", 0.1367011,

    30, "female",      "atypical",  "no",  "no",  "no",  "no",  "no",  "no", 0.01716589,
    39, "female",      "atypical",  "no",  "no",  "no", "yes",  "no",  "no", 0.03035877,
    30, "female",      "atypical",  "no", "yes", "yes",  "no",  "no",  "no", 0.04023304,
    39, "female",      "atypical",  "no",  "no",  "no", "yes", "yes", "yes", 0.06537395,
    30, "female",      "atypical",  "no", "yes", "yes", "no",  "yes", "yes", 0.09141424,
    39, "female",      "atypical",  "no", "yes", "yes", "yes", "yes", "yes", 0.1351461,

    30,   "male",      "atypical",  "no",  "no",  "no",  "no",  "no",  "no", 0.04295094,
    39,   "male",      "atypical",  "no",  "no",  "no",  "no", "yes",  "no", 0.07445962,
    30,   "male",      "atypical",  "no",  "no", "yes",  "no",  "no", "yes", 0.08182824,
    39,   "male",      "atypical",  "no", "yes",  "no", "yes", "yes",  "no", 0.1294554,
    30,   "male",      "atypical",  "no", "yes",  "no", "yes", "yes", "yes", 0.1503667,
    39,   "male",      "atypical",  "no", "yes", "yes", "yes", "yes", "yes", 0.2156084,

    30, "female",       "typical",  "no",  "no",  "no",  "no",  "no",  "no", 0.05206535,
    39, "female",       "typical",  "no",  "no",  "no",  "no",  "no", "yes", 0.08562114,
    30, "female",       "typical",  "no",  "no", "yes", "yes",  "no",  "no", 0.10542,
    39, "female",       "typical",  "no", "yes",  "no",  "no", "yes", "yes", 0.1575429,
    30, "female",       "typical",  "no",  "no", "yes", "yes", "yes", "yes", 0.2018103,
    39, "female",       "typical",  "no", "yes", "yes", "yes", "yes", "yes", 0.2719134,

    30,   "male",       "typical",  "no",  "no",  "no",  "no",  "no",  "no", 0.1236765,
    39,   "male",       "typical",  "no", "yes",  "no",  "no",  "no",  "no", 0.1939426,
    30,   "male",       "typical",  "no",  "no", "yes",  "no", "yes",  "no", 0.2003414,
    39,   "male",       "typical",  "no", "yes",  "no", "yes",  "no", "yes", 0.2844723,
    30,   "male",       "typical",  "no", "yes", "yes", "yes", "yes",  "no", 0.3078355,
    39,   "male",       "typical",  "no", "yes", "yes", "yes", "yes", "yes", 0.3964745,

    40, "female", "no chest pain",  "no",  "no",  "no",  "no",  "no",  "no", 0.01888155,
    49, "female", "no chest pain",  "no",  "no", "yes",  "no",  "no",  "no", 0.03334826,
    40, "female", "no chest pain",  "no", "yes",  "no", "yes",  "no",  "no", 0.04090821,
    49, "female", "no chest pain",  "no",  "no", "yes",  "no", "yes", "yes", 0.06644182,
    40, "female", "no chest pain",  "no", "yes", "yes", "yes",  "no", "yes", 0.08636852,
    49, "female", "no chest pain",  "no", "yes", "yes", "yes", "yes", "yes", 0.1280267,

    40,   "male",    "nonanginal",  "no",  "no",  "no",  "no",  "no",  "no", 0.04712015,
    49,   "male",    "nonanginal",  "no",  "no",  "no", "yes",  "no",  "no", 0.08142712,
    40,   "male",    "nonanginal",  "no",  "no",  "no", "yes",  "no", "yes", 0.08314098,
    49,   "male",    "nonanginal",  "no", "yes", "yes",  "no", "yes",  "no", 0.1314228,
    40,   "male",    "nonanginal",  "no", "yes", "yes",  "no", "yes", "yes", 0.1425776,
    49,   "male",    "nonanginal",  "no", "yes", "yes", "yes", "yes", "yes", 0.2052563,

    40, "female",    "nonanginal", "yes",  "no",  "no",  "no",  "no",  "no", 0.03232711,
    49, "female",    "nonanginal", "yes",  "no",  "no",  "no", "yes",  "no", 0.05650229,
    40, "female",    "nonanginal", "yes",  "no",  "no", "yes", "yes",  "no", 0.06893687,
    49, "female",    "nonanginal", "yes", "yes", "yes",  "no",  "no", "yes", 0.1099593,
    40, "female",    "nonanginal", "yes", "yes",  "no", "yes", "yes", "yes", 0.1409667,
    49, "female",    "nonanginal", "yes", "yes", "yes", "yes", "yes", "yes", 0.2031049,

    40,   "male",      "atypical", "yes",  "no",  "no",  "no",  "no",  "no", 0.07905416,
    49,   "male",      "atypical", "yes",  "no",  "no",  "no",  "no", "yes", 0.1333575,
    40,   "male",      "atypical", "yes", "yes",  "no",  "no", "yes",  "no", 0.1360026,
    49,   "male",      "atypical", "yes",  "no", "yes", "yes",  "no", "yes", 0.2080176,
    40,   "male",      "atypical", "yes",  "no", "yes", "yes", "yes", "yes", 0.223997,
    49,   "male",      "atypical", "yes", "yes", "yes", "yes", "yes", "yes", 0.3095465

  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_rasmussen_2025_rf_cl_ccta_ptp,
        allow_na_symptom_score = TRUE,
        max_na_num_of_rf = 0
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    medical_data[["expected_score"]],
    tolerance = 1e-5
  )

})

test_that("calculate_rasmussen_2025_cacs_cl_ccta_ptp works", {

  # Verified with https://github.com/CardioLab/cadptp/blob/master/R/cadptp_cta.R

  medical_data <- tibble::tribble(
    ~age,    ~sex,
    ~chest_pain_type, ~have_dyspnoea,
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes,
    ~coronary_calcium_score,
    ~expected_score,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no",  "no",  "no",    0, 0.01723182,
    39, "female",    "nonanginal",  "no", "yes",  "no",  "no",  "no",  "no",   10, 0.1407749,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no", "yes", "yes",  100, 0.3694392,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes",  "no",  "no",  400, 0.6164495,
    30, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes",  "no", 1000, 0.7638157,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes", "yes",    9, 0.08845846
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
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
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_rasmussen_2025_cacs_cl_ccta_ptp,
        allow_na_symptom_score = TRUE,
        max_na_num_of_rf = 0
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    medical_data[["expected_score"]],
    tolerance = 1e-5
  )

})