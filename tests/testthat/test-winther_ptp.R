test_that("calculate_winther_2020_basic_ptp works", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age,     ~sex, ~chest_pain_type,
    "40 male with typical chest pain",     40,   "male",        "typical",
    "40 female with atypical chest pain",  40, "female",       "atypical",
    "40 male with non anginal chest pain", 40,   "male",     "nonanginal",
    "79 female with typical chest pain",   79, "female",        "typical"
  )

  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_winther_2020_basic_ptp
      )
    )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric"]],
    c(0.14302369, 0.01090790,
      0.02218292, 0.22057821),
    tolerance = 1e-5
  )

})


test_that("calculate_winther_2020_rf_cl_ptp works", {

  # Verified with https://github.com/CardioLab/cadptp/blob/master/R/cadptp.R

  medical_data <- tibble::tribble(
    ~age,    ~sex,
    ~chest_pain_type, ~have_dyspnea,
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes,
    ~expected_score,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no",  "no",  "no", 0.00172552,
    39, "female",    "nonanginal",  "no", "yes",  "no",  "no",  "no",  "no", 0.003272902,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no", "yes", "yes", 0.005171025,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes",  "no",  "no", 0.00869995,
    30, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes",  "no", 0.01539042,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes", "yes", 0.02291907,

    30,   "male", "no chest pain",  "no",  "no",  "no",  "no",  "no",  "no", 0.006986254,
    39,   "male", "no chest pain",  "no",  "no", "yes",  "no",  "no",  "no", 0.01318894,
    30,   "male", "no chest pain",  "no", "yes",  "no",  "no",  "no", "yes", 0.01687505,
    39,   "male", "no chest pain",  "no",  "no", "yes", "yes", "yes",  "no", 0.02816516,
    30,   "male", "no chest pain",  "no", "yes", "yes", "yes",  "no", "yes", 0.04019452,
    39,   "male", "no chest pain",  "no", "yes", "yes", "yes", "yes", "yes", 0.05912814,

    30, "female",      "atypical",  "no",  "no",  "no",  "no",  "no",  "no", 0.002751099,
    39, "female",      "atypical",  "no",  "no",  "no", "yes",  "no",  "no", 0.005213379,
    30, "female",      "atypical",  "no", "yes", "yes",  "no",  "no",  "no", 0.008227594,
    39, "female",      "atypical",  "no",  "no",  "no", "yes", "yes", "yes", 0.01381349,
    30, "female",      "atypical",  "no", "yes", "yes", "no",  "yes", "yes", 0.02433984,
    39, "female",      "atypical",  "no", "yes", "yes", "yes", "yes", "yes", 0.0360859,

    30,   "male",      "atypical",  "no",  "no",  "no",  "no",  "no",  "no", 0.01110382,
    39,   "male",      "atypical",  "no",  "no",  "no",  "no", "yes",  "no", 0.02088537,
    30,   "male",      "atypical",  "no",  "no", "yes",  "no",  "no", "yes", 0.02666439,
    39,   "male",      "atypical",  "no", "yes",  "no", "yes", "yes",  "no", 0.04420946,
    30,   "male",      "atypical",  "no", "yes",  "no", "yes", "yes", "yes", 0.06264959,
    39,   "male",      "atypical",  "no", "yes", "yes", "yes", "yes", "yes", 0.09115609,

    30, "female",       "typical",  "no",  "no",  "no",  "no",  "no",  "no", 0.01758962,
    39, "female",       "typical",  "no",  "no",  "no",  "no",  "no", "yes", 0.02794158,
    30, "female",       "typical",  "no",  "no", "yes", "yes",  "no",  "no", 0.03910556,
    39, "female",       "typical",  "no", "yes",  "no",  "no", "yes", "yes", 0.05488326,
    30, "female",       "typical",  "no",  "no", "yes", "yes", "yes", "yes", 0.08467181,
    39, "female",       "typical",  "no", "yes", "yes", "yes", "yes", "yes", 0.1049966,

    30,   "male",       "typical",  "no",  "no",  "no",  "no",  "no",  "no", 0.06792558,
    39,   "male",       "typical",  "no", "yes",  "no",  "no",  "no",  "no", 0.1047431,
    30,   "male",       "typical",  "no",  "no", "yes",  "no", "yes",  "no", 0.1184699,
    39,   "male",       "typical",  "no", "yes",  "no", "yes",  "no", "yes", 0.1609063,
    30,   "male",       "typical",  "no", "yes", "yes", "yes", "yes",  "no", 0.1986107,
    39,   "male",       "typical",  "no", "yes", "yes", "yes", "yes", "yes", 0.23914,

    40, "female", "no chest pain",  "no",  "no",  "no",  "no",  "no",  "no", 0.00351393,
    49, "female", "no chest pain",  "no",  "no", "yes",  "no",  "no",  "no", 0.006654402,
    40, "female", "no chest pain",  "no", "yes",  "no", "yes",  "no",  "no", 0.009216506,
    49, "female", "no chest pain",  "no",  "no", "yes",  "no", "yes", "yes", 0.01546333,
    40, "female", "no chest pain",  "no", "yes", "yes", "yes",  "no", "yes", 0.02395106,
    49, "female", "no chest pain",  "no", "yes", "yes", "yes", "yes", "yes", 0.03551634,

    40,   "male",    "nonanginal",  "no",  "no",  "no",  "no",  "no",  "no", 0.01414985,
    49,   "male",    "nonanginal",  "no",  "no",  "no", "yes",  "no",  "no", 0.02654268,
    40,   "male",    "nonanginal",  "no",  "no",  "no", "yes",  "no", "yes", 0.02980271,
    49,   "male",    "nonanginal",  "no", "yes", "yes",  "no", "yes",  "no", 0.04930818,
    40,   "male",    "nonanginal",  "no", "yes", "yes",  "no", "yes", "yes", 0.0616876,
    49,   "male",    "nonanginal",  "no", "yes", "yes", "yes", "yes", "yes", 0.08979831,

    40, "female",    "nonanginal", "yes",  "no",  "no",  "no",  "no",  "no", 0.005596509,
    49, "female",    "nonanginal", "yes",  "no",  "no",  "no", "yes",  "no", 0.01057847,
    40, "female",    "nonanginal", "yes",  "no",  "no", "yes", "yes",  "no", 0.01462918,
    49, "female",    "nonanginal", "yes", "yes", "yes",  "no",  "no", "yes", 0.02445408,
    40, "female",    "nonanginal", "yes", "yes",  "no", "yes", "yes", "yes", 0.03768789,
    49, "female",    "nonanginal", "yes", "yes", "yes", "yes", "yes", "yes", 0.05550905,

    40,   "male",      "atypical", "yes",  "no",  "no",  "no",  "no",  "no", 0.0223943,
    49,   "male",      "atypical", "yes",  "no",  "no",  "no",  "no", "yes", 0.04170239,
    40,   "male",      "atypical", "yes", "yes",  "no",  "no", "yes",  "no", 0.04673498,
    49,   "male",      "atypical", "yes",  "no", "yes", "yes",  "no", "yes", 0.07644915,
    40,   "male",      "atypical", "yes",  "no", "yes", "yes", "yes", "yes", 0.09496201,
    49,   "male",      "atypical", "yes", "yes", "yes", "yes", "yes", "yes", 0.1360374

  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_winther_2020_rf_cl_ptp,
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



test_that("calculate_winther_2020_cacs_cl_ptp works", {

  # Verified with https://github.com/CardioLab/cadptp/blob/master/R/cadptp.R

  medical_data <- tibble::tribble(
    ~age,    ~sex,
    ~chest_pain_type, ~have_dyspnea,
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes,
    ~coronary_calcium_score,
    ~expected_score,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no",  "no",  "no",    0, 0.001648728,
    39, "female",    "nonanginal",  "no", "yes",  "no",  "no",  "no",  "no",   10, 0.02671354,
    30, "female",    "nonanginal",  "no",  "no",  "no",  "no", "yes", "yes",  100, 0.1175533,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes",  "no",  "no",  400, 0.2377107,
    30, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes",  "no", 1000, 0.4153192,
    39, "female",    "nonanginal",  "no", "yes", "yes", "yes", "yes", "yes",    9, 0.01713664,


  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_winther_2020_cacs_cl_ptp,
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
