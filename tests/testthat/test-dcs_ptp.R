test_that("calculate_dcs_1993_risk_factor_index works", {

  medical_data <- tibble::tribble(
     ~have_hypertension, ~have_dyslipidemia, ~have_diabetes, ~expected_score,
    "no",               "no",                "no",           0,
    "no",               "no",                "yes",          1,
    "no",               "yes",               "yes",          2,
    "yes",              "yes",               "yes",          3
  )

  medical_data_with_na <- tibble::tribble(
    ~have_hypertension, ~have_dyslipidemia, ~have_diabetes, ~expected_score,
     NA,               "no",           "no",               0,
    "no",               NA,          "yes",                1,
    "yes",              "yes",          NA,                2
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      risk_factor_index = purrr::pmap_dbl(
        .l = list(
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_risk_factor_index,
        max_na = 0
      )
    )

  medical_data_with_na <- medical_data_with_na |>
    dplyr::mutate(
      risk_factor_index = purrr::pmap_dbl(
        .l = list(
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_risk_factor_index,
        max_na = 1
      )
    )

  medical_data_with_na_fail <- medical_data_with_na |>
    dplyr::mutate(
      risk_factor_index = purrr::pmap_dbl(
        .l = list(
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_risk_factor_index,
        max_na = 0
      )
    )

  testthat::expect_equal(
    medical_data[["risk_factor_index"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data_with_na[["risk_factor_index"]],
    medical_data_with_na[["expected_score"]]
  )

  testthat::expect_equal(
    all(is.na(medical_data_with_na_fail[["risk_factor_index"]])),
    TRUE
  )

})

test_that("calculate_dcs_1993_pain_index works", {

  medical_data <- tibble::tribble(
    ~have_typical_chest_pain, ~frequency_of_angina_pain_per_week,
    ~have_progressive_angina, ~have_nocturnal_angina, ~have_q_waves, ~have_st_t_changes, ~expected_score,
    "no" , 0, "no" , "no" , "no", "no" , 0,
    "yes", 5, "yes", "no" , "no", "no" , 5,
    "yes", 5, "no" , "yes", "no", "no" , 10,
    "yes", 5, "no" , "no" , "no", "yes", 20
  )

  medical_data_with_na <- tibble::tribble(
    ~have_typical_chest_pain, ~frequency_of_angina_pain_per_week,
    ~have_progressive_angina, ~have_nocturnal_angina, ~have_q_waves, ~have_st_t_changes, ~expected_score,
    "no" , 0 , "no" , "no" , "no", NA   , 0,
    "yes", 5 , "yes", NA   , "no", "no" , 5,
    "yes", 5 , NA   , "yes", "no", "no" , 10,
    NA   , 5 , "no" , "no" , "no", "yes", 0,
    "yes", NA, "no" , "no" , "no", "yes", 0,
    "yes", 36, "no" , "no" , "no", "yes", 144,
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      pain_index = purrr::pmap_dbl(
        .l = list(
          have_typical_chest_pain = .data[["have_typical_chest_pain"]],
          frequency_of_angina_pain_per_week = .data[["frequency_of_angina_pain_per_week"]],
          have_progressive_angina = .data[["have_progressive_angina"]],
          have_nocturnal_angina = .data[["have_nocturnal_angina"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_pain_index,
        max_na = 0,
        max_frequency_of_angina_pain_per_week = 35
      )
    )

  medical_data_with_na <- medical_data_with_na |>
    dplyr::mutate(
      pain_index = purrr::pmap_dbl(
        .l = list(
          have_typical_chest_pain = .data[["have_typical_chest_pain"]],
          frequency_of_angina_pain_per_week = .data[["frequency_of_angina_pain_per_week"]],
          have_progressive_angina = .data[["have_progressive_angina"]],
          have_nocturnal_angina = .data[["have_nocturnal_angina"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_pain_index,
        max_na = 1,
        max_frequency_of_angina_pain_per_week = 40
      )
    )

  medical_data_with_na_fail <- medical_data_with_na |>
    dplyr::mutate(
      pain_index = purrr::pmap_dbl(
        .l = list(
          have_typical_chest_pain = .data[["have_typical_chest_pain"]],
          frequency_of_angina_pain_per_week = .data[["frequency_of_angina_pain_per_week"]],
          have_progressive_angina = .data[["have_progressive_angina"]],
          have_nocturnal_angina = .data[["have_nocturnal_angina"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_pain_index,
        max_na = 0,
        max_frequency_of_angina_pain_per_week = 35
      )
    )

  testthat::expect_equal(
    medical_data[["pain_index"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data_with_na[["pain_index"]],
    medical_data_with_na[["expected_score"]]
  )

  testthat::expect_equal(
    all(is.na(medical_data_with_na_fail[["pain_index"]])),
    TRUE
  )

})

test_that("calculate_dcs_1993_vascular_disease_index works", {

  medical_data <- tibble::tribble(
    ~have_peripheral_vascular_disease, ~have_cerebrovascular_disease, ~have_carotid_bruits, ~expected_score,
    "no",               "no",                "no",           0,
    "no",               "no",                "yes",          1,
    "no",               "yes",               "yes",          2,
    "yes",              "yes",               "yes",          3
  )

  medical_data_with_na <- tibble::tribble(
    ~have_peripheral_vascular_disease, ~have_cerebrovascular_disease, ~have_carotid_bruits, ~expected_score,
    NA,               "no",           "no",               0,
    "no",               NA,          "yes",                1,
    "yes",              "yes",          NA,                2
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      vascular_disease_index = purrr::pmap_dbl(
        .l = list(
          have_peripheral_vascular_disease = .data[["have_peripheral_vascular_disease"]],
          have_cerebrovascular_disease = .data[["have_cerebrovascular_disease"]],
          have_carotid_bruits = .data[["have_carotid_bruits"]]
        ),
        .f = pretestcad::calculate_dcs_1993_vascular_disease_index,
        max_na = 0
      )
    )

  medical_data_with_na <- medical_data_with_na |>
    dplyr::mutate(
      vascular_disease_index = purrr::pmap_dbl(
        .l = list(
          have_peripheral_vascular_disease = .data[["have_peripheral_vascular_disease"]],
          have_cerebrovascular_disease = .data[["have_cerebrovascular_disease"]],
          have_carotid_bruits = .data[["have_carotid_bruits"]]
        ),
        .f = pretestcad::calculate_dcs_1993_vascular_disease_index,
        max_na = 1
      )
    )

  medical_data_with_na_fail <- medical_data_with_na |>
    dplyr::mutate(
      vascular_disease_index = purrr::pmap_dbl(
        .l = list(
          have_peripheral_vascular_disease = .data[["have_peripheral_vascular_disease"]],
          have_cerebrovascular_disease = .data[["have_cerebrovascular_disease"]],
          have_carotid_bruits = .data[["have_carotid_bruits"]]
        ),
        .f = pretestcad::calculate_dcs_1993_vascular_disease_index,
        max_na = 0
      )
    )

  testthat::expect_equal(
    medical_data[["vascular_disease_index"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data_with_na[["vascular_disease_index"]],
    medical_data_with_na[["expected_score"]]
  )

  testthat::expect_equal(
    all(is.na(medical_data_with_na_fail[["vascular_disease_index"]])),
    TRUE
  )

})

test_that("calculate_dcs_1993_sig_cad_ptp works", {

  # Verified with https://www.medcentral.com/calculators/cardiology/coronary-artery-disease-risk-clinical-assessment-duke-study

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type, ~have_mi,
    ~have_diabetes, ~have_dyslipidemia, ~have_smoking_history,
    ~have_q_waves, ~have_st_t_changes,
    # 30 male with only typical chest pain
    30, "male"  , "typical"   , "no" , "no" , "no" , "no" , "no", "no",
    # 35 female with only atypical chest pain and Q waves on ECG
    35, "female", "atypical"  , "no" , "no" , "no" , "no" , "yes", "no",
    # 40 male with only nonanginal chest pain and ST-T changes on ECG
    40, "male"  , "nonanginal", "no" , "no" , "no" , "no" , "no", "yes",
    # 45 female with typical chest pain and history of MI
    # Q waves and ST-T changes on ECG
    45, "female", "typical"   , "yes", "no" , "no" , "no" , "yes", "yes",
    # 50 male with atypical chest pain and has diabetes
    50, "male"  , "atypical"  , "no" , "yes", "no" , "no" , "no", "no",
    # 55 female with nonanginal, has dyslipidemia and Q waves on ECG
    55, "female", "nonanginal", "no" , "no" , "yes", "no" , "yes", "no",
    # 60 male with typical chest pain, a smoker and ST-T changes on ECG
    60, "male"  , "typical"   , "no" , "no", "no" , "yes", "no", "yes",
    # 65 female with atypicial chest pain, a smoker,
    # Q waves and ST-T changes on ECG
    65, "female", "atypical"  , "no" , "no", "yes", "yes" , "yes", "yes",
    # 70 male with non-anginal chest pain, history of MI,
    # has dyslipidemia, diabetes and a smoker,
    # Q waves and ST-T changes on ECG
    70, "male"  , "nonanginal", "yes", "yes", "yes", "yes", "yes", "yes"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_mi = .data[["have_mi"]],
          have_diabetes = .data[["have_diabetes"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_sig_cad_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.19513232, 0.06738940, 0.09665121, 0.90665841,
      0.48100914, 0.18407187, 0.94103299, 0.77171140,
      0.99133706),
    tolerance = 1e-5
  )

})

test_that("calculate_dcs_1993_severe_cad_ptp works", {

  # Verified with https://www.medcentral.com/calculators/cardiology/coronary-artery-disease-risk-clinical-assessment-duke-study

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type,
    ~have_progressive_angina, ~have_nocturnal_angina,
    ~have_peripheral_vascular_disease, ~have_cerebrovascular_disease, ~have_carotid_bruits,
    ~have_hypertension, ~have_dyslipidemia, ~have_diabetes,
    ~have_q_waves, ~have_st_t_changes,
    ~frequency_of_angina_pain_per_week,
    ~duration_of_cad_symptoms_year,
    # 40 year old male with non anginal chest pain for one year,
    # He has no progressive angina and nocturnal angina.
    # He has no peripheral vascular and cerebrovascular disease.
    # He has no hypertension, dyslipidemia and not diabetic.
    # He has no Q waves and ST-T changes on ECG.
    40, "male"  , "nonanginal", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", 0, 1,
    # 40 year old female with atypical chest pain for three months,
    # She has no progressive angina and nocturnal angina.
    # She has no peripheral vascular and cerebrovascular disease.
    # She has hypertension, dyslipidemia and diabetic.
    # She has no Q waves and ST-T changes on ECG.
    40, "female", "atypical"  , "no", "no", "no", "no", "no", "yes", "yes", "yes", "no", "no", 0, 0.25,
    # 50 year old male with typical chest pain for four months,
    # She has progressive angina and nocturnal angina.
    # Angina pain lasted at most three times a week.
    # She has peripheral vascular and cerebrovascular disease.
    # She has no hypertension, dyslipidemia and not diabetic.
    # She has no Q waves and have ST-T changes on ECG.
    50, "male"  , "typical"  , "yes", "yes", "yes", "yes", "yes", "no", "no", "no", "no", "yes", 3, 1/3,
    # 60 year old female with typical chest pain for two years,
    # She has progressive angina and nocturnal angina.
    # Angina pain lasted at most five times a week.
    # She has peripheral vascular and cerebrovascular disease.
    # She has no hypertension, dyslipidemia and not diabetic.
    # She has no Q waves and have ST-T changes on ECG.
    60, "female"  , "typical"  , "yes", "yes", "yes", "yes", "yes", "no", "no", "no", "no", "no", 5, 2,
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_progressive_angina = .data[["have_progressive_angina"]],
          have_nocturnal_angina = .data[["have_nocturnal_angina"]],
          have_peripheral_vascular_disease = .data[["have_peripheral_vascular_disease"]],
          have_cerebrovascular_disease = .data[["have_cerebrovascular_disease"]],
          have_carotid_bruits = .data[["have_carotid_bruits"]],
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_diabetes = .data[["have_diabetes"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]],
          frequency_of_angina_pain_per_week = .data[["frequency_of_angina_pain_per_week"]],
          duration_of_cad_symptoms_year = .data[["duration_of_cad_symptoms_year"]]
        ),
        .f = pretestcad::calculate_dcs_1993_severe_cad_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.102098, 0.1431206, 0.3870653, 0.4146034),
    tolerance = 1e-5
  )

})
