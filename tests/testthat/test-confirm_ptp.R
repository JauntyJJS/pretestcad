test_that("calculate_confirm_2015_num_of_rf works", {

  medical_data <- tibble::tribble(
    ~have_typical_chest_pain, ~have_diabetes, ~have_hypertension, ~have_family_history, ~is_current_smoker, ~expected_score,
    "no",                     "no",           "no",               "no",                 "no",               0,
    "no",                     "no",           "no",               "no",                 "yes",              1,
    "no",                     "no",           "no",               "yes",                "yes",              2,
    "no",                     "no",           "yes",              "yes",                "yes",              3,
    "no",                     "yes",          "yes",              "yes",                "yes",              4,
    "yes",                    "yes",          "yes",              "yes",                "yes",              5

  )

  medical_data_with_na <- tibble::tribble(
    ~have_typical_chest_pain, ~have_diabetes, ~have_hypertension, ~have_family_history, ~is_current_smoker, ~expected_score,
    "no",                     NA,             "no",               "no",                 "no",               0,
    "no",                     "no",           NA,                 "no",                 "yes",              1,
    "no",                     "no",           "no",               NA,                   "yes",              1,
    NA,                       "no",           "yes",              "yes",                "yes",              3,
    "no",                     "yes",          "yes",              "yes",                NA,                 3,
    "yes",                    NA,             "yes",              "yes",                "yes",              4

  )

  medical_data <- medical_data |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          have_typical_chest_pain = .data[["have_typical_chest_pain"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          have_family_history = .data[["have_family_history"]],
          is_current_smoker = .data[["is_current_smoker"]]
        ),
        .f = pretestcad::calculate_confirm_2015_num_of_rf,
        max_na = 0
      )
    )

  medical_data_with_na <- medical_data_with_na |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          have_typical_chest_pain = .data[["have_typical_chest_pain"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          have_family_history = .data[["have_family_history"]],
          is_current_smoker = .data[["is_current_smoker"]]
        ),
        .f = pretestcad::calculate_confirm_2015_num_of_rf,
        max_na = 1
      )
    )

  medical_data_with_na_fail <- medical_data_with_na |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          have_typical_chest_pain = .data[["have_typical_chest_pain"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          have_family_history = .data[["have_family_history"]],
          is_current_smoker = .data[["is_current_smoker"]]
        ),
        .f = pretestcad::calculate_confirm_2015_num_of_rf,
        max_na = 0
      )
    )

  testthat::expect_equal(
    medical_data[["symptom_score"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data_with_na[["symptom_score"]],
    medical_data_with_na[["expected_score"]]
  )

  testthat::expect_equal(
    all(is.na(medical_data_with_na_fail[["symptom_score"]])),
    TRUE
  )

})

test_that("calculate_esc_2024_fig_4_ptp works", {

  testthat::expect_identical(
    calculate_confirm_2015_ptp(
      age = 30,
      sex = "male",
      have_typical_chest_pain = "yes",
      have_diabetes = "no",
      have_hypertension = "no",
      have_family_history = "no",
      is_current_smoker = "yes",
      max_na_num_of_rf = 0,
      output = "percentage"
    ),
    "5.5%"
  )

  testthat::expect_identical(
    calculate_confirm_2015_ptp(
      age = 30,
      sex = "male",
      have_typical_chest_pain = "yes",
      have_diabetes = "no",
      have_hypertension = "no",
      have_family_history = "no",
      is_current_smoker = "yes",
      max_na_num_of_rf = 0,
      output = "text"
    ),
    "5.5"
  )

})
