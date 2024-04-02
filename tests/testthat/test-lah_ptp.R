test_that("calculate_lah_2022_clinical_ptp works on typical chest pain", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain,
    ~has_diabetes, ~has_hypertension, ~has_dyslipidemia, ~has_smoking_history,
    # 30 male with only typical chest pain
    30, 1, 1, 0, 0, 0, 0,
    # 35 female with only atypical chest pain
    35, 0, 2, 0, 0, 0, 0,
    # 40 male with only non-anginal chest pain
    40, 1, 3, 0, 0, 0, 0,
    # 45 female with typical chest pain and has diabetes
    45, 0, 1, 1, 0, 0, 0,
    # 50 male with atypical chest pain and has diabetes
    50, 1, 2, 1, 0, 0, 0,
    # 55 female with non-anginal and has diabetes
    55, 0, 3, 1, 0, 0, 0,
    # 60 male with typical chest pain and has hypertension
    60, 1, 1, 0, 1, 0, 0,
    # 65 female with atypicial chest pain and has dyslipidemia
    65, 0, 2, 0, 0, 1, 0,
    # 70 male with non-anginal chest pain and has smoking history
    70, 1, 3, 0, 0, 0, 1
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain = .data[["chest_pain"]],
          has_diabetes = .data[["has_diabetes"]],
          has_hypertension = .data[["has_hypertension"]],
          has_dyslipidemia = .data[["has_dyslipidemia"]],
          has_smoking_history = .data[["has_smoking_history"]]
        ),
        .f = pretestcad::calculate_lah_2022_clinical_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.07069908, 0.01775803, 0.11204704,
      0.06464593, 0.25483299, 0.10284552,
      0.47277695, 0.16341981, 0.39555998),
    tolerance = 1e-5
  )

})

test_that("calculate_lah_2022_extended_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain,
    ~has_diabetes, ~has_hypertension, ~has_dyslipidemia, ~has_smoking_history,
    ~coronary_calcium_score,
    # 30 male with only typical chest pain with calcium score of 5
    30, 1, 1, 0, 0, 0, 0, 5,
    # 35 female with only atypical chest pain with calcium score of 15
    35, 0, 2, 0, 0, 0, 0, 15,
    # 40 male with only non-anginal chest pain with calcium score of 150
    40, 1, 3, 0, 0, 0, 0, 150,
    # 45 female with typical chest pain and has diabetes with calcium score of 300
    45, 0, 1, 1, 0, 0, 0, 300,
    # 50 male with atypical chest pain and has diabetes with calcium score of 600
    50, 1, 2, 1, 0, 0, 0, 600,
    # 55 female with non-anginal and has diabetes with calcium score of 1200
    55, 0, 3, 1, 0, 0, 0, 1200,
    # 60 male with typical chest pain and has hypertension with calcium score of 2400
    60, 1, 1, 0, 1, 0, 0, 2400,
    # 65 female with atypicial chest pain and has dyslipidemia with calcium score of 0
    65, 0, 2, 0, 0, 1, 0, 0,
    # 70 male with non-anginal chest pain and has smoking history with calcium score of 700
    70, 1, 3, 0, 0, 0, 1, 700
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain = .data[["chest_pain"]],
          has_diabetes = .data[["has_diabetes"]],
          has_hypertension = .data[["has_hypertension"]],
          has_dyslipidemia = .data[["has_dyslipidemia"]],
          has_smoking_history = .data[["has_smoking_history"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_lah_2022_extended_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.126037534, 0.121980543, 0.699230920,
      0.742869144, 0.864095213, 0.897915346,
      0.965881281, 0.009565319, 0.871917435),
    tolerance = 1e-5
  )

})

