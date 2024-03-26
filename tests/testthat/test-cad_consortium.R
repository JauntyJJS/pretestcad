test_that("calculate_cad2_2012_basic_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex,  ~chest_pain,
    "30 male with typical chest pain",     30,    1,             1,
    "39 female with typical chest pain",   35,    0,             1,
    "70 male with typical chest pain",     70,    1,             1,
    "79 female with typical chest pain",   79,    0,             1
  )

  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_cad2_2012_basic_ptp
      )
    )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric"]],
    c(0.15525053, 0.06082505,
      0.69550849, 0.50874911),
    tolerance = 1e-5
  )

})

test_that("calculate_cad2_2012_basic_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex,  ~chest_pain,
    "40 male with atypical chest pain",     40,    1,             2,
    "49 female with atypical chest pain",   49,    0,             2,
    "80 male with atypical chest pain",     80,    1,             2,
    "89 female with atypical chest pain",   89,    0,             2
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_cad2_2012_basic_ptp
      )
    )

  testthat::expect_equal(
    atypical_medical_data[["ptp_numeric"]],
    c(0.08463306, 0.04023312,
      0.53469416, 0.34253898),
    tolerance = 1e-5
  )

})

test_that("calculate_cad2_2012_basic_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                              ~age, ~sex,   ~chest_pain,
    "30 male with non-anginal chest pain",     30,    1,             3,
    "39 female with non-anginal chest pain",   39,    0,             3,
    "90 male with non-anginal chest pain",     90,    1,             3,
    "99 female with non-anginal chest pain",   99,    0,             3
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_cad2_2012_basic_ptp
      )
    )

  testthat::expect_equal(
    non_anginal_medical_data[["ptp_numeric"]],
    c(0.02486778, 0.01143020,
      0.52772154, 0.33626130),
    tolerance = 1e-5
  )

})


test_that("calculate_cad2_2012_clinical_ptp", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain,
    ~has_diabetes, ~has_hypertension, ~has_dyslipidemia, ~has_smoking_history,
    # 30 male with only typical chest pain
    30, 1, 1, 0, 0, 0, 0
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
        .f = pretestcad::calculate_cad2_2012_clinical_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.08714529),
    tolerance = 1e-5
  )

})
