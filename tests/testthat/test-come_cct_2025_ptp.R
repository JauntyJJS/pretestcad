test_that("calculate_come_cct_2025_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "30 male with typical chest pain",     30, "male"  ,        "typical",
    "39 female with typical chest pain",   39, "female",        "typical",
    "70 male with typical chest pain",     70, "male"  ,        "typical",
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
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric"]],
    c(0.4658035, 0.3102395,
      0.7863394, 0.6549795),
    tolerance = 1e-5
  )

})

test_that("calculate_come_cct_2025_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "40 male with atypical chest pain",     40,"male"  ,       "atypical",
    "49 female with atypical chest pain",   49,"female",       "atypical",
    "80 male with atypical chest pain",     80,"male"  ,       "atypical",
    "89 female with atypical chest pain",   89,"female",       "atypical"
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    atypical_medical_data[["ptp_numeric"]],
    c(0.2896674, 0.1737902,
      0.6325100, 0.4702851),
    tolerance = 1e-5
  )

})

test_that("calculate_come_cct_2025_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                              ~age, ~sex    , ~chest_pain_type,
    "50 male with non-anginal chest pain",     50, "male"  ,     "nonanginal",
    "59 female with non-anginal chest pain",   59, "female",     "nonanginal",
    "90 male with non-anginal chest pain",     90, "male"  ,     "nonanginal",
    "99 female with non-anginal chest pain",   99, "female",     "nonanginal"
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    non_anginal_medical_data[["ptp_numeric"]],
    c(0.3262929, 0.1998871,
      0.6715050, 0.5132469),
    tolerance = 1e-5
  )

})

test_that("calculate_come_cct_2025_ptp works on other forms of chest pain", {

  other_chest_pain_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "40 male with other forms of chest pain",     40, "male"  ,        "others",
    "49 female with other forms of chest pain",   49, "female",        "others",
    "50 male with other forms of chest pain",     50, "male"  ,        "others",
    "59 female with other forms of chest pain",   59, "female",        "others"
  )

  other_chest_pain_medical_data <- other_chest_pain_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    other_chest_pain_medical_data[["ptp_numeric"]],
    c(0.2307644, 0.1340052,
      0.3006930, 0.1815323),
    tolerance = 1e-5
  )

})

test_that("calculate_cta_alone_2025_ptp works on other forms of chest pain", {

  cta_medical_data <- tibble::tribble(
    ~unique_id, ~cta_result,
    "Patient 1", "obstructive",
    "Patient 2", "non_obstructive"
  )

  cta_medical_data <- cta_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          cta_result = .data[["cta_result"]]
        ),
        .f = pretestcad::calculate_cta_alone_2025_ptp
      )
    )

  testthat::expect_equal(
    cta_medical_data[["ptp_numeric"]],
    c(0.7464940,
      0.1309028),
    tolerance = 1e-5
  )

})

test_that("calculate_come_cct_with_cta_2025_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex  , ~chest_pain_type, ~cta_result       ,
    # 30 female with other forms of chest pain and non-obstructive CAD on CTA
    30, "female",         "others",  "non_obstructive",
    # 35 male with other forms of chest pain and non-obstructive CAD on CTA
    35,   "male",         "others",  "non_obstructive",
    # 40 female with other forms of chest pain and obstructive CAD on CTA
    40, "female",         "others",      "obstructive",
    # 45 male with other forms of chest pain and obstructive CAD on CTA
    45,   "male",         "others",      "obstructive",
    # 50 female with nonanginal chest pain and non-obstructive CAD on CTA
    50, "female",     "nonanginal",  "non_obstructive",
    # 55 male with atypical pain and obstructive CAD on CTA
    55,   "male",       "atypical",      "obstructive",
    # 60 female with typical chest pain and non-obstructive CAD on CTA
    60, "female",        "typical",  "non_obstructive",
    # 65 male with typical chest pain and obstructive CAD on CTA
    65,   "male",        "typical",      "obstructive",
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          cta_result = .data[["cta_result"]]
        ),
        .f = pretestcad::calculate_come_cct_with_cta_2025_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.01870651, 0.05011598,
      0.32387945, 0.57003648,
      0.03901547, 0.68503319,
      0.15264562, 0.90273589),
    tolerance = 1e-5
  )

})
