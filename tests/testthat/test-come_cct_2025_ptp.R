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
      ptp_numeric_with_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "yes"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      ),
      ptp_numeric_without_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "no"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric_with_ri"]],
    c(0.6269607, 0.4643605,
      0.8764463, 0.7853634),
    tolerance = 1e-5
  )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric_without_ri"]],
    c(0.4631171, 0.3079331,
      0.7845193, 0.6525348),
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
      ptp_numeric_with_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "yes"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      ),
      ptp_numeric_without_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "no"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    atypical_medical_data[["ptp_numeric_with_ri"]],
    c(0.4400892, 0.2884754,
      0.7683824, 0.6311608),
    tolerance = 1e-5
  )

  testthat::expect_equal(
    atypical_medical_data[["ptp_numeric_without_ri"]],
    c(0.2874502, 0.1722449,
      0.6299961, 0.4675955),
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
      ptp_numeric_with_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "yes"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      ),
      ptp_numeric_without_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "no"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    non_anginal_medical_data[["ptp_numeric_with_ri"]],
    c(0.4828068, 0.3250192,
      0.7975740, 0.6702244),
    tolerance = 1e-5
  )

  testthat::expect_equal(
    non_anginal_medical_data[["ptp_numeric_without_ri"]],
    c(0.3239233, 0.1981655,
      0.6691183, 0.5105484),
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
      ptp_numeric_with_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "yes"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      ),
      ptp_numeric_without_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          use_random_intercept = "no"
        ),
        .f = pretestcad::calculate_come_cct_2025_ptp
      )
    )

  testthat::expect_equal(
    other_chest_pain_medical_data[["ptp_numeric_with_ri"]],
    c(0.3663750, 0.2297364,
      0.4531875, 0.2994748),
    tolerance = 1e-5
  )

  testthat::expect_equal(
    other_chest_pain_medical_data[["ptp_numeric_without_ri"]],
    c(0.2288528, 0.1327569,
      0.2984269, 0.1799332),
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
      ptp_numeric_with_ri = purrr::pmap_dbl(
        .l = list(
          cta_result = .data[["cta_result"]],
          use_random_intercept = "yes"
        ),
        .f = pretestcad::calculate_come_cta_alone_2025_ptp
      ),
      ptp_numeric_without_ri = purrr::pmap_dbl(
        .l = list(
          cta_result = .data[["cta_result"]],
          use_random_intercept = "no"
        ),
        .f = pretestcad::calculate_come_cta_alone_2025_ptp
      )
    )

  testthat::expect_equal(
    cta_medical_data[["ptp_numeric_with_ri"]],
    c(0.8065895,
      0.1758096),
    tolerance = 1e-5
  )

  testthat::expect_equal(
    cta_medical_data[["ptp_numeric_without_ri"]],
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
      ptp_numeric_with_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          cta_result = .data[["cta_result"]],
          use_random_intercept = "yes"
        ),
        .f = pretestcad::calculate_come_cct_with_cta_2025_ptp
      ),
      ptp_numeric_without_ri = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          cta_result = .data[["cta_result"]],
          use_random_intercept = "no"
        ),
        .f = pretestcad::calculate_come_cct_with_cta_2025_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric_with_ri"]],
    c(0.02680490, 0.07083060,
      0.40902438, 0.65701046,
      0.05540952, 0.75859646,
      0.20652509, 0.93060370),
    tolerance = 1e-5
  )

  testthat::expect_equal(
    medical_data[["ptp_numeric_without_ri"]],
    c(0.01854201, 0.04968927,
      0.32191175, 0.56782925,
      0.03867943, 0.68308809,
      0.15148515, 0.90194279),
    tolerance = 1e-5
  )

})
