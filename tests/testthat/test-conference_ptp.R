test_that("calculate_esc_2019_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex, ~have_dyspnea, ~chest_pain,
    "30 male with typical chest pain",     30,    1,             0,           1,
    "39 female with typical chest pain",   39,    0,             0,           1,
    "40 male with typical chest pain",     40,    1,             0,           1,
    "49 female with typical chest pain",   49,    0,             0,           1,
    "50 male with typical chest pain",     50,    1,             0,           1,
    "59 female with typical chest pain",   59,    0,             0,           1,
    "60 male with typical chest pain",     60,    1,             0,           1,
    "69 female with typical chest pain",   69,    0,             0,           1,
    "70 male with typical chest pain",     70,    1,             0,           1,
    "79 female with typical chest pain",   79,    0,             0,           1
  )

  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_grouping"]],
    c("Low", "Low",
      "High", "Intermediate",
      "High", "Intermediate",
      "High", "High",
      "High", "High")
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_numeric"]],
    c( 3L,  5L,
      22L, 10L,
      32L, 13L,
      44L, 16L,
      52L, 27L)
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_percentage"]],
    c( "3%",  "5%",
      "22%", "10%",
      "32%", "13%",
      "44%", "16%",
      "52%", "27%")
  )

})

test_that("calculate_esc_2019_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex, ~have_dyspnea, ~chest_pain,
    "30 male with atypical chest pain",    30,    1,             0,           2,
    "39 female with atypical chest pain",  39,    0,             0,           2,
    "40 male with atypical chest pain",    40,    1,             0,           2,
    "49 female with atypical chest pain",  49,    0,             0,           2,
    "50 male with atypical chest pain",    50,    1,             0,           2,
    "59 female with atypical chest pain",  59,    0,             0,           2,
    "60 male with atypical chest pain",    60,    1,             0,           2,
    "69 female with atypical chest pain",  69,    0,             0,           2,
    "70 male with atypical chest pain",    70,    1,             0,           2,
    "79 female with atypical chest pain",  79,    0,             0,           2
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    atypical_medical_data[["ptp_grouping"]],
    c("Low", "Low",
      "Intermediate", "Intermediate",
      "High", "Intermediate",
      "High", "Intermediate",
      "High", "High")
  )

  testthat::expect_identical(
    atypical_medical_data[["ptp_numeric"]],
    c( 4L,  3L,
      10L,  6L,
      17L,  6L,
      26L, 11L,
      34L, 19L)
  )

  testthat::expect_identical(
    atypical_medical_data[["ptp_percentage"]],
    c( "4%",  "3%",
      "10%",  "6%",
      "17%",  "6%",
      "26%", "11%",
      "34%", "19%")
  )

})

test_that("calculate_esc_2019_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                             ~age, ~sex, ~have_dyspnea, ~chest_pain,
    "30 male with non-anginal chest pain",    30,    1,             0,           3,
    "39 female with non-anginal chest pain",  39,    0,             0,           3,
    "40 male with non-anginal chest pain",    40,    1,             0,           3,
    "49 female with non-anginal chest pain",  49,    0,             0,           3,
    "50 male with non-anginal chest pain",    50,    1,             0,           3,
    "59 female with non-anginal chest pain",  59,    0,             0,           3,
    "60 male with non-anginal chest pain",    60,    1,             0,           3,
    "69 female with non-anginal chest pain",  69,    0,             0,           3,
    "70 male with non-anginal chest pain",    70,    1,             0,           3,
    "79 female with non-anginal chest pain",  79,    0,             0,           3
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_grouping"]],
    c("Low", "Low",
      "Low", "Low",
      "Intermediate", "Low",
      "High", "Intermediate",
      "High", "Intermediate")
  )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_numeric"]],
    c(  1L,  1L,
        3L,  2L,
       11L,  3L,
       22L,  6L,
       24L, 10L)
  )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_percentage"]],
    c(  "1%",  "1%",
        "3%",  "2%",
       "11%",  "3%",
       "22%",  "6%",
       "24%", "10%")
  )

})

test_that("calculate_esc_2019_ptp works on dyspnea", {

  dyspnea_medical_data <- tibble::tribble(
    ~unique_id,              ~age, ~sex, ~have_dyspnea, ~chest_pain,
    "30 male with dyspnea",    30,    1,             1,           0,
    "39 female with dyspnea",  39,    0,             1,           0,
    "40 male with dyspnea",    40,    1,             1,           0,
    "49 female with dyspnea",  49,    0,             1,           0,
    "50 male with dyspnea",    50,    1,             1,           0,
    "59 female with dyspnea",  59,    0,             1,           0,
    "60 male with dyspnea",    60,    1,             1,           0,
    "69 female with dyspnea",  69,    0,             1,           0,
    "70 male with dyspnea",    70,    1,             1,           0,
    "79 female with dyspnea",  79,    0,             1,           0
  )

  dyspnea_medical_data <- dyspnea_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          dyspnea_only = .data[["have_dyspnea"]],
          chest_pain = .data[["chest_pain"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    dyspnea_medical_data[["ptp_grouping"]],
    c("Low", "Low",
      "Intermediate", "Low",
      "High", "Intermediate",
      "High", "Intermediate",
      "High", "Intermediate")
  )

  testthat::expect_identical(
    dyspnea_medical_data[["ptp_numeric"]],
    c(   0L,  3L,
        12L,  3L,
        20L,  9L,
        27L, 14L,
        32L, 12L)
  )

  testthat::expect_identical(
    dyspnea_medical_data[["ptp_percentage"]],
    c(   "0%",  "3%",
        "12%",  "3%",
        "20%",  "9%",
        "27%", "14%",
        "32%", "12%")
  )

})
