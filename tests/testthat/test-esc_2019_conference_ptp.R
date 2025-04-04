test_that("calculate_esc_2019_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type, ~have_dyspnoea, ~num_of_rf,
    "30 male with typical chest pain",     30, "male"  ,        "typical",          "no",          0,
    "39 female with typical chest pain",   39, "female",        "typical",          "no",          0,
    "40 male with typical chest pain",     40, "male"  ,        "typical",          "no",          0,
    "49 female with typical chest pain",   49, "female",        "typical",          "no",          0,
    "50 male with typical chest pain",     50, "male"  ,        "typical",          "no",          0,
    "59 female with typical chest pain",   59, "female",        "typical",          "no",          0,
    "60 male with typical chest pain",     60, "male"  ,        "typical",          "no",          0,
    "69 female with typical chest pain",   69, "female",        "typical",          "no",          0,
    "70 male with typical chest pain",     70, "male"  ,        "typical",          "no",          0,
    "79 female with typical chest pain",   79, "female",        "typical",          "no",          0
  )

  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_grouping"]],
    c("low",  "low",
      "high", "intermediate",
      "high", "intermediate",
      "high", "high",
      "high", "high")
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
    ~unique_id,                          ~age, ~sex    , ~have_dyspnoea, ~chest_pain_type,
    "30 male with atypical chest pain",    30, "male"  ,          "no",       "atypical",
    "39 female with atypical chest pain",  39, "female",          "no",       "atypical",
    "40 male with atypical chest pain",    40, "male"  ,          "no",       "atypical",
    "49 female with atypical chest pain",  49, "female",          "no",       "atypical",
    "50 male with atypical chest pain",    50, "male"  ,          "no",       "atypical",
    "59 female with atypical chest pain",  59, "female",          "no",       "atypical",
    "60 male with atypical chest pain",    60, "male"  ,          "no",       "atypical",
    "69 female with atypical chest pain",  69, "female",          "no",       "atypical",
    "70 male with atypical chest pain",    70, "male"  ,          "no",       "atypical",
    "79 female with atypical chest pain",  79, "female",          "no",       "atypical"
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    atypical_medical_data[["ptp_grouping"]],
    c("low", "low",
      "intermediate", "intermediate",
      "high", "intermediate",
      "high", "intermediate",
      "high", "high")
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
    ~unique_id,                             ~age, ~sex    , ~have_dyspnoea, ~chest_pain_type,
    "30 male with non-anginal chest pain",    30, "male"  ,          "no",    "nonanginal",
    "39 female with non-anginal chest pain",  39, "female",          "no",    "nonanginal",
    "40 male with non-anginal chest pain",    40, "male"  ,          "no",    "nonanginal",
    "49 female with non-anginal chest pain",  49, "female",          "no",    "nonanginal",
    "50 male with non-anginal chest pain",    50, "male"  ,          "no",    "nonanginal",
    "59 female with non-anginal chest pain",  59, "female",          "no",    "nonanginal",
    "60 male with non-anginal chest pain",    60, "male"  ,          "no",    "nonanginal",
    "69 female with non-anginal chest pain",  69, "female",          "no",    "nonanginal",
    "70 male with non-anginal chest pain",    70, "male"  ,          "no",    "nonanginal",
    "79 female with non-anginal chest pain",  79, "female",          "no",    "nonanginal"
    )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_grouping"]],
    c("low", "low",
      "low", "low",
      "intermediate", "low",
      "high", "intermediate",
      "high", "intermediate")
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

test_that("calculate_esc_2019_ptp works on dyspnoea", {

  dyspnea_medical_data <- tibble::tribble(
    ~unique_id,              ~age, ~sex    , ~have_dyspnoea, ~chest_pain_type,
    "30 male with dyspnoea",    30, "male"  ,         "yes",  "no chest pain",
    "39 female with dyspnoea",  39, "female",         "yes",  "no chest pain",
    "40 male with dyspnoea",    40, "male"  ,         "yes",  "no chest pain",
    "49 female with dyspnoea",  49, "female",         "yes",  "no chest pain",
    "50 male with dyspnoea",    50, "male"  ,         "yes",  "no chest pain",
    "59 female with dyspnoea",  59, "female",         "yes",  "no chest pain",
    "60 male with dyspnoea",    60, "male"  ,         "yes",  "no chest pain",
    "69 female with dyspnoea",  69, "female",         "yes",  "no chest pain",
    "70 male with dyspnoea",    70, "male"  ,         "yes",  "no chest pain",
    "79 female with dyspnoea",  79, "female",         "yes",  "no chest pain"
    )

  dyspnea_medical_data <- dyspnea_medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnoea = .data[["have_dyspnoea"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2019_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    dyspnea_medical_data[["ptp_grouping"]],
    c("low", "low",
      "intermediate", "low",
      "high", "intermediate",
      "high", "intermediate",
      "high", "intermediate")
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

test_that("calculate_esc_2019_ptp gives NA if some inputs are missing", {

  na_age <- calculate_esc_2019_ptp(
    age = NA,
    sex = "male",
    have_dyspnoea = "no",
    chest_pain_type = "no chest pain"
  )

  na_sex <- calculate_esc_2019_ptp(
    age = 55,
    sex = NA,
    have_dyspnoea = "no",
    chest_pain_type = "no chest pain"
  )

  na_dyspnea <- calculate_esc_2019_ptp(
    age = 55,
    sex = "male",
    have_dyspnoea = NA,
    chest_pain_type = "no chest pain"
  )

  na_chest_pain <- calculate_esc_2019_ptp(
    age = 55,
    sex = "male",
    have_dyspnoea = "no",
    chest_pain_type = NA
  )

  testthat::expect_equal(
    is.na(na_age),
    TRUE
  )

  testthat::expect_equal(
    is.na(na_sex),
    TRUE
  )

  testthat::expect_equal(
    is.na(na_dyspnea),
    TRUE
  )

  testthat::expect_equal(
    is.na(na_chest_pain),
    TRUE
  )

})
