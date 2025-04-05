test_that("calculate_esc_2013_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "30 male with typical chest pain",     30, "male"  ,        "typical",
    "39 female with typical chest pain",   39, "female",        "typical",
    "40 male with typical chest pain",     40, "male"  ,        "typical",
    "49 female with typical chest pain",   49, "female",        "typical",
    "50 male with typical chest pain",     50, "male"  ,        "typical",
    "59 female with typical chest pain",   59, "female",        "typical",
    "60 male with typical chest pain",     60, "male"  ,        "typical",
    "69 female with typical chest pain",   69, "female",        "typical",
    "70 male with typical chest pain",     70, "male"  ,        "typical",
    "79 female with typical chest pain",   79, "female",        "typical",
    "80 male with typical chest pain",     80, "male"  ,        "typical",
    "89 female with typical chest pain",   89, "female",        "typical"
  )


  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2013_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2013_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    typical_medical_data[["ptp_numeric"]],
    c( 59L, 28L,
       69L, 37L,
       77L, 47L,
       84L, 58L,
       89L, 68L,
       93L, 76L)
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_percentage"]],
    c( "59%", "28%",
       "69%", "37%",
       "77%", "47%",
       "84%", "58%",
       "89%", "68%",
       "93%", "76%")
  )

})

test_that("calculate_esc_2013_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "30 male with atypical chest pain",    30, "male"  ,      "atypical",
    "39 female with atypical chest pain",  39, "female",      "atypical",
    "40 male with atypical chest pain",    40, "male"  ,      "atypical",
    "49 female with atypical chest pain",  49, "female",      "atypical",
    "50 male with atypical chest pain",    50, "male"  ,      "atypical",
    "59 female with atypical chest pain",  59, "female",      "atypical",
    "60 male with atypical chest pain",    60, "male"  ,      "atypical",
    "69 female with atypical chest pain",  69, "female",      "atypical",
    "70 male with atypical chest pain",    70, "male"  ,      "atypical",
    "79 female with atypical chest pain",  79, "female",      "atypical",
    "80 male with atypical chest pain",    80, "male"  ,      "atypical",
    "89 female with atypical chest pain",  89, "female",      "atypical"
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2013_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2013_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    atypical_medical_data[["ptp_numeric"]],
    c(29L, 10L,
      38L, 14L,
      49L, 20L,
      59L, 28L,
      69L, 37L,
      78L, 47L)
  )

  testthat::expect_identical(
    atypical_medical_data[["ptp_percentage"]],
    c("29%", "10%",
      "38%", "14%",
      "49%", "20%",
      "59%", "28%",
      "69%", "37%",
      "78%", "47%")
  )

})

test_that("calculate_esc_2013_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                             ~age, ~sex    , ~chest_pain_type,
    "30 male with non-anginal chest pain",    30, "male"  ,   "nonanginal",
    "39 female with non-anginal chest pain",  39, "female",   "nonanginal",
    "40 male with non-anginal chest pain",    40, "male"  ,   "nonanginal",
    "49 female with non-anginal chest pain",  49, "female",   "nonanginal",
    "50 male with non-anginal chest pain",    50, "male"  ,   "nonanginal",
    "59 female with non-anginal chest pain",  59, "female",   "nonanginal",
    "60 male with non-anginal chest pain",    60, "male"  ,   "nonanginal",
    "69 female with non-anginal chest pain",  69, "female",   "nonanginal",
    "70 male with non-anginal chest pain",    70, "male"  ,   "nonanginal",
    "79 female with non-anginal chest pain",  79, "female",   "nonanginal",
    "80 male with non-anginal chest pain",    80, "male"  ,   "nonanginal",
    "89 female with non-anginal chest pain",  89, "female",   "nonanginal"
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2013_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_esc_2013_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_numeric"]],
    c(  18L,  5L,
        25L,  8L,
        34L, 12L,
        44L, 17L,
        54L, 24L,
        65L, 32L)
  )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_percentage"]],
    c( "18%",  "5%",
       "25%",  "8%",
       "34%", "12%",
       "44%", "17%",
       "54%", "24%",
       "65%", "32%")
  )

})

test_that("calculate_esc_2013_ptp gives NA if some inputs are missing", {

  na_age <- calculate_esc_2013_ptp(
    age = NA,
    sex = "male",
    chest_pain_type = "typical"
  )

  under_age <- calculate_esc_2013_ptp(
    age = 25,
    sex = "male",
    chest_pain_type = "typical"
  )

  na_sex <- calculate_esc_2013_ptp(
    age = 55,
    sex = NA,
    chest_pain_type = "typical"
  )

  na_chest_pain <- calculate_esc_2013_ptp(
    age = 55,
    sex = "male",
    chest_pain_type = NA
  )

  testthat::expect_equal(
    is.na(na_age),
    TRUE
  )

  testthat::expect_equal(
    is.na(under_age),
    TRUE
  )

  testthat::expect_equal(
    is.na(na_sex),
    TRUE
  )

  testthat::expect_equal(
    is.na(na_chest_pain),
    TRUE
  )

})
