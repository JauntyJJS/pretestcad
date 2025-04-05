test_that("calculate_diamond_forrester_1979_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "30 male with typical chest pain",     30, "male"  ,        "typical",
    "39 female with typical chest pain",   39, "female",        "typical",
    "40 male with typical chest pain",     40, "male"  ,        "typical",
    "49 female with typical chest pain",   49, "female",        "typical",
    "50 male with typical chest pain",     50, "male"  ,        "typical",
    "59 female with typical chest pain",   59, "female",        "typical",
    "60 male with typical chest pain",     60, "male"  ,        "typical",
    "69 female with typical chest pain",   69, "female",        "typical"
  )


  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_diamond_forrester_1979_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_diamond_forrester_1979_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    typical_medical_data[["ptp_numeric"]],
    c(69.7, 25.8,
      87.3, 55.2,
      92.0, 79.4,
      94.3, 90.6)
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_percentage"]],
    c("69.7%", "25.8%",
      "87.3%", "55.2%",
        "92%", "79.4%",
      "94.3%", "90.6%")
  )

})

test_that("calculate_diamond_forrester_1979_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "30 male with atypical chest pain",    30, "male"  ,      "atypical",
    "39 female with atypical chest pain",  39, "female",      "atypical",
    "40 male with atypical chest pain",    40, "male"  ,      "atypical",
    "49 female with atypical chest pain",  49, "female",      "atypical",
    "50 male with atypical chest pain",    50, "male"  ,      "atypical",
    "59 female with atypical chest pain",  59, "female",      "atypical",
    "60 male with atypical chest pain",    60, "male"  ,      "atypical",
    "69 female with atypical chest pain",  69, "female",      "atypical"
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_diamond_forrester_1979_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_diamond_forrester_1979_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    atypical_medical_data[["ptp_numeric"]],
    c(21.8, 4.2,
      46.1, 13.3,
      58.9, 32.4,
      67.1, 54.4)
  )

  testthat::expect_identical(
    atypical_medical_data[["ptp_percentage"]],
    c("21.8%", "4.2%",
      "46.1%", "13.3%",
      "58.9%", "32.4%",
      "67.1%", "54.4%")
  )

})

test_that("calculate_diamond_forrester_1979_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                             ~age, ~sex    , ~chest_pain_type,
    "30 male with non-anginal chest pain",    30, "male"  ,   "nonanginal",
    "39 female with non-anginal chest pain",  39, "female",   "nonanginal",
    "40 male with non-anginal chest pain",    40, "male"  ,   "nonanginal",
    "49 female with non-anginal chest pain",  49, "female",   "nonanginal",
    "50 male with non-anginal chest pain",    50, "male"  ,   "nonanginal",
    "59 female with non-anginal chest pain",  59, "female",   "nonanginal",
    "60 male with non-anginal chest pain",    60, "male"  ,   "nonanginal",
    "69 female with non-anginal chest pain",  69, "female",   "nonanginal"
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_diamond_forrester_1979_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_diamond_forrester_1979_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_numeric"]],
    c(5.2 ,  0.8,
      14.1,  2.8,
      21.5,  8.4,
      28.1, 18.6)
  )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_percentage"]],
    c( "5.2%",  "0.8%",
      "14.1%",  "2.8%",
      "21.5%",  "8.4%",
      "28.1%", "18.6%")
  )

})

test_that("calculate_diamond_forrester_1979_ptp gives NA if some inputs are missing", {

  na_age <- calculate_diamond_forrester_1979_ptp(
    age = NA,
    sex = "male",
    chest_pain_type = "typical"
  )

  under_age <- calculate_diamond_forrester_1979_ptp(
    age = 25,
    sex = "male",
    chest_pain_type = "typical"
  )

  na_sex <- calculate_diamond_forrester_1979_ptp(
    age = 55,
    sex = NA,
    chest_pain_type = "typical"
  )

  na_chest_pain <- calculate_diamond_forrester_1979_ptp(
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
