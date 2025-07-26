test_that("calculate_aha_2012_tbl_9_ptp works on typical chest pain", {

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
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_aha_2012_tbl_9_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_aha_2012_tbl_9_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    typical_medical_data[["ptp_numeric"]],
    c( 76L, 26L,
       87L, 55L,
       93L, 73L,
       94L, 86L)
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_percentage"]],
    c( "76%", "26%",
       "87%", "55%",
       "93%", "73%",
       "94%", "86%")
  )

})

test_that("calculate_aha_2012_tbl_9_ptp works on atypical chest pain", {

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
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_aha_2012_tbl_9_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_aha_2012_tbl_9_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    atypical_medical_data[["ptp_numeric"]],
    c(34L, 12L,
      51L, 22L,
      65L, 31L,
      72L, 51L)
  )

  testthat::expect_identical(
    atypical_medical_data[["ptp_percentage"]],
    c("34%", "12%",
      "51%", "22%",
      "65%", "31%",
      "72%", "51%")
  )

})

test_that("calculate_aha_2012_tbl_9_ptp works on non-anginal chest pain", {

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
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_aha_2012_tbl_9_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_aha_2012_tbl_9_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_numeric"]],
    c(  4L,  2L,
       13L,  3L,
       20L,  7L,
       27L, 14L)
  )

  testthat::expect_identical(
    non_anginal_medical_data[["ptp_percentage"]],
    c( "4%",  "2%",
      "13%",  "3%",
      "20%",  "7%",
      "27%", "14%")
  )

})

test_that("calculate_aha_2012_tbl_9_ptp gives error of invalid missing input of sex", {

  testthat::expect_snapshot(
    error = TRUE,
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = NA,
      chest_pain_type = "typical",
      label_sex_male = c("male"),
      label_sex_female = c("female"),
      label_sex_unknown = c("NIL")
    )
  )

})

test_that("calculate_aha_2012_tbl_9_ptp gives error of invalid male input", {

  testthat::expect_snapshot(
    error = TRUE,
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "Male",
      chest_pain_type = "typical",
      label_sex_male = c("male"),
      label_sex_female = c("female"),
      label_sex_unknown = c(NA, NaN)
    )
  )

})

test_that("calculate_aha_2012_tbl_9_ptp works on different valid inputs for sex", {
  testthat::expect_identical(
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "Male",
      chest_pain_type = "typical",
      label_sex_male = c("male", "Male"),
      label_sex_female = c("female"),
      label_sex_unknown = c(NA, NaN)
    ),
    93L
  )
})

test_that("calculate_aha_2012_tbl_9_ptp gives NA if there are valid missing input of sex", {
  testthat::expect_equal(
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = NA,
      chest_pain_type = "typical",
      label_sex_male = c("male"),
      label_sex_female = c("female"),
      label_sex_unknown = c(NA, NaN)
    ) |>
      is.na(),
    TRUE
  )

  testthat::expect_equal(
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "NIL",
      chest_pain_type = "typical",
      label_sex_male = c("male"),
      label_sex_female = c("female"),
      label_sex_unknown = c("NIL")
    ) |>
      is.na(),
    TRUE
  )

})


test_that("calculate_aha_2012_tbl_9_ptp gives error of invalid missing input of chest pain type", {

  testthat::expect_snapshot(
    error = TRUE,
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "male",
      chest_pain_type = NA,
      label_cpt_nonanginal = c("nonanginal"),
      label_cpt_atypical = c("atypical"),
      label_cpt_typical = c("typical"),
      label_cpt_unknown = c("NIL")
    )
  )

})

test_that("calculate_aha_2012_tbl_9_ptp gives error of invalid typical chest pain input", {

  testthat::expect_snapshot(
    error = TRUE,
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "male",
      chest_pain_type = "Typical",
      label_cpt_nonanginal = c("nonanginal"),
      label_cpt_atypical = c("atypical"),
      label_cpt_typical = c("typical"),
      label_cpt_unknown = c(NA, NaN)
    )
  )

})

test_that("calculate_aha_2012_tbl_9_ptp works on different valid inputs for chest pain type", {
  testthat::expect_identical(
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "male",
      chest_pain_type = "Typical",
      label_cpt_nonanginal = c("nonanginal"),
      label_cpt_atypical = c("atypical"),
      label_cpt_typical = c("typical", "Typical"),
      label_cpt_unknown = c(NA, NaN)
    ),
    93L
  )
})

test_that("calculate_aha_2012_tbl_9_ptp gives NA if there are valid missing input of chest pain type", {
  testthat::expect_equal(
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "male",
      chest_pain_type = NA,
      label_cpt_nonanginal = c("nonanginal"),
      label_cpt_atypical = c("atypical"),
      label_cpt_typical = c("typical", "Typical"),
      label_cpt_unknown = c(NA, NaN)
    ) |>
      is.na(),
    TRUE
  )

  testthat::expect_equal(
    calculate_aha_2012_tbl_9_ptp(
      age = 55,
      sex = "male",
      chest_pain_type = "NIL",
      label_cpt_nonanginal = c("nonanginal"),
      label_cpt_atypical = c("atypical"),
      label_cpt_typical = c("typical", "Typical"),
      label_cpt_unknown = c("NIL")
    ) |>
      is.na(),
    TRUE
  )

})

test_that("calculate_aha_2012_tbl_9_ptp gives NA if age is missing", {

  na_age <- calculate_aha_2012_tbl_9_ptp(
    age = NA,
    sex = "male",
    chest_pain_type = "typical"
  )

  under_age <- calculate_aha_2012_tbl_9_ptp(
    age = 25,
    sex = "male",
    chest_pain_type = "typical"
  )

  testthat::expect_equal(
    is.na(na_age),
    TRUE
  )

  testthat::expect_equal(
    is.na(under_age),
    TRUE
  )

})
