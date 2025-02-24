test_that("calculate_aha_2021_ptp works on patients with chest pain", {

  medical_data <- tibble::tribble(
    ~unique_id,                  ~age, ~sex    , ~have_chest_pain, ~have_dyspnea,
    "30 male with chest pain",     30, "male"  ,            "yes",          "no",
    "39 female with chest pain",   39, "female",            "yes",         "yes",
    "40 male with chest pain",     40, "male"  ,            "yes",         "yes",
    "49 female with chest pain",   49, "female",            "yes",          "no",
    "50 male with chest pain",     50, "male"  ,            "yes",          "no",
    "59 female with chest pain",   59, "female",            "yes",         "yes",
    "60 male with chest pain",     60, "male"  ,            "yes",          "no",
    "69 female with chest pain",   69, "female",            "yes",          "no",
    "70 male with chest pain",     70, "male"  ,            "yes",         "yes",
    "79 female with chest pain",   79, "female",            "yes",          "no"
  )


  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_chest_pain = .data[["have_chest_pain"]]
        ),
        .f = calculate_aha_2021_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_chest_pain = .data[["have_chest_pain"]]
        ),
        .f = calculate_aha_2021_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_chest_pain = .data[["have_chest_pain"]]
        ),
        .f = calculate_aha_2021_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    medical_data[["ptp_grouping"]],
    c("very low",  "very low",
      "intermediate", "low",
      "intermediate", "low",
      "intermediate", "intermediate",
      "high", "intermediate")
  )

  testthat::expect_identical(
    medical_data[["ptp_numeric"]],
    c( 4L,  5L,
       22L, 10L,
       32L, 13L,
       44L, 16L,
       52L, 27L)
  )

  testthat::expect_identical(
    medical_data[["ptp_percentage"]],
    c( "<=4%",  "<=5%",
       "<=22%", "<=10%",
       "<=32%", "<=13%",
       "<=44%", "<=16%",
       "<=52%", "<=27%")
  )
})


test_that("calculate_aha_2021_ptp works on patients with only dyspnea", {

  medical_data <- tibble::tribble(
    ~unique_id,                    ~age, ~sex    , ~have_chest_pain, ~have_dyspnea,
    "30 male with only dyspnea",     30, "male"  ,             "no",         "yes",
    "39 female with only dyspnea",   39, "female",             "no",         "yes",
    "40 male with only dyspnea",     40, "male"  ,             "no",         "yes",
    "49 female with only dyspnea",   49, "female",             "no",         "yes",
    "50 male with only dyspnea",     50, "male"  ,             "no",         "yes",
    "59 female with only dyspnea",   59, "female",             "no",         "yes",
    "60 male with only dyspnea",     60, "male"  ,             "no",         "yes",
    "69 female with only dyspnea",   69, "female",             "no",         "yes",
    "70 male with only dyspnea",     70, "male"  ,             "no",         "yes",
    "79 female with only dyspnea",   79, "female",             "no",         "yes"
  )


  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_chest_pain = .data[["have_chest_pain"]]
        ),
        .f = calculate_aha_2021_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_chest_pain = .data[["have_chest_pain"]]
        ),
        .f = calculate_aha_2021_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyspnea = .data[["have_dyspnea"]],
          have_chest_pain = .data[["have_chest_pain"]]
        ),
        .f = calculate_aha_2021_ptp,
        output = "percentage"
      )
    )

  testthat::expect_identical(
    medical_data[["ptp_grouping"]],
    c("very low",  "very low",
      "low", "very low",
      "intermediate", "low",
      "intermediate", "low",
      "intermediate", "low")
  )

  testthat::expect_identical(
    medical_data[["ptp_numeric"]],
    c( 0L,  3L,
       12L, 3L,
       20L, 9L,
       27L, 14L,
       32L, 12L)
  )

  testthat::expect_identical(
    medical_data[["ptp_percentage"]],
    c( "0%",  "3%",
       "12%", "3%",
       "20%", "9%",
       "27%", "14%",
       "32%", "12%")
  )
})

test_that("calculate_aha_2021_ptp gives NA if some inputs are missing", {

  na_age <- calculate_aha_2021_ptp(
    age = NA,
    sex = "male",
    have_dyspnea = "no",
    have_chest_pain = "yes"
  )

  under_age <- calculate_aha_2021_ptp(
    age = 25,
    sex = "male",
    have_dyspnea = "no",
    have_chest_pain = "yes"
  )

  na_sex <- calculate_aha_2021_ptp(
    age = 55,
    sex = NA,
    have_dyspnea = "no",
    have_chest_pain = "no"
  )

  na_dyspnea <- calculate_aha_2021_ptp(
    age = 55,
    sex = "male",
    have_dyspnea = NA,
    have_chest_pain = "no"
  )

  na_chest_pain <- calculate_aha_2021_ptp(
    age = 55,
    sex = "male",
    have_dyspnea = "yes",
    have_chest_pain = NA
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
    is.na(na_dyspnea),
    TRUE
  )

  testthat::expect_equal(
    is.na(na_chest_pain),
    TRUE
  )

})
