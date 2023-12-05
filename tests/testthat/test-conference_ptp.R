test_that("calculate_esc_2019_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id, ~age, ~sex, ~have_dyspnea, ~chest_pain,
    "30 male with typical chest pain", 35, 1, 0, 1,
    "39 female with typical chest pain", 35, 0, 0, 1,
    "40 male with typical chest pain", 40, 1, 0, 1,
    "49 female with typical chest pain", 49, 0, 0, 1,
    "50 male with typical chest pain", 50, 1, 0, 1,
    "59 female with typical chest pain", 59, 0, 0, 1,
    "60 male with typical chest pain", 60, 1, 0, 1,
    "69 female with typical chest pain", 69, 0, 0, 1,
    "70 male with typical chest pain", 70, 1, 0, 1,
    "79 female with typical chest pain", 79, 0, 0, 1,
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
    c(3L, 5L,
      22L, 10L,
      32L, 13L,
      44L, 16L,
      52L, 27L)
  )

  testthat::expect_identical(
    typical_medical_data[["ptp_percentage"]],
    c("3%", "5%",
      "22%", "10%",
      "32%", "13%",
      "44%", "16%",
      "52%", "27%")
  )

})
