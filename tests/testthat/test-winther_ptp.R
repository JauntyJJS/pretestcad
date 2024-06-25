test_that("calculate_winther_2020_basic_ptp works", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex,  ~chest_pain,
    "40 male with typical chest pain",     40,    1,             1,
    "40 female with atypical chest pain",  40,    0,             2,
    "40 male with non anginal chest pain", 40,    1,             3,
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
        .f = pretestcad::calculate_winther_2020_basic_ptp
      )
    )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric"]],
    c(0.14302369, 0.01090790,
      0.02218292, 0.22057821),
    tolerance = 1e-5
  )

})
