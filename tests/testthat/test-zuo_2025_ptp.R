test_that("calculate_zuo_2025_predict_ocad_ptp works", {

  # As seen in https://predict-obstructive-cad.shinyapps.io/dynnomapp/

  medical_data <- tibble::tribble(
    ~age, ~sex,
    ~have_dyslipidemia, ~have_diabetes,
    ~coronary_calcium_score,
    # 30 male without dyslipidemia and diabetes and calcium score of 0
    30, "male"  ,  "no" , "no" , 0,
    # 35 female with dyslipidemia but no diabetes and calcium score of 0
    35, "female",  "yes", "no" , 0,
    # 40 male without dyslipidemia but have diabetes and calcium score of 0
    40, "male"  ,  "no" , "yes", 0,
    # 45 female with dyslipidemia and diabetes and calcium score of 0
    45, "female",  "yes", "yes", 0,
    # 50 male without dyslipidemia and diabetes and calcium score of 1
    50, "male"  ,  "no" , "no" , 1,
    # 55 female without dyslipidemia and diabetes and calcium score of 100
    55, "female",  "yes", "no" , 100,
    # 60 male without dyslipidemia and diabetes and calcium score of 400
    60, "male"  ,  "no" , "yes", 101,
    # 60 female without dyslipidemia and diabetes and calcium score of 401
    65, "female",  "yes", "yes", 400,
    # 70 male without dyslipidemia and diabetes and calcium score of 401
    70, "male"  ,  "no" , "no" , 401,
    # 75 female without dyslipidemia and diabetes and calcium score of 500
    75, "female",  "yes", "yes", 500
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_diabetes = .data[["have_diabetes"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_zuo_2025_predict_ocad_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.01478526, 0.01920046,
      0.02430118, 0.03146728,
      0.15690250, 0.19534239,
      0.56517530, 0.62901372,
      0.79616523, 0.86281406),
    tolerance = 1e-5
  )

})
