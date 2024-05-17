test_that("calculate_prms_2017_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~is_minority_ethnicity, ~hdl_mg_dl,
    ~has_diabetes, ~has_hypertension, ~has_dyslipidemia, ~has_smoking_history,
    ~has_family_history_of_cad, ~has_stress_symptoms,
    # 50 year old white female with chest pain
    # a medical history of hypertension, and a
    # high-density lipoprotein cholesterol level of 70 mg/dL
    50, 0, 0, 70, 0, 1, 0, 0, 0, 0,
    # 40 year old non-white male with chest pain
    # a medical history of diabetes, unknown stress symptoms and a
    # high-density lipoprotein cholesterol level of 65 mg/dL
    40, 1, 1, 70, 1, 0, 0, 0, 0, NA,
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          is_minority_ethnicity = .data[["is_minority_ethnicity"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          has_diabetes = .data[["has_diabetes"]],
          has_hypertension = .data[["has_hypertension"]],
          has_dyslipidemia = .data[["has_dyslipidemia"]],
          has_smoking_history = .data[["has_smoking_history"]],
          has_family_history_of_cad = .data[["has_family_history_of_cad"]],
          has_stress_symptoms = .data[["has_stress_symptoms"]]
        ),
        .f = pretestcad::calculate_prms_2017_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.710744, 0.6974111),
    tolerance = 1e-5
  )

})
