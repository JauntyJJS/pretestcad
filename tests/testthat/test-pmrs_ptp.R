test_that("calculate_prms_2017_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~is_minority_ethnicity, ~hdl_mg_dl,
    ~have_diabetes, ~have_hypertension, ~have_dyslipidemia, ~have_smoking_history,
    ~have_family_history, ~have_stress_symptoms,
    # 50 year old white female with chest pain
    # a medical history of hypertension, and a
    # high-density lipoprotein cholesterol level of 70 mg/dL
    50, "female",  "no", 70,  "no", "yes", "no", "no", "no", "no",
    # 40 year old non-white male with chest pain
    # a medical history of diabetes, unknown stress symptoms and a
    # high-density lipoprotein cholesterol level of 70 mg/dL
    40,   "male", "yes", 70, "yes",  "no", "no", "no", "no",   NA,
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          is_minority_ethnicity = .data[["is_minority_ethnicity"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_family_history = .data[["have_family_history"]],
          have_stress_symptoms = .data[["have_stress_symptoms"]]
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
