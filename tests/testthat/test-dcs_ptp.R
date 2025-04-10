test_that("calculate_dcs_1993_sig_cad_ptp works", {

  # Verified with https://www.zunis.org/Duke%20Chest%20Pain%20-%20CAD%20Predictor.htm

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type, ~have_mi,
    ~have_diabetes, ~have_dyslipidemia, ~have_smoking_history,
    ~have_q_waves, ~have_st_t_changes,
    # 30 male with only typical chest pain
    30, "male"  , "typical"   , "no" , "no" , "no" , "no" , "no", "no",
    # 35 female with only atypical chest pain and Q waves on ECG
    35, "female", "atypical"  , "no" , "no" , "no" , "no" , "yes", "no",
    # 40 male with only nonanginal chest pain and ST-T changes on ECG
    40, "male"  , "nonanginal", "no" , "no" , "no" , "no" , "no", "yes",
    # 45 female with typical chest pain and history of MI
    # Q waves and ST-T changes on ECG
    45, "female", "typical"   , "yes", "no" , "no" , "no" , "yes", "yes",
    # 50 male with atypical chest pain and has diabetes
    50, "male"  , "atypical"  , "no" , "yes", "no" , "no" , "no", "no",
    # 55 female with nonanginal, has dyslipidemia and Q waves on ECG
    55, "female", "nonanginal", "no" , "no" , "yes", "no" , "yes", "no",
    # 60 male with typical chest pain, a smoker and ST-T changes on ECG
    60, "male"  , "typical"   , "no" , "no", "no" , "yes", "no", "yes",
    # 65 female with atypicial chest pain, a smoker,
    # Q waves and ST-T changes on ECG
    65, "female", "atypical"  , "no" , "no", "yes", "yes" , "yes", "yes",
    # 70 male with non-anginal chest pain, history of MI,
    # has dyslipidemia, diabetes and a smoker,
    # Q waves and ST-T changes on ECG
    70, "male"  , "nonanginal", "yes", "yes", "yes", "yes", "yes", "yes"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_mi = .data[["have_mi"]],
          have_diabetes = .data[["have_diabetes"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]]
        ),
        .f = pretestcad::calculate_dcs_1993_sig_cad_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.19513232, 0.06738940, 0.09665121, 0.90665841,
      0.48100914, 0.18407187, 0.94103299, 0.77171140,
      0.99133706),
    tolerance = 1e-5
  )

})
