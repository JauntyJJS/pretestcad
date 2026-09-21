test_that("calculate_chen_2018_mfs_fig_3_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~have_hypertension,
    ~tc_mg_dl, ~hdl_mg_dl, ~lvef_percent,
    ~hsCRP_mg_dl, ~have_anemia,
    ~expected_mfs_points, ~expected_risk_group,
    ~expected_risk_numeric, ~expected_risk_percentage,
    # 40 year old female with no hypertension
    # and anemia. She has a
    # total cholesterol level of 150 mg/dL
    # high-density lipoprotein cholesterol level of 70 mg/dL and
    # high-sensitivity C reactive protein level of 1 mg/dL with
    # a left Ventricular Ejection Fraction reading of 60%
    40, "female", "no", 150, 70, 60, 1, "no", 0, "low", 16, "16%",
    # 60 year old male with hypertension
    # and anemia. He has a
    # total cholesterol level of 200 mg/dL
    # high-density lipoprotein cholesterol level of 50 mg/dL and
    # high-sensitivity C reactive protein level of 6 mg/dL with
    # a left Ventricular Ejection Fraction reading of 40%
    60, "male", "yes", 200, 50, 40, 6, "yes", 45, "very high", 92, "92%",
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      mfs_points = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_hypertension = .data[["have_hypertension"]],
          tc_mg_dl = .data[["tc_mg_dl"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          lvef_percent = .data[["lvef_percent"]],
          hsCRP_mg_dl = .data[["hsCRP_mg_dl"]],
          have_anemia = .data[["have_anemia"]]
        ),
        .f = pretestcad::calculate_chen_2018_mfs_fig_3_ptp,
        output = "mfs_points"
      ),
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_hypertension = .data[["have_hypertension"]],
          tc_mg_dl = .data[["tc_mg_dl"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          lvef_percent = .data[["lvef_percent"]],
          hsCRP_mg_dl = .data[["hsCRP_mg_dl"]],
          have_anemia = .data[["have_anemia"]]
        ),
        .f = pretestcad::calculate_chen_2018_mfs_fig_3_ptp,
        output = "risk_group"
      ),
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_hypertension = .data[["have_hypertension"]],
          tc_mg_dl = .data[["tc_mg_dl"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          lvef_percent = .data[["lvef_percent"]],
          hsCRP_mg_dl = .data[["hsCRP_mg_dl"]],
          have_anemia = .data[["have_anemia"]]
        ),
        .f = pretestcad::calculate_chen_2018_mfs_fig_3_ptp,
        output = "risk_numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_hypertension = .data[["have_hypertension"]],
          tc_mg_dl = .data[["tc_mg_dl"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          lvef_percent = .data[["lvef_percent"]],
          hsCRP_mg_dl = .data[["hsCRP_mg_dl"]],
          have_anemia = .data[["have_anemia"]]
        ),
        .f = pretestcad::calculate_chen_2018_mfs_fig_3_ptp,
        output = "risk_percentage"
      )
    )

  testthat::expect_equal(
    medical_data[["mfs_points"]],
    medical_data[["expected_mfs_points"]]
  )

  testthat::expect_equal(
    medical_data[["ptp_grouping"]],
    medical_data[["expected_risk_group"]]
  )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    medical_data[["expected_risk_numeric"]]
  )

  testthat::expect_equal(
    medical_data[["ptp_percentage"]],
    medical_data[["expected_risk_percentage"]]
  )

})


test_that("calculate_chen_2018_mfs_formula_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~have_hypertension,
    ~tc_mg_dl, ~hdl_mg_dl, ~lvef_percent,
    ~hsCRP_mg_dl, ~have_anemia,
    # 40 year old female with no hypertension
    # and anemia. She has a
    # total cholesterol level of 150 mg/dL
    # high-density lipoprotein cholesterol level of 70 mg/dL and
    # high-sensitivity C reactive protein level of 1 mg/dL with
    # a left Ventricular Ejection Fraction reading of 60%
    40, "female", "no", 150, 70, 60, 1, "no",
    # 60 year old male with hypertension
    # and anemia. He has a
    # total cholesterol level of 200 mg/dL
    # high-density lipoprotein cholesterol level of 50 mg/dL and
    # high-sensitivity C reactive protein level of 6 mg/dL with
    # a left Ventricular Ejection Fraction reading of 40%
    60, "male", "yes", 200, 50, 40, 6, "yes"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_hypertension = .data[["have_hypertension"]],
          tc_mg_dl = .data[["tc_mg_dl"]],
          hdl_mg_dl = .data[["hdl_mg_dl"]],
          lvef_percent = .data[["lvef_percent"]],
          hsCRP_mg_dl = .data[["hsCRP_mg_dl"]],
          have_anemia = .data[["have_anemia"]]
        ),
        .f = pretestcad::calculate_chen_2018_mfs_formula_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.1825150, 0.9222129),
    tolerance = 1e-5
  )

})
