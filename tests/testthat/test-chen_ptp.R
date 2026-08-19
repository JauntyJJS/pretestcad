test_that("calculate_chen_2018_mfs_ptp works", {

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
        .f = pretestcad::calculate_chen_2018_mfs_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.1825150, 0.9222129),
    tolerance = 1e-5
  )

})

test_that("calculate_reeh_2019_basic_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~symptom_type,
    "40 male with atypical chest pain",     40,"male"  ,    "atypical",
    "49 female with atypical chest pain",   49,"female",    "atypical",
    "80 male with atypical chest pain",     80,"male"  ,    "atypical",
    "89 female with atypical chest pain",   89,"female",    "atypical"
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_type = .data[["symptom_type"]]
        ),
        .f = pretestcad::calculate_reeh_2019_basic_ptp
      )
    )

  testthat::expect_equal(
    atypical_medical_data[["ptp_numeric"]],
    c(0.08083170, 0.03217069,
      0.37681243, 0.18603236),
    tolerance = 1e-5
  )

})

test_that("calculate_reeh_2019_basic_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                              ~age, ~sex    , ~symptom_type,
    "30 male with non-anginal chest pain",     30, "male"  ,  "nonanginal",
    "39 female with non-anginal chest pain",   39, "female",  "nonanginal",
    "90 male with non-anginal chest pain",     90, "male"  ,  "nonanginal",
    "99 female with non-anginal chest pain",   99, "female",  "nonanginal"
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_type = .data[["symptom_type"]]
        ),
        .f = pretestcad::calculate_reeh_2019_basic_ptp
      )
    )

  testthat::expect_equal(
    non_anginal_medical_data[["ptp_numeric"]],
    c(0.008308772, 0.003156909,
      0.131233068, 0.054013208),
    tolerance = 1e-5
  )

})

test_that("calculate_reeh_2019_basic_ptp works on dyspnoea", {

  dyspnoea_medical_data <- tibble::tribble(
    ~unique_id,                              ~age, ~sex    , ~symptom_type,
    "30 male with dyspnoea chest pain",        30, "male"  ,    "dyspnoea",
    "39 female with dyspnoea chest pain",      39, "female",    "dyspnoea",
    "90 male with dyspnoea chest pain",        90, "male"  ,    "dyspnoea",
    "99 female with dyspnoea chest pain",      99, "female",    "dyspnoea"
  )

  dyspnoea_medical_data <- dyspnoea_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_type = .data[["symptom_type"]]
        ),
        .f = pretestcad::calculate_reeh_2019_basic_ptp
      )
    )

  testthat::expect_equal(
    dyspnoea_medical_data[["ptp_numeric"]],
    c(0.018154776, 0.006940616,
      0.250021055, 0.111907825),
    tolerance = 1e-5
  )

})

test_that("calculate_reeh_2019_clinical_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~symptom_type,
    ~have_dyslipidemia, ~have_family_history, ~have_diabetes,
    # 30 male with only typical chest pain
    30, "male"  , "typical"   , "no" , "no" , "no" ,
    # 35 female with only atypical chest pain
    35, "female", "atypical"  , "no" , "no" , "no" ,
    # 40 male with only nonanginal chest pain
    40, "male"  , "nonanginal", "no" , "no" , "no" ,
    # 45 female with only dyspnoea
    45, "female", "dyspnoea"  , "no" , "no" , "no" ,
    # 50 female with typical chest pain and has dyslipidemia
    50, "female", "typical"   , "yes", "no" , "no" ,
    # 55 male with atypical chest pain and has dyslipidemia
    55, "male"  , "atypical"  , "yes", "no" , "no" ,
    # 60 female with nonanginal chest pain and has dyslipidemia
    60, "female", "nonanginal", "yes", "no" , "no" ,
    # 65 male with only dyspnoea and has dyslipidemia
    65, "male"  , "dyspnoea"  , "yes", "no" , "no" ,
    # 70 male with typical chest pain and has family history of CAD
    70, "male"  , "typical"   , "no" , "yes", "no" ,
    # 75 female with atypical chest pain and has family history of CAD
    75, "female", "atypical"  , "no" , "yes", "no" ,
    # 80 male with nonanginal chest pain and has diabetes
    80, "male"  , "nonanginal", "no" , "no" , "yes",
    # 85 female with only dyspnoea and has diabetes
    85, "female", "dyspnoea"  , "no" , "no" , "yes"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_type = .data[["symptom_type"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_family_history = .data[["have_family_history"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_reeh_2019_clinical_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.056046246, 0.006658700, 0.006116074, 0.004156776,
      0.090347443, 0.168394322, 0.010189054, 0.111972441,
      0.408033685, 0.072201751, 0.062879010, 0.043532043),
    tolerance = 1e-5
  )

})
