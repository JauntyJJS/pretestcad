test_that("calculate_precise_2021_simple_ptp works", {

  # Verified with https://webapps.duke-nus.edu.sg/tools/PRECISE

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type, ~have_neck_radiation,
    ~have_diabetes, ~have_hypertension, ~smoking_history_type,
    # 30 male with only typical chest pain
    30, "male"  , "typical"   , "no" , "no" , "no" , "none" ,
    # 35 female with only atypical chest pain
    35, "female", "atypical"  , "no" , "no" , "no" , "none" ,
    # 40 male with only nonanginal chest pain
    40, "male"  , "nonanginal", "no" , "no" , "no" , "none" ,
    # 45 female with typical chest pain, radiating to the neck
    45, "female", "typical"   , "yes", "no" , "no" , "none" ,
    # 50 male with atypical chest pain and has diabetes
    50, "male"  , "atypical"  , "no" , "yes", "no" , "none" ,
    # 55 female with nonanginal and has hypertension
    55, "female", "nonanginal", "no" , "no" , "yes", "none" ,
    # 60 male with typical chest pain and a current smoker
    60, "male"  , "typical"   , "no" , "no", "no" , "current" ,
    # 65 female with atypicial chest pain and a past smoker
    65, "female", "atypical"  , "no" , "no", "yes", "past" ,
    # 70 male with non-anginal chest pain, radiating to the neck,
    # has hypertension, diabetes and a current smoker
    70, "male"  , "nonanginal", "yes", "yes", "yes", "current"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_neck_radiation = .data[["have_neck_radiation"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          smoking_history_type = .data[["smoking_history_type"]]
        ),
        .f = pretestcad::calculate_precise_2021_simple_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.07635037, 0.01066569, 0.02825014, 0.07453690,
      0.15472666, 0.01529223, 0.34253898, 0.05340333,
      0.64588506),
    tolerance = 1e-5
  )

})

test_that("calculate_precise_2021_clinical_ptp works", {

  # Verified with https://webapps.duke-nus.edu.sg/tools/PRECISE

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type, ~have_neck_radiation,
    ~have_diabetes, ~have_hypertension, ~smoking_history_type,
    ~have_q_waves, ~have_st_t_changes,
    # 30 male with only typical chest pain
    30, "male"  , "typical"   , "no" , "no" , "no" , "none" , "no", "no",
    # 35 female with only atypical chest pain and Q waves on ECG
    35, "female", "atypical"  , "no" , "no" , "no" , "none" , "yes", "no",
    # 40 male with only nonanginal chest pain and ST-T changes on ECG
    40, "male"  , "nonanginal", "no" , "no" , "no" , "none" , "no", "yes",
    # 45 female with typical chest pain, radiating to the neck,
    # Q waves and ST-T changes on ECG
    45, "female", "typical"   , "yes", "no" , "no" , "none" , "yes", "yes",
    # 50 male with atypical chest pain and has diabetes
    50, "male"  , "atypical"  , "no" , "yes", "no" , "none" , "no", "no",
    # 55 female with nonanginal, has hypertension and Q waves on ECG
    55, "female", "nonanginal", "no" , "no" , "yes", "none" , "yes", "no",
    # 60 male with typical chest pain, a current smoker and ST-T changes on ECG
    60, "male"  , "typical"   , "no" , "no", "no" , "current", "no", "yes",
    # 65 female with atypicial chest pain, a past smoker,
    # Q waves and ST-T changes on ECG
    65, "female", "atypical"  , "no" , "no", "yes", "past" , "yes", "yes",
    # 70 male with non-anginal chest pain, radiating to the neck,
    # has hypertension, diabetes and a current smoker,
    # Q waves and ST-T changes on ECG
    70, "male"  , "nonanginal", "yes", "yes", "yes", "current", "yes", "yes"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_neck_radiation = .data[["have_neck_radiation"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          smoking_history_type = .data[["smoking_history_type"]],
          have_q_waves = .data[["have_q_waves"]],
          have_st_t_changes = .data[["have_st_t_changes"]]
        ),
        .f = pretestcad::calculate_precise_2021_clinical_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.06913842, 0.02498932, 0.04343846, 0.24490004,
      0.13681493, 0.03286277, 0.41945770, 0.17436529,
      0.87011772),
    tolerance = 1e-5
  )

})
