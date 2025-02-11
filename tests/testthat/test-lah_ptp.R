test_that("calculate_lah_2022_clinical_ptp works on typical chest pain", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type,
    ~have_diabetes, ~have_hypertension, ~have_dyslipidemia, ~have_smoking_history,
    # 30 male with only typical chest pain
    30, "male"  , "typical"   , "no" , "no" , "no" , "no" ,
    # 35 female with only atypical chest pain
    35, "female", "atypical"  , "no" , "no" , "no" , "no" ,
    # 40 male with only nonanginal chest pain
    40, "male"  , "nonanginal", "no" , "no" , "no" , "no" ,
    # 45 female with typical chest pain and has diabetes
    45, "female", "typical"   , "yes", "no" , "no" , "no" ,
    # 50 male with atypical chest pain and has diabetes
    50, "male"  , "atypical"  , "yes", "no" , "no" , "no" ,
    # 55 female with nonanginal and has diabetes
    55, "female", "nonanginal", "yes", "no" , "no" , "no" ,
    # 60 male with typical chest pain and has hypertension
    60, "male"  , "typical"   , "no" , "yes", "no" , "no" ,
    # 65 female with atypicial chest pain and has dyslipidemia
    65, "female", "atypical"  , "no" , "no" , "yes", "no" ,
    # 70 male with non-anginal chest pain and has smoking history
    70, "male",   "nonanginal", "no" , "no" , "no" , "yes"
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_smoking_history = .data[["have_smoking_history"]]
        ),
        .f = pretestcad::calculate_lah_2022_clinical_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.07069908, 0.01775803, 0.11204704,
      0.06464593, 0.25483299, 0.10284552,
      0.47277695, 0.16341981, 0.39555998),
    tolerance = 1e-5
  )

})

test_that("calculate_lah_2022_extended_ptp works", {

  medical_data <- tibble::tribble(
    ~age, ~sex, ~chest_pain_type,
    ~have_diabetes, ~have_hypertension, ~have_dyslipidemia, ~have_smoking_history,
    ~coronary_calcium_score,
    # 30 male with only typical chest pain with calcium score of 5
    30, "male"  , "typical"   , "no" , "no" , "no" , "no" ,    5,
    # 35 female with only atypical chest pain with calcium score of 15
    35, "female", "atypical"  , "no" , "no" , "no" , "no" ,   15,
    # 40 male with only non-anginal chest pain with calcium score of 150
    40, "male"  , "nonanginal", "no" , "no" , "no" , "no" ,  150,
    # 45 female with typical chest pain and has diabetes with calcium score of 300
    45, "female", "typical"   , "yes", "no" , "no" , "no" ,  300,
    # 50 male with atypical chest pain and has diabetes with calcium score of 600
    50, "male"  , "atypical"  , "yes", "no" , "no" , "no" ,  600,
    # 55 female with non-anginal and has diabetes with calcium score of 1200
    55, "female", "nonanginal", "yes", "no" , "no" , "no" , 1200,
    # 60 male with typical chest pain and has hypertension with calcium score of 2400
    60, "male"  , "typical"   , "no" , "yes", "no" , "no" , 2400,
    # 65 female with atypicial chest pain and has dyslipidemia with calcium score of 0
    65, "female", "atypical"  , "no" , "no" , "yes", "no" ,    0,
    # 70 male with non-anginal chest pain and has smoking history with calcium score of 700
    70, "male",   "nonanginal", "no" , "no" , "no" , "yes",  700
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]],
          have_diabetes = .data[["have_diabetes"]],
          have_hypertension = .data[["have_hypertension"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_smoking_history = .data[["have_smoking_history"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_lah_2022_extended_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.126037534, 0.121980543, 0.699230920,
      0.742869144, 0.864095213, 0.897915346,
      0.965881281, 0.009565319, 0.871917435),
    tolerance = 1e-5
  )

})

