test_that("calculate_cad2_2012_basic_ptp works on typical chest pain", {

  typical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "30 male with typical chest pain",     30, "male"  ,        "typical",
    "39 female with typical chest pain",   35, "female",        "typical",
    "70 male with typical chest pain",     70, "male"  ,        "typical",
    "79 female with typical chest pain",   79, "female",        "typical"
  )

  typical_medical_data <- typical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_cad2_2012_basic_ptp
      )
    )

  testthat::expect_equal(
    typical_medical_data[["ptp_numeric"]],
    c(0.15525053, 0.06082505,
      0.69550849, 0.50874911),
    tolerance = 1e-5
  )

})

test_that("calculate_cad2_2012_basic_ptp works on atypical chest pain", {

  atypical_medical_data <- tibble::tribble(
    ~unique_id,                          ~age, ~sex    , ~chest_pain_type,
    "40 male with atypical chest pain",     40,"male"  ,       "atypical",
    "49 female with atypical chest pain",   49,"female",       "atypical",
    "80 male with atypical chest pain",     80,"male"  ,       "atypical",
    "89 female with atypical chest pain",   89,"female",       "atypical"
  )

  atypical_medical_data <- atypical_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_cad2_2012_basic_ptp
      )
    )

  testthat::expect_equal(
    atypical_medical_data[["ptp_numeric"]],
    c(0.08463306, 0.04023312,
      0.53469416, 0.34253898),
    tolerance = 1e-5
  )

})

test_that("calculate_cad2_2012_basic_ptp works on non-anginal chest pain", {

  non_anginal_medical_data <- tibble::tribble(
    ~unique_id,                              ~age, ~sex    , ~chest_pain_type,
    "30 male with non-anginal chest pain",     30, "male"  ,     "nonanginal",
    "39 female with non-anginal chest pain",   39, "female",     "nonanginal",
    "90 male with non-anginal chest pain",     90, "male"  ,     "nonanginal",
    "99 female with non-anginal chest pain",   99, "female",     "nonanginal"
  )

  non_anginal_medical_data <- non_anginal_medical_data |>
    dplyr::mutate(
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          chest_pain_type = .data[["chest_pain_type"]]
        ),
        .f = pretestcad::calculate_cad2_2012_basic_ptp
      )
    )

  testthat::expect_equal(
    non_anginal_medical_data[["ptp_numeric"]],
    c(0.02486778, 0.01143020,
      0.52772154, 0.33626130),
    tolerance = 1e-5
  )

})


test_that("calculate_cad2_2012_clinical_ptp works", {

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
        .f = pretestcad::calculate_cad2_2012_clinical_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.087145291, 0.008697363, 0.023499411, 0.089073528,
      0.161650294, 0.035536899, 0.462321565, 0.079146529,
      0.196865728),
    tolerance = 1e-5
  )

})


test_that("calculate_cad2_2012_clinical_ccs_ptp works", {

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
        .f = pretestcad::calculate_cad2_2012_clinical_ccs_ptp
      )
    )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    c(0.14171055, 0.03653673, 0.13540048, 0.42922903,
      0.60563229, 0.34965750, 0.90213371, 0.01265459,
      0.39383912),
    tolerance = 1e-5
  )

})
