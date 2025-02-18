test_that("calculate_esc_2024_symptom_score works", {

  medical_data <- tibble::tribble(
    ~unique_id,                                            ~chest_pain_type, ~have_dyspnea, ~expected_score,
    "Patient with no chest pain and no dyspnea",            "no chest pain",          "no",               0,
    "Patient with no chest pain and have dyspnea",          "no chest pain",         "yes",               2,
    "Patient with nonanginal chest pain and no dyspnea",       "nonanginal",          "no",               1,
    "Patient with nonanginal chest pain and have dyspnea",     "nonanginal",         "yes",               2,
    "Patient with atypical chest pain and no dyspnea",           "atypical",          "no",               2,
    "Patient with atypical chest pain and have dyspnea",         "atypical",         "yes",               2,
    "Patient with typical chest pain and no dyspnea",             "typical",          "no",               3,
    "Patient with typical chest pain and have dyspnea",           "typical",         "yes",               3,
  )

  medical_data_with_na <- tibble::tribble(
    ~unique_id,                                            ~chest_pain_type, ~have_dyspnea, ~expected_score,
    "Patient with no chest pain",                           "no chest pain",            NA,               0,
    "Patient with nonanginal chest pain",                      "nonanginal",            NA,               1,
    "Patient with atypical chest pain",                          "atypical",            NA,               2,
    "Patient with typical chest pain",                            "typical",            NA,               3,
    "Patient with no dyspnea",                                           NA,          "no",               0,
    "Patient with dyspnea",                                              NA,         "yes",               2,
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnea = .data[["have_dyspnea"]]
        ),
        .f = pretestcad::calculate_esc_2024_symptom_score,
        allow_na = TRUE
      )
    )

  medical_data_with_na <- medical_data_with_na |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnea = .data[["have_dyspnea"]]
        ),
        .f = pretestcad::calculate_esc_2024_symptom_score,
        allow_na = TRUE
      )
    )

  medical_data_with_na_fail <- medical_data_with_na |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          chest_pain_type = .data[["chest_pain_type"]],
          have_dyspnea = .data[["have_dyspnea"]]
        ),
        .f = pretestcad::calculate_esc_2024_symptom_score,
        allow_na = FALSE
      )
    )

  testthat::expect_equal(
    medical_data[["symptom_score"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data_with_na[["symptom_score"]],
    medical_data_with_na[["expected_score"]]
  )

  testthat::expect_equal(
    all(is.na(medical_data_with_na_fail[["symptom_score"]])),
    TRUE
  )

})

test_that("calculate_esc_2024_num_of_rf works", {

  medical_data <- tibble::tribble(
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes, ~expected_score,
                    "no",                  "no",               "no",               "no",           "no",               0,
                    "no",                  "no",               "no",               "no",          "yes",               1,
                    "no",                  "no",               "no",              "yes",          "yes",               2,
                    "no",                  "no",              "yes",              "yes",          "yes",               3,
                    "no",                 "yes",              "yes",              "yes",          "yes",               4,
                   "yes",                 "yes",              "yes",              "yes",          "yes",               5

  )

  medical_data_with_na <- tibble::tribble(
    ~have_family_history, ~have_smoking_history, ~have_dyslipidemia, ~have_hypertension, ~have_diabetes, ~expected_score,
     "no",                    NA,               "no",               "no",           "no",               0,
     "no",                  "no",                 NA,               "no",          "yes",               1,
     "no",                  "no",               "no",                 NA,          "yes",               1,
      NA,                   "no",              "yes",              "yes",          "yes",               3,
     "no",                 "yes",              "yes",              "yes",             NA,               3,
    "yes",                    NA,              "yes",              "yes",          "yes",               4

  )

  medical_data <- medical_data |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_esc_2024_num_of_rf,
        max_na = 0
      )
    )

  medical_data_with_na <- medical_data_with_na |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_esc_2024_num_of_rf,
        max_na = 1
      )
    )

  medical_data_with_na_fail <- medical_data_with_na |>
    dplyr::mutate(
      symptom_score = purrr::pmap_int(
        .l = list(
          have_family_history = .data[["have_family_history"]],
          have_smoking_history = .data[["have_smoking_history"]],
          have_dyslipidemia = .data[["have_dyslipidemia"]],
          have_hypertension = .data[["have_hypertension"]],
          have_diabetes = .data[["have_diabetes"]]
        ),
        .f = pretestcad::calculate_esc_2024_num_of_rf,
        max_na = 0
      )
    )

  testthat::expect_equal(
    medical_data[["symptom_score"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data_with_na[["symptom_score"]],
    medical_data_with_na[["expected_score"]]
  )

  testthat::expect_equal(
    all(is.na(medical_data_with_na_fail[["symptom_score"]])),
    TRUE
  )

})

test_that("calculate_esc_2024_fig_4_ptp_simplfied works", {

  medical_data <- tibble::tribble(
    ~age,     ~sex, ~symptom_score, ~num_of_rf, ~expected_grouping, ~expected_score, ~expected_percentage,
      30, "female",              0,          0,         "very low",               0,                 "0%",
      30, "female",              0,          1,         "very low",               0,                 "0%",
      30, "female",              0,          2,         "very low",               1,                 "1%",
      30, "female",              0,          3,         "very low",               1,                 "1%",
      30, "female",              0,          4,         "very low",               2,                 "2%",
      30, "female",              0,          5,         "very low",               2,                 "2%",
      39,   "male",              1,          0,         "very low",               1,                 "1%",
      39,   "male",              1,          1,         "very low",               1,                 "1%",
      39,   "male",              1,          2,         "very low",               2,                 "2%",
      39,   "male",              1,          3,         "very low",               2,                 "2%",
      39,   "male",              1,          4,         "very low",               5,                 "5%",
      39,   "male",              1,          5,         "very low",               5,                 "5%",

      39, "female",              2,          0,         "very low",               0,                 "0%",
      39, "female",              2,          1,         "very low",               0,                 "0%",
      39, "female",              2,          2,         "very low",               1,                 "1%",
      39, "female",              2,          3,         "very low",               1,                 "1%",
      39, "female",              2,          4,         "very low",               3,                 "3%",
      39, "female",              2,          5,         "very low",               3,                 "3%",
      30,   "male",              2,          0,         "very low",               2,                 "2%",
      30,   "male",              2,          1,         "very low",               2,                 "2%",
      30,   "male",              2,          2,         "very low",               4,                 "4%",
      30,   "male",              2,          3,         "very low",               4,                 "4%",
      30,   "male",              2,          4,              "low",               8,                 "8%",
      30,   "male",              2,          5,              "low",               8,                 "8%",

      30, "female",              3,          0,         "very low",               2,                 "2%",
      30, "female",              3,          1,         "very low",               2,                 "2%",
      30, "female",              3,          2,         "very low",               5,                 "5%",
      30, "female",              3,          3,         "very low",               5,                 "5%",
      30, "female",              3,          4,              "low",              10,                "10%",
      30, "female",              3,          5,              "low",              10,                "10%",
      39,   "male",              3,          0,              "low",               9,                 "9%",
      39,   "male",              3,          1,              "low",               9,                 "9%",
      39,   "male",              3,          2,              "low",              14,                "14%",
      39,   "male",              3,          3,              "low",              14,                "14%",
      39,   "male",              3,          4,         "moderate",              22,                "22%",
      39,   "male",              3,          5,         "moderate",              22,                "22%",

      40, "female",              0,          0,         "very low",               1,                 "1%",
      40, "female",              0,          1,         "very low",               1,                 "1%",
      40, "female",              0,          2,         "very low",               1,                 "1%",
      40, "female",              0,          3,         "very low",               1,                 "1%",
      40, "female",              0,          4,         "very low",               3,                 "3%",
      40, "female",              0,          5,         "very low",               3,                 "3%",
      49,   "male",              1,          0,         "very low",               2,                 "2%",
      49,   "male",              1,          1,         "very low",               2,                 "2%",
      49,   "male",              1,          2,         "very low",               4,                 "4%",
      49,   "male",              1,          3,         "very low",               4,                 "4%",
      49,   "male",              1,          4,              "low",               8,                 "8%",
      49,   "male",              1,          5,              "low",               8,                 "8%",

      49, "female",              2,          0,         "very low",               1,                 "1%",
      49, "female",              2,          1,         "very low",               1,                 "1%",
      49, "female",              2,          2,         "very low",               2,                 "2%",
      49, "female",              2,          3,         "very low",               2,                 "2%",
      49, "female",              2,          4,         "very low",               5,                 "5%",
      49, "female",              2,          5,         "very low",               5,                 "5%",
      40,   "male",              2,          0,         "very low",               3,                 "3%",
      40,   "male",              2,          1,         "very low",               3,                 "3%",
      40,   "male",              2,          2,              "low",               6,                 "6%",
      40,   "male",              2,          3,              "low",               6,                 "6%",
      40,   "male",              2,          4,              "low",              12,                "12%",
      40,   "male",              2,          5,              "low",              12,                "12%",

      40, "female",              3,          0,         "very low",               4,                 "4%",
      40, "female",              3,          1,         "very low",               4,                 "4%",
      40, "female",              3,          2,              "low",               7,                 "7%",
      40, "female",              3,          3,              "low",               7,                 "7%",
      40, "female",              3,          4,              "low",              12,                "12%",
      40, "female",              3,          5,              "low",              12,                "12%",
      49,   "male",              3,          0,              "low",              14,                "14%",
      49,   "male",              3,          1,              "low",              14,                "14%",
      49,   "male",              3,          2,         "moderate",              20,                "20%",
      49,   "male",              3,          3,         "moderate",              20,                "20%",
      49,   "male",              3,          4,         "moderate",              27,                "27%",
      49,   "male",              3,          5,         "moderate",              27,                "27%",

      50, "female",              0,          0,         "very low",               1,                 "1%",
      50, "female",              0,          1,         "very low",               1,                 "1%",
      50, "female",              0,          2,         "very low",               2,                 "2%",
      50, "female",              0,          3,         "very low",               2,                 "2%",
      50, "female",              0,          4,         "very low",               5,                 "5%",
      50, "female",              0,          5,         "very low",               5,                 "5%",
      59,   "male",              1,          0,         "very low",               4,                 "4%",
      59,   "male",              1,          1,         "very low",               4,                 "4%",
      59,   "male",              1,          2,              "low",               7,                 "7%",
      59,   "male",              1,          3,              "low",               7,                 "7%",
      59,   "male",              1,          4,              "low",              12,                "12%",
      59,   "male",              1,          5,              "low",              12,                "12%",

      59, "female",              2,          0,         "very low",               2,                 "2%",
      59, "female",              2,          1,         "very low",               2,                 "2%",
      59, "female",              2,          2,         "very low",               3,                 "3%",
      59, "female",              2,          3,         "very low",               3,                 "3%",
      59, "female",              2,          4,              "low",               7,                 "7%",
      59, "female",              2,          5,              "low",               7,                 "7%",
      50,   "male",              2,          0,              "low",               6,                 "6%",
      50,   "male",              2,          1,              "low",               6,                 "6%",
      50,   "male",              2,          2,              "low",              11,                "11%",
      50,   "male",              2,          3,              "low",              11,                "11%",
      50,   "male",              2,          4,         "moderate",              17,                "17%",
      50,   "male",              2,          5,         "moderate",              17,                "17%",

      50, "female",              3,          0,              "low",               6,                 "6%",
      50, "female",              3,          1,              "low",               6,                 "6%",
      50, "female",              3,          2,              "low",              10,                "10%",
      50, "female",              3,          3,              "low",              10,                "10%",
      50, "female",              3,          4,              "low",              15,                "15%",
      50, "female",              3,          5,              "low",              15,                "15%",
      59,   "male",              3,          0,         "moderate",              21,                "21%",
      59,   "male",              3,          1,         "moderate",              21,                "21%",
      59,   "male",              3,          2,         "moderate",              27,                "27%",
      59,   "male",              3,          3,         "moderate",              27,                "27%",
      59,   "male",              3,          4,         "moderate",              33,                "33%",
      59,   "male",              3,          5,         "moderate",              33,                "33%",

      60, "female",              0,          0,         "very low",               2,                 "2%",
      60, "female",              0,          1,         "very low",               2,                 "2%",
      60, "female",              0,          2,         "very low",               4,                 "4%",
      60, "female",              0,          3,         "very low",               4,                 "4%",
      60, "female",              0,          4,              "low",               7,                 "7%",
      60, "female",              0,          5,              "low",               7,                 "7%",
      69,   "male",              1,          0,              "low",               8,                 "8%",
      69,   "male",              1,          1,              "low",               8,                 "8%",
      69,   "male",              1,          2,              "low",              12,                "12%",
      69,   "male",              1,          3,              "low",              12,                "12%",
      69,   "male",              1,          4,         "moderate",              17,                "17%",
      69,   "male",              1,          5,         "moderate",              17,                "17%",

      69, "female",              2,          0,         "very low",               3,                 "3%",
      69, "female",              2,          1,         "very low",               3,                 "3%",
      69, "female",              2,          2,              "low",               6,                 "6%",
      69, "female",              2,          3,              "low",               6,                 "6%",
      69, "female",              2,          4,              "low",              11,                "11%",
      69, "female",              2,          5,              "low",              11,                "11%",
      60,   "male",              2,          0,              "low",              12,                "12%",
      60,   "male",              2,          1,              "low",              12,                "12%",
      60,   "male",              2,          2,         "moderate",              17,                "17%",
      60,   "male",              2,          3,         "moderate",              17,                "17%",
      60,   "male",              2,          4,         "moderate",              25,                "25%",
      60,   "male",              2,          5,         "moderate",              25,                "25%",

      60, "female",              3,          0,              "low",              10,                "10%",
      60, "female",              3,          1,              "low",              10,                "10%",
      60, "female",              3,          2,              "low",              14,                "14%",
      60, "female",              3,          3,              "low",              14,                "14%",
      60, "female",              3,          4,         "moderate",              19,                "19%",
      60, "female",              3,          5,         "moderate",              19,                "19%",
      69,   "male",              3,          0,         "moderate",              32,                "32%",
      69,   "male",              3,          1,         "moderate",              32,                "32%",
      69,   "male",              3,          2,         "moderate",              35,                "35%",
      69,   "male",              3,          3,         "moderate",              35,                "35%",
      69,   "male",              3,          4,         "moderate",              39,                "39%",
      69,   "male",              3,          5,         "moderate",              39,                "39%",

      70, "female",              0,          0,         "very low",               4,                 "4%",
      70, "female",              0,          1,         "very low",               4,                 "4%",
      70, "female",              0,          2,              "low",               7,                 "7%",
      70, "female",              0,          3,              "low",               7,                 "7%",
      70, "female",              0,          4,              "low",              11,                "11%",
      70, "female",              0,          5,              "low",              11,                "11%",
      80,   "male",              1,          0,              "low",              15,                "15%",
      80,   "male",              1,          1,              "low",              15,                "15%",
      80,   "male",              1,          2,         "moderate",              19,                "19%",
      80,   "male",              1,          3,         "moderate",              19,                "19%",
      80,   "male",              1,          4,         "moderate",              24,                "24%",
      80,   "male",              1,          5,         "moderate",              24,                "24%",

      80, "female",              2,          0,              "low",               6,                 "6%",
      80, "female",              2,          1,              "low",               6,                 "6%",
      80, "female",              2,          2,              "low",              10,                "10%",
      80, "female",              2,          3,              "low",              10,                "10%",
      80, "female",              2,          4,         "moderate",              16,                "16%",
      80, "female",              2,          5,         "moderate",              16,                "16%",
      70,   "male",              2,          0,         "moderate",              22,                "22%",
      70,   "male",              2,          1,         "moderate",              22,                "22%",
      70,   "male",              2,          2,         "moderate",              27,                "27%",
      70,   "male",              2,          3,         "moderate",              27,                "27%",
      70,   "male",              2,          4,         "moderate",              34,                "34%",
      70,   "male",              2,          5,         "moderate",              34,                "34%",

      70, "female",              3,          0,         "moderate",              16,                "16%",
      70, "female",              3,          1,         "moderate",              16,                "16%",
      70, "female",              3,          2,         "moderate",              19,                "19%",
      70, "female",              3,          3,         "moderate",              19,                "19%",
      70, "female",              3,          4,         "moderate",              23,                "23%",
      70, "female",              3,          5,         "moderate",              23,                "23%",
      80,   "male",              3,          0,         "moderate",              44,                "44%",
      80,   "male",              3,          1,         "moderate",              44,                "44%",
      80,   "male",              3,          2,         "moderate",              44,                "44%",
      80,   "male",              3,          3,         "moderate",              44,                "44%",
      80,   "male",              3,          4,         "moderate",              45,                "45%",
      80,   "male",              3,          5,         "moderate",              45,                "45%",
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_score = .data[["symptom_score"]],
          num_of_rf = .data[["num_of_rf"]]
        ),
        .f = pretestcad::calculate_esc_2024_fig_4_ptp_simplfied,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_int(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_score = .data[["symptom_score"]],
          num_of_rf = .data[["num_of_rf"]]
        ),
        .f = pretestcad::calculate_esc_2024_fig_4_ptp_simplfied,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          symptom_score = .data[["symptom_score"]],
          num_of_rf = .data[["num_of_rf"]]
        ),
        .f = pretestcad::calculate_esc_2024_fig_4_ptp_simplfied,
        output = "percentage"
      ),
    )

  testthat::expect_equal(
    medical_data[["ptp_grouping"]],
    medical_data[["expected_grouping"]]
  )

  testthat::expect_equal(
    medical_data[["ptp_numeric"]],
    medical_data[["expected_score"]]
  )

  testthat::expect_equal(
    medical_data[["ptp_percentage"]],
    medical_data[["expected_percentage"]]
  )

})

test_that("calculate_esc_2024_fig_4_ptp works", {

  testthat::expect_identical(
    calculate_esc_2024_fig_4_ptp(
      age = 30,
      sex = "female",
      chest_pain_type = "no chest pain",
      have_dyspnea = "no",
      have_family_history = "no",
      have_smoking_history = "no",
      have_dyslipidemia = "no",
      have_hypertension = "no",
      have_diabetes = "no",
      allow_na_symptom_score = TRUE,
      max_na_num_of_rf = 0,
      output = "percentage"
    ),
    "0%"
  )

  testthat::expect_identical(
    calculate_esc_2024_fig_4_ptp(
      age = 30,
      sex = "female",
      chest_pain_type = "no chest pain",
      have_dyspnea = "no",
      have_family_history = "no",
      have_smoking_history = "no",
      have_dyslipidemia = "no",
      have_hypertension = "no",
      have_diabetes = "no",
      allow_na_symptom_score = TRUE,
      max_na_num_of_rf = 0,
      output = "grouping"
    ),
    "very low"
  )

  testthat::expect_identical(
    calculate_esc_2024_fig_4_ptp(
      age = 30,
      sex = "female",
      chest_pain_type = "no chest pain",
      have_dyspnea = "no",
      have_family_history = "no",
      have_smoking_history = "no",
      have_dyslipidemia = "no",
      have_hypertension = "no",
      have_diabetes = "no",
      allow_na_symptom_score = TRUE,
      max_na_num_of_rf = 0,
      output = "numeric"
    ),
    0
  )

})

