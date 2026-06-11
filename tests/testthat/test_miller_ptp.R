test_that("calculate_miller_2023_vessel_50_cad_ptp works", {

  medical_data <- tibble::tribble(
    ~age,     ~sex, ~have_chest_pain, ~coronary_calcium_score, ~expected_grouping, ~expected_score, ~expected_percentage,
      30, "female",             "no",                       0,              "low",             1.0,               "1.0%",
      30, "female",             "no",                       1,              "low",             9.4,               "9.4%",
      30, "female",             "no",                     100,     "intermediate",            27.9,              "27.9%",
      30, "female",             "no",                     400,     "intermediate",            48.7,              "48.7%",
      30, "female",             "no",                    1000,             "high",            60.2,              "60.2%",
      39,   "male",             "no",                       0,              "low",             1.7,               "1.7%",
      39,   "male",             "no",                      99,              "low",            13.9,              "13.9%",
      39,   "male",             "no",                     399,     "intermediate",            33.3,              "33.3%",
      39,   "male",             "no",                     999,             "high",            53.9,              "53.9%",
      39,   "male",             "no",                    1500,             "high",            77.6,              "77.6%",

      40, "female",             "no",                       0,              "low",             1.4,               "1.4%",
      40, "female",             "no",                      99,              "low",            12.1,              "12.1%",
      40, "female",             "no",                     399,     "intermediate",            33.9,              "33.9%",
      40, "female",             "no",                     999,             "high",            55.6,              "55.6%",
      40, "female",             "no",                    1500,             "high",            66.7,              "66.7%",
      49,   "male",             "no",                       0,              "low",             2.2,               "2.2%",
      49,   "male",             "no",                       1,     "intermediate",            17.7,              "17.7%",
      49,   "male",             "no",                     100,     "intermediate",            39.8,              "39.8%",
      49,   "male",             "no",                     400,             "high",            60.8,              "60.8%",
      49,   "male",             "no",                    1000,             "high",            82.1,              "82.1%",      

      50, "female",             "no",                       0,              "low",             1.8,               "1.8%",
      50, "female",             "no",                       1,     "intermediate",            15.2,              "15.2%",
      50, "female",             "no",                     100,     "intermediate",            40.0,              "40.0%",
      50, "female",             "no",                     400,             "high",            62.0,              "62.0%",
      50, "female",             "no",                    1000,             "high",            72.2,              "72.2%",
      59,   "male",             "no",                       0,              "low",             2.9,               "2.9%",
      59,   "male",             "no",                      99,     "intermediate",            21.8,              "21.8%",
      59,   "male",             "no",                     399,     "intermediate",            46.2,              "46.2%",
      59,   "male",             "no",                     999,             "high",            66.8,              "66.8%",
      59,   "male",             "no",                    1500,             "high",            85.6,              "85.6%",
      
      60, "female",             "no",                       0,              "low",             1.4,               "1.4%",
      60, "female",             "no",                      99,              "low",            12.4,              "12.4%",
      60, "female",             "no",                     399,     "intermediate",            34.6,              "34.6%",
      60, "female",             "no",                     999,             "high",            56.4,              "56.4%",
      60, "female",             "no",                    1500,             "high",            67.4,              "67.4%",
      69,   "male",             "no",                       0,              "low",             2.3,               "2.3%",
      69,   "male",             "no",                       1,     "intermediate",            18.1,              "18.1%",
      69,   "male",             "no",                     100,     "intermediate",            40.5,              "40.5%",
      69,   "male",             "no",                     400,             "high",            61.5,              "61.5%",
      69,   "male",             "no",                    1000,             "high",            82.5,              "82.5%",
      
      70, "female",             "no",                       0,              "low",             1.3,               "1.3%",
      70, "female",             "no",                       1,              "low",            12.0,              "12.0%",
      70, "female",             "no",                     100,     "intermediate",            33.7,              "33.7%",
      70, "female",             "no",                     400,             "high",            55.4,              "55.4%",
      70, "female",             "no",                    1000,             "high",            66.5,              "66.5%",
      79,   "male",             "no",                       0,              "low",             2.2,               "2.2%",
      79,   "male",             "no",                      99,     "intermediate",            17.5,              "17.5%",
      79,   "male",             "no",                     399,     "intermediate",            39.6,              "39.6%",
      79,   "male",             "no",                     999,             "high",            60.5,              "60.5%",
      79,   "male",             "no",                    1500,             "high",            82.0,              "82.0%",
      
      30, "female",            "yes",                       0,              "low",             2.2,               "2.2%",
      30, "female",            "yes",                       1,     "intermediate",            18.3,              "18.3%",
      30, "female",            "yes",                     100,     "intermediate",            45.5,              "45.5%",
      30, "female",            "yes",                     400,             "high",            67.1,              "67.1%",
      30, "female",            "yes",                    1000,             "high",            76.5,              "76.5%",
      39,   "male",            "yes",                       0,              "low",             3.4,               "3.4%",
      39,   "male",            "yes",                      99,     "intermediate",            24.9,              "24.9%",
      39,   "male",            "yes",                     399,             "high",            50.5,              "50.5%",
      39,   "male",            "yes",                     999,             "high",            70.5,              "70.5%",
      39,   "male",            "yes",                    1500,             "high",            87.6,              "87.6%",

      40, "female",            "yes",                       0,              "low",             2.9,               "2.9%",
      40, "female",            "yes",                      99,     "intermediate",            22.9,              "22.9%",
      40, "female",            "yes",                     399,             "high",            52.5,              "52.5%",
      40, "female",            "yes",                     999,             "high",            73.0,              "73.0%",
      40, "female",            "yes",                    1500,             "high",            81.2,              "81.2%",
      49,   "male",            "yes",                       0,              "low",             4.4,               "4.4%",
      49,   "male",            "yes",                       1,     "intermediate",            30.5,              "30.5%",
      49,   "male",            "yes",                     100,             "high",            57.5,              "57.5%",
      49,   "male",            "yes",                     400,             "high",            76.0,              "76.0%",
      49,   "male",            "yes",                    1000,             "high",            90.4,              "90.4%",
      
      50, "female",            "yes",                       0,              "low",             3.7,               "3.7%",
      50, "female",            "yes",                       1,     "intermediate",            27.8,              "27.8%",
      50, "female",            "yes",                     100,             "high",            58.9,              "58.9%",
      50, "female",            "yes",                     400,             "high",            77.8,              "77.8%",
      50, "female",            "yes",                    1000,             "high",            84.8,              "84.8%",
      59,   "male",            "yes",                       0,              "low",             5.7,               "5.7%",
      59,   "male",            "yes",                      99,     "intermediate",            36.3,              "36.3%",
      59,   "male",            "yes",                     399,             "high",            63.7,              "63.7%",
      59,   "male",            "yes",                     999,             "high",            80.4,              "80.4%",
      59,   "male",            "yes",                    1500,             "high",            92.4,              "92.4%",

      60, "female",            "yes",                       0,              "low",             3.0,               "3.0%",
      60, "female",            "yes",                      99,     "intermediate",            23.4,              "23.4%",
      60, "female",            "yes",                     399,             "high",            53.2,              "53.2%",
      60, "female",            "yes",                     999,             "high",            73.6,              "73.6%",
      60, "female",            "yes",                    1500,             "high",            81.6,              "81.6%",
      69,   "male",            "yes",                       0,              "low",             4.6,               "4.6%",
      69,   "male",            "yes",                       1,     "intermediate",            31.1,              "31.1%",
      69,   "male",            "yes",                     100,             "high",            58.2,              "58.2%",
      69,   "male",            "yes",                     400,             "high",            76.5,              "76.5%",
      69,   "male",            "yes",                    1000,             "high",            90.6,              "90.6%",
      
      70, "female",            "yes",                       0,              "low",             2.8,               "2.8%",
      70, "female",            "yes",                       1,     "intermediate",            22.7,              "22.7%",
      70, "female",            "yes",                     100,             "high",            52.2,              "52.2%",
      70, "female",            "yes",                     400,             "high",            72.8,              "72.8%",
      70, "female",            "yes",                    1000,             "high",            81.0,              "81.0%",
      79,   "male",            "yes",                       0,              "low",             4.4,               "4.4%",
      79,   "male",            "yes",                      99,     "intermediate",            30.3,              "30.3%",
      79,   "male",            "yes",                     399,             "high",            57.2,              "57.2%",
      79,   "male",            "yes",                     999,             "high",            75.8,              "75.8%",
      79,   "male",            "yes",                    1500,             "high",            90.3,              "90.3%"      
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_chest_pain = .data[["have_chest_pain"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_miller_2023_vessel_50_cad_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_chest_pain = .data[["have_chest_pain"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_miller_2023_vessel_50_cad_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_chest_pain = .data[["have_chest_pain"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_miller_2023_vessel_50_cad_ptp,
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

test_that("calculate_miller_2023_lm_50_non_lm_70_cad_ptp works", {

  medical_data <- tibble::tribble(
    ~age,     ~sex, ~have_chest_pain, ~coronary_calcium_score, ~expected_grouping, ~expected_score, ~expected_percentage,
      30, "female",             "no",                       0,              "low",             0.5,               "0.5%",
      30, "female",             "no",                       1,              "low",             4.3,               "4.3%",
      30, "female",             "no",                     100,     "intermediate",            19.8,              "19.8%",
      30, "female",             "no",                     400,     "intermediate",            22.0,              "22.0%",
      30, "female",             "no",                    1000,     "intermediate",            39.9,              "39.9%",
      39,   "male",             "no",                       0,              "low",             1.2,               "1.2%",
      39,   "male",             "no",                      99,              "low",             6.3,               "6.3%",
      39,   "male",             "no",                     399,     "intermediate",            23.2,              "23.2%",
      39,   "male",             "no",                     999,     "intermediate",            37.3,              "37.3%",
      39,   "male",             "no",                    1500,             "high",            62.4,              "62.4%",

      40, "female",             "no",                       0,              "low",             0.7,               "0.7%",
      40, "female",             "no",                      99,              "low",             5.9,               "5.9%",
      40, "female",             "no",                     399,     "intermediate",            25.7,              "25.7%",
      40, "female",             "no",                     999,     "intermediate",            28.2,              "28.2%",
      40, "female",             "no",                    1500,     "intermediate",            48.1,              "48.1%",
      49,   "male",             "no",                       0,              "low",             1.7,               "1.7%",
      49,   "male",             "no",                       1,              "low",             8.6,               "8.6%",
      49,   "male",             "no",                     100,     "intermediate",            29.7,              "29.7%",
      49,   "male",             "no",                     400,     "intermediate",            45.3,              "45.3%",
      49,   "male",             "no",                    1000,             "high",            69.9,              "69.9%",      

      50, "female",             "no",                       0,              "low",             0.5,               "0.5%",
      50, "female",             "no",                       1,              "low",             4.3,               "4.3%",
      50, "female",             "no",                     100,     "intermediate",            19.8,              "19.8%",
      50, "female",             "no",                     400,     "intermediate",            22.0,              "22.0%",
      50, "female",             "no",                    1000,     "intermediate",            39.8,              "39.8%",
      59,   "male",             "no",                       0,              "low",             1.2,               "1.2%",
      59,   "male",             "no",                      99,              "low",             6.3,               "6.3%",
      59,   "male",             "no",                     399,     "intermediate",            23.2,              "23.2%",
      59,   "male",             "no",                     999,     "intermediate",            37.2,              "37.2%",
      59,   "male",             "no",                    1500,             "high",            62.4,              "62.4%",
      
      60, "female",             "no",                       0,              "low",             0.5,               "0.5%",
      60, "female",             "no",                      99,              "low",             4.6,               "4.6%",
      60, "female",             "no",                     399,     "intermediate",            20.9,              "20.9%",
      60, "female",             "no",                     999,     "intermediate",            23.1,              "23.1%",
      60, "female",             "no",                    1500,     "intermediate",            41.4,              "41.4%",
      69,   "male",             "no",                       0,              "low",             1.3,               "1.3%",
      69,   "male",             "no",                       1,              "low",             6.7,               "6.7%",
      69,   "male",             "no",                     100,     "intermediate",            24.4,              "24.4%",
      69,   "male",             "no",                     400,     "intermediate",            38.8,              "38.8%",
      69,   "male",             "no",                    1000,             "high",            63.9,              "63.9%",
      
      70, "female",             "no",                       0,              "low",             0.4,               "0.4%",
      70, "female",             "no",                       1,              "low",             3.3,               "3.3%",
      70, "female",             "no",                     100,     "intermediate",            15.9,              "15.9%",
      70, "female",             "no",                     400,     "intermediate",            17.8,              "17.8%",
      70, "female",             "no",                    1000,     "intermediate",            33.7,              "33.7%",
      79,   "male",             "no",                       0,              "low",             0.9,               "0.9%",
      79,   "male",             "no",                      99,              "low",             4.9,               "4.9%",
      79,   "male",             "no",                     399,     "intermediate",            18.8,              "18.8%",
      79,   "male",             "no",                     999,     "intermediate",            31.3,              "31.3%",
      79,   "male",             "no",                    1500,             "high",            56.1,              "56.1%",
      
      30, "female",            "yes",                       0,              "low",             1.1,               "1.1%",
      30, "female",            "yes",                       1,              "low",             9.2,               "9.2%",
      30, "female",            "yes",                     100,     "intermediate",            35.7,              "35.7%",
      30, "female",            "yes",                     400,     "intermediate",            38.8,              "38.8%",
      30, "female",            "yes",                    1000,             "high",            59.9,              "59.9%",
      39,   "male",            "yes",                       0,              "low",             2.5,               "2.5%",
      39,   "male",            "yes",                      99,              "low",            12.3,              "12.3%",
      39,   "male",            "yes",                     399,     "intermediate",            38.6,              "38.6%",
      39,   "male",            "yes",                     999,             "high",            55.3,              "55.3%",
      39,   "male",            "yes",                    1500,             "high",            77.6,              "77.6%",

      40, "female",            "yes",                       0,              "low",             1.6,               "1.6%",
      40, "female",            "yes",                      99,              "low",            12.3,              "12.3%",
      40, "female",            "yes",                     399,     "intermediate",            43.7,              "43.7%",
      40, "female",            "yes",                     999,     "intermediate",            46.9,              "46.9%",
      40, "female",            "yes",                    1500,             "high",            67.6,              "67.6%",
      49,   "male",            "yes",                       0,              "low",             3.4,               "3.4%",
      49,   "male",            "yes",                       1,     "intermediate",            16.3,              "16.3%",
      49,   "male",            "yes",                     100,     "intermediate",            46.8,              "46.8%",
      49,   "male",            "yes",                     400,             "high",            63.3,              "63.3%",
      49,   "male",            "yes",                    1000,             "high",            82.8,              "82.8%",
      
      50, "female",            "yes",                       0,              "low",             1.1,               "1.1%",
      50, "female",            "yes",                       1,              "low",             9.1,               "9.1%",
      50, "female",            "yes",                     100,     "intermediate",            35.6,              "35.6%",
      50, "female",            "yes",                     400,     "intermediate",            38.7,              "38.7%",
      50, "female",            "yes",                    1000,             "high",            59.8,              "59.8%",
      59,   "male",            "yes",                       0,              "low",             2.5,               "2.5%",
      59,   "male",            "yes",                      99,              "low",            12.2,              "12.2%",
      59,   "male",            "yes",                     399,     "intermediate",            38.6,              "38.6%",
      59,   "male",            "yes",                     999,             "high",            55.2,              "55.2%",
      59,   "male",            "yes",                    1500,             "high",            77.5,              "77.5%",

      60, "female",            "yes",                       0,              "low",             1.2,               "1.2%",
      60, "female",            "yes",                      99,              "low",             9.7,               "9.7%",
      60, "female",            "yes",                     399,     "intermediate",            37.2,              "37.2%",
      60, "female",            "yes",                     999,     "intermediate",            40.3,              "40.3%",
      60, "female",            "yes",                    1500,             "high",            61.4,              "61.4%",
      69,   "male",            "yes",                       0,              "low",             2.6,               "2.6%",
      69,   "male",            "yes",                       1,              "low",            13.0,              "13.0%",
      69,   "male",            "yes",                     100,     "intermediate",            40.2,              "40.2%",
      69,   "male",            "yes",                     400,             "high",            56.9,              "56.9%",
      69,   "male",            "yes",                    1000,             "high",            78.7,              "78.7%",
      
      70, "female",            "yes",                       0,              "low",             0.9,               "0.9%",
      70, "female",            "yes",                       1,              "low",             7.2,               "7.2%",
      70, "female",            "yes",                     100,     "intermediate",            29.9,              "29.9%",
      70, "female",            "yes",                     400,     "intermediate",            32.7,              "32.7%",
      70, "female",            "yes",                    1000,             "high",            53.4,              "53.4%",
      79,   "male",            "yes",                       0,              "low",             1.9,               "1.9%",
      79,   "male",            "yes",                      99,              "low",             9.7,               "9.7%",
      79,   "male",            "yes",                     399,     "intermediate",            32.6,              "32.6%",
      79,   "male",            "yes",                     999,     "intermediate",            48.7,              "48.7%",
      79,   "male",            "yes",                    1500,             "high",            72.6,              "72.6%"     
  )

  medical_data <- medical_data |>
    dplyr::mutate(
      ptp_grouping = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_chest_pain = .data[["have_chest_pain"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_miller_2023_lm_50_non_lm_70_cad_ptp,
        output = "grouping"
      ),
      ptp_numeric = purrr::pmap_dbl(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_chest_pain = .data[["have_chest_pain"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_miller_2023_lm_50_non_lm_70_cad_ptp,
        output = "numeric"
      ),
      ptp_percentage = purrr::pmap_chr(
        .l = list(
          age = .data[["age"]],
          sex = .data[["sex"]],
          have_chest_pain = .data[["have_chest_pain"]],
          coronary_calcium_score = .data[["coronary_calcium_score"]]
        ),
        .f = pretestcad::calculate_miller_2023_lm_50_non_lm_70_cad_ptp,
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