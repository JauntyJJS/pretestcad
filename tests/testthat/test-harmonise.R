test_that("harmonise_two_labels harmonise variables with NA as missing values", {

  have_dyspnoea_test_1 <- "No"
  have_dyspnoea_test_2 <- "Yes"

  label_have_dyspnoea_no <- c("no", "No")
  label_have_dyspnoea_yes <- c("yes", "Yes")
  label_have_dyspnoea_unknown <- c(NA, NaN)

  testthat::expect_equal(
    harmonise_two_labels(
      arg = have_dyspnoea_test_1,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    ),
    "no"
  )

  testthat::expect_equal(
    harmonise_two_labels(
      arg = have_dyspnoea_test_2,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    ),
    "yes"
  )

})

test_that("harmonise_two_labels harmonise NA", {

  have_dyspnoea_test <- NaN

  label_have_dyspnoea_no <- c("no")
  label_have_dyspnoea_yes <- c("yes")
  label_have_dyspnoea_unknown <- c(NA, NaN)

  testthat::expect_equal(
    is.na(harmonise_two_labels(
      arg = have_dyspnoea_test,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )),
    TRUE
  )

})

test_that("harmonise_two_labels harmonise variables without NA as missing values", {

  have_dyspnoea_test <- "NIL"

  label_have_dyspnoea_no <- c("no")
  label_have_dyspnoea_yes <- c("yes")
  label_have_dyspnoea_unknown <- c("NIL")

  testthat::expect_equal(
    is.na(harmonise_two_labels(
      arg = have_dyspnoea_test,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )),
    TRUE
  )

})

test_that("harmonise_two_labels gives error of invalid missing input of have_dyspnoea_test", {

  have_dyspnoea_test <- NA

  label_have_dyspnoea_no <- c("no")
  label_have_dyspnoea_yes <- c("yes")
  label_have_dyspnoea_unknown <- c("NIL")

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_two_labels(
      arg = have_dyspnoea_test,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_two_labels gives error of invalid non-missing input with partial match", {

  have_dyspnoea_test <- "Not"

  label_have_dyspnoea_no <- c("no")
  label_have_dyspnoea_yes <- c("yes")
  label_have_dyspnoea_unknown <- c(NA, NaN)

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_two_labels(
      arg = have_dyspnoea_test,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_two_labels gives error of invalid non-missing input with no partial match", {

  have_dyspnoea_test <- "Something"

  label_have_dyspnoea_no <- c("no")
  label_have_dyspnoea_yes <- c("yes")
  label_have_dyspnoea_unknown <- c(NA, NaN)

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_two_labels(
      arg = have_dyspnoea_test,
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_three_labels harmonise variables with NA as missing values", {

  chest_pain_type_test_1 <- "unspecified"
  chest_pain_type_test_2 <- "Atypical"
  chest_pain_type_test_3 <- "angina"

  label_cpt_nonanginal <- c("nonanginal", "unspecified")
  label_cpt_atypical <- c("atypical", "Atypical")
  label_cpt_typical <- c("typical", "angina")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_equal(
    harmonise_three_labels(
      arg = chest_pain_type_test_1,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    ),
    "nonanginal"
  )

  testthat::expect_equal(
    harmonise_three_labels(
      arg <- chest_pain_type_test_2,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    ),
    "atypical"
  )

  testthat::expect_equal(
    harmonise_three_labels(
      arg = chest_pain_type_test_3,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    ),
    "typical"
  )

})

test_that("harmonise_three_labels harmonise NA", {

  chest_pain_type_test <- NaN

  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_equal(
    is.na(harmonise_three_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )),
    TRUE
  )

})

test_that("harmonise_three_labels harmonise variables without NA as missing values", {

  chest_pain_type_test <- "NIL"

  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c("NIL")

  testthat::expect_equal(
    is.na(harmonise_three_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )),
    TRUE
  )

})

test_that("harmonise_three_labels gives error of invalid missing input of chest pain type", {

  chest_pain_type_test <- NA

  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c("NIL")

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_three_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_three_labels gives error of invalid typical chest pain input with partial match", {

  chest_pain_type_test <- "Typical"

  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_three_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_three_labels gives error of invalid typical chest pain input without partial match", {

  chest_pain_type_test <- "Something"

  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_three_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_four_labels harmonise variables with NA as missing values", {

  chest_pain_type_test_1 <- "normal"
  chest_pain_type_test_2 <- "unspecified"
  chest_pain_type_test_3 <- "Atypical"
  chest_pain_type_test_4 <- "angina"

  label_cpt_no_chest_pain <- c("no chest pain", "normal")
  label_cpt_nonanginal <- c("nonanginal", "unspecified")
  label_cpt_atypical <- c("atypical", "Atypical")
  label_cpt_typical <- c("typical", "angina")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_equal(
    harmonise_four_labels(
      arg = chest_pain_type_test_1,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    ),
    "no chest pain"
  )

  testthat::expect_equal(
    harmonise_four_labels(
      arg = chest_pain_type_test_2,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    ),
    "nonanginal"
  )

  testthat::expect_equal(
    harmonise_four_labels(
      arg = chest_pain_type_test_3,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    ),
    "atypical"
  )

  testthat::expect_equal(
    harmonise_four_labels(
      arg = chest_pain_type_test_4,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    ),
    "typical"
  )

})

test_that("harmonise_four_labels harmonise NA", {

  chest_pain_type_test <- NaN

  label_cpt_no_chest_pain <- c("no chest pain")
  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_equal(
    is.na(harmonise_four_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    )),
    TRUE
  )

})

test_that("harmonise_four_labels harmonise variables without NA as missing values", {

  chest_pain_type_test <- "NIL"

  label_cpt_no_chest_pain <- c("no chest pain", "normal")
  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c("NIL")

  testthat::expect_equal(
    is.na(harmonise_four_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    )),
    TRUE
  )

})

test_that("harmonise_four_labels gives error of invalid missing input of chest pain type", {

  chest_pain_type_test <- NA

  label_cpt_no_chest_pain <- c("no chest pain", "normal")
  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c("NIL")

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_four_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_four_labels gives error of invalid typical chest pain input with partial match", {

  chest_pain_type_test <- "Typical"

  label_cpt_no_chest_pain <- c("no chest pain", "normal")
  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_four_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    )
  )

})

test_that("harmonise_four_labels gives error of invalid typical chest pain input without partial match", {

  chest_pain_type_test <- "Something"

  label_cpt_no_chest_pain <- c("no chest pain", "normal")
  label_cpt_nonanginal <- c("nonanginal")
  label_cpt_atypical <- c("atypical")
  label_cpt_typical <- c("typical")
  label_cpt_unknown <- c(NA, NaN)

  testthat::expect_snapshot(
    error = TRUE,
    harmonise_four_labels(
      arg = chest_pain_type_test,
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    )
  )

})

