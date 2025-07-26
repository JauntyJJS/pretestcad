#' @title Calculate 2017 PROMISE Minimal-Risk Score for obstructive CAD
#' @description This function returns
#' a symptomatic (have chest pain or dyspnoea) patient's
#' minimal risk score for obstructive
#' coronary artery disease based on the
#' 2017 PROMISE Minimal-Risk Score.
#' @inheritParams calculate_esc_2024_num_of_rf
#' @inheritParams calculate_esc_2024_fig_4_ptp_simplfied
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param hdl_mg_dl Input positive numeric value to indicate the
#' patient's high-density lipoprotein (HDL) in \eqn{mg/dL}.
#' @param is_minority_ethnicity The value of variable in the parameters
#' \code{label_is_minority_ethnicity_no}, \code{label_is_minority_ethnicity_yes}
#' and \code{label_is_minority_ethnicity_unknown}.
#' @param have_stress_symptoms The value of variable in the parameters
#' \code{label_have_stress_symptoms_no}, \code{label_have_stress_symptoms_yes}
#' and \code{label_have_stress_symptoms_unknown}.
#' Default: \code{NA}
#' @param label_is_minority_ethnicity_no Label(s) for patient not from a
#' racial or minority ethnicity (or patient is a non-Hispanic/Latino White).
#' Default: \code{c("no")}
#' @param label_is_minority_ethnicity_yes Label(s) for patient from a
#' racial or minority ethnicity (or patient is not a non-Hispanic/Latino White).
#' E.g. Blacks, Asians, etc.
#' Default: \code{c("yes")}
#' @param label_is_minority_ethnicity_unknown Label(s) for patient from an unknown ethnicity
#' Default: \code{c(NA, NaN)}
#' @param label_have_stress_symptoms_no Label(s) for patient with
#' no symptoms (negative results) related to physical or mental stress.
#' Default: \code{c("no")}
#' @param label_have_stress_symptoms_yes Label(s) for patient with
#' symptoms (positive results) related to physical or mental stress.
#' Default: \code{c("yes")}
#' @param label_have_stress_symptoms_unknown Label(s) for patient with
#' inconclusive results or patient has not taken any stress test
#' Default: \code{c(NA, NaN)}
#' @return A numeric value representing the patient's minimal risk
#' score for obstructive CAD based on the 2017 PROMISE Minimal-Risk Score.
#' @details The predictive model is based on CCTA images from 4632
#' patients in the Prospective Multicenter imaging Study for Evaluation
#' of Chest Pain (PROMISE) trial.
#' @examples
#' # 50 year old white female with chest pain
#' # a medical history of hypertension, and a
#' # high-density lipoprotein cholesterol level of 70 mg/dL
#' calculate_prms_2017_ptp(
#'     age = 50,
#'     sex = "female",
#'     hdl_mg_dl = 70,
#'     is_minority_ethnicity = "no",
#'     have_diabetes = "no",
#'     have_hypertension = "yes",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no",
#'     have_family_history = "no",
#'     have_stress_symptoms = "no"
#' )
#'
#' # 40 year old non-white male with chest pain
#' # a medical history of diabetes, unknown stress symptoms and a
#' # high-density lipoprotein cholesterol level of 70 mg/dL
#' calculate_prms_2017_ptp(
#'     age = 40,
#'     sex = "male",
#'     hdl_mg_dl = 70,
#'     is_minority_ethnicity = "yes",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no",
#'     have_family_history = "no",
#'     have_stress_symptoms = NA
#' )
#' @rdname calculate_prms_2017_ptp
#' @export
calculate_prms_2017_ptp <- function(
    age,
    sex,
    hdl_mg_dl,
    is_minority_ethnicity,
    have_diabetes,
    have_hypertension,
    have_dyslipidemia,
    have_smoking_history,
    have_family_history,
    have_stress_symptoms = NA,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_is_minority_ethnicity_no = c("no"),
    label_is_minority_ethnicity_yes = c("yes"),
    label_is_minority_ethnicity_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_smoking_history_no = c("no"),
    label_have_smoking_history_yes = c("yes"),
    label_have_smoking_history_unknown = c(NA, NaN),
    label_have_family_history_no = c("no"),
    label_have_family_history_yes = c("yes"),
    label_have_family_history_unknown = c(NA, NaN),
    label_have_stress_symptoms_no = c("no"),
    label_have_stress_symptoms_yes = c("yes"),
    label_have_stress_symptoms_unknown = c(NA, NaN)
) {

  check_if_positive(x = age, allow_na = TRUE)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_cat_missing = label_sex_unknown
  )

  # Ensure sex is valid and mapped to a unified group (male, female, NA)
  sex <- sex |>
    harmonise_two_labels(
      label_one = label_sex_male,
      label_two = label_sex_female,
      label_unknown = label_sex_unknown,
      harmonise_label_one = "male",
      harmonise_label_two = "female",
      harmonise_label_unknown = NA
    )

  is_female <- dplyr::case_when(
    sex == "female" ~ 1,
    sex == "male" ~ 0,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_is_minority_ethnicity_no,
    label_is_minority_ethnicity_yes,
    label_cat_missing = label_is_minority_ethnicity_unknown
  )

  # Ensure is minority ethnicity is valid and mapped to a unified group (yes, no, NA)
  is_minority_ethnicity <- is_minority_ethnicity |>
    harmonise_two_labels(
      label_one = label_is_minority_ethnicity_no,
      label_two = label_is_minority_ethnicity_yes,
      label_unknown = label_is_minority_ethnicity_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  is_minority_ethnicity <- dplyr::case_when(
    is_minority_ethnicity == "yes" ~ 1,
    is_minority_ethnicity == "no" ~ 0,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_smoking_history_no,
    label_have_smoking_history_yes,
    label_cat_missing = label_have_smoking_history_unknown
  )

  # Ensure have smoking history is valid and mapped to a unified group (yes, no, NA)
  have_smoking_history <- have_smoking_history |>
    harmonise_two_labels(
      label_one = label_have_smoking_history_no,
      label_two = label_have_smoking_history_yes,
      label_unknown = label_have_smoking_history_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  is_non_smoker <- dplyr::case_when(
    have_smoking_history == "yes" ~ 0,
    have_smoking_history == "no" ~ 1,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_diabetes_no,
    label_have_diabetes_yes,
    label_cat_missing = label_have_diabetes_unknown
  )

  # Ensure have diabetes is valid and mapped to a unified group (yes, no, NA)
  have_diabetes <- have_diabetes |>
    harmonise_two_labels(
      label_one = label_have_diabetes_no,
      label_two = label_have_diabetes_yes,
      label_unknown = label_have_diabetes_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_no_diabetes <- dplyr::case_when(
    have_diabetes == "yes" ~ 0,
    have_diabetes == "no" ~ 1,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_dyslipidemia_no,
    label_have_dyslipidemia_yes,
    label_cat_missing = label_have_dyslipidemia_unknown
  )

  # Ensure have dyslipidemia is valid and mapped to a unified group (yes, no, NA)
  have_dyslipidemia <- have_dyslipidemia |>
    harmonise_two_labels(
      label_one = label_have_dyslipidemia_no,
      label_two = label_have_dyslipidemia_yes,
      label_unknown = label_have_dyslipidemia_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_no_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "yes" ~ 0,
    have_dyslipidemia == "no" ~ 1,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_family_history_no,
    label_have_family_history_yes,
    label_cat_missing = label_have_family_history_unknown
  )

  # Ensure have family history is valid and mapped to a unified group (yes, no, NA)
  have_family_history <- have_family_history |>
    harmonise_two_labels(
      label_one = label_have_family_history_no,
      label_two = label_have_family_history_yes,
      label_unknown = label_have_family_history_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_no_family_history <- dplyr::case_when(
    have_family_history == "yes" ~ 0,
    have_family_history == "no" ~ 1,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_hypertension_no,
    label_have_hypertension_yes,
    label_cat_missing = label_have_hypertension_unknown
  )

  # Ensure have hypertension is valid and mapped to a unified group (yes, no, NA)
  have_hypertension <- have_hypertension |>
    harmonise_two_labels(
      label_one = label_have_hypertension_no,
      label_two = label_have_hypertension_yes,
      label_unknown = label_have_hypertension_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_no_hypertension <- dplyr::case_when(
    have_hypertension == "yes" ~ 0,
    have_hypertension == "no" ~ 1,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_stress_symptoms_no,
    label_have_stress_symptoms_yes,
    label_cat_missing = label_have_stress_symptoms_unknown
  )

  # Ensure have stress symptoms is valid and mapped to a unified group (yes, no, NA)
  have_stress_symptoms <- have_stress_symptoms |>
    harmonise_two_labels(
      label_one = label_have_stress_symptoms_no,
      label_two = label_have_stress_symptoms_yes,
      label_unknown = label_have_stress_symptoms_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_no_stress_symptoms <- dplyr::case_when(
    is.na(have_stress_symptoms) ~ 0,
    have_stress_symptoms == "yes" ~ 0,
    have_stress_symptoms == "no" ~ 1,
    .default = NA_integer_
  )

  have_unknown_stress_symptoms <- dplyr::case_when(
    is.na(have_stress_symptoms) ~ 1,
    have_stress_symptoms == "yes" ~ 0,
    have_stress_symptoms == "no" ~ 0,
    .default = NA_integer_
  )

  check_if_non_negative(x = hdl_mg_dl, allow_na = TRUE)

  prms_2017_ptp  <- 1 /
    (1 + exp((-1.783 +
             ( 0.084 * age) +
             (-1.026 * is_female) +
             (-0.142 * is_minority_ethnicity) +
             (-0.526 * is_non_smoker) +
             (-0.314 * have_no_diabetes) +
             (-0.412 * have_no_dyslipidemia) +
             (-0.309 * have_no_family_history) +
             (-0.408 * have_no_hypertension) +
             (-0.309 * have_no_stress_symptoms) +
             (-0.195 * have_unknown_stress_symptoms) +
             (-0.006 * hdl_mg_dl)
    )
    )
    )

  return(prms_2017_ptp)
}
