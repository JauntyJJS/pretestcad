#' @title Calculate Number Of Risk Factors (CONFIRM 2015)
#' @description A function used to calculate the number of
#' risk factors the patient has. This is used to calculate the pretest
#' probability of coronary artery disease (CAD) based on the
#' 2015 CONFIRM Risk Score.
#' @inheritParams calculate_esc_2024_num_of_rf
#' @param have_typical_chest_pain The value of variable in the parameters
#' \code{label_have_typical_chest_pain_no}, \code{label_have_typical_chest_pain_yes}
#' and \code{label_have_typical_chest_pain_unknown}.
#' @param is_current_smoker The value of variable in the parameters
#' \code{label_is_current_smoker_no}, \code{label_is_current_smoker_yes}
#' and \code{label_is_current_smoker_unknown}.
#' @param max_na Input integer 0 to 5 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
#' @param label_have_typical_chest_pain_no Label(s) for patient not having
#' typical chest pain symptom.
#' Default: \code{c("no")}
#' @param label_have_typical_chest_pain_yes Label(s) for patient having
#' typical chest pain symptom.
#' Default: \code{c("yes")}
#' @param label_have_typical_chest_pain_unknown Label(s) for patient having unknown
#' typical chest pain symptom.
#' @param label_is_current_smoker_no Label(s) for patients who are not current smokers.
#' Default: \code{c("no")}
#' @param label_is_current_smoker_yes Label(s) for patients who are current smokers.
#' Default: \code{c("yes")}
#' @param label_is_current_smoker_unknown Label(s) for patient with unknown smoking status.
#' @return An integer indicating the number of risk factors the patient has.
#' It can also be \code{NA} if the number of missing risk factors exceeds the \code{max_na}
#' input value
#' @examples
#' calculate_confirm_2015_num_of_rf(
#'   have_typical_chest_pain = "yes",
#'   have_diabetes = "yes",
#'   have_hypertension = "yes",
#'   have_family_history = "yes",
#'   is_current_smoker = "no"
#' )
#'
#' calculate_confirm_2015_num_of_rf(
#'   have_typical_chest_pain = "no",
#'   have_diabetes = "no",
#'   have_hypertension = "no",
#'   have_family_history = NA,
#'   is_current_smoker = "no",
#'   max_na = 0
#' )
#'
#' calculate_confirm_2015_num_of_rf(
#'   have_typical_chest_pain = "no",
#'   have_diabetes = "no",
#'   have_hypertension = "no",
#'   have_family_history = NA,
#'   is_current_smoker = "no",
#'   max_na = 1
#' )
#' @rdname calculate_confirm_2015_num_of_rf
#' @export
calculate_confirm_2015_num_of_rf  <- function(
    have_typical_chest_pain,
    have_diabetes,
    have_hypertension,
    have_family_history,
    is_current_smoker,
    max_na = 0,
    label_have_typical_chest_pain_no = c("no"),
    label_have_typical_chest_pain_yes = c("yes"),
    label_have_typical_chest_pain_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_family_history_no = c("no"),
    label_have_family_history_yes = c("yes"),
    label_have_family_history_unknown = c(NA, NaN),
    label_is_current_smoker_no = c("no"),
    label_is_current_smoker_yes = c("yes"),
    label_is_current_smoker_unknown = c(NA, NaN)
)
{

  check_if_two_categories_are_mutually_exclusive(
    label_have_typical_chest_pain_no,
    label_have_typical_chest_pain_yes,
    label_cat_missing = label_have_typical_chest_pain_unknown
  )

  # Ensure have typical chest pain is valid and mapped to a unified group (yes, no, NA)
  have_typical_chest_pain <- have_typical_chest_pain |>
    harmonise_two_labels(
      label_one = label_have_typical_chest_pain_no,
      label_two = label_have_typical_chest_pain_yes,
      label_unknown = label_have_typical_chest_pain_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
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

  check_if_two_categories_are_mutually_exclusive(
    label_is_current_smoker_no,
    label_is_current_smoker_yes,
    label_cat_missing = label_is_current_smoker_unknown
  )

  # Ensure is current smoker is valid and mapped to a unified group (yes, no, NA)
  is_current_smoker <- is_current_smoker |>
    harmonise_two_labels(
      label_one = label_is_current_smoker_no,
      label_two = label_is_current_smoker_yes,
      label_unknown = label_is_current_smoker_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  is_current_smoker <- is_current_smoker |>
    arg_match0_allow_na(values = c("no","yes"))

  max_na <- max_na |>
    arg_match0_integer(values = c(0:5))

  number_of_na <- 0
  num_of_rf <- 0

  number_of_na <- dplyr::case_when(
    is.na(have_typical_chest_pain) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_diabetes) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_hypertension) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_family_history) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(is_current_smoker) ~ number_of_na + 1,
    .default = number_of_na
  )

  if (number_of_na > max_na) {return(NA)}

  num_of_rf <- dplyr::case_when(
    have_typical_chest_pain == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_diabetes == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_hypertension == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_family_history == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    is_current_smoker == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  )

  return(num_of_rf)

}


#' @title Calculate 2015 CONFIRM Risk Score for obstructive CAD
#' @description This function returns
#' a patient's risk score for obstructive
#' coronary artery disease based on the
#' 2015 CONFIRM Risk Score.
#' @inheritParams calculate_esc_2019_ptp
#' @inheritParams calculate_confirm_2015_num_of_rf
#' @param max_na_num_of_rf Input integer 0 to 5 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("text", "percentage")
#' \itemize{
#'   \item text means the PTP will be expressed as a probability in text (0 to > 82.4).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#' @return A numeric value representing the patient's risk
#' score for obstructive CAD based on the 2015 CONFIRM Risk Score.
#' @details The predictive model is based on CCTA images from 9093
#' patients from Phase I of the Coronary CT Angiography
#' EvaluatioN For Clinical Outcomes: An InteRnational Multicenter (CONFIRM) registry.
#' @examples
#' # 30 years old male current smoker with typical chest pain
#' calculate_confirm_2015_ptp(
#'   age = 30,
#'   sex = "male",
#'   have_typical_chest_pain = "yes",
#'   have_diabetes = "no",
#'   have_hypertension = "no",
#'   have_family_history = "no",
#'   is_current_smoker = "yes",
#'   max_na_num_of_rf = 0,
#'   output = "percentage"
#' )
#' @rdname calculate_confirm_2015_ptp
#' @export
calculate_confirm_2015_ptp <- function(
    age,
    sex,
    have_typical_chest_pain,
    have_diabetes,
    have_hypertension,
    have_family_history,
    is_current_smoker,
    max_na_num_of_rf = 0,
    output = c("text", "percentage"),
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_typical_chest_pain_no = c("no"),
    label_have_typical_chest_pain_yes = c("yes"),
    label_have_typical_chest_pain_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_family_history_no = c("no"),
    label_have_family_history_yes = c("yes"),
    label_have_family_history_unknown = c(NA, NaN),
    label_is_current_smoker_no = c("no"),
    label_is_current_smoker_yes = c("yes"),
    label_is_current_smoker_unknown = c(NA, NaN)
) {

  check_if_positive(x = age, allow_na = TRUE)
  check_if_integer(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  num_of_rf <- calculate_confirm_2015_num_of_rf(
    have_typical_chest_pain = have_typical_chest_pain,
    have_diabetes = have_diabetes,
    have_hypertension = have_hypertension,
    have_family_history = have_family_history,
    is_current_smoker = is_current_smoker,
    max_na = max_na_num_of_rf,
    label_have_typical_chest_pain_no = label_have_typical_chest_pain_no,
    label_have_typical_chest_pain_yes = label_have_typical_chest_pain_yes,
    label_have_typical_chest_pain_unknown = label_have_typical_chest_pain_unknown,
    label_have_diabetes_no = label_have_diabetes_no,
    label_have_diabetes_yes = label_have_diabetes_yes,
    label_have_diabetes_unknown = label_have_diabetes_unknown,
    label_have_hypertension_no = label_have_hypertension_no,
    label_have_hypertension_yes = label_have_hypertension_yes,
    label_have_hypertension_unknown = label_have_hypertension_unknown,
    label_have_family_history_no = label_have_family_history_no,
    label_have_family_history_yes = label_have_family_history_yes,
    label_have_family_history_unknown = label_have_family_history_unknown,
    label_is_current_smoker_no = label_is_current_smoker_no,
    label_is_current_smoker_yes = label_is_current_smoker_yes,
    label_is_current_smoker_unknown = label_is_current_smoker_unknown
  )

  age_group <- dplyr::case_when(
    dplyr::between(age, 18, 39) ~ 3L,
    dplyr::between(age, 40, 49) ~ 4L,
    dplyr::between(age, 50, 59) ~ 5L,
    dplyr::between(age, 60, 69) ~ 6L,
    age >= 70 ~ 7L,
    .default = NA_integer_
  )

  sex_group <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 1L,
    .default = NA_integer_
  )

  points = age_group + sex_group + num_of_rf

  ptp_results <- dplyr::case_when(
    points == 3  ~ "0",
    points == 4  ~ "1.4",
    points == 5  ~ "3.4",
    points == 6  ~ "5.5",
    points == 7  ~ "13.2",
    points == 8  ~ "21.3",
    points == 9  ~ "31.0",
    points == 10 ~ "43.2",
    points == 11 ~ "52.5",
    points == 12 ~ "82.4",
    points == 13 ~ ">82.4",
    .default = NA
  )

  if (isTRUE(output == "percentage")) {
    ptp_results <- stringr::str_c(ptp_results, "%")
    return(ptp_results)
  }

  return(ptp_results)

}
