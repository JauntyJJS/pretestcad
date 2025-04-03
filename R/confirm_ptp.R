#' @title Calculate Number Of Risk Factors (CONFIRM 2015)
#' @description A function used to calculate the number of
#' risk factors the patient has. This is used to calculate the pretest
#' probability of coronary artery disease (CAD) based on the
#' 2015 CONFIRM Risk Score.
#' @param have_typical_chest_pain Input characters (no, yes) to indicate if the patient
#' has typical chest pain.
#' \itemize{
#'   \item no stands for not having typical chest pain.
#'   \item yes stands for having typical chest pain.
#' }
#' @param have_diabetes Input characters (no, yes) to indicate if the patient
#' only has diabetes.
#' \itemize{
#'   \item no stands for not having diabetes.
#'   \item yes stands for having diabetes.
#' }
#' @param have_hypertension Input characters (no, yes) to indicate if the patient
#' only has hypertension.
#' \itemize{
#'   \item no stands for not having hypertension.
#'   \item yes stands for having a hypertension.
#' }
#' @param have_family_history Input characters (no, yes) to indicate if the patient
#' only has a family history of CAD.
#' \itemize{
#'   \item no stands for not having a family history of CAD.
#'   \item yes stands for having a family history of CAD.
#' }
#' @param is_current_smoker Input characters (no, yes) to indicate if the patient
#' is a current smoker.
#' \itemize{
#'   \item no stands for patient is a current smoker.
#'   \item yes stands for patient is a not current smoker (past or non-smoker).
#' }
#' @param max_na Input integer 0 to 5 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
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
    max_na = 0
)
{
  have_typical_chest_pain <- have_typical_chest_pain |>
    arg_match0_allow_na(values = c("no","yes"))

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  have_hypertension <- have_hypertension |>
    arg_match0_allow_na(values = c("no","yes"))

  have_family_history <- have_family_history |>
    arg_match0_allow_na(values = c("no","yes"))

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
#' @inheritParams calculate_confirm_2015_num_of_rf
#' @param age Input numeric value to indicate the age of the patient.
#' @param sex Input characters (female, male) to indicate the sex of the patient.
#' \itemize{
#'   \item female
#'   \item male
#' }
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
    output = c("text", "percentage")
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
    max_na = max_na_num_of_rf
  )

  age_group <- dplyr::case_when(
    dplyr::between(age, 18, 39) ~ 3,
    dplyr::between(age, 40, 49) ~ 4,
    dplyr::between(age, 50, 59) ~ 5,
    dplyr::between(age, 60, 69) ~ 6,
    age >= 70 ~ 7,
    .default = NA
  )

  sex_group <- dplyr::case_when(
    sex == "female" ~ 0,
    sex == "male" ~ 1,
    .default = NA
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
