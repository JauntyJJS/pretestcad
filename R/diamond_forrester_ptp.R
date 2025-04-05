#' @title Calculate Diamond-Forrester 1979 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on Diamond-Forrester 1979 model.
#' @param age Input integer value to indicate the age of the patient.
#' @param sex Input characters (female, male) to indicate the sex of the patient.
#' \itemize{
#'   \item female
#'   \item male
#' }
#' @param chest_pain_type Input characters (typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#'
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("numeric", "percentage")
#' \itemize{
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#'
#' @return A numeric or percentage representing the patient's PTP for obstructive CAD
#' based on Diamond-Forrester 1979 model.
#'
#' @examples
#' # 35 year old female with typical chest pain
#' calculate_diamond_forrester_1979_ptp(
#'     age = 35,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     output = "percentage"
#' )
#'
#' # 65 year old male with nonanginal chest pain
#' calculate_diamond_forrester_1979_ptp(
#'     age = 65,
#'     sex = "male",
#'     chest_pain_type = "nonanginal",
#'     output = "percentage"
#' )
#' @rdname calculate_diamond_forrester_1979_ptp
#' @export
calculate_diamond_forrester_1979_ptp <- function(
    age,
    sex,
    chest_pain_type,
    output = c("numeric", "percentage")
)
{
  check_if_positive(x = age, allow_na = TRUE)
  check_if_integer(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  chest_pain_type <- chest_pain_type |>
    arg_match0_allow_na(values = c("typical", "atypical", "nonanginal"))

  output <- output |>
    rlang::arg_match()

  # TODO: Work on case when age < 30

  age_group <- dplyr::case_when(
    dplyr::between(age, 30, 39) ~ "30-39",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    .default = NA_character_
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "male"   ~ 69.7,
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "female" ~ 25.8,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "male"   ~ 87.3,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "female" ~ 55.2,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "male"   ~ 92.0,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "female" ~ 79.4,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "male"   ~ 94.3,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "female" ~ 90.6,

    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "male"   ~ 21.8,
    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "female" ~ 4.2,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "male"   ~ 46.1,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "female" ~ 13.3,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "male"   ~ 58.9,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "female" ~ 32.4,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "male"   ~ 67.1,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "female" ~ 54.4,

    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "male"   ~ 5.2,
    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "female" ~ 0.8,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "male"   ~ 14.1,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "female" ~ 2.8,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "male"   ~ 21.5,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "female" ~ 8.4,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "male"   ~ 28.1,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "female" ~ 18.6,

    .default = NA_real_
  )

  if (isTRUE(output == "numeric")) {
    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- stringr::str_c(ptp_percentage_group, "%")
    return(ptp_percentage_group)
  }


}
