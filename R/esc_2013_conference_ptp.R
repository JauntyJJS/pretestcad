#' @title Calculate ESC 2013 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' European Society of Cardiology (ESC) 2013 guidelines.
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
#' @return An integer or percentage representing the patient's PTP for obstructive CAD
#' based on the ESC 2013 guidelines.
#'
#' @details The predictive model used to create the guidelines are based on
#' the journal A clinical prediction rule for the diagnosis of coronary artery disease:
#' validation, updating, and extension by 2011 Genders et. al.
#'
#' @examples
#' # 35 year old female with typical chest pain
#' calculate_esc_2013_ptp(
#'     age = 35,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     output = "percentage"
#' )
#'
#' # 65 year old male with nonanginal chest pain
#' calculate_esc_2013_ptp(
#'     age = 65,
#'     sex = "male",
#'     chest_pain_type = "nonanginal",
#'     output = "percentage"
#' )
#' @rdname calculate_esc_2013_ptp
#' @export
calculate_esc_2013_ptp <- function(
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
    dplyr::between(age, 70, 79) ~ "70-79",
    age >= 80 ~ "80+",
    .default = NA_character_
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "male"   ~ 59L,
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "female" ~ 28L,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "male"   ~ 69L,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "female" ~ 37L,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "male"   ~ 77L,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "female" ~ 47L,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "male"   ~ 84L,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "female" ~ 58L,
    age_group == "70-79" & chest_pain_type == "typical"    & sex == "male"   ~ 89L,
    age_group == "70-79" & chest_pain_type == "typical"    & sex == "female" ~ 68L,
    age_group == "80+"   & chest_pain_type == "typical"    & sex == "male"   ~ 93L,
    age_group == "80+"   & chest_pain_type == "typical"    & sex == "female" ~ 76L,

    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "male"   ~ 29L,
    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "female" ~ 10L,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "male"   ~ 38L,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "female" ~ 14L,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "male"   ~ 49L,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "female" ~ 20L,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "male"   ~ 59L,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "female" ~ 28L,
    age_group == "70-79" & chest_pain_type == "atypical"   & sex == "male"   ~ 69L,
    age_group == "70-79" & chest_pain_type == "atypical"   & sex == "female" ~ 37L,
    age_group == "80+"   & chest_pain_type == "atypical"   & sex == "male"   ~ 78L,
    age_group == "80+"   & chest_pain_type == "atypical"   & sex == "female" ~ 47L,

    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "male"   ~ 18L,
    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "female" ~ 5L,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "male"   ~ 25L,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "female" ~ 8L,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "male"   ~ 34L,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "female" ~ 12L,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "male"   ~ 44L,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "female" ~ 17L,
    age_group == "70-79" & chest_pain_type == "nonanginal" & sex == "male"   ~ 54L,
    age_group == "70-79" & chest_pain_type == "nonanginal" & sex == "female" ~ 24L,
    age_group == "80+"   & chest_pain_type == "nonanginal" & sex == "male"   ~ 65L,
    age_group == "80+"   & chest_pain_type == "nonanginal" & sex == "female" ~ 32L,

    .default = NA_integer_
  )

  if (isTRUE(output == "numeric")) {
    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- stringr::str_c(ptp_percentage_group, "%")
    return(ptp_percentage_group)
  }


}
