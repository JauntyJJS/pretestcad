#' @title Calculate ESC 2019 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' European Society of Cardiology (ESC) 2019 guidelines.
#' @param age Input integer value to indicate the age of the patient.
#' @param sex Input characters (female, male) to indicate the sex of the patient.
#' \itemize{
#'   \item female
#'   \item male
#' }
#' @param have_dyspnoea Input characters (no, yes) to indicate if the patient
#' only has dyspnoea symptoms.
#' \itemize{
#'   \item no stands for not having dyspnoea symptoms.
#'   \item yes stands for having dyspnoea symptoms.
#' }
#' @param chest_pain_type Input characters (no chest pain, typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item no chest pain stands for the patient having no chest pain.
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#'
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("grouping", "numeric", "percentage")
#' \itemize{
#'   \item grouping means the PTP will be expressed as Low, Intermediate and High.
#'   \itemize{
#'      \item low if PTP is less than 5\%.
#'      \item intermediate if PTP is in between 5\% to 15\%.
#'      \item high if PTP is more than 15\%.
#'   }
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#'
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on the ESC 2019 guidelines.
#' See parameter option \code{output} for more information.
#' @details The predictive model used to create the guidelines are based on
#' patients from European countries with low cardiovascular disease (CVD) risk.
#'
#' If the patient has both dyspnoea and a particular
#' chest pain type (typical, atypical, nonanginal),
#' The chest pain type will take precedence over dyspnoea.
#'
#' @examples
#' # 35 year old female with typical chest pain
#' calculate_esc_2019_ptp(
#'     age = 35,
#'     sex = "female",
#'     have_dyspnoea = "no",
#'     chest_pain_type = "typical",
#'     output = "percentage"
#' )
#'
#' # 75 year old male with only dyspnoea
#' calculate_esc_2019_ptp(
#'     age = 75,
#'     sex = "male",
#'     have_dyspnoea = "yes",
#'     chest_pain_type = "no chest pain",
#'     output = "percentage"
#' )
#' @rdname calculate_esc_2019_ptp
#' @export
calculate_esc_2019_ptp <- function(
  age,
  sex,
  have_dyspnoea,
  chest_pain_type,
  output = c("grouping", "numeric", "percentage")
  )
{
  check_if_positive(x = age, allow_na = TRUE)
  check_if_integer(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  have_dyspnoea <- have_dyspnoea |>
    arg_match0_allow_na(values = c("no","yes"))

  chest_pain_type <- chest_pain_type |>
    arg_match0_allow_na(values = c("no chest pain","typical", "atypical", "nonanginal"))

  output <- output |>
    rlang::arg_match()

  # TODO: Work on case when age < 30

  age_group <- dplyr::case_when(
    dplyr::between(age, 30, 39) ~ "30-39",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    age >= 70 ~ "70+"
  )

  have_only_dyspnea <- dplyr::case_when(
    have_dyspnoea == "no" | chest_pain_type != "no chest pain" ~ "no",
    have_dyspnoea == "yes" & chest_pain_type == "no chest pain" ~ "yes"
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "male"   ~ 3L ,
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "female" ~ 5L ,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "male"   ~ 22L,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "female" ~ 10L,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "male"   ~ 32L,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "female" ~ 13L,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "male"   ~ 44L,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "female" ~ 16L,
    age_group == "70+"   & chest_pain_type == "typical"    & sex == "male"   ~ 52L,
    age_group == "70+"   & chest_pain_type == "typical"    & sex == "female" ~ 27L,
    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "male"   ~ 4L ,
    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "female" ~ 3L ,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "male"   ~ 10L,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "female" ~ 6L ,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "male"   ~ 17L,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "female" ~ 6L ,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "male"   ~ 26L,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "female" ~ 11L,
    age_group == "70+"   & chest_pain_type == "atypical"   & sex == "male"   ~ 34L,
    age_group == "70+"   & chest_pain_type == "atypical"   & sex == "female" ~ 19L,
    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "male"   ~ 1L ,
    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "female" ~ 1L ,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "male"   ~ 3L ,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "female" ~ 2L ,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "male"   ~ 11L,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "female" ~ 3L ,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "male"   ~ 22L,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "female" ~ 6L ,
    age_group == "70+"   & chest_pain_type == "nonanginal" & sex == "male"   ~ 24L,
    age_group == "70+"   & chest_pain_type == "nonanginal" & sex == "female" ~ 10L,
    age_group == "30-39" & have_only_dyspnea == "yes"      & sex == "male"   ~ 0L ,
    age_group == "30-39" & have_only_dyspnea == "yes"      & sex == "female" ~ 3L ,
    age_group == "40-49" & have_only_dyspnea == "yes"      & sex == "male"   ~ 12L,
    age_group == "40-49" & have_only_dyspnea == "yes"      & sex == "female" ~ 3L ,
    age_group == "50-59" & have_only_dyspnea == "yes"      & sex == "male"   ~ 20L,
    age_group == "50-59" & have_only_dyspnea == "yes"      & sex == "female" ~ 9L ,
    age_group == "60-69" & have_only_dyspnea == "yes"      & sex == "male"   ~ 27L,
    age_group == "60-69" & have_only_dyspnea == "yes"      & sex == "female" ~ 14L,
    age_group == "70+"   & have_only_dyspnea == "yes"      & sex == "male"   ~ 32L,
    age_group == "70+"   & have_only_dyspnea == "yes"      & sex == "female" ~ 12L,
    .default = NA_integer_
  )

  if (isTRUE(output == "numeric")) {
    return(ptp_percentage_group)
  }

  if (isTRUE(output == "grouping")) {
    ptp_percentage_group <- dplyr::case_when(
      ptp_percentage_group <= 5 ~ "low",
      dplyr::between(ptp_percentage_group, 6, 15) ~ "intermediate",
      ptp_percentage_group > 15 ~ "high"

    )
    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- stringr::str_c(ptp_percentage_group, "%")
    return(ptp_percentage_group)
  }
}
