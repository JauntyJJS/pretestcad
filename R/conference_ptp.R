#' @title Calculate ESC 2019 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' European Society of Cardiology (ESC) 2019 guidelines.
#' @param age Input integer to indicate the age of the patient.
#' @param sex Input integer 0 or 1 to indicate the sex of the patient.
#' \itemize{
#'   \item 0 stands for Female
#'   \item 1 stands for Male
#' }
#' @param dyspnea_only Input integer 0 or 1 to indicate if the patient
#' only has dyspnea symptoms.
#' \itemize{
#'   \item 0 stands for not having dyspnea-only symptoms. If the patient
#'   has chest pain symptoms, this value should also be set as 0 regardless
#'   if the patient has dyspnea or not.
#'   \item 1 stands for having dyspnea as the only primary symptom.
#' }
#' @param chest_pain Input integer 0 to 3 to indicate the chest pain
#' characteristics of the patient.
#' \itemize{
#'   \item 0 stands for the patient having no chest pain.
#'   \item 1 stands for the patient having typical chest pain.
#'   \item 2 stands for the patient having atypical chest pain.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#'
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("grouping", "numeric", "percentage")
#' \itemize{
#'   \item grouping means the PTP will be expressed as Low, Intermediate and High.
#'   \itemize{
#'      \item Low if PTP is less than 5%.
#'      \item Intermediate if PTP is in between 5% to 15%.
#'      \item High if PTP is more than 15%.
#'   }
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#'
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on the ESC 2019 guidelines.
#' See parameter option `output` for more information.
#' @details The predictive model used to create the guidelines are based on
#' patients from European countries with low cardiovascular disease (CVD) risk.
#'
#' The ESC 2019 PTP for CAD table is as follows:
#'
#' \if{html}{\figure{esc_2019.jpg}{alt="ESC 2019 pre-test probabilities of obstructive coronary artery disease table."}}
#' \emph{ESC 2019 pre-test probabilities of obstructive coronary artery disease (CAD) table taken from}
#' \href{https://doi.org/10.1007/s00059-020-04935-x}{\emph{Juhani Knuuti et. al.}} from
#' \href{https://link.springer.com/}{\emph{SpringerLink}} \emph{is licensed under}
#' \href{http://creativecommons.org/licenses/by/4.0}{\emph{CC BY 4.0}}.
#'
#' @examples
#' # 35 year old female with typical chest pain
#' calculate_esc_2019_ptp(
#'     age = 35,
#'     sex = 0,
#'     dyspnea_only = 0,
#'     chest_pain = 1,
#'     output = "percentage"
#' )
#'
#' # 75 year old male with only dyspnea
#' calculate_esc_2019_ptp(
#'     age = 75,
#'     sex = 0,
#'     dyspnea_only = 1,
#'     chest_pain = 0,
#'     output = "percentage"
#' )
#' @rdname calculate_esc_2019_ptp
#' @export
calculate_esc_2019_ptp <- function(
  age,
  sex,
  dyspnea_only,
  chest_pain,
  output = c("grouping", "numeric", "percentage")
  )
{
  # TODO: Work on case when age < 30

  age_group <- dplyr::case_when(
    dplyr::between(age, 30, 39) ~ "30-39",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    age >= 70 ~ "70+"
  )

  sex_group <- dplyr::case_when(
    sex == 0 ~ "Female",
    sex == 1 ~ "Male"
  )

  dyspnea_only_group <- dplyr::case_when(
    dyspnea_only == 0 | chest_pain != 0 ~ "No",
    dyspnea_only == 1 & chest_pain == 0 ~ "Yes"
  )

  chest_pain_group <- dplyr::case_when(
    chest_pain == 0 ~ "No Chest Pain",
    chest_pain == 1 ~ "Typical",
    chest_pain == 2 ~ "Atypical",
    chest_pain == 3 ~ "Non-anginal"
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & chest_pain_group == "Typical" & sex_group == "Male" ~ "3",
    age_group == "30-39" & chest_pain_group == "Typical" & sex_group == "Female" ~ "5",
    age_group == "40-49" & chest_pain_group == "Typical" & sex_group == "Male" ~ "22",
    age_group == "40-49" & chest_pain_group == "Typical" & sex_group == "Female" ~ "10",
    age_group == "50-59" & chest_pain_group == "Typical" & sex_group == "Male" ~ "32",
    age_group == "50-59" & chest_pain_group == "Typical" & sex_group == "Female" ~ "13",
    age_group == "60-69" & chest_pain_group == "Typical" & sex_group == "Male" ~ "44",
    age_group == "60-69" & chest_pain_group == "Typical" & sex_group == "Female" ~ "16",
    age_group == "70+" & chest_pain_group == "Typical" & sex_group == "Male" ~ "52",
    age_group == "70+" & chest_pain_group == "Typical" & sex_group == "Female" ~ "27",
    age_group == "30-39" & chest_pain_group == "Atypical" & sex_group == "Male" ~ "4",
    age_group == "30-39" & chest_pain_group == "Atypical" & sex_group == "Female" ~ "3",
    age_group == "40-49" & chest_pain_group == "Atypical" & sex_group == "Male" ~ "10",
    age_group == "40-49" & chest_pain_group == "Atypical" & sex_group == "Female" ~ "6",
    age_group == "50-59" & chest_pain_group == "Atypical" & sex_group == "Male" ~ "17",
    age_group == "50-59" & chest_pain_group == "Atypical" & sex_group == "Female" ~ "6",
    age_group == "60-69" & chest_pain_group == "Atypical" & sex_group == "Male" ~ "26",
    age_group == "60-69" & chest_pain_group == "Atypical" & sex_group == "Female" ~ "11",
    age_group == "70+" & chest_pain_group == "Atypical" & sex_group == "Male" ~ "34",
    age_group == "70+" & chest_pain_group == "Atypical" & sex_group == "Female" ~ "19",
    age_group == "30-39" & chest_pain_group == "Non-anginal" & sex_group == "Male" ~ "1",
    age_group == "30-39" & chest_pain_group == "Non-anginal" & sex_group == "Female" ~ "1",
    age_group == "40-49" & chest_pain_group == "Non-anginal" & sex_group == "Male" ~ "3",
    age_group == "40-49" & chest_pain_group == "Non-anginal" & sex_group == "Female" ~ "2",
    age_group == "50-59" & chest_pain_group == "Non-anginal" & sex_group == "Male" ~ "11",
    age_group == "50-59" & chest_pain_group == "Non-anginal" & sex_group == "Female" ~ "3",
    age_group == "60-69" & chest_pain_group == "Non-anginal" & sex_group == "Male" ~ "22",
    age_group == "60-69" & chest_pain_group == "Non-anginal" & sex_group == "Female" ~ "6",
    age_group == "70+" & chest_pain_group == "Non-anginal" & sex_group == "Male" ~ "24",
    age_group == "70+" & chest_pain_group == "Non-anginal" & sex_group == "Female" ~ "10",
    age_group == "30-39" & dyspnea_only_group == "Yes" & sex_group == "Male" ~ "0",
    age_group == "30-39" & dyspnea_only_group == "Yes" & sex_group == "Female" ~ "3",
    age_group == "40-49" & dyspnea_only_group == "Yes" & sex_group == "Male" ~ "12",
    age_group == "40-49" & dyspnea_only_group == "Yes" & sex_group == "Female" ~ "3",
    age_group == "50-59" & dyspnea_only_group == "Yes" & sex_group == "Male" ~ "20",
    age_group == "50-59" & dyspnea_only_group == "Yes" & sex_group == "Female" ~ "9",
    age_group == "60-69" & dyspnea_only_group == "Yes" & sex_group == "Male" ~ "27",
    age_group == "60-69" & dyspnea_only_group == "Yes" & sex_group == "Female" ~ "14",
    age_group == "70+" & dyspnea_only_group == "Yes" & sex_group == "Male" ~ "32",
    age_group == "70+" & dyspnea_only_group == "Yes" & sex_group == "Female" ~ "12",
    .default = "0"
  )

  if (isTRUE(output %in% c("numeric", "grouping"))) {
    ptp_percentage_group <- ptp_percentage_group |>
      as.integer() |>
      # Symmetric rounding to the nearest integer
      round_to_nearest_digit()

    if (isTRUE(output == "numeric")) {
      return(ptp_percentage_group)
    }

    ptp_percentage_group <- dplyr::case_when(
      ptp_percentage_group <= 5 ~ "Low",
      dplyr::between(ptp_percentage_group, 6, 15) ~ "Intermediate",
      ptp_percentage_group > 15 ~ "High"

    )

    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- paste0(ptp_percentage_group, "%")
    return(ptp_percentage_group)
  }
}
