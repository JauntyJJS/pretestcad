#' @title Calculate Symptom Score (ESC 2024)
#' @description A function used to calculate the symptom score of the patient.
#' This is used to calculate the pretest
#' probability of coronary artery disease (CAD) based on the
#' ESC 2024 guidelines.
#' @inheritParams calculate_esc_2019_ptp
#' @param allow_na A logical evaluating to \code{TRUE} or \code{FALSE} indicating whether we can
#' allow `chest_pain_type` or `have_dyspnoea` to be \code{NA} when calculating the score.
#' Default: \code{TRUE}
#' @return An integer indicating the symptom score of the patient.
#' It can also be \code{NA} if both \code{chest_pain_type} and \code{have_dyspnoea} are \code{NA}.
#' Patients with both nonanginal chest pain and dyspnoea will be given a score of 2
#' @examples
#' calculate_esc_2024_symptom_score(
#'   chest_pain_type = "nonanginal",
#'   have_dyspnoea = "yes",
#'   allow_na = TRUE
#' )
#'
#' calculate_esc_2024_symptom_score(
#'   chest_pain_type = "nonanginal",
#'   have_dyspnoea = NA,
#'   allow_na = FALSE
#' )
#'
#' calculate_esc_2024_symptom_score(
#'   chest_pain_type = "nonanginal",
#'   have_dyspnoea = NA,
#'   allow_na = TRUE
#' )
#'
#' @rdname calculate_esc_2024_symptom_score
#' @export
calculate_esc_2024_symptom_score <- function(
    chest_pain_type,
    have_dyspnoea,
    allow_na = TRUE
)
{
  chest_pain_type <- chest_pain_type |>
    arg_match0_allow_na(values = c("no chest pain","typical", "atypical", "nonanginal"))

  have_dyspnoea <- have_dyspnoea |>
    arg_match0_allow_na(values = c("no","yes"))

  allow_na <- allow_na |>
    arg_match0_true_or_false()

  symptom_score <- dplyr::case_when(
    chest_pain_type == "no chest pain" & have_dyspnoea == "no"  ~ 0,
    chest_pain_type == "no chest pain" & have_dyspnoea == "yes" ~ 2,
    chest_pain_type == "nonanginal"    & have_dyspnoea == "no"  ~ 1,
    chest_pain_type == "nonanginal"    & have_dyspnoea == "yes" ~ 2,
    chest_pain_type == "atypical"      & have_dyspnoea == "no"  ~ 2,
    chest_pain_type == "atypical"      & have_dyspnoea == "yes" ~ 2,
    chest_pain_type == "typical"       & have_dyspnoea == "no"  ~ 3,
    chest_pain_type == "typical"       & have_dyspnoea == "yes" ~ 3,
    isTRUE(allow_na) & chest_pain_type == "no chest pain" & is.na(have_dyspnoea) ~ 0,
    isTRUE(allow_na) & chest_pain_type == "nonanginal"    & is.na(have_dyspnoea) ~ 1,
    isTRUE(allow_na) & chest_pain_type == "atypical"      & is.na(have_dyspnoea) ~ 2,
    isTRUE(allow_na) & chest_pain_type == "typical"       & is.na(have_dyspnoea) ~ 3,
    isTRUE(allow_na) & is.na(chest_pain_type) & have_dyspnoea == "no"  ~ 0,
    isTRUE(allow_na) & is.na(chest_pain_type) & have_dyspnoea == "yes" ~ 2,
    .default = NA
  )

  return(as.integer(symptom_score))

}


#' @title Calculate Number Of Risk Factors (ESC 2024)
#' @description A function used to calculate the number of
#' risk factors the patient has. This is used to calculate the pretest
#' probability of coronary artery disease (CAD) based on the
#' ESC 2024 guidelines.
#' @param have_family_history Input characters (no, yes) to indicate if the patient
#' has a family history of CAD.
#' \itemize{
#'   \item no stands for not having a family history of CAD.
#'   \item yes stands for having a family history of CAD.
#' }
#' @param have_smoking_history Input characters (no, yes) to indicate if the patient
#' has a smoking history (current or past smoker).
#' \itemize{
#'   \item no stands for not having a smoking history (non-smoker).
#'   \item yes stands for having a smoking history (current or past smoker).
#' }
#' @param have_dyslipidemia Input characters (no, yes) to indicate if the patient
#' has dyslipidemia.
#' \itemize{
#'   \item no stands for not having dyslipidemia.
#'   \item yes stands for having dyslipidemia.
#' }
#' @param have_hypertension Input characters (no, yes) to indicate if the patient
#' has hypertension.
#' \itemize{
#'   \item no stands for not having hypertension.
#'   \item yes stands for having hypertension.
#' }
#' @param have_diabetes Input characters (no, yes) to indicate if the patient
#' has diabetes.
#' \itemize{
#'   \item no stands for not having diabetes.
#'   \item yes stands for having diabetes.
#' }
#' @param max_na Input integer 0 to 5 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
#' @return An integer indicating the number of risk factors the patient has.
#' It can also be \code{NA} if the number of missing risk factors exceeds the \code{max_na}
#' input value.
#' @examples
#' calculate_esc_2024_num_of_rf(
#'   have_family_history = "yes",
#'   have_smoking_history = "yes",
#'   have_dyslipidemia = "yes",
#'   have_hypertension = "yes",
#'   have_diabetes = "no"
#' )
#'
#' calculate_esc_2024_num_of_rf(
#'   have_family_history = "no",
#'   have_smoking_history = "no",
#'   have_dyslipidemia = "no",
#'   have_hypertension = NA,
#'   have_diabetes = "no",
#'   max_na = 0
#' )
#'
#' calculate_esc_2024_num_of_rf(
#'   have_family_history = "no",
#'   have_smoking_history = "no",
#'   have_dyslipidemia = "no",
#'   have_hypertension = NA,
#'   have_diabetes = "no",
#'   max_na = 1
#' )
#' @rdname calculate_esc_2024_num_of_rf
#' @export
calculate_esc_2024_num_of_rf <- function(
    have_family_history,
    have_smoking_history,
    have_dyslipidemia,
    have_hypertension,
    have_diabetes,
    max_na = 0
  )
{
  have_family_history <- have_family_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_smoking_history <- have_smoking_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_hypertension <- have_hypertension |>
    arg_match0_allow_na(values = c("no","yes"))

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  max_na <- max_na |>
    arg_match0_integer(values = c(0:5))

  number_of_na <- 0
  num_of_rf <- 0

  number_of_na <- dplyr::case_when(
    is.na(have_family_history) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_smoking_history) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_dyslipidemia) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_hypertension) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_diabetes) ~ number_of_na + 1,
    .default = number_of_na
  )

  if (number_of_na > max_na) {return(NA)}

  num_of_rf <- dplyr::case_when(
    have_family_history == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_smoking_history == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_dyslipidemia == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_hypertension == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  ) + dplyr::case_when(
    have_diabetes == "yes" ~ num_of_rf + 1,
    .default = num_of_rf
  )

  return(num_of_rf)

}


#' @title Calculate ESC 2024 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' European Society of Cardiology (ESC) 2024 guidelines.
#' @inheritParams calculate_esc_2019_ptp
#' @param symptom_score An integer indicating the symptom score of the patient.
#' This value can be calculated via the \code{\link{calculate_esc_2024_symptom_score}}
#' @param num_of_rf An integer indicating the number of risk factors the patient has.
#' This value can be calculated via the \code{\link{calculate_esc_2024_num_of_rf}}
#' Risk factors are:
#' \itemize{
#'   \item having a family history of CAD.
#'   \item having a smoking history (current and past smoker).
#'   \item having dyslipidemia.
#'   \item having hypertension.
#'   \item having diabetes.
#' }
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("grouping", "numeric", "percentage")
#' \itemize{
#'   \item grouping means the PTP will be expressed as Low, Intermediate and High.
#'   \itemize{
#'      \item very low if PTP is less than or equal to 5\%.
#'      \item low if PTP is in between 6\% to 15\%.
#'      \item moderate if PTP is more than 15\%.
#'   }
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on the ESC 2024 guidelines.
#' See parameter option \code{output} for more information.
#' @examples
#' # 30 female with symptom score of 0 and 0 risk factors
#' calculate_esc_2024_fig_4_ptp_simplfied(
#'   age = 30,
#'   sex = "female",
#'   symptom_score = 0,
#'   num_of_rf = 0,
#'   output = "percentage"
#' )
#'
#' @rdname calculate_esc_2024_fig_4_ptp_simplfied
#' @export
calculate_esc_2024_fig_4_ptp_simplfied <- function(
    age,
    sex,
    symptom_score,
    num_of_rf,
    output = c("grouping", "numeric", "percentage")
    )
{
  check_if_positive(x = age, allow_na = TRUE)
  check_if_integer(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  symptom_score <- symptom_score |>
    arg_match0_integer(values = c(0:3),
                       allow_na = TRUE)

  num_of_rf <- num_of_rf |>
    arg_match0_integer(values = c(0:5),
                       allow_na = TRUE)

  output <- output |>
    rlang::arg_match()

  # TODO: Work on case when age < 30 and age > 80

  age_group <- dplyr::case_when(
    dplyr::between(age, 30, 39) ~ "30-39",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    dplyr::between(age, 70, 80) ~ "70-80",
    .default = NA
  )

  symptom_score_group <- dplyr::case_when(
    symptom_score == 0  ~ "0-1",
    symptom_score == 1  ~ "0-1",
    symptom_score == 2  ~ "2",
    symptom_score == 3  ~ "3",
    .default = NA
  )

  rf_group <- dplyr::case_when(
    dplyr::between(num_of_rf, 0, 1) ~ "0-1",
    dplyr::between(num_of_rf, 2, 3) ~ "2-3",
    dplyr::between(num_of_rf, 4, 5) ~ "4-5",
    .default = NA
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & symptom_score_group == "0-1" & sex == "female" & rf_group == "0-1" ~ 0  ,
    age_group == "40-49" & symptom_score_group == "0-1" & sex == "female" & rf_group == "0-1" ~ 1  ,
    age_group == "50-59" & symptom_score_group == "0-1" & sex == "female" & rf_group == "0-1" ~ 1  ,
    age_group == "60-69" & symptom_score_group == "0-1" & sex == "female" & rf_group == "0-1" ~ 2  ,
    age_group == "70-80" & symptom_score_group == "0-1" & sex == "female" & rf_group == "0-1" ~ 4  ,

    age_group == "30-39" & symptom_score_group == "0-1" & sex == "female" & rf_group == "2-3" ~ 1  ,
    age_group == "40-49" & symptom_score_group == "0-1" & sex == "female" & rf_group == "2-3" ~ 1  ,
    age_group == "50-59" & symptom_score_group == "0-1" & sex == "female" & rf_group == "2-3" ~ 2  ,
    age_group == "60-69" & symptom_score_group == "0-1" & sex == "female" & rf_group == "2-3" ~ 4  ,
    age_group == "70-80" & symptom_score_group == "0-1" & sex == "female" & rf_group == "2-3" ~ 7  ,

    age_group == "30-39" & symptom_score_group == "0-1" & sex == "female" & rf_group == "4-5" ~ 2  ,
    age_group == "40-49" & symptom_score_group == "0-1" & sex == "female" & rf_group == "4-5" ~ 3  ,
    age_group == "50-59" & symptom_score_group == "0-1" & sex == "female" & rf_group == "4-5" ~ 5  ,
    age_group == "60-69" & symptom_score_group == "0-1" & sex == "female" & rf_group == "4-5" ~ 7  ,
    age_group == "70-80" & symptom_score_group == "0-1" & sex == "female" & rf_group == "4-5" ~ 11 ,

    age_group == "30-39" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "0-1" ~ 1  ,
    age_group == "40-49" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "0-1" ~ 2  ,
    age_group == "50-59" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "0-1" ~ 4  ,
    age_group == "60-69" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "0-1" ~ 8  ,
    age_group == "70-80" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "0-1" ~ 15 ,

    age_group == "30-39" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "2-3" ~ 2  ,
    age_group == "40-49" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "2-3" ~ 4  ,
    age_group == "50-59" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "2-3" ~ 7  ,
    age_group == "60-69" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "2-3" ~ 12 ,
    age_group == "70-80" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "2-3" ~ 19 ,

    age_group == "30-39" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "4-5" ~ 5  ,
    age_group == "40-49" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "4-5" ~ 8  ,
    age_group == "50-59" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "4-5" ~ 12 ,
    age_group == "60-69" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "4-5" ~ 17 ,
    age_group == "70-80" & symptom_score_group == "0-1" & sex == "male"   & rf_group == "4-5" ~ 24 ,

    age_group == "30-39" & symptom_score_group == "2"   & sex == "female" & rf_group == "0-1" ~ 0  ,
    age_group == "40-49" & symptom_score_group == "2"   & sex == "female" & rf_group == "0-1" ~ 1  ,
    age_group == "50-59" & symptom_score_group == "2"   & sex == "female" & rf_group == "0-1" ~ 2  ,
    age_group == "60-69" & symptom_score_group == "2"   & sex == "female" & rf_group == "0-1" ~ 3  ,
    age_group == "70-80" & symptom_score_group == "2"   & sex == "female" & rf_group == "0-1" ~ 6  ,

    age_group == "30-39" & symptom_score_group == "2"   & sex == "female" & rf_group == "2-3" ~ 1  ,
    age_group == "40-49" & symptom_score_group == "2"   & sex == "female" & rf_group == "2-3" ~ 2  ,
    age_group == "50-59" & symptom_score_group == "2"   & sex == "female" & rf_group == "2-3" ~ 3  ,
    age_group == "60-69" & symptom_score_group == "2"   & sex == "female" & rf_group == "2-3" ~ 6  ,
    age_group == "70-80" & symptom_score_group == "2"   & sex == "female" & rf_group == "2-3" ~ 10 ,

    age_group == "30-39" & symptom_score_group == "2"   & sex == "female" & rf_group == "4-5" ~ 3  ,
    age_group == "40-49" & symptom_score_group == "2"   & sex == "female" & rf_group == "4-5" ~ 5  ,
    age_group == "50-59" & symptom_score_group == "2"   & sex == "female" & rf_group == "4-5" ~ 7  ,
    age_group == "60-69" & symptom_score_group == "2"   & sex == "female" & rf_group == "4-5" ~ 11 ,
    age_group == "70-80" & symptom_score_group == "2"   & sex == "female" & rf_group == "4-5" ~ 16 ,

    age_group == "30-39" & symptom_score_group == "2"   & sex == "male"   & rf_group == "0-1" ~ 2  ,
    age_group == "40-49" & symptom_score_group == "2"   & sex == "male"   & rf_group == "0-1" ~ 3  ,
    age_group == "50-59" & symptom_score_group == "2"   & sex == "male"   & rf_group == "0-1" ~ 6  ,
    age_group == "60-69" & symptom_score_group == "2"   & sex == "male"   & rf_group == "0-1" ~ 12 ,
    age_group == "70-80" & symptom_score_group == "2"   & sex == "male"   & rf_group == "0-1" ~ 22 ,

    age_group == "30-39" & symptom_score_group == "2"   & sex == "male"   & rf_group == "2-3" ~ 4  ,
    age_group == "40-49" & symptom_score_group == "2"   & sex == "male"   & rf_group == "2-3" ~ 6  ,
    age_group == "50-59" & symptom_score_group == "2"   & sex == "male"   & rf_group == "2-3" ~ 11 ,
    age_group == "60-69" & symptom_score_group == "2"   & sex == "male"   & rf_group == "2-3" ~ 17 ,
    age_group == "70-80" & symptom_score_group == "2"   & sex == "male"   & rf_group == "2-3" ~ 27 ,

    age_group == "30-39" & symptom_score_group == "2"   & sex == "male"   & rf_group == "4-5" ~ 8  ,
    age_group == "40-49" & symptom_score_group == "2"   & sex == "male"   & rf_group == "4-5" ~ 12 ,
    age_group == "50-59" & symptom_score_group == "2"   & sex == "male"   & rf_group == "4-5" ~ 17 ,
    age_group == "60-69" & symptom_score_group == "2"   & sex == "male"   & rf_group == "4-5" ~ 25 ,
    age_group == "70-80" & symptom_score_group == "2"   & sex == "male"   & rf_group == "4-5" ~ 34 ,

    age_group == "30-39" & symptom_score_group == "3"   & sex == "female" & rf_group == "0-1" ~ 2  ,
    age_group == "40-49" & symptom_score_group == "3"   & sex == "female" & rf_group == "0-1" ~ 4  ,
    age_group == "50-59" & symptom_score_group == "3"   & sex == "female" & rf_group == "0-1" ~ 6  ,
    age_group == "60-69" & symptom_score_group == "3"   & sex == "female" & rf_group == "0-1" ~ 10 ,
    age_group == "70-80" & symptom_score_group == "3"   & sex == "female" & rf_group == "0-1" ~ 16 ,

    age_group == "30-39" & symptom_score_group == "3"   & sex == "female" & rf_group == "2-3" ~ 5  ,
    age_group == "40-49" & symptom_score_group == "3"   & sex == "female" & rf_group == "2-3" ~ 7  ,
    age_group == "50-59" & symptom_score_group == "3"   & sex == "female" & rf_group == "2-3" ~ 10 ,
    age_group == "60-69" & symptom_score_group == "3"   & sex == "female" & rf_group == "2-3" ~ 14 ,
    age_group == "70-80" & symptom_score_group == "3"   & sex == "female" & rf_group == "2-3" ~ 19 ,

    age_group == "30-39" & symptom_score_group == "3"   & sex == "female" & rf_group == "4-5" ~ 10 ,
    age_group == "40-49" & symptom_score_group == "3"   & sex == "female" & rf_group == "4-5" ~ 12 ,
    age_group == "50-59" & symptom_score_group == "3"   & sex == "female" & rf_group == "4-5" ~ 15 ,
    age_group == "60-69" & symptom_score_group == "3"   & sex == "female" & rf_group == "4-5" ~ 19 ,
    age_group == "70-80" & symptom_score_group == "3"   & sex == "female" & rf_group == "4-5" ~ 23 ,

    age_group == "30-39" & symptom_score_group == "3"   & sex == "male"   & rf_group == "0-1" ~ 9  ,
    age_group == "40-49" & symptom_score_group == "3"   & sex == "male"   & rf_group == "0-1" ~ 14 ,
    age_group == "50-59" & symptom_score_group == "3"   & sex == "male"   & rf_group == "0-1" ~ 21 ,
    age_group == "60-69" & symptom_score_group == "3"   & sex == "male"   & rf_group == "0-1" ~ 32 ,
    age_group == "70-80" & symptom_score_group == "3"   & sex == "male"   & rf_group == "0-1" ~ 44 ,

    age_group == "30-39" & symptom_score_group == "3"   & sex == "male"   & rf_group == "2-3" ~ 14 ,
    age_group == "40-49" & symptom_score_group == "3"   & sex == "male"   & rf_group == "2-3" ~ 20 ,
    age_group == "50-59" & symptom_score_group == "3"   & sex == "male"   & rf_group == "2-3" ~ 27 ,
    age_group == "60-69" & symptom_score_group == "3"   & sex == "male"   & rf_group == "2-3" ~ 35 ,
    age_group == "70-80" & symptom_score_group == "3"   & sex == "male"   & rf_group == "2-3" ~ 44 ,

    age_group == "30-39" & symptom_score_group == "3"   & sex == "male"   & rf_group == "4-5" ~ 22 ,
    age_group == "40-49" & symptom_score_group == "3"   & sex == "male"   & rf_group == "4-5" ~ 27 ,
    age_group == "50-59" & symptom_score_group == "3"   & sex == "male"   & rf_group == "4-5" ~ 33 ,
    age_group == "60-69" & symptom_score_group == "3"   & sex == "male"   & rf_group == "4-5" ~ 39 ,
    age_group == "70-80" & symptom_score_group == "3"   & sex == "male"   & rf_group == "4-5" ~ 45 ,

    .default = NA
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
      ptp_percentage_group <= 5 ~ "very low",
      dplyr::between(ptp_percentage_group, 6, 15) ~ "low",
      ptp_percentage_group > 15 ~ "moderate"

    )

    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- stringr::str_c(ptp_percentage_group, "%")
    return(ptp_percentage_group)
  }

}

#' @title Calculate ESC 2024 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' European Society of Cardiology (ESC) 2024 guidelines.
#' @inheritParams calculate_esc_2024_symptom_score
#' @inheritParams calculate_esc_2024_num_of_rf
#' @inheritParams calculate_esc_2024_fig_4_ptp_simplfied
#' @param allow_na_symptom_score A logical evaluating to \code{TRUE} or \code{FALSE} indicating whether we can
#' allow \code{chest_pain_type} or \code{have_dyspnoea} to be \code{NA} when calculating the score
#' @param max_na_num_of_rf Input integer 0 to 5 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on the ESC 2024 guidelines.
#' See parameter option \code{output} for more information.
#' @examples
#' # 30 female with symptom score of 0 and 0 risk factors
#' calculate_esc_2024_fig_4_ptp(
#'   age = 30,
#'   sex = "female",
#'   chest_pain_type = "no chest pain",
#'   have_dyspnoea = "no",
#'   have_family_history = "no",
#'   have_smoking_history = "no",
#'   have_dyslipidemia = "no",
#'   have_hypertension = "no",
#'   have_diabetes = "no",
#'   allow_na_symptom_score = TRUE,
#'   max_na_num_of_rf = 0,
#'   output = "percentage"
#' )
#'
#' @rdname calculate_esc_2024_fig_4_ptp
#' @export
calculate_esc_2024_fig_4_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_dyspnoea,
    have_family_history,
    have_smoking_history,
    have_dyslipidemia,
    have_hypertension,
    have_diabetes,
    allow_na_symptom_score = TRUE,
    max_na_num_of_rf = 0,
    output = c("grouping", "numeric", "percentage")
)
{
  symptom_score <- calculate_esc_2024_symptom_score(
    chest_pain_type = chest_pain_type,
    have_dyspnoea = have_dyspnoea,
    allow_na = allow_na_symptom_score
  )

  num_of_rf <- calculate_esc_2024_num_of_rf(
    have_family_history = have_family_history,
    have_smoking_history = have_smoking_history,
    have_dyslipidemia = have_dyslipidemia,
    have_hypertension = have_hypertension,
    have_diabetes = have_diabetes,
    max_na = max_na_num_of_rf
  )

  ptp_results <- calculate_esc_2024_fig_4_ptp_simplfied(
    age = age,
    sex = sex,
    symptom_score = symptom_score,
    num_of_rf = num_of_rf,
    output = output
  )

  return(ptp_results)


}
