#' @title Calculate 2021 PRECISE Simple PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2021 Predictive Risk scorE for CAD In Southeast Asians with chEst pain
#' (PRECISE) simple model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param smoking_history_type Input characters (current, past, none) to indicate
#' the smoking history of the patient.
#' \itemize{
#'   \item current stands for being a current smoker.
#'   \item past stands for being a past smoker.
#'   \item none stands for not having a smoking history (non-smoker).
#' }
#' @param chest_pain_type Input characters (typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#' @param have_neck_radiation Input characters (no, yes) to indicate if the patient
#' has chest pain radiating to the neck.
#' \itemize{
#'   \item no stands for not chest pain radiating to the neck.
#'   \item yes stands for having chest pain radiating to the neck.
#' }
#'
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2021 Predictive Risk scorE for CAD In Southeast Asians
#' with chEst pain (PRECISE) simple model.
#' @details The predictive model is based on
#' patients a mixed Asian cohort within Singapore with stable chest pain.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' # radiating to the neck, has diabetes
#' # but no hypertension and a non-smoker
#'
#' calculate_precise_2021_simple_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_neck_radiation = "yes",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     smoking_history_type = "none"
#'
#' )
#' @rdname calculate_precise_2021_simple_ptp
#' @export
calculate_precise_2021_simple_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_neck_radiation,
    have_diabetes,
    have_hypertension,
    smoking_history_type
)
{
  check_if_positive(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  sex <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 1L,
    .default = NA_integer_
  )

  have_neck_radiation <- have_neck_radiation |>
    arg_match0_allow_na(values = c("no","yes"))

  have_neck_radiation <- dplyr::case_when(
    have_neck_radiation == "no" ~ 0L,
    have_neck_radiation == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_hypertension <- have_hypertension |>
    arg_match0_allow_na(values = c("no","yes"))

  have_hypertension <- dplyr::case_when(
    have_hypertension == "no" ~ 0L,
    have_hypertension == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  have_diabetes <- dplyr::case_when(
    have_diabetes == "no" ~ 0L,
    have_diabetes == "yes" ~ 1L,
    .default = NA_integer_
  )

  chest_pain_type <- chest_pain_type |>
    arg_match0_allow_na(values = c("typical", "atypical", "nonanginal"))

  have_atypical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "nonanginal") ~ 0L,
    chest_pain_type == "atypical" ~ 1L,
    .default = NA_integer_
  )

  have_typical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("atypical", "nonanginal") ~ 0L,
    chest_pain_type == "typical" ~ 1L,
    .default = NA
  )

  smoking_history_type <- smoking_history_type |>
    arg_match0_allow_na(values = c("current", "past", "none"))

  is_current_smoker <- dplyr::case_when(
    smoking_history_type %in% c("past", "none") ~ 0L,
    smoking_history_type == "current" ~ 1L,
    .default = NA_integer_
  )

  is_past_smoker <- dplyr::case_when(
    smoking_history_type %in% c("current", "none") ~ 0L,
    smoking_history_type == "past" ~ 1L,
    .default = NA_integer_
  )

  precise_2021_simple_ptp <- 1 /
    (1 + exp(-(-6.632 +
              (0.035  * age) +
              (1.694  * sex) +
              (0.613  * have_diabetes) +
              (0.542  * have_hypertension) +
              (0.791  * is_current_smoker) +
              (0.063  * is_past_smoker) +
              (1.395  * have_typical_chest_pain) +
              (0.877  * have_atypical_chest_pain) +
              (1.143  * have_neck_radiation)
    )
    )
    )

  return(precise_2021_simple_ptp)

}


#' @title Calculate 2021 PRECISE Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2021 Predictive Risk scorE for CAD In Southeast Asians with chEst pain
#' (PRECISE) clinical model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param smoking_history_type Input characters (current, past, none) to indicate
#' the smoking history of the patient.
#' \itemize{
#'   \item current stands for being a current smoker.
#'   \item past stands for being a past smoker.
#'   \item none stands for not having a smoking history (non-smoker).
#' }
#' @param chest_pain_type Input characters (typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#' @param have_neck_radiation Input characters (no, yes) to indicate if the patient
#' has chest pain radiating to the neck.
#' \itemize{
#'   \item no stands for not chest pain radiating to the neck.
#'   \item yes stands for having chest pain radiating to the neck.
#' }
#' @param have_q_waves Input characters (no, yes) to indicate if the patient
#' has Q waves on electrocardiogram (ECG).
#' \itemize{
#'   \item no stands for the patient not having Q waves on ECG.
#'   \item yes stands for the patient having Q waves on ECG.
#' }
#' @param have_st_t_changes Input characters (no, yes) to indicate if the patient
#' has ST-T changes on electrocardiogram (ECG).
#' \itemize{
#'   \item no stands for the patient not having ST-T changes on ECG.
#'   \item yes stands for the patient having ST-T changes on ECG.
#' }
#'
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2021 Predictive Risk scorE for CAD In Southeast Asians
#' with chEst pain (PRECISE) clinical model.
#' @details The predictive model is based on
#' patients a mixed Asian cohort within Singapore with stable chest pain.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' # radiating to the neck, has diabetes
#' # but no hypertension and a non-smoker.
#' # She has Q waves but no ST-T changes on ECG.
#'
#' calculate_precise_2021_clinical_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_neck_radiation = "yes",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     smoking_history_type = "none",
#'     have_q_waves = "yes",
#'     have_st_t_changes = "no"
#' )
#' @rdname calculate_precise_2021_clinical_ptp
#' @export
calculate_precise_2021_clinical_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_neck_radiation,
    have_diabetes,
    have_hypertension,
    smoking_history_type,
    have_q_waves,
    have_st_t_changes
)
{
  check_if_positive(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  sex <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 1L,
    .default = NA_integer_
  )

  have_neck_radiation <- have_neck_radiation |>
    arg_match0_allow_na(values = c("no","yes"))

  have_neck_radiation <- dplyr::case_when(
    have_neck_radiation == "no" ~ 0L,
    have_neck_radiation == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_hypertension <- have_hypertension |>
    arg_match0_allow_na(values = c("no","yes"))

  have_hypertension <- dplyr::case_when(
    have_hypertension == "no" ~ 0L,
    have_hypertension == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  have_diabetes <- dplyr::case_when(
    have_diabetes == "no" ~ 0L,
    have_diabetes == "yes" ~ 1L,
    .default = NA_integer_
  )

  chest_pain_type <- chest_pain_type |>
    arg_match0_allow_na(values = c("typical", "atypical", "nonanginal"))

  have_atypical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "nonanginal") ~ 0L,
    chest_pain_type == "atypical" ~ 1L,
    .default = NA_integer_
  )

  have_typical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("atypical", "nonanginal") ~ 0L,
    chest_pain_type == "typical" ~ 1L,
    .default = NA
  )

  smoking_history_type <- smoking_history_type |>
    arg_match0_allow_na(values = c("current", "past", "none"))

  is_current_smoker <- dplyr::case_when(
    smoking_history_type %in% c("past", "none") ~ 0L,
    smoking_history_type == "current" ~ 1L,
    .default = NA_integer_
  )

  is_past_smoker <- dplyr::case_when(
    smoking_history_type %in% c("current", "none") ~ 0L,
    smoking_history_type == "past" ~ 1L,
    .default = NA_integer_
  )

  have_q_waves <- have_q_waves |>
    arg_match0_allow_na(values = c("no","yes"))

  have_q_waves <- dplyr::case_when(
    have_q_waves == "no" ~ 0L,
    have_q_waves == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_st_t_changes <- have_st_t_changes |>
    arg_match0_allow_na(values = c("no","yes"))

  have_st_t_changes <- dplyr::case_when(
    have_st_t_changes == "no" ~ 0L,
    have_st_t_changes == "yes" ~ 1L,
    .default = NA_integer_
  )

  precise_2021_clinical_ptp <- 1 /
    (1 + exp(-(-6.714 +
              (0.033  * age) +
              (1.75   * sex) +
              (0.597  * have_diabetes) +
              (0.497  * have_hypertension) +
              (0.733  * is_current_smoker) +
              (0.07   * is_past_smoker) +
              (1.374  * have_typical_chest_pain) +
              (0.875  * have_atypical_chest_pain) +
              (1.157  * have_neck_radiation) +
              (1.02   * have_q_waves) +
              (0.552  * have_st_t_changes)
    )
    )
    )

  return(precise_2021_clinical_ptp)

}
