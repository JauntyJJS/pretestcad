#' @title Calculate 1993 Duke Clinical Score for Significant CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of significant (>75\% luminal diameter narrowing
#' of at least one major coronary artery) coronary artery disease based on the
#' 1993 Duke Clinical Score.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param chest_pain_type Input characters (typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#' @param have_mi Input characters (no, yes) to indicate if the patient
#' has a previous history of Myocardial Infarction (MI).
#' \itemize{
#'   \item no stands for the patient not having a previous history of MI.
#'   \item yes stands for the patient a previous history of MI.
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
#' @return A numeric value representing the patient's PTP for significant
#' (>75\% luminal diameter narrowing of at least one major coronary artery) CAD
#' based on the 1993 Duke Clinical Score.
#' @details The predictive model is based on
#' patients referred for cardiac catheterisation between 1969 and 1983.
#'
#' @examples
#' # 40 year old female with typical chest pain,
#' # previous history of MI,
#' # has diabetes but no dyslipidemia and a non-smoker.
#' # She has Q waves but no ST-T changes on ECG.
#'
#' calculate_dcs_1993_sig_cad_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_mi = "yes",
#'     have_smoking_history = "no",
#'     have_dyslipidemia = "no",
#'     have_diabetes = "yes",
#'     have_q_waves = "yes",
#'     have_st_t_changes = "no"
#' )
#' @rdname calculate_dcs_1993_sig_cad_ptp
#' @export
calculate_dcs_1993_sig_cad_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_mi,
    have_smoking_history,
    have_dyslipidemia,
    have_diabetes,
    have_q_waves,
    have_st_t_changes
)
{
  check_if_positive(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  sex <- dplyr::case_when(
    sex == "female" ~ 1L,
    sex == "male" ~ 0L,
    .default = NA_integer_
  )

  have_mi <- have_mi |>
    arg_match0_allow_na(values = c("no","yes"))

  have_mi <- dplyr::case_when(
    have_mi == "no" ~ 0L,
    have_mi == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "no" ~ 0L,
    have_dyslipidemia == "yes" ~ 1L,
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

  have_smoking_history <- have_smoking_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_smoking_history <- dplyr::case_when(
    have_smoking_history == "no" ~ 0L,
    have_smoking_history == "yes" ~ 1L,
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

  dcs_1993_sig_cad_ptp <- 1 /
    (1 + exp(-(-7.376 +
              (0.1126  * age) +
              (-0.328  * sex) +
              (-0.0301 * age * sex) +
              (2.581   * have_typical_chest_pain) +
              (0.976   * have_atypical_chest_pain) +
              (1.093   * have_mi) +
              (1.213   * have_q_waves) +
              (0.741   * have_mi * have_q_waves) +
              (2.596   * have_smoking_history) +
              (1.845   * have_dyslipidemia) +
              (0.694   * have_diabetes) +
              (0.637   * have_st_t_changes) +
              (-0.0404 * age * have_smoking_history) +
              (-0.0251 * age * have_dyslipidemia) +
              (0.550   * sex * have_smoking_history)
    )
    )
    )

  return(dcs_1993_sig_cad_ptp)

}
