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

#' @title Calculate The Risk Factor Index For Duke Clinical Score 1993
#' @description A function used to calculate the patient's
#' risk factor index. This is used to calculate the likelihood
#' of severe coronary artery disease in the
#' Duke Clinical Score 1993 paper.
#' @param have_hypertension Input characters (no, yes) to indicate if the patient
#' has hypertension.
#' \itemize{
#'   \item no stands for not having hypertension.
#'   \item yes stands for having hypertension.
#' }
#' @param have_dyslipidemia Input characters (no, yes) to indicate if the patient
#' has dyslipidemia.
#' \itemize{
#'   \item no stands for not having dyslipidemia.
#'   \item yes stands for having dyslipidemia.
#' }
#' @param have_diabetes Input characters (no, yes) to indicate if the patient
#' has diabetes.
#' \itemize{
#'   \item no stands for not having diabetes.
#'   \item yes stands for having diabetes.
#' }
#' @param max_na Input integer 0 to 3 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
#' @return An integer indicating the patient's risk factor index.
#' It can also be \code{NA} if the number of missing risk factors exceeds the \code{max_na}
#' input value.
#' @examples
#' calculate_dcs_1993_risk_factor_index(
#'   have_hypertension = "yes",
#'   have_dyslipidemia = "yes",
#'   have_diabetes = "no"
#' )
#'
#' calculate_dcs_1993_risk_factor_index(
#'   have_hypertension = NA,
#'   have_dyslipidemia = "yes",
#'   have_diabetes = "no",
#'   max_na = 0
#' )
#'
#' calculate_dcs_1993_risk_factor_index(
#'   have_hypertension = NA,
#'   have_dyslipidemia = "yes",
#'   have_diabetes = "no",
#'   max_na = 1
#' )
#' @rdname calculate_dcs_1993_risk_factor_index
#' @export
calculate_dcs_1993_risk_factor_index <- function(
    have_hypertension,
    have_dyslipidemia,
    have_diabetes,
    max_na = 0
) {

  have_hypertension <- have_hypertension |>
    arg_match0_allow_na(values = c("no","yes"))

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  max_na <- max_na |>
    arg_match0_integer(values = c(0:3))

  number_of_na <- 0
  risk_factor_index <- 0

  number_of_na <- dplyr::case_when(
    is.na(have_hypertension) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_dyslipidemia) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_diabetes) ~ number_of_na + 1,
    .default = number_of_na
  )

  if (number_of_na > max_na) {return(NA)}

  risk_factor_index <- dplyr::case_when(
    have_hypertension == "yes" ~ risk_factor_index + 1,
    .default = risk_factor_index
  ) + dplyr::case_when(
    have_dyslipidemia == "yes" ~ risk_factor_index + 1,
    .default = risk_factor_index
  ) + dplyr::case_when(
    have_diabetes == "yes" ~ risk_factor_index + 1,
    .default = risk_factor_index
  )

  return(risk_factor_index)

}

#' @title Calculate The Pain Index For Duke Clinical Score 1993
#' @description A function used to calculate the patient's
#' pain index. This is used to calculate the likelihood
#' of severe coronary artery disease in the
#' Duke Clinical Score 1993 paper.
#' @param have_typical_chest_pain Input characters (no, yes) to indicate if the patient
#' has typical chest pain.
#' \itemize{
#'   \item no stands for not having typical chest pain.
#'   \item yes stands for having typical chest pain.
#' }
#' @param frequency_of_angina_pain_per_week Input integer to indicate the patient's
#' frequency of angina per week.
#' @param have_progressive_angina Input characters (no, yes) to indicate if the patient
#' has progressive angina.
#' \itemize{
#'   \item no stands for not having progressive angina.
#'   \item yes stands for having progressive angina.
#' }
#' @param have_nocturnal_angina Input characters (no, yes) to indicate if the patient
#' has nocturnal angina.
#' \itemize{
#'   \item no stands for not having nocturnal angina.
#'   \item yes stands for having nocturnal angina.
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
#' @param max_na Input integer 0 to 6 to indicate the maximum number of
#' missing symptoms to tolerate before outputting an \code{NA}.
#' Default: 0
#' @param max_frequency_of_angina_pain_per_week Input non-negative integer to
#' indicate the maximum frequency angina per week to tolerate before outputting an \code{NA}.
#' In the Duke Clinical Score 1993 paper, the maximum value is set as 35.
#' Default: 35
#' @return An integer indicating the patient's pain index.
#' It can also be \code{NA} if the number of missing symptoms exceeds the \code{max_na}
#' input value or the frequency of angina per week exceed the
#' \code{max_frequency_of_angina_pain_per_week} input value.
#' @examples
#' calculate_dcs_1993_pain_index(
#'   have_typical_chest_pain = "yes",
#'   frequency_of_angina_pain_per_week = 10,
#'   have_progressive_angina = "yes",
#'   have_nocturnal_angina = "no",
#'   have_q_waves = "no",
#'   have_st_t_changes = "no",
#'   max_na = 0,
#'   max_frequency_of_angina_pain_per_week = 35
#' )
#'
#' calculate_dcs_1993_pain_index(
#'   have_typical_chest_pain = "yes",
#'   frequency_of_angina_pain_per_week = 10,
#'   have_progressive_angina = "yes",
#'   have_nocturnal_angina = NA,
#'   have_q_waves = "no",
#'   have_st_t_changes = "no",
#'   max_na = 0,
#'   max_frequency_of_angina_pain_per_week = 35
#' )
#'
#' calculate_dcs_1993_pain_index(
#'   have_typical_chest_pain = "yes",
#'   frequency_of_angina_pain_per_week = 10,
#'   have_progressive_angina = "yes",
#'   have_nocturnal_angina = NA,
#'   have_q_waves = "no",
#'   have_st_t_changes = "no",
#'   max_na = 1,
#'   max_frequency_of_angina_pain_per_week = 35
#' )
#'
#' calculate_dcs_1993_pain_index(
#'   have_typical_chest_pain = "yes",
#'   frequency_of_angina_pain_per_week = 40,
#'   have_progressive_angina = "yes",
#'   have_nocturnal_angina = "no",
#'   have_q_waves = "no",
#'   have_st_t_changes = "no",
#'   max_na = 0,
#'   max_frequency_of_angina_pain_per_week = 35
#' )
#'
#' calculate_dcs_1993_pain_index(
#'   have_typical_chest_pain = "yes",
#'   frequency_of_angina_pain_per_week = 40,
#'   have_progressive_angina = "yes",
#'   have_nocturnal_angina = "no",
#'   have_q_waves = "no",
#'   have_st_t_changes = "no",
#'   max_na = 0,
#'   max_frequency_of_angina_pain_per_week = NA
#' )
#'
#' @rdname calculate_dcs_1993_pain_index
#' @export
calculate_dcs_1993_pain_index <- function(
    have_typical_chest_pain,
    frequency_of_angina_pain_per_week,
    have_progressive_angina,
    have_nocturnal_angina,
    have_q_waves,
    have_st_t_changes,
    max_na = 0,
    max_frequency_of_angina_pain_per_week = 35
) {

  check_if_non_negative(x = frequency_of_angina_pain_per_week, allow_na = TRUE)
  check_if_integer(x = frequency_of_angina_pain_per_week, allow_na = TRUE)

  check_if_non_negative(x = max_frequency_of_angina_pain_per_week, allow_na = TRUE)
  check_if_integer(x = max_frequency_of_angina_pain_per_week, allow_na = TRUE)

  have_typical_chest_pain <- have_typical_chest_pain |>
    arg_match0_allow_na(values = c("no","yes"))

  have_progressive_angina <- have_progressive_angina |>
    arg_match0_allow_na(values = c("no","yes"))

  have_nocturnal_angina <- have_nocturnal_angina |>
    arg_match0_allow_na(values = c("no","yes"))

  have_q_waves <- have_q_waves |>
    arg_match0_allow_na(values = c("no","yes"))

  have_st_t_changes <- have_st_t_changes |>
    arg_match0_allow_na(values = c("no","yes"))

  max_na <- max_na |>
    arg_match0_integer(values = c(0:6))

  number_of_na <- 0

  number_of_na <- dplyr::case_when(
    is.na(have_typical_chest_pain) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(frequency_of_angina_pain_per_week) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_progressive_angina) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_nocturnal_angina) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_q_waves) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_st_t_changes) ~ number_of_na + 1,
    .default = number_of_na
  )

  if (number_of_na > max_na) {return(NA)}

  if (is.na(frequency_of_angina_pain_per_week)) {
    frequency_of_angina_pain_per_week = 0
  }

  if (!is.na(max_frequency_of_angina_pain_per_week)) {
    if (isTRUE(frequency_of_angina_pain_per_week > max_frequency_of_angina_pain_per_week)) {
      return(NA)
    }
  }

  have_typical_chest_pain_score <- dplyr::case_when(
    have_typical_chest_pain == "yes" ~ 1,
    .default = 0
  )

  have_progressive_angina <- dplyr::case_when(
    have_progressive_angina == "yes" ~ 1,
    .default = 0
  )

  have_nocturnal_angina <- dplyr::case_when(
    have_nocturnal_angina == "yes" ~ 1,
    .default = 0
  )

  have_st_t_changes_but_no_q_waves <- dplyr::case_when(
    have_st_t_changes == "yes" & have_q_waves == "no" ~ 1,
    .default = 0
  )

  pain_index <-
    (have_typical_chest_pain_score * frequency_of_angina_pain_per_week) *
    (1 * have_progressive_angina + 4 * have_st_t_changes_but_no_q_waves + 2 * have_nocturnal_angina)

  return(pain_index)

}


#' @title Calculate The Vascular Disease Index For Duke Clinical Score 1993
#' @description A function used to calculate the patient's
#' vascular disease index. This is used to calculate the likelihood
#' of severe coronary artery disease in the
#' Duke Clinical Score 1993 paper.
#' @param have_peripheral_vascular_disease Input characters (no, yes) to indicate if the patient
#' has peripheral vascular disease.
#' \itemize{
#'   \item no stands for not having peripheral vascular disease.
#'   \item yes stands for having peripheral vascular disease.
#' }
#' @param have_cerebrovascular_disease Input characters (no, yes) to indicate if the patient
#' has cerebrovascular disease.
#' \itemize{
#'   \item no stands for not having cerebrovascular disease.
#'   \item yes stands for having cerebrovascular disease.
#' }
#' @param have_carotid_bruits Input characters (no, yes) to indicate if the patient
#' has carotid bruits.
#' \itemize{
#'   \item no stands for not having carotid bruits.
#'   \item yes stands for having carotid bruits.
#' }
#' @param max_na Input integer 0 to 3 to indicate the maximum number of
#' missing disease history to tolerate before outputting an \code{NA}.
#' Default: 0
#' @return An integer indicating the patient's vascular disease index.
#' It can also be \code{NA} if the number of missing disease history exceeds the \code{max_na}
#' input value.
#' @examples
#' calculate_dcs_1993_vascular_disease_index(
#'   have_peripheral_vascular_disease = "yes",
#'   have_cerebrovascular_disease = "yes",
#'   have_carotid_bruits = "no"
#' )
#'
#' calculate_dcs_1993_vascular_disease_index(
#'   have_peripheral_vascular_disease = NA,
#'   have_cerebrovascular_disease = "yes",
#'   have_carotid_bruits = "no",
#'   max_na = 0
#' )
#'
#' calculate_dcs_1993_vascular_disease_index(
#'   have_peripheral_vascular_disease = NA,
#'   have_cerebrovascular_disease = "yes",
#'   have_carotid_bruits = "no",
#'   max_na = 1
#' )
#' @rdname calculate_dcs_1993_vascular_disease_index
#' @export
calculate_dcs_1993_vascular_disease_index <- function(
    have_peripheral_vascular_disease,
    have_cerebrovascular_disease,
    have_carotid_bruits,
    max_na = 0
) {

  have_peripheral_vascular_disease <- have_peripheral_vascular_disease |>
    arg_match0_allow_na(values = c("no","yes"))

  have_cerebrovascular_disease <- have_cerebrovascular_disease |>
    arg_match0_allow_na(values = c("no","yes"))

  have_carotid_bruits <- have_carotid_bruits |>
    arg_match0_allow_na(values = c("no","yes"))

  max_na <- max_na |>
    arg_match0_integer(values = c(0:3))

  number_of_na <- 0
  risk_factor_index <- 0

  number_of_na <- dplyr::case_when(
    is.na(have_peripheral_vascular_disease) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_cerebrovascular_disease) ~ number_of_na + 1,
    .default = number_of_na
  ) + dplyr::case_when(
    is.na(have_carotid_bruits) ~ number_of_na + 1,
    .default = number_of_na
  )

  if (number_of_na > max_na) {return(NA)}

  vascular_disease_index <- dplyr::case_when(
    have_peripheral_vascular_disease == "yes" ~ risk_factor_index + 1,
    .default = risk_factor_index
  ) + dplyr::case_when(
    have_cerebrovascular_disease == "yes" ~ risk_factor_index + 1,
    .default = risk_factor_index
  ) + dplyr::case_when(
    have_carotid_bruits == "yes" ~ risk_factor_index + 1,
    .default = risk_factor_index
  )

  return(vascular_disease_index)

}


#' @title Calculate 1993 Duke Clinical Score for Severe CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of severe (>75\% luminal diameter narrowing
#' of all three major coronary arteries or of the left main coronary artery)
#' coronary artery disease based on the
#' 1993 Duke Clinical Score.
#' @inheritParams calculate_dcs_1993_risk_factor_index
#' @inheritParams calculate_dcs_1993_pain_index
#' @inheritParams calculate_dcs_1993_vascular_disease_index
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
#' @param have_q_waves Input characters (no, yes) to indicate if the patient
#' has Q waves on electrocardiogram (ECG).
#' \itemize{
#'   \item no stands for the patient not having Q waves on ECG.
#'   \item yes stands for the patient having Q waves on ECG.
#' }
#' @param duration_of_cad_symptoms_year Input integer to indicate the duration of
#' coronary artery disease symptoms in years.
#' @param max_na_risk_factor_index Input integer 0 to 3 to indicate the maximum number of
#' missing risk factors to tolerate before outputting an \code{NA}.
#' Default: 0
#' @param max_na_pain_index Input integer 0 to 5 to indicate the maximum number of
#' missing symptoms to tolerate before outputting an \code{NA}.
#' Default: 0
#' @param max_na_vascular_disease_index Input integer 0 to 3 to indicate the maximum number of
#' missing disease history to tolerate before outputting an \code{NA}.
#' Default: 0
#' @param max_frequency_of_angina_pain_per_week Input non-negative integer to
#' indicate the maximum frequency angina per week to tolerate before outputting an \code{NA}.
#' In the Duke Clinical Score 1993 paper, the maximum value is set as 35.
#' Default: 35
#' @return A numeric value representing the patient's PTP for severe
#' (>75\% luminal diameter narrowing of all three major coronary arteries
#' or of the left main coronary artery) CAD
#' based on the 1993 Duke Clinical Score.
#' @details The predictive model is based on
#' patients referred for cardiac catheterisation between 1969 and 1983.
#'
#' @examples
#' # 40 year old female with typical chest pain for one year,
#' # She has progressive angina but no nocturnal angina.
#' # Angina pain lasted at most five times a week.
#' # She has peripheral vascular and cerebrovascular disease.
#' # She has hypertension but has no dyslipidemia and not diabetic.
#' # She has Q waves and ST-T changes on ECG.
#'
#' calculate_dcs_1993_severe_cad_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_progressive_angina = "yes",
#'     have_nocturnal_angina = "no",
#'     have_peripheral_vascular_disease = "yes",
#'     have_cerebrovascular_disease = "yes",
#'     have_carotid_bruits = "no",
#'     have_hypertension = "yes",
#'     have_dyslipidemia = "no",
#'     have_diabetes = "no",
#'     have_q_waves = "yes",
#'     have_st_t_changes = "yes",
#'     frequency_of_angina_pain_per_week = 5,
#'     duration_of_cad_symptoms_year = 1,
#' )
#' @rdname calculate_dcs_1993_severe_cad_ptp
#' @export
calculate_dcs_1993_severe_cad_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_progressive_angina,
    have_nocturnal_angina,
    have_peripheral_vascular_disease,
    have_cerebrovascular_disease,
    have_carotid_bruits,
    have_hypertension,
    have_dyslipidemia,
    have_diabetes,
    have_q_waves,
    have_st_t_changes,
    frequency_of_angina_pain_per_week,
    duration_of_cad_symptoms_year,
    max_na_risk_factor_index = 0,
    max_na_pain_index = 0,
    max_na_vascular_disease_index = 0,
    max_frequency_of_angina_pain_per_week = 35
) {

  check_if_positive(x = age, allow_na = TRUE)
  check_if_non_negative(x = duration_of_cad_symptoms_year, allow_na = TRUE)

  log_transformed_duration_of_cad_symptoms_year <- log10(
    duration_of_cad_symptoms_year + 1
  )

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  sex <- dplyr::case_when(
    sex == "female" ~ 1L,
    sex == "male" ~ 0L,
    .default = NA_integer_
  )

  have_q_waves <- have_q_waves |>
    arg_match0_allow_na(values = c("no","yes"))

  chest_pain_type <- chest_pain_type |>
    arg_match0_allow_na(values = c("typical", "atypical", "nonanginal"))

  type_of_pain <- dplyr::case_when(
    chest_pain_type == "nonanginal" ~ 0,
    chest_pain_type == "atypical" ~ 1,
    chest_pain_type == "typical" ~ 2,
    .default = NA
  )

  have_typical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("atypical", "nonanginal") ~ "no",
    chest_pain_type == "typical" ~ "yes",
    .default = NA
  )

  risk_factor_index <- calculate_dcs_1993_risk_factor_index(
    have_hypertension = have_hypertension,
    have_dyslipidemia = have_dyslipidemia,
    have_diabetes = have_diabetes,
    max_na = max_na_risk_factor_index
  )

  vascular_disease_index <- calculate_dcs_1993_vascular_disease_index(
    have_peripheral_vascular_disease = have_peripheral_vascular_disease,
    have_cerebrovascular_disease = have_cerebrovascular_disease,
    have_carotid_bruits = have_carotid_bruits,
    max_na = max_na_vascular_disease_index
  )

  pain_index <- calculate_dcs_1993_pain_index(
    have_typical_chest_pain = have_typical_chest_pain,
    frequency_of_angina_pain_per_week = frequency_of_angina_pain_per_week,
    have_progressive_angina = have_progressive_angina,
    have_nocturnal_angina = have_nocturnal_angina,
    have_st_t_changes = have_st_t_changes,
    have_q_waves = have_q_waves,
    max_na = max_na_pain_index,
    max_frequency_of_angina_pain_per_week = max_frequency_of_angina_pain_per_week
  )

  have_q_waves <- dplyr::case_when(
    have_q_waves == "no" ~ 0L,
    have_q_waves == "yes" ~ 1L,
    .default = NA_integer_
  )

  dcs_1993_severe_cad_ptp <- 1 /
    (1 + exp(-(-3.4732 +
              ( 0.3424 * log_transformed_duration_of_cad_symptoms_year) +
              ( 0.3014 * type_of_pain) +
              ( 0.1559 * log_transformed_duration_of_cad_symptoms_year * type_of_pain) +
              ( 0.0299 * age) +
              ( 0.3513 * have_q_waves) +
              ( 0.0054 * pain_index) +
              (-0.3823 * sex) +
              ( 0.1734 * risk_factor_index) +
              ( 0.2402 * vascular_disease_index)

    )
    )
    )

  return(dcs_1993_severe_cad_ptp)
}
