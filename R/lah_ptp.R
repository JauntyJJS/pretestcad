#' @title Calculate 2022 LAH Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2022 Local Assessment of the Heart (LAH) clinical model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param chest_pain_type Input characters (typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2022 Local Assessment of the Heart (LAH) clinical model.
#' @details The predictive model is based on
#' patients a mixed Asian cohort within Singapore with stable chest pain.
#'
#' @examples
#' # 40 year old female with typical chest pain,
#' # diabetes but no hypertension, dyslipidemia
#' # and a non-smoker
#' calculate_lah_2022_clinical_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no"
#'
#' )
#' @rdname calculate_lah_2022_clinical_ptp
#' @export
calculate_lah_2022_clinical_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_diabetes,
    have_hypertension,
    have_dyslipidemia,
    have_smoking_history
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

  have_smoking_history <- have_smoking_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_smoking_history <- dplyr::case_when(
      have_smoking_history == "no" ~ 0L,
      have_smoking_history == "yes" ~ 1L,
      .default = NA_integer_
  )

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_dyslipidemia <- dplyr::case_when(
      have_dyslipidemia == "no" ~ 0L,
      have_dyslipidemia == "yes" ~ 1L,
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

  lah_2022_clinical_ptp <- 1 /
    (1 + exp(-(-6.268 +
              (0.067  * age) +
              (1.518  * sex) +
              (-0.090 * have_atypical_chest_pain) +
              (0.164  * have_typical_chest_pain) +
              (0.417  * have_diabetes) +
              (0.457  * have_hypertension) +
              (0.370  * have_dyslipidemia) +
              (-0.364 * have_smoking_history)
    )
    )
    )

  return(lah_2022_clinical_ptp)

}

#' @title Calculate 2022 LAH Extended PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2022 Local Assessment of the Heart (LAH) extended model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param chest_pain_type Input characters (typical, atypical, nonanginal)
#' to indicate the chest pain characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#' }
#' @param coronary_calcium_score Input non-negative numeric to indicate the
#' total coronary calcium score of the patient.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2022 Local Assessment of the Heart (LAH) extended model.
#' @details The predictive model is based on
#' patients a mixed Asian cohort within Singapore with stable chest pain.
#'
#' @examples
#' # 40 year old female with typical chest pain,
#' # diabetes but no hypertension, dyslipidemia,
#' # a non-smoker and a coronary calcium score of 0
#' calculate_lah_2022_extended_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no",
#'     coronary_calcium_score = 0
#'
#' )
#' @rdname calculate_lah_2022_extended_ptp
#' @export
calculate_lah_2022_extended_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_diabetes,
    have_hypertension,
    have_dyslipidemia,
    have_smoking_history,
    coronary_calcium_score
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

  have_smoking_history <- have_smoking_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_smoking_history <- dplyr::case_when(
    have_smoking_history == "no" ~ 0L,
    have_smoking_history == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "no" ~ 0L,
    have_dyslipidemia == "yes" ~ 1L,
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

  check_if_non_negative(x = coronary_calcium_score, allow_na = TRUE)

  log_transformed_ccs <- log(coronary_calcium_score + 1)

  lah_2022_extended_ptp <- 1 /
    (1 + exp(-(-4.241 +
              (0      * age) +
              (0.544  * sex) +
              (-0.242 * have_atypical_chest_pain) +
              (0.139  * have_typical_chest_pain) +
              (-0.002 * have_diabetes) +
              (-0.143 * have_hypertension) +
              (-0.157 * have_dyslipidemia) +
              (-0.315 * have_smoking_history) +
              (0.905  * log_transformed_ccs)
    )
    )
    )

  return(lah_2022_extended_ptp)

}
