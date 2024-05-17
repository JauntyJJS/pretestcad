#' @title Calculate 2022 LAH Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2022 Local Assessment of the Heart (LAH) clinical model.
#' @param age Input integer to indicate the age of the patient.
#' @param sex Input integer 0 or 1 to indicate the sex of the patient.
#' \itemize{
#'   \item 0 stands for Female
#'   \item 1 stands for Male
#' }
#' @param chest_pain Input integer 1 to 3 to indicate the chest pain
#' characteristics of the patient.
#' \itemize{
#'   \item 1 stands for the patient having typical chest pain.
#'   \item 2 stands for the patient having atypical chest pain.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#' @param has_diabetes Input integer 0 or 1 to indicate if the patient
#' has diabetes.
#' \itemize{
#'   \item 0 stands for not having diabetes.
#'   \item 1 stands for having diabetes.
#' }
#' @param has_hypertension Input integer 0 or 1 to indicate if the patient
#' has hypertension.
#' \itemize{
#'   \item 0 stands for not having hypertension.
#'   \item 1 stands for having hypertension.
#' }
#' @param has_dyslipidemia Input integer 0 or 1 to indicate if the patient
#' has dyslipidemia.
#' \itemize{
#'   \item 0 stands for not having dyslipidemia.
#'   \item 1 stands for having dyslipidemia.
#' }
#' @param has_smoking_history Input integer 0 or 1 to indicate if the patient
#' has a smoking history (current or past smoker).
#' \itemize{
#'   \item 0 stands for not having a smoking history (non-smoker).
#'   \item 1 stands for having a smoking history (current or past smoker).
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
#'     sex = 0,
#'     chest_pain = 1,
#'     has_diabetes = 1,
#'     has_hypertension = 0,
#'     has_dyslipidemia = 0,
#'     has_smoking_history = 0
#'
#' )
#' @rdname calculate_lah_2022_clinical_ptp
#' @export
calculate_lah_2022_clinical_ptp <- function(
    age,
    sex,
    chest_pain,
    has_diabetes,
    has_hypertension,
    has_dyslipidemia,
    has_smoking_history
)
{

  has_atypical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(1, 3) ~ 0,
    chest_pain == 2 ~ 1
  )

  has_typical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(2, 3) ~ 0,
    chest_pain == 1 ~ 1
  )

  lah_2022_clinical_ptp <- 1 /
    (1 + exp(-(-6.268 +
              (0.067 * age) +
              (1.518 * sex) +
              (-0.090 * has_atypical_chest_pain) +
              (0.164 * has_typical_chest_pain) +
              (0.417 * has_diabetes) +
              (0.457 * has_hypertension) +
              (0.370 * has_dyslipidemia) +
              (-0.364 * has_smoking_history)
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
#' @param age Input integer to indicate the age of the patient.
#' @param sex Input integer 0 or 1 to indicate the sex of the patient.
#' \itemize{
#'   \item 0 stands for Female
#'   \item 1 stands for Male
#' }
#' @param chest_pain Input integer 1 to 3 to indicate the chest pain
#' characteristics of the patient.
#' \itemize{
#'   \item 1 stands for the patient having typical chest pain.
#'   \item 2 stands for the patient having atypical chest pain.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#' @param has_diabetes Input integer 0 or 1 to indicate if the patient
#' has diabetes.
#' \itemize{
#'   \item 0 stands for not having diabetes.
#'   \item 1 stands for having diabetes.
#' }
#' @param has_hypertension Input integer 0 or 1 to indicate if the patient
#' has hypertension.
#' \itemize{
#'   \item 0 stands for not having hypertension.
#'   \item 1 stands for having hypertension.
#' }
#' @param has_dyslipidemia Input integer 0 or 1 to indicate if the patient
#' has dyslipidemia.
#' \itemize{
#'   \item 0 stands for not having dyslipidemia.
#'   \item 1 stands for having dyslipidemia.
#' }
#' @param has_smoking_history Input integer 0 or 1 to indicate if the patient
#' has a smoking history (current or past smoker).
#' \itemize{
#'   \item 0 stands for not having a smoking history (non-smoker).
#'   \item 1 stands for having a smoking history (current or past smoker).
#' }
#' @param coronary_calcium_score Input positive numeric to indicate the
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
#'     sex = 0,
#'     chest_pain = 1,
#'     has_diabetes = 1,
#'     has_hypertension = 0,
#'     has_dyslipidemia = 0,
#'     has_smoking_history = 0,
#'     coronary_calcium_score = 0
#'
#' )
#' @rdname calculate_lah_2022_extended_ptp
#' @export
calculate_lah_2022_extended_ptp <- function(
    age,
    sex,
    chest_pain,
    has_diabetes,
    has_hypertension,
    has_dyslipidemia,
    has_smoking_history,
    coronary_calcium_score
)
{

  has_atypical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(1, 3) ~ 0,
    chest_pain == 2 ~ 1
  )

  has_typical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(2, 3) ~ 0,
    chest_pain == 1 ~ 1
  )

  log_transformed_ccs <- log(coronary_calcium_score + 1)

  lah_2022_extended_ptp <- 1 /
    (1 + exp(-(-4.241 +
              (0 * age) +
              (0.544 * sex) +
              (-0.242 * has_atypical_chest_pain) +
              (0.139 * has_typical_chest_pain) +
              (-0.002 * has_diabetes) +
              (-0.143 * has_hypertension) +
              (-0.157 * has_dyslipidemia) +
              (-0.315 * has_smoking_history) +
              (0.905 * log_transformed_ccs)
    )
    )
    )

  return(lah_2022_extended_ptp)

}
