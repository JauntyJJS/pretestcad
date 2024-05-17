#' @title Calculate 2017 PROMISE Minimal-Risk Score for obstructive CAD
#' @description This function returns
#' a symptomatic (have chest pain or dyspnea) patient's
#' minimal risk score for obstructive
#' coronary artery disease based on the
#' 2017 PROMISE Minimal-Risk Score.
#' @param age Input integer to indicate the age of the patient.
#' @param sex Input integer 0 or 1 to indicate the sex of the patient.
#' \itemize{
#'   \item 0 stands for Female
#'   \item 1 stands for Male
#' }
#' @param hdl_mg_dl Input positive numeric value to indicate the
#' patient's high-density lipoprotein (HDL) in \eqn{mg/dL}.
#' @param is_minority_ethnicity Input integer 0 or 1
#' to indicate if the patient is from a racial or minority ethnicity
#' (or patient is not a non-Hispanic/Latino White).
#' \itemize{
#'   \item 0 stands for patient is a non-Hispanic/Latino White.
#'   \item 1 stands for patient is not a non-Hispanic/Latino White. E.g. Blacks, Asians, etc.
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
#' @param has_family_history_of_cad Input integer 0 or 1 to indicate if the patient
#' has a family history of CAD.
#' \itemize{
#'   \item 0 stands for not having a family history of CAD.
#'   \item 1 stands for having a family history of CAD.
#' }
#' @param has_stress_symptoms Input integer 0 or 1 to indicate if the patient
#' has symptoms related to physical or mental stress. It can be set
#' to \code{NA} if the patient results are inconclusive or have not taken any stress test
#' such as exercise treadmill testing, stress echocardiography, or stress
#' nuclear imaging.
#' \itemize{
#'   \item 0 stands for no symptoms (negative results) related to physical or mental stress.
#'   \item 1 stands for having symptoms (positive results) related to physical or mental stress.
#'   \item \code{NA} stands for inconclusive results or patient has not taken any stress test
#' }
#' Default: \code{NA}
#' @return A numeric value representing the patient's minimal risk
#' score for obstructive CAD based on the 2017 PROMISE Minimal-Risk Score.
#' @details The predictive model is based on CCTA images from 4632
#' patients in the Prospective Multicenter imaging Study for Evaluation
#' of Chest Pain (PROMISE) trial.
#' @examples
#' # 50 year old white female with chest pain
#' # a medical history of hypertension, and a
#' # high-density lipoprotein cholesterol level of 70 mg/dL
#' calculate_prms_2017_ptp(
#'     age = 50,
#'     sex = 0,
#'     hdl_mg_dl = 70,
#'     is_minority_ethnicity = 0,
#'     has_diabetes = 0,
#'     has_hypertension = 1,
#'     has_dyslipidemia = 0,
#'     has_smoking_history = 0,
#'     has_family_history_of_cad = 0,
#'     has_stress_symptoms = 0
#' )
#'
#' # 40 year old non-white male with chest pain
#' # a medical history of diabetes, unknown stress symptoms and a
#' # high-density lipoprotein cholesterol level of 65 mg/dL
#' calculate_prms_2017_ptp(
#'     age = 40,
#'     sex = 1,
#'     hdl_mg_dl = 70,
#'     is_minority_ethnicity = 1,
#'     has_diabetes = 1,
#'     has_hypertension = 0,
#'     has_dyslipidemia = 0,
#'     has_smoking_history = 0,
#'     has_family_history_of_cad = 0,
#'     has_stress_symptoms = NA
#' )
#' @rdname calculate_prms_2017_ptp
#' @export
calculate_prms_2017_ptp <- function(
    age,
    sex,
    hdl_mg_dl,
    is_minority_ethnicity,
    has_diabetes,
    has_hypertension,
    has_dyslipidemia,
    has_smoking_history,
    has_family_history_of_cad,
    has_stress_symptoms = NA
) {

  is_female <- dplyr::case_when(
    sex == 1 ~ 0,
    sex == 0 ~ 1,
    .default = NA_integer_
  )

  is_non_smoker <- dplyr::case_when(
    has_smoking_history == 1 ~ 0,
    has_smoking_history == 0 ~ 1,
    .default = NA_integer_
  )

  has_no_diabetes <- dplyr::case_when(
    has_diabetes == 1 ~ 0,
    has_diabetes == 0 ~ 1,
    .default = NA_integer_
  )

  has_no_dyslipidemia <- dplyr::case_when(
    has_dyslipidemia == 1 ~ 0,
    has_dyslipidemia == 0 ~ 1,
    .default = NA_integer_
  )

  has_no_family_history_of_cad <- dplyr::case_when(
    has_family_history_of_cad == 1 ~ 0,
    has_family_history_of_cad == 0 ~ 1,
    .default = NA_integer_
  )

  has_no_hypertension <- dplyr::case_when(
    has_hypertension == 1 ~ 0,
    has_hypertension == 0 ~ 1,
    .default = NA_integer_
  )

  has_no_stress_symptoms <- dplyr::case_when(
    is.na(has_stress_symptoms) ~ 0,
    has_stress_symptoms == 1 ~ 0,
    has_stress_symptoms == 0 ~ 1,
    .default = NA_integer_
  )

  has_unknown_stress_symptoms <- dplyr::case_when(
    is.na(has_stress_symptoms) ~ 1,
    has_stress_symptoms == 1 ~ 0,
    has_stress_symptoms == 0 ~ 0,
    .default = NA_integer_
  )

  prms_2017_ptp  <- 1 /
    (1 + exp((-1.783 +
             ( 0.084 * age) +
             (-1.026 * is_female) +
             (-0.142 * is_minority_ethnicity) +
             (-0.526 * is_non_smoker) +
             (-0.314 * has_no_diabetes) +
             (-0.412 * has_no_dyslipidemia) +
             (-0.309 * has_no_family_history_of_cad) +
             (-0.408 * has_no_hypertension) +
             (-0.309 * has_no_stress_symptoms) +
             (-0.195 * has_unknown_stress_symptoms) +
             (-0.006 * hdl_mg_dl)
    )
    )
    )

  return(prms_2017_ptp)
}
