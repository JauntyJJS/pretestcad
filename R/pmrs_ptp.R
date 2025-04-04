#' @title Calculate 2017 PROMISE Minimal-Risk Score for obstructive CAD
#' @description This function returns
#' a symptomatic (have chest pain or dyspnoea) patient's
#' minimal risk score for obstructive
#' coronary artery disease based on the
#' 2017 PROMISE Minimal-Risk Score.
#' @inheritParams calculate_esc_2024_num_of_rf
#' @inheritParams calculate_esc_2024_fig_4_ptp_simplfied
#' @param age Input numeric value to indicate the age of the patient.
#' @param hdl_mg_dl Input positive numeric value to indicate the
#' patient's high-density lipoprotein (HDL) in \eqn{mg/dL}.
#' @param is_minority_ethnicity Input characters (no, yes) to indicate if the patient
#' is from a racial or minority ethnicity
#' (or patient is not a non-Hispanic/Latino White).
#' \itemize{
#'   \item no stands for patient is a non-Hispanic/Latino White.
#'   \item yes stands for patient is not a non-Hispanic/Latino White. E.g. Blacks, Asians, etc.
#' }
#' @param have_stress_symptoms Input characters (no, yes) to indicate if the patient
#' has symptoms related to physical or mental stress. It can be set
#' to \code{NA} if the patient results are inconclusive or have not taken any stress test
#' such as exercise treadmill testing, stress echocardiography, or stress
#' nuclear imaging.
#' \itemize{
#'   \item no stands for no symptoms (negative results) related to physical or mental stress.
#'   \item yes stands for having symptoms (positive results) related to physical or mental stress.
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
#'     sex = "female",
#'     hdl_mg_dl = 70,
#'     is_minority_ethnicity = "no",
#'     have_diabetes = "no",
#'     have_hypertension = "yes",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no",
#'     have_family_history = "no",
#'     have_stress_symptoms = "no"
#' )
#'
#' # 40 year old non-white male with chest pain
#' # a medical history of diabetes, unknown stress symptoms and a
#' # high-density lipoprotein cholesterol level of 70 mg/dL
#' calculate_prms_2017_ptp(
#'     age = 40,
#'     sex = "male",
#'     hdl_mg_dl = 70,
#'     is_minority_ethnicity = "yes",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no",
#'     have_family_history = "no",
#'     have_stress_symptoms = NA
#' )
#' @rdname calculate_prms_2017_ptp
#' @export
calculate_prms_2017_ptp <- function(
    age,
    sex,
    hdl_mg_dl,
    is_minority_ethnicity,
    have_diabetes,
    have_hypertension,
    have_dyslipidemia,
    have_smoking_history,
    have_family_history,
    have_stress_symptoms = NA
) {

  check_if_positive(x = age, allow_na = TRUE)

  sex <- sex |>
    arg_match0_allow_na(values = c("female","male"))

  is_female <- dplyr::case_when(
    sex == "female" ~ 1,
    sex == "male" ~ 0,
    .default = NA_integer_
  )

  is_minority_ethnicity <- is_minority_ethnicity |>
    arg_match0_allow_na(values = c("no","yes"))

  is_minority_ethnicity <- dplyr::case_when(
    is_minority_ethnicity == "yes" ~ 1,
    is_minority_ethnicity == "no" ~ 0,
    .default = NA_integer_
  )

  have_smoking_history <- have_smoking_history |>
    arg_match0_allow_na(values = c("no","yes"))

  is_non_smoker <- dplyr::case_when(
    have_smoking_history == "yes" ~ 0,
    have_smoking_history == "no" ~ 1,
    .default = NA_integer_
  )

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  have_no_diabetes <- dplyr::case_when(
    have_diabetes == "yes" ~ 0,
    have_diabetes == "no" ~ 1,
    .default = NA_integer_
  )

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_no_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "yes" ~ 0,
    have_dyslipidemia == "no" ~ 1,
    .default = NA_integer_
  )

  have_family_history <- have_family_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_no_family_history <- dplyr::case_when(
    have_family_history == "yes" ~ 0,
    have_family_history == "no" ~ 1,
    .default = NA_integer_
  )

  have_hypertension <- have_hypertension |>
    arg_match0_allow_na(values = c("no","yes"))

  have_no_hypertension <- dplyr::case_when(
    have_hypertension == "yes" ~ 0,
    have_hypertension == "no" ~ 1,
    .default = NA_integer_
  )

  have_stress_symptoms <- have_stress_symptoms |>
    arg_match0_allow_na(values = c("no","yes"))

  have_no_stress_symptoms <- dplyr::case_when(
    is.na(have_stress_symptoms) ~ 0,
    have_stress_symptoms == "yes" ~ 0,
    have_stress_symptoms == "no" ~ 1,
    .default = NA_integer_
  )

  have_unknown_stress_symptoms <- dplyr::case_when(
    is.na(have_stress_symptoms) ~ 1,
    have_stress_symptoms == "yes" ~ 0,
    have_stress_symptoms == "no" ~ 0,
    .default = NA_integer_
  )

  check_if_non_negative(x = hdl_mg_dl, allow_na = TRUE)

  prms_2017_ptp  <- 1 /
    (1 + exp((-1.783 +
             ( 0.084 * age) +
             (-1.026 * is_female) +
             (-0.142 * is_minority_ethnicity) +
             (-0.526 * is_non_smoker) +
             (-0.314 * have_no_diabetes) +
             (-0.412 * have_no_dyslipidemia) +
             (-0.309 * have_no_family_history) +
             (-0.408 * have_no_hypertension) +
             (-0.309 * have_no_stress_symptoms) +
             (-0.195 * have_unknown_stress_symptoms) +
             (-0.006 * hdl_mg_dl)
    )
    )
    )

  return(prms_2017_ptp)
}
