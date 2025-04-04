#' @title Calculate 2019 Reeh Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2019 Reeh et. al. basic model.
#' @inheritParams calculate_esc_2019_ptp
#' @param symptom_type Input characters (typical, atypical, nonanginal, dyspnoea)
#' to indicate the symptom characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#'   \item dyspnoea stands for the patient having dyspnoea.
#' }
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2019 Reeh et. al. basic model.
#' @details The predictive model is based on 3903
#' patients free of CAD and heart failure and suspected of angina, who were referred
#' to a single, large, urban university hospital for assessment in 2012–15.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' calculate_reeh_2019_basic_ptp(
#'     age = 40,
#'     sex = "female",
#'     symptom_type = "typical"
#' )
#' @rdname calculate_reeh_2019_basic_ptp
#' @export
calculate_reeh_2019_basic_ptp <- function(
    age,
    sex,
    symptom_type
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

  symptom_type <- symptom_type |>
    arg_match0_allow_na(values = c("typical", "atypical", "nonanginal", "dyspnoea"))

  have_atypical_chest_pain <- dplyr::case_when(
    symptom_type %in% c("typical", "nonanginal", "dyspnoea") ~ 0L,
    symptom_type == "atypical" ~ 1L,
    .default = NA_integer_
  )

  have_typical_chest_pain <- dplyr::case_when(
    symptom_type %in% c("atypical", "nonanginal", "dyspnoea") ~ 0L,
    symptom_type == "typical" ~ 1L,
    .default = NA_integer_
  )

  have_dyspnoea <- dplyr::case_when(
    symptom_type %in% c("typical", "atypical", "nonanginal") ~ 0L,
    symptom_type == "dyspnoea" ~ 1L,
    .default = NA_integer_
  )

  reeh_2019_basic_ptp <- 1 /
    (1 + exp(-(-7.6348 +
              (1.4067 * sex) +
              (0.4820 * age / 10) +
              (2.8779 * have_typical_chest_pain) +
              (1.8690 * have_atypical_chest_pain) +
              (0.7916 * have_dyspnoea)
    )
    )
    )

  return(reeh_2019_basic_ptp)

}


#' @title Calculate 2019 Reeh Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2019 Reeh et. al. clinical model.
#' @inheritParams calculate_reeh_2019_basic_ptp
#' @inheritParams calculate_esc_2024_num_of_rf
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2019 Reeh et. al. clinical model.
#' @details The predictive model is based on 3903
#' patients free of CAD and heart failure and suspected of angina, who were referred
#' to a single, large, urban university hospital for assessment in 2012–15.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' calculate_reeh_2019_clinical_ptp(
#'     age = 40,
#'     sex = "female",
#'     symptom_type = "typical",
#'     have_dyslipidemia = "no",
#'     have_family_history = "no",
#'     have_diabetes = "no"
#' )
#' @rdname calculate_reeh_2019_clinical_ptp
#' @export
calculate_reeh_2019_clinical_ptp <- function(
    age,
    sex,
    symptom_type,
    have_dyslipidemia,
    have_family_history,
    have_diabetes
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

  symptom_type <- symptom_type |>
    arg_match0_allow_na(values = c("typical", "atypical", "nonanginal", "dyspnoea"))

  have_dyslipidemia <- have_dyslipidemia |>
    arg_match0_allow_na(values = c("no","yes"))

  have_family_history <- have_family_history |>
    arg_match0_allow_na(values = c("no","yes"))

  have_diabetes <- have_diabetes |>
    arg_match0_allow_na(values = c("no","yes"))

  have_atypical_chest_pain <- dplyr::case_when(
    symptom_type %in% c("typical", "nonanginal", "dyspnoea") ~ 0L,
    symptom_type == "atypical" ~ 1L,
    .default = NA_integer_
  )

  have_typical_chest_pain <- dplyr::case_when(
    symptom_type %in% c("atypical", "nonanginal", "dyspnoea") ~ 0L,
    symptom_type == "typical" ~ 1L,
    .default = NA_integer_
  )

  have_dyspnoea <- dplyr::case_when(
    symptom_type %in% c("typical", "atypical", "nonanginal") ~ 0L,
    symptom_type == "dyspnoea" ~ 1L,
    .default = NA_integer_
  )

  have_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "no" ~ 0L,
    have_dyslipidemia == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_family_history <- dplyr::case_when(
    have_family_history == "no" ~ 0L,
    have_family_history == "yes" ~ 1L,
    .default = NA_integer_
  )

  have_diabetes <- dplyr::case_when(
    have_diabetes == "no" ~ 0L,
    have_diabetes == "yes" ~ 1L,
    .default = NA_integer_
  )

  reeh_2019_clinical_ptp <- 1 /
    (1 + exp(-(-8.5499 +
              (1.4468 * sex) +
              (0.5031 * age / 10) +
              (2.7699 * have_typical_chest_pain) +
              (1.7839 * have_atypical_chest_pain) +
              (0.8071 * have_dyspnoea) +
              (0.9551 * have_dyslipidemia) +
              (0.4394 * have_family_history) +
              (0.3767 * have_diabetes)
    )
    )
    )

  return(reeh_2019_clinical_ptp)

}
