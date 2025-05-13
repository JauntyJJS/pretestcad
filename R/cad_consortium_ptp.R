#' @title Calculate 2011 CAD1 Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2011 CAD Consortium 1 (CAD1) basic model.
#' @inheritParams calculate_lah_2022_extended_ptp
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2011 CAD Consortium 1 (CAD1) basic model.
#' @details The predictive model is based on
#' patients from 14 hospitals in Europe and the United States.
#'
#' This model is also called the updated Diamond-Forrester model.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' calculate_cad1_2011_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical"
#' )
#' @rdname calculate_cad1_2011_ptp
#' @export
calculate_cad1_2011_ptp <- function(
    age,
    sex,
    chest_pain_type
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

  cad1_2011_ptp <- 1 /
    (1 + exp(-(-4.37 +
              (0.04 * age) +
              (1.34 * sex) +
              (0.64 * have_atypical_chest_pain) +
              (1.91 * have_typical_chest_pain)
    )
    )
    )

  return(cad1_2011_ptp)

}

#' @title Calculate 2012 CAD2 Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2012 CAD Consortium 2 (CAD2) basic model.
#' @inheritParams calculate_lah_2022_extended_ptp
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2012 CAD Consortium 2 (CAD2) basic model.
#' @details The predictive model is based on
#' patients from 18 hospitals in Europe and the United States.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' calculate_cad2_2012_basic_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical"
#' )
#' @rdname calculate_cad2_2012_basic_ptp
#' @export
calculate_cad2_2012_basic_ptp <- function(
    age,
    sex,
    chest_pain_type
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

  cad2_2012_basic_ptp <- 1 /
    (1 + exp(-(-6.917 +
               (0.063 * age) +
               (1.358 * sex) +
               (0.658 * have_atypical_chest_pain) +
               (1.975 * have_typical_chest_pain)
    )
    )
    )

  return(cad2_2012_basic_ptp)

}

#' @title Calculate 2012 CAD2 Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2012 CAD Consortium 2 (CAD2) clinical model.
#' @inheritParams calculate_lah_2022_extended_ptp
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2012 CAD Consortium 2 (CAD2) clinical model.
#' @details The predictive model is based on
#' patients from 18 hospitals in Europe and the United States.
#'
#' @examples
#' # 40 year old female with typical chest pain,
#' # diabetes but no hypertension, dyslipidemia
#' # and a non-smoker
#' calculate_cad2_2012_clinical_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     have_dyslipidemia = "no",
#'     have_smoking_history = "no"
#'
#' )
#' @rdname calculate_cad2_2012_clinical_ptp
#' @export
calculate_cad2_2012_clinical_ptp <- function(
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

  cad2_2012_clinical_ptp <- 1 /
    (1 + exp(-(-7.539 +
              (0.062 * age) +
              (1.332 * sex) +
              (0.633 * have_atypical_chest_pain) +
              (1.998 * have_typical_chest_pain) +
              (0.828 * have_diabetes) +
              (0.338 * have_hypertension) +
              (0.422 * have_dyslipidemia) +
              (0.461 * have_smoking_history) +
              (-0.402 * have_diabetes * have_typical_chest_pain)
    )
    )
    )

  return(cad2_2012_clinical_ptp)

}

#' @title Calculate 2012 CAD2 Clinical and CCS PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2012 CAD Consortium 2 (CAD2) clinical and coronary calcium score (CCS) model.
#' @inheritParams calculate_lah_2022_extended_ptp
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2012 CAD Consortium 2 (CAD2) clinical and coronary calcium score (CCS) model.
#' @details The predictive model is based on
#' patients from 18 hospitals in Europe and the United States.
#'
#' @examples
#' # 40 year old female with typical chest pain,
#' # diabetes but no hypertension, dyslipidemia,
#' # a non-smoker and a coronary calcium score of 0
#' calculate_cad2_2012_clinical_ccs_ptp(
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
#' @rdname calculate_cad2_2012_clinical_ccs_ptp
#' @export
calculate_cad2_2012_clinical_ccs_ptp <- function(
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

  cad2_2012_clinical_ccs_ptp <- 1 /
    (1 + exp(-(-5.975 +
              (0.011 * age) +
              (0.786 * sex) +
              (0.718 * have_atypical_chest_pain) +
              (2.024 * have_typical_chest_pain) +
              (0.658 * have_diabetes) +
              (0.235 * have_hypertension) +
              (0.185 * have_dyslipidemia) +
              (0.207 * have_smoking_history) +
              (0.577 * log_transformed_ccs) +
              (-0.780 * have_diabetes * have_typical_chest_pain)
    )
    )
    )

  return(cad2_2012_clinical_ccs_ptp)

}
