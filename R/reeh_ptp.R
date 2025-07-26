#' @title Calculate 2019 Reeh Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2019 Reeh et. al. basic model.
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param sex The value of variable in the parameters \code{label_sex_male},
#' \code{label_sex_female} and \code{label_sex_unknown}.
#' @param symptom_type Input characters (typical, atypical, nonanginal, dyspnoea)
#' to indicate the symptom characteristics of the patient.
#' \itemize{
#'   \item typical stands for the patient having typical chest pain.
#'   \item atypical stands for the patient having atypical chest pain.
#'   \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
#'   \item dyspnoea stands for the patient having dyspnoea.
#' }
#' @param label_sex_male Label(s) for definition(s) of male sex.
#' Default: \code{c("male")}
#' @param label_sex_female Label(s) for definition(s) of female sex.
#' Default: \code{c("female")}
#' @param label_sex_unknown Label(s) for definition(s) of missing sex.
#' Default: \code{c(NA, NaN)}
#' @param label_symptom_type_typical Label(s) for patient having typical chest pain.
#' Default: \code{c("typical")}
#' @param label_symptom_type_atypical Label(s) for patient having atypical chest pain.
#' Default: \code{c("atypical")}
#' @param label_symptom_type_nonanginal Label(s) for patient having nonanginal
#' or non-specific chest pain.
#' Default: \code{c("nonanginal")}
#' @param label_symptom_type_dyspnoea Label(s) for patient having dyspnoea.
#' Default: \code{c("dyspnoea")}
#' @param label_symptom_type_unknown Label(s) for patient having unknown symptoms.
#' Default: \code{c(NA, NaN)}
#'
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
    symptom_type,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_symptom_type_typical = c("typical"),
    label_symptom_type_atypical = c("atypical"),
    label_symptom_type_nonanginal = c("nonanginal"),
    label_symptom_type_dyspnoea = c("dyspnoea"),
    label_symptom_type_unknown = c(NA, NaN)
    )
{
  check_if_positive(x = age, allow_na = TRUE)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_cat_missing = label_sex_unknown
  )

  # Ensure sex is valid and mapped to a unified group (male, female, NA)
  sex <- sex |>
    harmonise_two_labels(
      label_one = label_sex_male,
      label_two = label_sex_female,
      label_unknown = label_sex_unknown,
      harmonise_label_one = "male",
      harmonise_label_two = "female",
      harmonise_label_unknown = NA
    )

  sex <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 1L,
    .default = NA_integer_
  )

  check_if_four_categories_are_mutually_exclusive(
    label_symptom_type_typical,
    label_symptom_type_atypical,
    label_symptom_type_nonanginal,
    label_symptom_type_dyspnoea,
    label_cat_missing = label_symptom_type_unknown
  )

  # Ensure symptom type is valid and mapped to a unified group
  # (typical, atypical, nonanginal, dyspnoea)
  symptom_type <- symptom_type |>
    harmonise_four_labels(
      label_one = label_symptom_type_typical,
      label_two = label_symptom_type_atypical,
      label_three = label_symptom_type_nonanginal,
      label_four = label_symptom_type_dyspnoea,
      label_unknown = label_symptom_type_unknown,
      harmonise_label_one = "typical",
      harmonise_label_two = "atypical",
      harmonise_label_three = "nonanginal",
      harmonise_label_four = "dyspnoea",
      harmonise_label_unknown = NA
    )

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
#' @inheritParams calculate_esc_2024_num_of_rf
#' @inheritParams calculate_reeh_2019_basic_ptp
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
    have_diabetes,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_symptom_type_typical = c("typical"),
    label_symptom_type_atypical = c("atypical"),
    label_symptom_type_nonanginal = c("nonanginal"),
    label_symptom_type_dyspnoea = c("dyspnoea"),
    label_symptom_type_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_family_history_no = c("no"),
    label_have_family_history_yes = c("yes"),
    label_have_family_history_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN)
)
{
  check_if_positive(x = age, allow_na = TRUE)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_cat_missing = label_sex_unknown
  )

  # Ensure sex is valid and mapped to a unified group (male, female, NA)
  sex <- sex |>
    harmonise_two_labels(
      label_one = label_sex_male,
      label_two = label_sex_female,
      label_unknown = label_sex_unknown,
      harmonise_label_one = "male",
      harmonise_label_two = "female",
      harmonise_label_unknown = NA
    )

  sex <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 1L,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_dyslipidemia_no,
    label_have_dyslipidemia_yes,
    label_cat_missing = label_have_dyslipidemia_unknown
  )

  # Ensure have dyslipidemia is valid and mapped to a unified group (yes, no, NA)
  have_dyslipidemia <- have_dyslipidemia |>
    harmonise_two_labels(
      label_one = label_have_dyslipidemia_no,
      label_two = label_have_dyslipidemia_yes,
      label_unknown = label_have_dyslipidemia_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "no" ~ 0L,
    have_dyslipidemia == "yes" ~ 1L,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_family_history_no,
    label_have_family_history_yes,
    label_cat_missing = label_have_family_history_unknown
  )

  # Ensure have family history is valid and mapped to a unified group (yes, no, NA)
  have_family_history <- have_family_history |>
    harmonise_two_labels(
      label_one = label_have_family_history_no,
      label_two = label_have_family_history_yes,
      label_unknown = label_have_family_history_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_family_history <- dplyr::case_when(
    have_family_history == "no" ~ 0L,
    have_family_history == "yes" ~ 1L,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_diabetes_no,
    label_have_diabetes_yes,
    label_cat_missing = label_have_diabetes_unknown
  )

  # Ensure have diabetes is valid and mapped to a unified group (yes, no, NA)
  have_diabetes <- have_diabetes |>
    harmonise_two_labels(
      label_one = label_have_diabetes_no,
      label_two = label_have_diabetes_yes,
      label_unknown = label_have_diabetes_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_diabetes <- dplyr::case_when(
    have_diabetes == "no" ~ 0L,
    have_diabetes == "yes" ~ 1L,
    .default = NA_integer_
  )

  check_if_four_categories_are_mutually_exclusive(
    label_symptom_type_typical,
    label_symptom_type_atypical,
    label_symptom_type_nonanginal,
    label_symptom_type_dyspnoea,
    label_cat_missing = label_symptom_type_unknown
  )

  # Ensure symptom type is valid and mapped to a unified group
  # (typical, atypical, nonanginal, dyspnoea)
  symptom_type <- symptom_type |>
    harmonise_four_labels(
      label_one = label_symptom_type_typical,
      label_two = label_symptom_type_atypical,
      label_three = label_symptom_type_nonanginal,
      label_four = label_symptom_type_dyspnoea,
      label_unknown = label_symptom_type_unknown,
      harmonise_label_one = "typical",
      harmonise_label_two = "atypical",
      harmonise_label_three = "nonanginal",
      harmonise_label_four = "dyspnoea",
      harmonise_label_unknown = NA
    )

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
