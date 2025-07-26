#' @title Calculate 2022 LAH Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2022 Local Assessment of the Heart (LAH) clinical model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @inheritParams calculate_esc_2024_num_of_rf
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param chest_pain_type The value of variable in the parameters,
#' \code{label_cpt_nonanginal}, \code{label_cpt_atypical}, \code{label_cpt_typical} and
#' \code{label_cpt_unknown}.
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
    have_smoking_history,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_smoking_history_no = c("no"),
    label_have_smoking_history_yes = c("yes"),
    label_have_smoking_history_unknown = c(NA, NaN)
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
    label_have_smoking_history_no,
    label_have_smoking_history_yes,
    label_cat_missing = label_have_smoking_history_unknown
  )

  # Ensure have smoking history is valid and mapped to a unified group (yes, no, NA)
  have_smoking_history <- have_smoking_history |>
    harmonise_two_labels(
      label_one = label_have_smoking_history_no,
      label_two = label_have_smoking_history_yes,
      label_unknown = label_have_smoking_history_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_smoking_history <- dplyr::case_when(
      have_smoking_history == "no" ~ 0L,
      have_smoking_history == "yes" ~ 1L,
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
    label_have_hypertension_no,
    label_have_hypertension_yes,
    label_cat_missing = label_have_hypertension_unknown
  )

  # Ensure have hypertension is valid and mapped to a unified group (yes, no, NA)
  have_hypertension <- have_hypertension |>
    harmonise_two_labels(
      label_one = label_have_hypertension_no,
      label_two = label_have_hypertension_yes,
      label_unknown = label_have_hypertension_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_hypertension <- dplyr::case_when(
      have_hypertension == "no" ~ 0L,
      have_hypertension == "yes" ~ 1L,
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

  check_if_three_categories_are_mutually_exclusive(
    label_cpt_nonanginal,
    label_cpt_atypical,
    label_cpt_typical,
    label_cat_missing = label_cpt_unknown
  )

  # Ensure chest pain type is valid and mapped to a unified group
  # (nonanginal, atypical, typical)
  chest_pain_type <- chest_pain_type |>
    harmonise_three_labels(
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )

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
#' @inheritParams calculate_lah_2022_clinical_ptp
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
    coronary_calcium_score,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_smoking_history_no = c("no"),
    label_have_smoking_history_yes = c("yes"),
    label_have_smoking_history_unknown = c(NA, NaN)
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
    label_have_smoking_history_no,
    label_have_smoking_history_yes,
    label_cat_missing = label_have_smoking_history_unknown
  )

  # Ensure have smoking history is valid and mapped to a unified group (yes, no, NA)
  have_smoking_history <- have_smoking_history |>
    harmonise_two_labels(
      label_one = label_have_smoking_history_no,
      label_two = label_have_smoking_history_yes,
      label_unknown = label_have_smoking_history_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_smoking_history <- dplyr::case_when(
    have_smoking_history == "no" ~ 0L,
    have_smoking_history == "yes" ~ 1L,
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
    label_have_hypertension_no,
    label_have_hypertension_yes,
    label_cat_missing = label_have_hypertension_unknown
  )

  # Ensure have hypertension is valid and mapped to a unified group (yes, no, NA)
  have_hypertension <- have_hypertension |>
    harmonise_two_labels(
      label_one = label_have_hypertension_no,
      label_two = label_have_hypertension_yes,
      label_unknown = label_have_hypertension_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_hypertension <- dplyr::case_when(
    have_hypertension == "no" ~ 0L,
    have_hypertension == "yes" ~ 1L,
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

  check_if_three_categories_are_mutually_exclusive(
    label_cpt_nonanginal,
    label_cpt_atypical,
    label_cpt_typical,
    label_cat_missing = label_cpt_unknown
  )

  # Ensure chest pain type is valid and mapped to a unified group
  # (nonanginal, atypical, typical)
  chest_pain_type <- chest_pain_type |>
    harmonise_three_labels(
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_unknown = NA
    )

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
