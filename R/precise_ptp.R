#' @title Calculate 2021 PRECISE Simple PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2021 Predictive Risk scorE for CAD In Southeast Asians with chEst pain
#' (PRECISE) simple model.
#' @inheritParams calculate_lah_2022_clinical_ptp
#' @param smoking_history_type The value of variable in the parameters
#' \code{label_smoking_history_type_current}, \code{label_smoking_history_type_past},
#' \code{label_smoking_history_type_none} and \code{label_smoking_history_type_unknown}
#' @param have_neck_radiation The value of variable in the parameters
#' \code{label_have_neck_radiation_no}, \code{label_have_neck_radiation_yes}
#' and \code{label_have_neck_radiation_unknown}.
#' @param label_smoking_history_type_current Label(s) for patient who is a current smoker.
#' Default: \code{c("current")}
#' @param label_smoking_history_type_past Label(s) for patient who is a past smoker.
#' Default: \code{c("past")}
#' @param label_smoking_history_type_none Label(s) for patient who is a non-smoker.
#' Default: \code{c("none")}
#' @param label_smoking_history_type_unknown Label(s) for patient with unknown smoking history.
#' Default: \code{c(NA, NaN)}
#' @param label_have_neck_radiation_no Label(s) for patient without
#' chest pain radiating to the neck.
#' Default: \code{c("no")}
#' @param label_have_neck_radiation_yes Label(s) for patient with
#' chest pain radiating to the neck.
#' Default: \code{c("yes")}
#' @param label_have_neck_radiation_unknown Label(s) for patient with unknown
#' chest pain radiating to the neck
#' Default: \code{c(NA, NaN)}
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2021 Predictive Risk scorE for CAD In Southeast Asians
#' with chEst pain (PRECISE) simple model.
#' @details The predictive model is based on
#' patients a mixed Asian cohort within Singapore with stable chest pain.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' # radiating to the neck, has diabetes
#' # but no hypertension and a non-smoker
#'
#' calculate_precise_2021_simple_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_neck_radiation = "yes",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     smoking_history_type = "none"
#'
#' )
#' @rdname calculate_precise_2021_simple_ptp
#' @export
calculate_precise_2021_simple_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_neck_radiation,
    have_diabetes,
    have_hypertension,
    smoking_history_type,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN),
    label_have_neck_radiation_no = c("no"),
    label_have_neck_radiation_yes = c("yes"),
    label_have_neck_radiation_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_smoking_history_type_current = c("current"),
    label_smoking_history_type_past = c("past"),
    label_smoking_history_type_none = c("none"),
    label_smoking_history_type_unknown = c(NA, NaN)
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
    label_have_neck_radiation_no,
    label_have_neck_radiation_yes,
    label_cat_missing = label_have_neck_radiation_unknown
  )

  # Ensure have neck radiation is valid and mapped to a unified group (yes, no, NA)
  have_neck_radiation <- have_neck_radiation |>
    harmonise_two_labels(
      label_one = label_have_neck_radiation_no,
      label_two = label_have_neck_radiation_yes,
      label_unknown = label_have_neck_radiation_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_neck_radiation <- dplyr::case_when(
    have_neck_radiation == "no" ~ 0L,
    have_neck_radiation == "yes" ~ 1L,
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

  check_if_three_categories_are_mutually_exclusive(
    label_smoking_history_type_current,
    label_smoking_history_type_past,
    label_smoking_history_type_none,
    label_cat_missing = label_smoking_history_type_unknown
  )

  # Ensure smoking history type is valid and mapped to a unified group
  # (current, past, none, unknown)
  smoking_history_type <- smoking_history_type |>
    harmonise_three_labels(
      label_one = label_smoking_history_type_current,
      label_two = label_smoking_history_type_past,
      label_three = label_smoking_history_type_none,
      label_unknown = label_smoking_history_type_unknown,
      harmonise_label_one = "current",
      harmonise_label_two = "past",
      harmonise_label_three = "none",
      harmonise_label_unknown = NA
    )

  is_current_smoker <- dplyr::case_when(
    smoking_history_type %in% c("past", "none") ~ 0L,
    smoking_history_type == "current" ~ 1L,
    .default = NA_integer_
  )

  is_past_smoker <- dplyr::case_when(
    smoking_history_type %in% c("current", "none") ~ 0L,
    smoking_history_type == "past" ~ 1L,
    .default = NA_integer_
  )

  precise_2021_simple_ptp <- 1 /
    (1 + exp(-(-6.632 +
              (0.035  * age) +
              (1.694  * sex) +
              (0.613  * have_diabetes) +
              (0.542  * have_hypertension) +
              (0.791  * is_current_smoker) +
              (0.063  * is_past_smoker) +
              (1.395  * have_typical_chest_pain) +
              (0.877  * have_atypical_chest_pain) +
              (1.143  * have_neck_radiation)
    )
    )
    )

  return(precise_2021_simple_ptp)

}


#' @title Calculate 2021 PRECISE Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2021 Predictive Risk scorE for CAD In Southeast Asians with chEst pain
#' (PRECISE) clinical model.
#' @inheritParams calculate_precise_2021_simple_ptp
#' @param have_q_waves The value of variable in the parameters
#' \code{label_have_q_waves_no}, \code{label_have_q_waves_yes}
#' and \code{label_have_q_waves_unknown}.
#' @param have_st_t_changes The value of variable in the parameters
#' \code{label_have_st_t_changes_no}, \code{label_have_st_t_changes_yes}
#' and \code{label_have_st_t_changes_unknown}.
#' @param label_have_q_waves_no Label(s) for patient not having Q waves on ECG.
#' Default: \code{c("no")}
#' @param label_have_q_waves_yes Label(s) for patient having Q waves on ECG.
#' Default: \code{c("yes")}
#' @param label_have_q_waves_unknown Label(s) for patient with unknown Q waves on ECG.
#' Default: \code{c(NA, NaN)}
#' @param label_have_st_t_changes_no Label(s) for patient not having ST-T changes on ECG.
#' Default: \code{c("no")}
#' @param label_have_st_t_changes_yes Label(s) for patient having ST-T changes on ECG.
#' Default: \code{c("yes")}
#' @param label_have_st_t_changes_unknown Label(s) for patient with unknown ST-T changes on ECG.
#' Default: \code{c(NA, NaN)}
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2021 Predictive Risk scorE for CAD In Southeast Asians
#' with chEst pain (PRECISE) clinical model.
#' @details The predictive model is based on
#' patients a mixed Asian cohort within Singapore with stable chest pain.
#'
#' @examples
#' # 40 year old female with typical chest pain
#' # radiating to the neck, has diabetes
#' # but no hypertension and a non-smoker.
#' # She has Q waves but no ST-T changes on ECG.
#'
#' calculate_precise_2021_clinical_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     have_neck_radiation = "yes",
#'     have_diabetes = "yes",
#'     have_hypertension = "no",
#'     smoking_history_type = "none",
#'     have_q_waves = "yes",
#'     have_st_t_changes = "no"
#' )
#' @rdname calculate_precise_2021_clinical_ptp
#' @export
calculate_precise_2021_clinical_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_neck_radiation,
    have_diabetes,
    have_hypertension,
    smoking_history_type,
    have_q_waves,
    have_st_t_changes,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN),
    label_have_neck_radiation_no = c("no"),
    label_have_neck_radiation_yes = c("yes"),
    label_have_neck_radiation_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_smoking_history_type_current = c("current"),
    label_smoking_history_type_past = c("past"),
    label_smoking_history_type_none = c("none"),
    label_smoking_history_type_unknown = c(NA, NaN),
    label_have_q_waves_no = c("no"),
    label_have_q_waves_yes = c("yes"),
    label_have_q_waves_unknown = c(NA, NaN),
    label_have_st_t_changes_no = c("no"),
    label_have_st_t_changes_yes = c("yes"),
    label_have_st_t_changes_unknown = c(NA, NaN)
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
    label_have_neck_radiation_no,
    label_have_neck_radiation_yes,
    label_cat_missing = label_have_neck_radiation_unknown
  )

  # Ensure have neck radiation is valid and mapped to a unified group (yes, no, NA)
  have_neck_radiation <- have_neck_radiation |>
    harmonise_two_labels(
      label_one = label_have_neck_radiation_no,
      label_two = label_have_neck_radiation_yes,
      label_unknown = label_have_neck_radiation_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_neck_radiation <- dplyr::case_when(
    have_neck_radiation == "no" ~ 0L,
    have_neck_radiation == "yes" ~ 1L,
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

  check_if_three_categories_are_mutually_exclusive(
    label_smoking_history_type_current,
    label_smoking_history_type_past,
    label_smoking_history_type_none,
    label_cat_missing = label_smoking_history_type_unknown
  )

  # Ensure smoking history type is valid and mapped to a unified group
  # (current, past, none, unknown)
  smoking_history_type <- smoking_history_type |>
    harmonise_three_labels(
      label_one = label_smoking_history_type_current,
      label_two = label_smoking_history_type_past,
      label_three = label_smoking_history_type_none,
      label_unknown = label_smoking_history_type_unknown,
      harmonise_label_one = "current",
      harmonise_label_two = "past",
      harmonise_label_three = "none",
      harmonise_label_unknown = NA
    )

  is_current_smoker <- dplyr::case_when(
    smoking_history_type %in% c("past", "none") ~ 0L,
    smoking_history_type == "current" ~ 1L,
    .default = NA_integer_
  )

  is_past_smoker <- dplyr::case_when(
    smoking_history_type %in% c("current", "none") ~ 0L,
    smoking_history_type == "past" ~ 1L,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_q_waves_no,
    label_have_q_waves_yes,
    label_cat_missing = label_have_q_waves_unknown
  )

  # Ensure have q waves is valid and mapped to a unified group (yes, no, NA)
  have_q_waves <- have_q_waves |>
    harmonise_two_labels(
      label_one = label_have_q_waves_no,
      label_two = label_have_q_waves_yes,
      label_unknown = label_have_q_waves_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_q_waves <- dplyr::case_when(
    have_q_waves == "no" ~ 0L,
    have_q_waves == "yes" ~ 1L,
    .default = NA_integer_
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_st_t_changes_no,
    label_have_st_t_changes_yes,
    label_cat_missing = label_have_st_t_changes_unknown
  )

  # Ensure have st t changes is valid and mapped to a unified group (yes, no, NA)
  have_st_t_changes <- have_st_t_changes |>
    harmonise_two_labels(
      label_one = label_have_st_t_changes_no,
      label_two = label_have_st_t_changes_yes,
      label_unknown = label_have_st_t_changes_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_st_t_changes <- dplyr::case_when(
    have_st_t_changes == "no" ~ 0L,
    have_st_t_changes == "yes" ~ 1L,
    .default = NA_integer_
  )

  precise_2021_clinical_ptp <- 1 /
    (1 + exp(-(-6.714 +
              (0.033  * age) +
              (1.75   * sex) +
              (0.597  * have_diabetes) +
              (0.497  * have_hypertension) +
              (0.733  * is_current_smoker) +
              (0.07   * is_past_smoker) +
              (1.374  * have_typical_chest_pain) +
              (0.875  * have_atypical_chest_pain) +
              (1.157  * have_neck_radiation) +
              (1.02   * have_q_waves) +
              (0.552  * have_st_t_changes)
    )
    )
    )

  return(precise_2021_clinical_ptp)

}
