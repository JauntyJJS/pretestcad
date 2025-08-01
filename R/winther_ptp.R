#' @title Calculate 2020 Winther Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2020 Winther et. al. basic model (Basic_PTP).
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2020 Winther et. al. basic model (Basic_PTP).
#' @details The predictive model is based on > 40000 symptomatic
#' patients from 2008 to 2017 from 13 hospitals in Western Denmark. These
#' patients are registered under the Western Denmark Heart Registry.
#'
#' @examples
#' # 40 year old Male with typical chest pain
#' calculate_winther_2020_basic_ptp(
#'     age = 40,
#'     sex = "male",
#'     chest_pain_type = "typical"
#' )
#'
#' # 40 year old Male with nonanginal chest pain
#' calculate_winther_2020_basic_ptp(
#'     age = 40,
#'     sex = "male",
#'     chest_pain_type = "nonanginal"
#' )
#' @rdname calculate_winther_2020_basic_ptp
#' @export
calculate_winther_2020_basic_ptp <- function(
    age,
    sex,
    chest_pain_type,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_no_chest_pain = c("no chest pain"),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN)
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
    label_cpt_no_chest_pain,
    label_cpt_nonanginal,
    label_cpt_atypical,
    label_cpt_typical,
    label_cat_missing = label_cpt_unknown
  )

  # Ensure chest pain type is valid and mapped to a unified group
  # (no chest pain, nonanginal, atypical, typical)
  chest_pain_type <- chest_pain_type |>
    harmonise_four_labels(
      label_one = label_cpt_no_chest_pain,
      label_two = label_cpt_nonanginal,
      label_three = label_cpt_atypical,
      label_four = label_cpt_typical,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "no chest pain",
      harmonise_label_two = "nonanginal",
      harmonise_label_three = "atypical",
      harmonise_label_four = "typical",
      harmonise_label_unknown = NA
    )

  # In the supplementary document, patients with no chest pain or have one chest pain symptom
  # will be grouped as nonanginal chest pain
  have_nonanginal_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "atypical") ~ 0L,
    chest_pain_type %in% c("no chest pain", "nonanginal") ~ 1L,
    .default = NA_integer_
  )

  # In the supplementary document, patients with no chest pain or have one chest pain symptom
  # will be grouped as nonanginal chest pain
  have_typical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("no chest pain", "atypical", "nonanginal") ~ 0L,
    chest_pain_type == "typical" ~ 1L,
    .default = NA_integer_
  )

  winther_2020_basic_ptp <- 1 /
    (1 + exp(-(-7.0753 +
              ( 1.2308 * sex) +
              ( 0.0642 * age) +
              ( 2.2501 * have_typical_chest_pain) +
              (-0.5095 * have_nonanginal_chest_pain) +
              (-0.0191 * age * have_typical_chest_pain)
    )
    )
    )

  return(winther_2020_basic_ptp)
}


#' @title Calculate 2020 Winther RF-CL PTP model for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2020 Winther et. al. Risk Factor-Weighted Clinical Likelihood (RF-CL) model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2020 Winther et. al. Risk Factor-Weighted Clinical Likelihood (RF-CL) model.
#' @details The predictive model is based on > 40000 symptomatic
#' patients from 2008 to 2017 from 13 hospitals in Western Denmark. These
#' patients are registered under the Western Denmark Heart Registry.
#'
#' @examples
#' # 40 year old Male with nonanginal chest pain
#' calculate_winther_2020_rf_cl_ptp(
#'     age = 40,
#'     sex = "male",
#'     chest_pain_type = "no chest pain",
#'     have_dyspnoea = "no",
#'     have_family_history = "no",
#'     have_smoking_history = "no",
#'     have_dyslipidemia = "no",
#'     have_hypertension = "no",
#'     have_diabetes = "no",
#'     allow_na_symptom_score = TRUE,
#'     max_na_num_of_rf = 0
#' )
#' @rdname calculate_winther_2020_rf_cl_ptp
#' @export
calculate_winther_2020_rf_cl_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_dyspnoea,
    have_family_history,
    have_smoking_history,
    have_dyslipidemia,
    have_hypertension,
    have_diabetes,
    allow_na_symptom_score = TRUE,
    max_na_num_of_rf = 0,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_dyspnoea_no = c("no"),
    label_have_dyspnoea_yes = c("yes"),
    label_have_dyspnoea_unknown = c(NA, NaN),
    label_cpt_no_chest_pain = c("no chest pain"),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN),
    label_have_family_history_no = c("no"),
    label_have_family_history_yes = c("yes"),
    label_have_family_history_unknown = c(NA, NaN),
    label_have_smoking_history_no = c("no"),
    label_have_smoking_history_yes = c("yes"),
    label_have_smoking_history_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
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

  symptom_score <- calculate_esc_2024_symptom_score(
    chest_pain_type = chest_pain_type,
    have_dyspnoea = have_dyspnoea,
    allow_na = allow_na_symptom_score,
    label_have_dyspnoea_no = label_have_dyspnoea_no,
    label_have_dyspnoea_yes = label_have_dyspnoea_yes,
    label_have_dyspnoea_unknown = label_have_dyspnoea_unknown,
    label_cpt_no_chest_pain = label_cpt_no_chest_pain,
    label_cpt_nonanginal = label_cpt_nonanginal,
    label_cpt_atypical = label_cpt_atypical,
    label_cpt_typical = label_cpt_typical,
    label_cpt_unknown = label_cpt_unknown
  )

  have_typical_chest_pain <- dplyr::case_when(
    symptom_score == 0  ~ 0,
    symptom_score == 1  ~ 0,
    symptom_score == 2  ~ 0,
    symptom_score == 3  ~ 1,
    .default = NA_integer_
  )

  # In the supplementary document, patients with no chest pain or have one chest pain symptom
  # will be grouped as nonanginal chest pain
  have_nonanginal_chest_pain <- dplyr::case_when(
    symptom_score == 0  ~ 1,
    symptom_score == 1  ~ 1,
    symptom_score == 2  ~ 0,
    symptom_score == 3  ~ 0,
    .default = NA_integer_
  )

  num_of_rf <- calculate_esc_2024_num_of_rf(
    have_family_history = have_family_history,
    have_smoking_history = have_smoking_history,
    have_dyslipidemia = have_dyslipidemia,
    have_hypertension = have_hypertension,
    have_diabetes = have_diabetes,
    max_na = max_na_num_of_rf,
    label_have_family_history_no = label_have_family_history_no,
    label_have_family_history_yes = label_have_family_history_yes,
    label_have_family_history_unknown = label_have_family_history_unknown,
    label_have_smoking_history_no = label_have_smoking_history_no,
    label_have_smoking_history_yes = label_have_smoking_history_yes,
    label_have_smoking_history_unknown = label_have_smoking_history_unknown,
    label_have_dyslipidemia_no = label_have_dyslipidemia_no,
    label_have_dyslipidemia_yes = label_have_dyslipidemia_yes,
    label_have_dyslipidemia_unknown = label_have_dyslipidemia_unknown,
    label_have_hypertension_no = label_have_hypertension_no,
    label_have_hypertension_yes = label_have_hypertension_yes,
    label_have_hypertension_unknown = label_have_hypertension_unknown,
    label_have_diabetes_no = label_have_diabetes_no,
    label_have_diabetes_yes = label_have_diabetes_yes,
    label_have_diabetes_unknown = label_have_diabetes_unknown
  )

  rf_group <- dplyr::case_when(
    dplyr::between(num_of_rf, 0, 1) ~ 1,
    dplyr::between(num_of_rf, 2, 3) ~ 2,
    dplyr::between(num_of_rf, 4, 5) ~ 3,
    .default = NA_integer_
  )

  winther_2020_rf_cl_ptp <- 1 /
    (1 + exp(-(-9.5260 +
                 ( 1.6128 * sex) +
                 ( 0.0844 * age) +
                 ( 2.7112 * have_typical_chest_pain) +
                 (-0.4675 * have_nonanginal_chest_pain) +
                 ( 1.4940 * rf_group) +
                 (-0.0187 * age * have_typical_chest_pain) +
                 (-0.0131 * age * rf_group) +
                 (-0.2799 * have_typical_chest_pain * rf_group) +
                 (-0.2091 * sex * rf_group))
    )
    )

  return(winther_2020_rf_cl_ptp)

}

#' @title Calculate 2020 Winther CACS-CL PTP model for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on
#' 2020 Winther et. al.
#' Coronary Artery Calcium Score-Weighted Clinical Likelihood (CACS-CL) model.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @inheritParams calculate_lah_2022_extended_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2020 Winther et. al.
#' Coronary Artery Calcium Score-Weighted Clinical Likelihood (CACS-CL) model.
#' @details The predictive model is based on > 40000 symptomatic
#' patients from 2008 to 2017 from 13 hospitals in Western Denmark. These
#' patients are registered under the Western Denmark Heart Registry.
#' @examples
#' # 40 year old Male with nonanginal chest pain and coronary calcium score of 0
#' calculate_winther_2020_cacs_cl_ptp(
#'     age = 40,
#'     sex = "male",
#'     chest_pain_type = "no chest pain",
#'     have_dyspnoea = "no",
#'     have_family_history = "no",
#'     have_smoking_history = "no",
#'     have_dyslipidemia = "no",
#'     have_hypertension = "no",
#'     have_diabetes = "no",
#'     coronary_calcium_score = 0,
#'     allow_na_symptom_score = TRUE,
#'     max_na_num_of_rf = 0
#' )
#' @rdname calculate_winther_2020_cacs_cl_ptp
#' @export
calculate_winther_2020_cacs_cl_ptp <- function(
    age,
    sex,
    chest_pain_type,
    have_dyspnoea,
    have_family_history,
    have_smoking_history,
    have_dyslipidemia,
    have_hypertension,
    have_diabetes,
    coronary_calcium_score,
    allow_na_symptom_score = TRUE,
    max_na_num_of_rf = 0,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_dyspnoea_no = c("no"),
    label_have_dyspnoea_yes = c("yes"),
    label_have_dyspnoea_unknown = c(NA, NaN),
    label_cpt_no_chest_pain = c("no chest pain"),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN),
    label_have_family_history_no = c("no"),
    label_have_family_history_yes = c("yes"),
    label_have_family_history_unknown = c(NA, NaN),
    label_have_smoking_history_no = c("no"),
    label_have_smoking_history_yes = c("yes"),
    label_have_smoking_history_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN)
)
{

  check_if_non_negative(x = coronary_calcium_score, allow_na = TRUE)
  check_if_integer(x = coronary_calcium_score, allow_na = TRUE)

  winther_2020_rf_cl_ptp <- calculate_winther_2020_rf_cl_ptp(
    age = age,
    sex = sex,
    chest_pain_type = chest_pain_type,
    have_dyspnoea = have_dyspnoea,
    have_family_history = have_family_history,
    have_smoking_history = have_smoking_history,
    have_dyslipidemia = have_dyslipidemia,
    have_hypertension = have_hypertension,
    have_diabetes = have_diabetes,
    allow_na_symptom_score = allow_na_symptom_score,
    max_na_num_of_rf = max_na_num_of_rf,
    label_sex_male = label_sex_male,
    label_sex_female = label_sex_female,
    label_sex_unknown = label_sex_unknown,
    label_have_dyspnoea_no = label_have_dyspnoea_no,
    label_have_dyspnoea_yes = label_have_dyspnoea_yes,
    label_have_dyspnoea_unknown = label_have_dyspnoea_unknown,
    label_cpt_no_chest_pain = label_cpt_no_chest_pain,
    label_cpt_nonanginal = label_cpt_nonanginal,
    label_cpt_atypical = label_cpt_atypical,
    label_cpt_typical = label_cpt_typical,
    label_cpt_unknown = label_cpt_unknown,
    label_have_family_history_no = label_have_family_history_no,
    label_have_family_history_yes = label_have_family_history_yes,
    label_have_family_history_unknown = label_have_family_history_unknown,
    label_have_smoking_history_no = label_have_smoking_history_no,
    label_have_smoking_history_yes = label_have_smoking_history_yes,
    label_have_smoking_history_unknown = label_have_smoking_history_unknown,
    label_have_dyslipidemia_no = label_have_dyslipidemia_no,
    label_have_dyslipidemia_yes = label_have_dyslipidemia_yes,
    label_have_dyslipidemia_unknown = label_have_dyslipidemia_unknown,
    label_have_hypertension_no = label_have_hypertension_no,
    label_have_hypertension_yes = label_have_hypertension_yes,
    label_have_hypertension_unknown = label_have_hypertension_unknown,
    label_have_diabetes_no = label_have_diabetes_no,
    label_have_diabetes_yes = label_have_diabetes_yes,
    label_have_diabetes_unknown = label_have_diabetes_unknown
  )

  cacs_1_to_9 <- dplyr::case_when(
    coronary_calcium_score >= 1 & coronary_calcium_score <= 9 ~ 1,
    coronary_calcium_score == 0 ~ 0,
    coronary_calcium_score >= 10 ~ 0,
    .default = NA_integer_
  )

  cacs_10_to_99 <- dplyr::case_when(
    coronary_calcium_score >= 10 & coronary_calcium_score <= 99 ~ 1,
    coronary_calcium_score >= 0 & coronary_calcium_score <= 9 ~ 0,
    coronary_calcium_score >= 100 ~ 0,
    .default = NA_integer_
  )

  cacs_100_to_399 <- dplyr::case_when(
    coronary_calcium_score >= 100 & coronary_calcium_score <= 399 ~ 1,
    coronary_calcium_score >= 0 & coronary_calcium_score <= 99 ~ 0,
    coronary_calcium_score >= 400 ~ 0,
    .default = NA_integer_
  )

  cacs_400_to_999 <- dplyr::case_when(
    coronary_calcium_score >= 400 & coronary_calcium_score <= 999 ~ 1,
    coronary_calcium_score >= 0 & coronary_calcium_score <= 399 ~ 0,
    coronary_calcium_score >= 1000 ~ 0,
    .default = NA_integer_
  )

  cacs_1000_or_more <- dplyr::case_when(
    coronary_calcium_score >= 1000 ~ 1,
    coronary_calcium_score >= 0 & coronary_calcium_score <= 999 ~ 0,
    .default = NA_integer_
  )

  winther_2020_cacs_cl_ptp = 0.0013 +
    (winther_2020_rf_cl_ptp * 0.2021) +
    (cacs_1_to_9 * 0.0082) +
    (cacs_10_to_99 * 0.0238) +
    (cacs_100_to_399 * 0.1131) +
    (cacs_400_to_999 * 0.2306) +
    (cacs_1000_or_more * 0.4040) +
    (winther_2020_rf_cl_ptp * cacs_1_to_9 * 0.1311) +
    (winther_2020_rf_cl_ptp * cacs_10_to_99 * 0.2909) +
    (winther_2020_rf_cl_ptp * cacs_100_to_399 * 0.4077) +
    (winther_2020_rf_cl_ptp * cacs_400_to_999 * 0.4658) +
    (winther_2020_rf_cl_ptp * cacs_1000_or_more * 0.4489)

  return(winther_2020_cacs_cl_ptp)

}
