
#' @title Calculate 2025 Rasmussen \ifelse{html}{\out{RF-CL<sub>CCTA</sub>}}{\eqn{RF-CL_{CCTA}}} PTP model for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2025 Rasmussen et. al. Risk Factor-Weighted Clinical Likelihood CCTA (\ifelse{html}{\out{RF-CL<sub>CCTA</sub>}}{\eqn{RF-CL_{CCTA}}}) model.
#' Obstructive CAD was defined as a stenosis causing ≥ 50% diameter stenosis
#' on coronary CTA.
#' @inheritParams calculate_esc_2024_fig_4_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2025 Rasmussen et. al. Risk Factor-Weighted Clinical Likelihood CCTA (\ifelse{html}{\out{RF-CL<sub>CCTA</sub>}}{\eqn{RF-CL_{CCTA}}}) model.
#' @details The predictive model is based on 38,269 symptomatic
#' patients with CACS and interpretable CCTA from 2008 to 2017 across 13 hospitals in Western Denmark. These
#' patients are registered under the Western Denmark Heart Registry. It is then further
#' calibrated against observed obstructive CAD at CCTA and corresponding prevalence of
#' diagnosed obstructive CAD at Invasive Coronary Angiography.
#'
#' Model formula used is from Development Table 2.
#'
#' It is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' -7.1076\quad+ \\\\
#' (1.1332 * sex\_is\_male)\quad+ \\\\
#' (0.072819 * age)\quad+ \\\\
#' (1.4252 * have\_typical\_chest\_pain)\quad+ \\\\
#' (-0.55152 * have\_nonanginal\_chest\_pain)\quad+ \\\\
#' (1.1145 * risk\_factor\_subgroups)\quad+ \\\\
#' (-0.005578 * age * have\_typical\_chest\_pain)\quad+ \\\\
#' (-0.0079662 * age * risk\_factor\_subgroups)\quad+ \\\\
#' (-0.11213 * have\_typical\_chest\_pain * risk\_factor\_subgroups)\quad+ \\\\
#' (-0.18948 * sex\_is\_male * risk\_factor\_subgroups)
#' \end{array}
#' }
#' @examples
#' # 40 year old Male with nonanginal chest pain
#' calculate_rasmussen_2025_rf_cl_ccta_ptp(
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
#' @rdname calculate_rasmussen_2025_rf_cl_ccta_ptp
#' @export
calculate_rasmussen_2025_rf_cl_ccta_ptp <- function(
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

  sex_male <- dplyr::case_when(
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
    symptom_score == 0  ~ 0L,
    symptom_score == 1  ~ 0L,
    symptom_score == 2  ~ 0L,
    symptom_score == 3  ~ 1L,
    .default = NA_integer_
  )

  # In the supplementary document, patients with no chest pain or have one chest pain symptom
  # will be grouped as nonanginal chest pain
  have_nonanginal_chest_pain <- dplyr::case_when(
    symptom_score == 0  ~ 1L,
    symptom_score == 1  ~ 1L,
    symptom_score == 2  ~ 0L,
    symptom_score == 3  ~ 0L,
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
    dplyr::between(num_of_rf, 0, 1) ~ 1L,
    dplyr::between(num_of_rf, 2, 3) ~ 2L,
    dplyr::between(num_of_rf, 4, 5) ~ 3L,
    .default = NA_integer_
  )

  rasmussen_2025_rf_cl_ccta_ptp <- 1 /
    (1 + exp(-(-7.1076 +
              ( 1.1332 * sex_male) +
              ( 0.072819 * age) +
              ( 1.4252 * have_typical_chest_pain) +
              (-0.55152 * have_nonanginal_chest_pain) +
              ( 1.1145 * rf_group) +
              (-0.005578 * age * have_typical_chest_pain) +
              (-0.0079662 * age * rf_group) +
              (-0.11213 * have_typical_chest_pain * rf_group) +
              (-0.18948 * sex_male * rf_group))
    )
    )

  return(rasmussen_2025_rf_cl_ccta_ptp)

}

#' @title Calculate 2025 Rasmussen \ifelse{html}{\out{CACS-CL<sub>CCTA</sub>}}{\eqn{CACS-CL_{CCTA}}} PTP model for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on
#' 2025 Rasmussen et. al.
#' Coronary Artery Calcium Score-Weighted Clinical Likelihood CCTA (\ifelse{html}{\out{RF-CL<sub>CCTA</sub>}}{\eqn{RF-CL_{CCTA}}}) model.
#' Obstructive CAD was defined as a stenosis causing ≥ 50% diameter stenosis
#' on coronary CTA.
#' @inheritParams calculate_rasmussen_2025_rf_cl_ccta_ptp
#' @inheritParams calculate_lah_2022_extended_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2025 Rasmussen et. al.
#' Coronary Artery Calcium Score-Weighted Clinical Likelihood CCTA (\ifelse{html}{\out{RF-CL<sub>CCTA</sub>}}{\eqn{RF-CL_{CCTA}}}) model.
#'
#' Model formula used is from Development Table 2.
#'
#' It is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' 0.014533\quad+ \\\\
#' (0.27093 * rf\_ptp)\quad+ \\\\
#' (0.047056 * cacs\_1\_to\_9)\quad+ \\\\
#' (0.11890 * cacs\_10\_to\_99)\quad+ \\\\
#' (0.34417 * cacs\_100\_to\_399)\quad+ \\\\
#' (0.5881 * cacs\_400\_to\_999)\quad+ \\\\
#' (0.73892 * cacs\_1000\_or\_more)\quad+ \\\\
#' (0.054423 * rf\_ptp * cacs\_1\_to\_9)\quad+ \\\\
#' (0.14347 * rf\_ptp * cacs\_10\_to\_99)\quad+ \\\\
#' (0.18439 * rf\_ptp * cacs\_100\_to\_399)\quad+ \\\\
#' (0.085774 * rf\_ptp * cacs\_400\_to\_999)\quad+ \\\\
#' (-0.081776 * rf\_ptp * cacs\_1000\_or\_more)
#' \end{array}
#' }
#' @examples
#' # 40 year old Male with nonanginal chest pain and coronary calcium score of 0
#' calculate_rasmussen_2025_cacs_cl_ccta_ptp(
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
#' @rdname calculate_rasmussen_2025_cacs_cl_ccta_ptp
#' @export
calculate_rasmussen_2025_cacs_cl_ccta_ptp <- function(
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

  rasmussen_2025_rf_cl_ccta_ptp <- calculate_rasmussen_2025_rf_cl_ccta_ptp(
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

  rasmussen_2025_cacs_cl_ccta_ptp = 0.014533 +
    (rasmussen_2025_rf_cl_ccta_ptp * 0.27093) +
    (cacs_1_to_9 * 0.047056) +
    (cacs_10_to_99 * 0.11890) +
    (cacs_100_to_399 * 0.34417) +
    (cacs_400_to_999 * 0.5881) +
    (cacs_1000_or_more * 0.73892) +
    (rasmussen_2025_rf_cl_ccta_ptp * cacs_1_to_9 * 0.054423) +
    (rasmussen_2025_rf_cl_ccta_ptp * cacs_10_to_99 * 0.14347) +
    (rasmussen_2025_rf_cl_ccta_ptp * cacs_100_to_399 * 0.18439) +
    (rasmussen_2025_rf_cl_ccta_ptp * cacs_400_to_999 * 0.085774) +
    (rasmussen_2025_rf_cl_ccta_ptp * cacs_1000_or_more * -0.081776)

  return(rasmussen_2025_cacs_cl_ccta_ptp)

}
