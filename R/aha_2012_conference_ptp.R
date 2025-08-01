#' @title Calculate ACCF/AHA/ACP/AATS/PCNA/SCAI/STS 2012 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' American College of Cardiology Foundation,
#' American Heart Association,
#' American College of Physicians,
#' American Association for Thoracic Surgery,
#' Preventive Cardiovascular Nurses Association,
#' Society for Cardiovascular Angiography and Interventions,
#' and Society of Thoracic Surgeons 2012 guidelines.
#' @inheritParams calculate_esc_2019_ptp
#' @param chest_pain_type The value of variable in the parameters,
#' \code{label_cpt_nonanginal}, \code{label_cpt_atypical}, \code{label_cpt_typical} and
#' \code{label_cpt_unknown}.
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("numeric", "percentage")
#' \itemize{
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#'
#' @return An integer or percentage representing the patient's PTP for obstructive CAD
#' based on the ACCF/AHA/ACP/AATS/PCNA/SCAI/STS 2012 guidelines.
#'
#' @details The predictive model used to create the guidelines are based on
#' patients from the Diamond and Forrester and the Coronary Artery Surgery Study.
#'
#' @examples
#' # 35 year old female with typical chest pain
#' calculate_aha_2012_tbl_9_ptp(
#'     age = 35,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     output = "percentage"
#' )
#'
#' # 65 year old male with nonanginal chest pain
#' calculate_aha_2012_tbl_9_ptp(
#'     age = 65,
#'     sex = "male",
#'     chest_pain_type = "nonanginal",
#'     output = "percentage"
#' )
#' @rdname calculate_aha_2012_tbl_9_ptp
#' @export
calculate_aha_2012_tbl_9_ptp <- function(
    age,
    sex,
    chest_pain_type,
    output = c("numeric", "percentage"),
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_unknown = c(NA, NaN)

)
{
  check_if_positive(x = age, allow_na = TRUE)
  check_if_integer(x = age, allow_na = TRUE)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_cat_missing = label_sex_unknown
  )

  check_if_three_categories_are_mutually_exclusive(
    label_cpt_nonanginal,
    label_cpt_atypical,
    label_cpt_typical,
    label_cat_missing = label_cpt_unknown
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

  # Ensure chest pain type is valid and mapped to a unified group
  # (no chest pain, nonanginal, atypical, typical)
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

  output <- output |>
    rlang::arg_match()

  # TODO: Work on case when age < 30

  age_group <- dplyr::case_when(
    dplyr::between(age, 30, 39) ~ "30-39",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    .default = NA_character_
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "male"   ~ 76L,
    age_group == "30-39" & chest_pain_type == "typical"    & sex == "female" ~ 26L,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "male"   ~ 87L,
    age_group == "40-49" & chest_pain_type == "typical"    & sex == "female" ~ 55L,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "male"   ~ 93L,
    age_group == "50-59" & chest_pain_type == "typical"    & sex == "female" ~ 73L,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "male"   ~ 94L,
    age_group == "60-69" & chest_pain_type == "typical"    & sex == "female" ~ 86L,

    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "male"   ~ 34L,
    age_group == "30-39" & chest_pain_type == "atypical"   & sex == "female" ~ 12L,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "male"   ~ 51L,
    age_group == "40-49" & chest_pain_type == "atypical"   & sex == "female" ~ 22L,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "male"   ~ 65L,
    age_group == "50-59" & chest_pain_type == "atypical"   & sex == "female" ~ 31L,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "male"   ~ 72L,
    age_group == "60-69" & chest_pain_type == "atypical"   & sex == "female" ~ 51L,

    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "male"   ~ 4L,
    age_group == "30-39" & chest_pain_type == "nonanginal" & sex == "female" ~ 2L,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "male"   ~ 13L,
    age_group == "40-49" & chest_pain_type == "nonanginal" & sex == "female" ~ 3L,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "male"   ~ 20L,
    age_group == "50-59" & chest_pain_type == "nonanginal" & sex == "female" ~ 7L,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "male"   ~ 27L,
    age_group == "60-69" & chest_pain_type == "nonanginal" & sex == "female" ~ 14L,

    .default = NA_integer_
  )

  if (isTRUE(output == "numeric")) {
    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- stringr::str_c(ptp_percentage_group, "%")
    return(ptp_percentage_group)
  }


}
