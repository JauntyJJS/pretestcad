#' @title Calculate AHA/ACC 2021 PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' American Heart Association/American College of Cardiology (AHA/ACC) 2021 guidelines.
#' @inheritParams calculate_esc_2019_ptp
#' @param have_chest_pain The value of variable in the parameters \code{label_have_chest_pain_no},
#' \code{label_have_chest_pain_yes} and \code{label_have_chest_pain_unknown}.
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("grouping", "numeric", "percentage")
#' \itemize{
#'   \item grouping means the PTP will be expressed as Low, Intermediate and High.
#'   \itemize{
#'      \item very low if PTP is less than 5\%.
#'      \item low if PTP is in between 5\% to 15\%.
#'      \item intermediate if PTP is in between 15\% to 50\%.
#'      \item high if PTP is more than 50\%.
#'   }
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100\%).
#' }
#' @param label_have_chest_pain_no Label(s) for patient not having chest pain symptoms.
#' Default: \code{c("no")}
#' @param label_have_chest_pain_yes Label(s) for patient having chest pain symptoms.
#' Default: \code{c("yes")}
#' @param label_have_chest_pain_unknown Label(s) for patient with unknown chest pain symptoms.
#' Default: \code{c(NA, NaN)}
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on the AHA/ACC 2021 guidelines.
#' See parameter option \code{output} for more information.
#' @details The predictive model used to create the guidelines are based on
#' patients from European countries with low cardiovascular disease (CVD) risk.
#'
#' If the patient has both dyspnoea and a particular
#' chest pain type (typical, atypical, nonanginal),
#' The chest pain type will take precedence over dyspnoea
#'
#' @examples
#' # 35 year old female with chest pain
#' calculate_aha_2021_ptp(
#'     age = 35,
#'     sex = "female",
#'     have_dyspnoea = "no",
#'     have_chest_pain = "yes",
#'     output = "percentage"
#' )
#'
#' # 75 year old male with only dyspnoea
#' calculate_aha_2021_ptp(
#'     age = 75,
#'     sex = "male",
#'     have_dyspnoea = "yes",
#'     have_chest_pain = "no",
#'     output = "percentage"
#' )
#' @rdname calculate_aha_2021_ptp
#' @export
calculate_aha_2021_ptp <- function(
    age,
    sex,
    have_dyspnoea,
    have_chest_pain,
    output = c("grouping", "numeric", "percentage"),
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_dyspnoea_no = c("no"),
    label_have_dyspnoea_yes = c("yes"),
    label_have_dyspnoea_unknown = c(NA, NaN),
    label_have_chest_pain_no = c("no"),
    label_have_chest_pain_yes = c("yes"),
    label_have_chest_pain_unknown = c(NA, NaN)
)
{

  check_if_positive(x = age, allow_na = TRUE)
  check_if_integer(x = age, allow_na = TRUE)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_cat_missing = label_sex_unknown
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_dyspnoea_no,
    label_have_dyspnoea_yes,
    label_cat_missing = label_have_dyspnoea_unknown
  )

  check_if_two_categories_are_mutually_exclusive(
    label_have_chest_pain_no,
    label_have_chest_pain_yes,
    label_cat_missing = label_have_chest_pain_unknown
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

  # Ensure dyspnoea is valid and mapped to a unified group (yes, no, NA)
  have_dyspnoea <- have_dyspnoea |>
    harmonise_two_labels(
      label_one = label_have_dyspnoea_no,
      label_two = label_have_dyspnoea_yes,
      label_unknown = label_have_dyspnoea_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  # Ensure chest pain is valid and mapped to a unified group (yes, no, NA)
  have_chest_pain <- have_chest_pain |>
    harmonise_two_labels(
      label_one = label_have_chest_pain_no,
      label_two = label_have_chest_pain_yes,
      label_unknown = label_have_chest_pain_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
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
    age >= 70 ~ "70+"
  )

  have_only_dyspnea <- dplyr::case_when(
    have_chest_pain == "yes" ~ "no",
    have_dyspnoea == "yes" & have_chest_pain == "no" ~ "yes"
  )

  ptp_percentage_group <- dplyr::case_when(
    age_group == "30-39" & have_chest_pain == "yes"    & sex == "male"   ~ 4 ,
    age_group == "30-39" & have_chest_pain == "yes"    & sex == "female" ~ 5 ,
    age_group == "40-49" & have_chest_pain == "yes"    & sex == "male"   ~ 22,
    age_group == "40-49" & have_chest_pain == "yes"    & sex == "female" ~ 10,
    age_group == "50-59" & have_chest_pain == "yes"    & sex == "male"   ~ 32,
    age_group == "50-59" & have_chest_pain == "yes"    & sex == "female" ~ 13,
    age_group == "60-69" & have_chest_pain == "yes"    & sex == "male"   ~ 44,
    age_group == "60-69" & have_chest_pain == "yes"    & sex == "female" ~ 16,
    age_group == "70+"   & have_chest_pain == "yes"    & sex == "male"   ~ 52,
    age_group == "70+"   & have_chest_pain == "yes"    & sex == "female" ~ 27,
    age_group == "30-39" & have_only_dyspnea == "yes"      & sex == "male"   ~ 0 ,
    age_group == "30-39" & have_only_dyspnea == "yes"      & sex == "female" ~ 3 ,
    age_group == "40-49" & have_only_dyspnea == "yes"      & sex == "male"   ~ 12,
    age_group == "40-49" & have_only_dyspnea == "yes"      & sex == "female" ~ 3 ,
    age_group == "50-59" & have_only_dyspnea == "yes"      & sex == "male"   ~ 20,
    age_group == "50-59" & have_only_dyspnea == "yes"      & sex == "female" ~ 9 ,
    age_group == "60-69" & have_only_dyspnea == "yes"      & sex == "male"   ~ 27,
    age_group == "60-69" & have_only_dyspnea == "yes"      & sex == "female" ~ 14,
    age_group == "70+"   & have_only_dyspnea == "yes"      & sex == "male"   ~ 32,
    age_group == "70+"   & have_only_dyspnea == "yes"      & sex == "female" ~ 12,
    .default = NA
  )

  if (isTRUE(output %in% c("numeric", "grouping"))) {
    ptp_percentage_group <- ptp_percentage_group |>
      as.integer() |>
      # Symmetric rounding to the nearest integer
      round_to_nearest_digit()

    if (isTRUE(output == "numeric")) {
      return(ptp_percentage_group)
    }

    ptp_percentage_group <- dplyr::case_when(
      ptp_percentage_group <= 5 ~ "very low",
      dplyr::between(ptp_percentage_group, 6, 15) ~ "low",
      dplyr::between(ptp_percentage_group, 16, 50) ~ "intermediate",
      ptp_percentage_group > 50 ~ "high"

    )

    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    if (isTRUE(have_chest_pain == "yes")) {
      ptp_percentage_group <- stringr::str_c("<=", ptp_percentage_group, "%")
    } else if (isTRUE(have_only_dyspnea == "yes")) {
      ptp_percentage_group <- stringr::str_c(ptp_percentage_group, "%")
    }

    return(ptp_percentage_group)
  }
}
