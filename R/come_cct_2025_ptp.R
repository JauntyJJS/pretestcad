#' @title Calculate 2025 COME-CCT-PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2025 Collaborative Meta-Analysis of Cardiac CT (COME-CCT) PTP model.
#' Obstructive coronary artery disease was defined as at least one
#' ≥ 50% diameter stenosis by invasive coronary angiography (ICA).
#' @inheritParams calculate_esc_2019_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param chest_pain_type The value of variable in the parameters,
#' \code{label_cpt_nonanginal}, \code{label_cpt_atypical}, \code{label_cpt_typical},
#' \code{label_cpt_others} and \code{label_cpt_unknown}.
#' @param label_cpt_others Label(s) for patient having other forms of chest pain.
#' Default: \code{c("others")}
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2025 Collaborative Meta-Analysis of Cardiac CT (COME-CCT) PTP model.
#' @details The predictive model is based on
#' 5332 stable chest pain patients with clinically indicated ICA
#' from 22 countries.
#'
#' Model formula is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' -1.434\quad+ \\\\
#' (0.036 * (age - 61))\quad+ \\\\
#' (0.986 * sex\_is\_male)\quad+ \\\\
#' (1.427 * have\_typical\_chest\_pain)\quad+ \\\\
#' (0.307 * have\_atypical\_chest\_pain)\quad+ \\\\
#' (0.119 * have\_nonanginal\_chest\_pain)
#' \end{array}
#' }
#' @examples
#' # 40 year old female with typical chest pain,
#' calculate_come_cct_2025_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical"
#' )
#' @rdname calculate_come_cct_2025_ptp
#' @export
calculate_come_cct_2025_ptp <- function(
    age,
    sex,
    chest_pain_type,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_others = c("others"),
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

  sex_male <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 1L,
    .default = NA_integer_
  )

  check_if_four_categories_are_mutually_exclusive(
    label_cpt_nonanginal,
    label_cpt_atypical,
    label_cpt_typical,
    label_cpt_others,
    label_cat_missing = label_cpt_unknown
  )

  # Ensure chest pain type is valid and mapped to a unified group
  # (nonanginal, atypical, typical, others)
  chest_pain_type <- chest_pain_type |>
    harmonise_four_labels(
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_four = label_cpt_others,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_four = "others",
      harmonise_label_unknown = NA
    )

  have_nonanginal_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "atypical", "others") ~ 0L,
    chest_pain_type == "nonanginal" ~ 1L,
    .default = NA_integer_
  )

  have_atypical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "nonanginal", "others") ~ 0L,
    chest_pain_type == "atypical" ~ 1L,
    .default = NA_integer_
  )

  have_typical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("atypical", "nonanginal", "others") ~ 0L,
    chest_pain_type == "typical" ~ 1L,
    .default = NA
  )

  come_cct_2025_ptp <- 1 /
    (1 + exp(-(-1.434 +
              ( 0.036 * (age - 61)) +
              ( 0.986 * sex_male) +
              ( 1.427 * have_typical_chest_pain) +
              ( 0.307 * have_atypical_chest_pain) +
              ( 0.119 * have_nonanginal_chest_pain)
    )
    )
    )

  return(come_cct_2025_ptp)

}

#' @title Calculate PTP for obstructive CAD using CTA results
#' from COME-CCT Consortium
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the model that uses
#' only CTA results from the COME-CCT Consortium cohort.
#' Obstructive coronary artery disease was defined as at least one
#' ≥ 50% diameter stenosis by invasive coronary angiography (ICA).
#' @param cta_result The value of variable in the parameters,
#' \code{label_cta_obstructive}, \code{label_cta_non_obstructive}, and \code{label_cta_unknown}.
#' @param label_cta_obstructive Label(s) for patient having obstructive CAD
#' defined as at least one ≥ 50% diameter stenosis in computed tomography angiography (CTA).
#' Default: \code{c("obstructive")}
#' @param label_cta_non_obstructive Label(s) for patient having non-obstructive CAD
#' defined as at least one < 50% diameter stenosis in computed tomography angiography (CTA).
#' Default: \code{c("non_obstructive")}
#' @param label_cta_unknown Label(s) for patient unknown CTA results.
#' Default: \code{c(NA, NaN)}
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the model that uses only CTA results from the COME-CCT Consortium cohort.
#' @details The predictive model is based on
#' 5332 stable chest pain patients with clinically indicated ICA
#' from 22 countries.
#'
#' Model formula is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' -1.893\quad+ \\\\
#' (2.973 * have\_obstructive\_CAD\_in\_CTA)
#' \end{array}
#' }
#' @examples
#' # A patient with obstructive CAD in CTA
#' calculate_cta_alone_2025_ptp(
#'     cta_result = "obstructive"
#' )
#'
#' # A patient with non-obstructive CAD in CTA
#' calculate_cta_alone_2025_ptp(
#'     cta_result = "non_obstructive"
#' )
#' @rdname calculate_cta_alone_2025_ptp
#' @export
calculate_cta_alone_2025_ptp <- function(
    cta_result,
    label_cta_obstructive = c("obstructive"),
    label_cta_non_obstructive = c("non_obstructive"),
    label_cta_unknown = c(NA, NaN)
)
{
  check_if_two_categories_are_mutually_exclusive(
    label_cta_obstructive,
    label_cta_non_obstructive,
    label_cat_missing = label_cta_unknown
  )

  # Ensure cta_result is valid and mapped to a unified group
  # (obstructive, non-obstructive, NA)
  cta_result <- cta_result |>
    harmonise_two_labels(
      label_one = label_cta_obstructive,
      label_two = label_cta_non_obstructive,
      label_unknown = label_cta_unknown,
      harmonise_label_one = "obstructive",
      harmonise_label_two = "non_obstructive",
      harmonise_label_unknown = NA
    )

  have_obstructuive_cad_in_cta <- dplyr::case_when(
    cta_result == "non_obstructive" ~ 0L,
    cta_result == "obstructive" ~ 1L,
    .default = NA_integer_
  )

  cta_alone_2025_ptp <- 1 /
    (1 + exp(-(-1.893 +
              ( 2.973 * have_obstructuive_cad_in_cta)
    )
    )
    )

  return(cta_alone_2025_ptp)

}

#' @title Calculate 2025 COME-CCT-PTP with CTA for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2025 Collaborative Meta-Analysis of Cardiac CT (COME-CCT) with CTA PTP model.
#' Obstructive coronary artery disease was defined as at least one
#' ≥ 50% diameter stenosis by invasive coronary angiography (ICA).
#' @inheritParams calculate_come_cct_2025_ptp
#' @inheritParams calculate_cta_alone_2025_ptp
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2025 Collaborative Meta-Analysis of Cardiac CT (COME-CCT)
#' with CTA PTP model.
#' @details The predictive model is based on
#' 5332 stable chest pain patients with clinically indicated ICA
#' from 22 countries.
#'
#' Model formula is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' -3.030\quad+ \\\\
#' (0.030 * (age - 61))\quad+ \\\\
#' (0.868 * sex\_is\_male)\quad+ \\\\
#' (1.346 * have\_typical\_chest\_pain)\quad+ \\\\
#' (0.195 * have\_atypical\_chest\_pain)\quad+ \\\\
#' (0.156 * have\_nonanginal\_chest\_pain)\quad+ \\\\
#' (2.924 * have\_obstructive\_CAD\_in\_CTA)
#' \end{array}
#' }
#' @examples
#' # 40 year old female with typical chest pain
#' # and obstructive CAD in CTA
#' calculate_come_cct_with_cta_2025_ptp(
#'     age = 40,
#'     sex = "female",
#'     chest_pain_type = "typical",
#'     cta_result = "obstructive"
#' )
#' @rdname calculate_come_cct_with_cta_2025_ptp
#' @export
calculate_come_cct_with_cta_2025_ptp <- function(
    age,
    sex,
    chest_pain_type,
    cta_result,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_cpt_nonanginal = c("nonanginal"),
    label_cpt_atypical = c("atypical"),
    label_cpt_typical = c("typical"),
    label_cpt_others = c("others"),
    label_cpt_unknown = c(NA, NaN),
    label_cta_obstructive = c("obstructive"),
    label_cta_non_obstructive = c("non_obstructive"),
    label_cta_unknown = c(NA, NaN)
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

  check_if_four_categories_are_mutually_exclusive(
    label_cpt_nonanginal,
    label_cpt_atypical,
    label_cpt_typical,
    label_cpt_others,
    label_cat_missing = label_cpt_unknown
  )

  # Ensure chest pain type is valid and mapped to a unified group
  # (nonanginal, atypical, typical, others)
  chest_pain_type <- chest_pain_type |>
    harmonise_four_labels(
      label_one = label_cpt_nonanginal,
      label_two = label_cpt_atypical,
      label_three = label_cpt_typical,
      label_four = label_cpt_others,
      label_unknown = label_cpt_unknown,
      harmonise_label_one = "nonanginal",
      harmonise_label_two = "atypical",
      harmonise_label_three = "typical",
      harmonise_label_four = "others",
      harmonise_label_unknown = NA
    )

  have_nonanginal_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "atypical", "others") ~ 0L,
    chest_pain_type == "nonanginal" ~ 1L,
    .default = NA_integer_
  )

  have_atypical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("typical", "nonanginal", "others") ~ 0L,
    chest_pain_type == "atypical" ~ 1L,
    .default = NA_integer_
  )

  have_typical_chest_pain <- dplyr::case_when(
    chest_pain_type %in% c("atypical", "nonanginal", "others") ~ 0L,
    chest_pain_type == "typical" ~ 1L,
    .default = NA
  )

  check_if_two_categories_are_mutually_exclusive(
    label_cta_obstructive,
    label_cta_non_obstructive,
    label_cat_missing = label_cta_unknown
  )

  # Ensure cta_result is valid and mapped to a unified group
  # (obstructive, non-obstructive, NA)
  cta_result <- cta_result |>
    harmonise_two_labels(
      label_one = label_cta_obstructive,
      label_two = label_cta_non_obstructive,
      label_unknown = label_cta_unknown,
      harmonise_label_one = "obstructive",
      harmonise_label_two = "non_obstructive",
      harmonise_label_unknown = NA
    )

  have_obstructuive_cad_in_cta <- dplyr::case_when(
    cta_result == "non_obstructive" ~ 0L,
    cta_result == "obstructive" ~ 1L,
    .default = NA_integer_
  )

  come_cct_with_cta_2025_ptp <- 1 /
    (1 + exp(-(-3.030 +
              ( 0.030 * (age - 61)) +
              ( 0.868 * sex_male) +
              ( 1.346 * have_typical_chest_pain) +
              ( 0.195 * have_atypical_chest_pain) +
              ( 0.156 * have_nonanginal_chest_pain) +
              ( 2.924 * have_obstructuive_cad_in_cta)
    )
    )
    )

  return(come_cct_with_cta_2025_ptp)

}
