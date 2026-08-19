#' @title Calculate 2018 Chen MFS PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2018 Chen et. al. modified Framingham scoring (MFS) model.
#' Obstructive CAD was defined as
#' (≥50% stenosis by diameter in at least one major
#' coronary vessel based on coronary angiography)
#' @inheritParams calculate_prms_2017_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param tc_mg_dl Input positive numeric value to indicate the
#' patient's total cholesterol in \eqn{mg/dL}.
#' @param lvef_percent Input positive numeric value to indicate the
#' patient's Left Ventricular Ejection Fraction (LVEF) in \eqn{\%}
#' @param hsCRP_mg_dl Input positive numeric value to indicate the
#' patient's high-sensitivity C reactive protein (hs-CRP) in \eqn{mg/dL}.
#' @param have_anemia The value of variable in the parameters \code{label_have_anemia_no},
#' \code{label_have_anemia_yes} and \code{label_have_anemia_unknown}.
#' @param label_have_anemia_no Label(s) for patient without anemia.
#' Default: \code{c("no")}
#' @param label_have_anemia_yes Label(s) for patient with anemia. The
#' paper uses baseline hematocrit value <39% for men and <36% for women.
#' Default: \code{c("yes")}
#' @param label_have_anemia_unknown Label(s) for patient with
#' no information if they have anemia or not.
#' Default: \code{c(NA, NaN)}
#'
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2018 modified Framingham scoring (MFS) model.
#' @details The predictive model is based on
#' patients from a prospective observational study
#' Predictive Value of Contrast Volume to Creatinine Clearance Ratio (PRECOMIN)
#' conducted primarily at Guangdong General Hospital.
#'
#' @examples
#' # 40 year old female with no hypertension
#' # and anemia. She has a
#' # total cholesterol level of 150 mg/dL
#' # high-density lipoprotein cholesterol level of 70 mg/dL and
#' # high-sensitivity C reactive protein level of 1 mg/dL with
#' # a left Ventricular Ejection Fraction reading of 60%
#' calculate_chen_2018_mfs_ptp(
#'     age = 40,
#'     sex = "female",
#'     have_hypertension = "no",
#'     tc_mg_dl = 150,
#'     hdl_mg_dl = 70,
#'     lvef_percent = 60,
#'     hsCRP_mg_dl = 1,
#'     have_anemia = "no"
#' )
#' @rdname calculate_chen_2018_mfs_ptp
#' @export
calculate_chen_2018_mfs_ptp <- function(
    age,
    sex,
    have_hypertension,
    tc_mg_dl,
    hdl_mg_dl,
    lvef_percent,
    hsCRP_mg_dl,
    have_anemia,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_hypertension_no = c("no"),
    label_have_hypertension_yes = c("yes"),
    label_have_hypertension_unknown = c(NA, NaN),
    label_have_anemia_no = c("no"),
    label_have_anemia_yes = c("yes"),
    label_have_anemia_unknown = c(NA, NaN)

)
{
  check_if_positive(x = age, allow_na = TRUE)
  check_if_non_negative(x = tc_mg_dl, allow_na = TRUE)
  check_if_non_negative(x = hdl_mg_dl, allow_na = TRUE)
  check_if_positive(x = lvef_percent, allow_na = TRUE)
  check_if_non_negative(x = hsCRP_mg_dl, allow_na = TRUE)

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
    label_have_anemia_no,
    label_have_anemia_yes,
    label_cat_missing = label_have_anemia_unknown
  )

  # Ensure have anemia is valid and mapped to a unified group (yes, no, NA)
  have_anemia <- have_anemia |>
    harmonise_two_labels(
      label_one = label_have_anemia_no,
      label_two = label_have_anemia_yes,
      label_unknown = label_have_anemia_unknown,
      harmonise_label_one = "no",
      harmonise_label_two = "yes",
      harmonise_label_unknown = NA
    )

  have_anemia <- dplyr::case_when(
    have_anemia == "no" ~ 0L,
    have_anemia == "yes" ~ 1L,
    .default = NA_integer_
  )

  chen_2018_mfs_ptp <- 1 /
    (1 + exp(-(-0.0468 +
                0.0204  * age +
                1.0961  * sex +
                0.5444  * have_hypertension +
                0.0055  * tc_mg_dl +
                -0.0257 * hdl_mg_dl +
                -0.022  * lvef_percent +
                0.5677  * have_anemia +
                0.0254  * hsCRP_mg_dl
     )
     )
     )

  return(chen_2018_mfs_ptp)

}
