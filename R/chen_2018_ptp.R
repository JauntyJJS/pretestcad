
#' @title Calculate 2018 Chen MFS PTP (figure 3 version) for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2018 Chen et. al. modified Framingham scoring (MFS) model.
#' Obstructive CAD was defined as
#' (\eqn{\geq}50% stenosis by diameter in at least one major
#' coronary vessel based on coronary angiography)
#' @inheritParams calculate_prms_2017_ptp
#' @param age Input integer value to indicate the age of the patient in years.
#' @param tc_mg_dl Input positive integer value to indicate the
#' patient's total cholesterol in \eqn{mg/dL}.
#' @param lvef_percent Input positive integer value to indicate the
#' patient's Left Ventricular Ejection Fraction (LVEF) in \eqn{\%}
#' @param hsCRP_mg_dl Input positive integer value to indicate the
#' patient's high-sensitivity C reactive protein (hs-CRP) in \eqn{mg/dL}.
#' @param have_anemia The value of variable in the parameters
#' \code{label_have_anemia_no}, \cr
#' \code{label_have_anemia_yes}, and \cr
#' \code{label_have_anemia_unknown}.
#' @param label_have_anemia_no Label(s) for patient without anemia.
#' Default: \code{c("no")}
#' @param label_have_anemia_yes Label(s) for patient with anemia. The
#' paper uses baseline hematocrit value <39% for men and <36% for women.
#' Default: \code{c("yes")}
#' @param label_have_anemia_unknown Label(s) for patient with
#' no information if they have anemia or not.
#' Default: \code{c(NA, NaN)}
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("risk_group", "risk_numeric", "risk_percentage", "mfs_points")
#' \itemize{
#'   \item risk_group means the PTP will be expressed as Low, Moderate, High and Very High.
#'   \itemize{
#'      \item low risk score if MFS is \eqn{\leq} 17.
#'      \item moderate risk score if MFS is in between 18 to 26.
#'      \item high risk score if MFS is in between 27 to 41.
#'      \item very high risk score if MFS is \eqn{\geq} 42.
#'   }
#'   \item risk_numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item risk_percentage means the PTP will be expressed as percentage text (0-100%).
#'   \item mfs_points means the total risk points as calculated in figure 3.
#'   This value will be later converted to risk of obstructive CAD \code{risk_numeric} based on the table presented in figure 3.
#' }
#' @return An integer, percentage or category representing the patient's risk points or PTP for obstructive CAD
#' based on 2018 Chen et. al. predicted risk if obstructive CAD defined as stenosis \eqn{\geq} 50% in any vessel..
#' See parameter option \code{output} for more information.
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
#' calculate_chen_2018_mfs_fig_3_ptp(
#'     age = 40,
#'     sex = "female",
#'     have_hypertension = "no",
#'     tc_mg_dl = 150,
#'     hdl_mg_dl = 70,
#'     lvef_percent = 60,
#'     hsCRP_mg_dl = 1,
#'     have_anemia = "no",
#'     output = "risk_percentage"
#' )
#' @rdname calculate_chen_2018_mfs_fig_3_ptp
#' @export
calculate_chen_2018_mfs_fig_3_ptp <- function(
    age,
    sex,
    have_hypertension,
    tc_mg_dl,
    hdl_mg_dl,
    lvef_percent,
    hsCRP_mg_dl,
    have_anemia,
    output = c("risk_group", "risk_numeric", "risk_percentage", "mfs_points"),
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

  check_if_integer(x = age, allow_na = TRUE)
  check_if_integer(x = tc_mg_dl, allow_na = TRUE)
  check_if_integer(x = hdl_mg_dl, allow_na = TRUE)
  check_if_integer(x = lvef_percent, allow_na = TRUE)
  check_if_integer(x = hsCRP_mg_dl, allow_na = TRUE)

  output <- output |>
    rlang::arg_match()

  age_points <- dplyr::case_when(
    dplyr::between(age, 18, 44) ~ 0L,
    dplyr::between(age, 45, 54) ~ 2L,
    dplyr::between(age, 55, 64) ~ 4L,
    dplyr::between(age, 65, 74) ~ 6L,
    age >= 75 ~ 8L,
    .default = NA_integer_
  )

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

  sex_points <- dplyr::case_when(
    sex == "female" ~ 0L,
    sex == "male" ~ 11L,
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

  have_hypertension_points <- dplyr::case_when(
    have_hypertension == "no" ~ 0L,
    have_hypertension == "yes" ~ 5L,
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

  have_anemia_points <- dplyr::case_when(
    have_anemia == "no" ~ 0L,
    have_anemia == "yes" ~ 6L,
    .default = NA_integer_
  )

  lvef_percent_points <- dplyr::case_when(
      lvef_percent >= 50 ~ 0L,
      dplyr::between(lvef_percent, 40, 49) ~ 5L,
      lvef_percent < 40 ~ 7L,
      .default = NA_integer_
  )

  tc_mg_dl_points <- dplyr::case_when(
      tc_mg_dl < 160 ~ 0L,
      dplyr::between(tc_mg_dl, 160, 199) ~ 3L,
      dplyr::between(tc_mg_dl, 200, 239) ~ 5L,
      dplyr::between(tc_mg_dl, 240, 279) ~ 7L,
      tc_mg_dl >= 280 ~ 9L,
      .default = NA_integer_
    )

  hdl_mg_dl_points <- dplyr::case_when(
      hdl_mg_dl >= 60 ~ 0L,
      dplyr::between(hdl_mg_dl, 50, 59) ~ 3L,
      dplyr::between(hdl_mg_dl, 40, 49) ~ 6L,
      hdl_mg_dl < 40 ~ 9L,
      .default = NA_integer_
    )

  hsCRP_mg_dl_points <- dplyr::case_when(
      dplyr::between(hsCRP_mg_dl, 0, 3) ~ 0L,
      hsCRP_mg_dl > 3 ~ 6L,
      .default = NA_integer_
    )

  total_points <- as.integer(age_points + sex_points +
    have_hypertension_points + have_anemia_points +
    lvef_percent_points +
    tc_mg_dl_points + hdl_mg_dl_points + hsCRP_mg_dl_points)

  if (isTRUE(output == "mfs_points")) {
    return(total_points)
  }

  if (isTRUE(output == "risk_group")) {
    total_points_group <- dplyr::case_when(
        dplyr::between(total_points, 0, 17) ~ "low",
        dplyr::between(total_points, 18, 26) ~ "moderate",
        dplyr::between(total_points, 27, 41) ~ "high",
        dplyr::between(total_points, 42, 461) ~ "very high",
        .default = NA
      )
    return(total_points_group)
  }

  chen_2018_mfs_fig_3_ptp <- dplyr::case_when(
      total_points == 0  ~ 16L,
      total_points == 1  ~ 17L,
      total_points == 2  ~ 19L,
      total_points == 3  ~ 20L,
      total_points == 4  ~ 21L,
      total_points == 5  ~ 23L,
      total_points == 6  ~ 25L,
      total_points == 7  ~ 27L,
      total_points == 8  ~ 29L,
      total_points == 9  ~ 31L,
      total_points == 10 ~ 33L,
      total_points == 11 ~ 35L,
      total_points == 12 ~ 37L,
      total_points == 13 ~ 39L,
      total_points == 14 ~ 41L,
      total_points == 15 ~ 43L,
      total_points == 16 ~ 46L,
      total_points == 17 ~ 48L,

      total_points == 18 ~ 50L,
      total_points == 19 ~ 53L,
      total_points == 20 ~ 55L,
      total_points == 21 ~ 57L,
      total_points == 22 ~ 59L,
      total_points == 23 ~ 62L,
      total_points == 24 ~ 64L,
      total_points == 25 ~ 66L,
      total_points == 26 ~ 68L,

      total_points == 27 ~ 70L,
      total_points == 28 ~ 72L,
      total_points == 29 ~ 74L,
      total_points == 30 ~ 75L,
      total_points == 31 ~ 77L,
      total_points == 32 ~ 79L,
      total_points == 33 ~ 80L,
      total_points == 34 ~ 82L,
      total_points == 35 ~ 83L,
      total_points == 36 ~ 84L,
      total_points == 37 ~ 85L,
      total_points == 38 ~ 87L,
      total_points == 39 ~ 88L,
      total_points == 40 ~ 89L,
      total_points == 41 ~ 89L,

      total_points == 42 ~ 90L,
      total_points == 43 ~ 91L,
      total_points == 44 ~ 92L,
      total_points == 45 ~ 92L,
      total_points == 46 ~ 93L,
      total_points == 47 ~ 94L,
      total_points == 48 ~ 94L,
      total_points == 49 ~ 95L,
      total_points == 50 ~ 95L,
      total_points == 51 ~ 96L,
      total_points == 52 ~ 96L,
      total_points == 53 ~ 96L,
      total_points == 54 ~ 97L,
      total_points == 55 ~ 97L,
      total_points == 56 ~ 97L,
      total_points == 57 ~ 97L,
      total_points == 58 ~ 98L,
      total_points == 59 ~ 98L,
      total_points == 60 ~ 98L,
      total_points == 61 ~ 98L,

      .default = NA_integer_
    )

    if (isTRUE(output == "risk_numeric")) {
      return(chen_2018_mfs_fig_3_ptp)
    }

    if (isTRUE(output == "risk_percentage")) {
      chen_2018_mfs_fig_3_ptp <- stringr::str_c(chen_2018_mfs_fig_3_ptp, "%")
      return(chen_2018_mfs_fig_3_ptp)
    }

}




#' @title Calculate 2018 Chen MFS PTP (formula version) for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2018 Chen et. al. modified Framingham scoring (MFS) model.
#' Obstructive CAD was defined as
#' (\eqn{\geq}50% stenosis by diameter in at least one major
#' coronary vessel based on coronary angiography)
#' @inheritParams calculate_prms_2017_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param tc_mg_dl Input positive numeric value to indicate the
#' patient's total cholesterol in \eqn{mg/dL}.
#' @param lvef_percent Input positive numeric value to indicate the
#' patient's Left Ventricular Ejection Fraction (LVEF) in \eqn{\%}
#' @param hsCRP_mg_dl Input positive numeric value to indicate the
#' patient's high-sensitivity C reactive protein (hs-CRP) in \eqn{mg/dL}.
#' @param have_anemia The value of variable in the parameters
#' \code{label_have_anemia_no}, \cr
#' \code{label_have_anemia_yes}, and \cr
#' \code{label_have_anemia_unknown}.
#' @param label_have_anemia_no Label(s) for patient without anemia.
#' Default: \code{c("no")}
#' @param label_have_anemia_yes Label(s) for patient with anemia. The
#' paper uses baseline hematocrit value <39% for men and <36% for women.
#' Default: \code{c("yes")}
#' @param label_have_anemia_unknown Label(s) for patient with
#' no information if they have anemia or not.
#' Default: \code{c(NA, NaN)}
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2018 modified Framingham scoring (MFS) model.
#' @details The predictive model is based on
#' patients from a prospective observational study
#' Predictive Value of Contrast Volume to Creatinine Clearance Ratio (PRECOMIN)
#' conducted primarily at Guangdong General Hospital.
#'
#' Model formula is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' -0.0468\quad+ \\\\
#' (0.0204 * age)\quad+ \\\\
#' (1.0961 * sex\_is\_male)\quad+ \\\\
#' (0.5444 * have\_hypertension)\quad+ \\\\
#' (0.0055 * total\_cholesterol\_mg\_dl)\quad+ \\\\
#' (-0.0257 * hdl\_mg\_dl)\quad+ \\\\
#' (-0.022  * LVEF\_\%)\quad+ \\\\
#' (0.5677 * have\_anemia)\quad+ \\\\
#' (0.0254 * hs-CRP\_mg\_dl)
#' \end{array}
#' }
#' @examples
#' # 40 year old female with no hypertension
#' # and anemia. She has a
#' # total cholesterol level of 150 mg/dL
#' # high-density lipoprotein cholesterol level of 70 mg/dL and
#' # high-sensitivity C reactive protein level of 1 mg/dL with
#' # a left Ventricular Ejection Fraction reading of 60%
#' calculate_chen_2018_mfs_formula_ptp(
#'     age = 40,
#'     sex = "female",
#'     have_hypertension = "no",
#'     tc_mg_dl = 150,
#'     hdl_mg_dl = 70,
#'     lvef_percent = 60,
#'     hsCRP_mg_dl = 1,
#'     have_anemia = "no"
#' )
#' @rdname calculate_chen_2018_mfs_formula_ptp
#' @export
calculate_chen_2018_mfs_formula_ptp <- function(
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

  sex_male <- dplyr::case_when(
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

  chen_2018_mfs_formula_ptp <- 1 /
    (1 + exp(-(-0.0468 +
              ( 0.0204 * age) +
              ( 1.0961 * sex_male) +
              ( 0.5444 * have_hypertension) +
              ( 0.0055 * tc_mg_dl) +
              (-0.0257 * hdl_mg_dl) +
              (-0.022  * lvef_percent) +
              ( 0.5677 * have_anemia) +
              ( 0.0254 * hsCRP_mg_dl)
     )
     )
     )

  return(chen_2018_mfs_formula_ptp)

}
