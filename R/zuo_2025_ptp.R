#' @title Calculate 2025 Zuo PREDICT-OCAD PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease (CAD) based on the
#' 2025 Zuo et. al. PREDICT-OCAD model.
#' Obstructive CAD was defined as ≥50% stenosis on CCTA.
#' @inheritParams calculate_lah_2022_extended_ptp
#' @param age Input numeric value to indicate the age of the patient in years.
#' @param coronary_calcium_score Input non-negative integer to indicate the
#' total coronary calcium score of the patient.
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2025 Zuo et. al. PREDICT-OCAD model.
#' @details The predictive model is based on 2649
#' patients suspected of CAD at Jinling Hospital, Nanjing University
#' (Jiangsu, China), spanning from January 2010 to December 2020.
#'
#' Model formula used is taken from the web-based calculator, accessible at
#' \url{https://predict-obstructive-cad.shinyapps.io/dynnomapp/}.
#'
#' It is of the form
#' \deqn{\frac{1}{(1 + e^{-F(x)})}}
#' where \eqn{F(x)} equals
#' \deqn{
#' \begin{array}{l}
#' -4.336859\quad+ \\\\
#' (-0.422110 * sex\_is\_female)\quad+ \\\\
#' (0.029603 * age)\quad+ \\\\
#' (-0.539890 * no\_dyslipidemia)\quad+ \\\\
#' (-0.210570 * no\_diabetes)\quad+ \\\\
#' (1.925711 * cacs\_1\_to\_100)\quad+ \\\\
#' (3.362762 * cacs\_101\_to\_400)\quad+ \\\\
#' (4.377606 * cacs\_more\_than\_400)
#' \end{array}
#' }
#' @examples
#' # 40 year old female without dyslipidemia and diabetes
#' # with coronary calcium score of 0
#' calculate_zuo_2025_predict_ocad_ptp(
#'     age = 40,
#'     sex = "female",
#'     have_dyslipidemia = "no",
#'     have_diabetes = "no",
#'     coronary_calcium_score = 0
#' )
#' @rdname calculate_zuo_2025_predict_ocad_ptp
#' @export
calculate_zuo_2025_predict_ocad_ptp <- function(
    age,
    sex,
    have_dyslipidemia,
    have_diabetes,
    coronary_calcium_score,
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_dyslipidemia_no = c("no"),
    label_have_dyslipidemia_yes = c("yes"),
    label_have_dyslipidemia_unknown = c(NA, NaN),
    label_have_diabetes_no = c("no"),
    label_have_diabetes_yes = c("yes"),
    label_have_diabetes_unknown = c(NA, NaN)
    )
{
  check_if_positive(x = age, allow_na = TRUE)
  check_if_non_negative(x = coronary_calcium_score, allow_na = TRUE)
  check_if_integer(x = coronary_calcium_score, allow_na = TRUE)

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

  sex_female <- dplyr::case_when(
    sex == "female" ~ 1L,
    sex == "male" ~ 0L,
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

  no_dyslipidemia <- dplyr::case_when(
    have_dyslipidemia == "no" ~ 1L,
    have_dyslipidemia == "yes" ~ 0L,
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

  no_diabetes <- dplyr::case_when(
    have_diabetes == "no" ~ 1L,
    have_diabetes == "yes" ~ 0L,
    .default = NA_integer_
  )

  cacs_1_to_100 <- dplyr::case_when(
    coronary_calcium_score >= 1 & coronary_calcium_score <= 100 ~ 1L,
    coronary_calcium_score == 0 ~ 0L,
    coronary_calcium_score > 100 ~ 0L,
    .default = NA_integer_
  )

  cacs_101_to_400 <- dplyr::case_when(
    coronary_calcium_score >= 101 & coronary_calcium_score <= 400 ~ 1L,
    coronary_calcium_score >= 0 & coronary_calcium_score <= 100 ~ 0L,
    coronary_calcium_score > 400 ~ 0L,
    .default = NA_integer_
  )

  cacs_more_than_400 <- dplyr::case_when(
    coronary_calcium_score > 400 ~ 1L,
    coronary_calcium_score >= 0 & coronary_calcium_score <= 400 ~ 0L,
    .default = NA_integer_
  )

  zuo_2025_predict_ocad_ptp <- 1 /
    (1 + exp(-(-4.336859 +
              (-0.422110 * sex_female) +
              ( 0.029603 * age) +
              (-0.539890 * no_dyslipidemia) +
              (-0.210570 * no_diabetes) +
              ( 1.925711 * cacs_1_to_100) +
              ( 3.362762 * cacs_101_to_400) +
              ( 4.377606 * cacs_more_than_400)
    )
    )
    )

  return(zuo_2025_predict_ocad_ptp)

}


