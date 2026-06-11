#' @title Calculate 2023 Miller PTP for obstructive CAD (stenosis ≥ 50% in any vessel)
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on 2023 Miller et. al. 
#' likelihood table for CAD (stenosis ≥ 50% in any vessel)
#' @inheritParams rlang::args_error_context
#' @inheritParams calculate_aha_2021_ptp
#' @inheritParams calculate_lah_2022_extended_ptp
#' @param output Input text to indicate the how pre-test
#' probability results be expressed
#' Default: c("grouping", "numeric", "percentage")
#' \itemize{
#'   \item grouping means the PTP will be expressed as Low, Intermediate and High.
#'   \itemize{
#'      \item low if PTP is less than 15%.
#'      \item intermediate if PTP is in between 15% to 50%.
#'      \item high if PTP is more than 50%.
#'   }
#'   \item numeric means the PTP will be expressed as an integer probability (0-100).
#'   \item percentage means the PTP will be expressed as percentage text (0-100%).
#' }
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on 2023 Miller et. al. likelihood table for CAD defined as stenosis ≥ 50% in any vessel.
#' See parameter option \code{output} for more information.
#' @details The predictive model is based on 2055
#' patients from 3 multinational sites. They are
#' Henry Ford Hospital (n=853), 
#' Ottawa Heart Institute (n=808), and 
#' University Hospital Zurich (n=394).
#' @examples
#' # 40 female with cardiac chest pain and coronary calcium score of 0
#' calculate_miller_2023_vessel_50_cad_ptp(
#'   age = 40,
#'   sex = "female",
#'   have_chest_pain = "yes",
#'   coronary_calcium_score = 0,
#'   output = "percentage"
#' )
#'
#' @rdname calculate_miller_2023_vessel_50_cad_ptp
#' @export
calculate_miller_2023_vessel_50_cad_ptp <- function(
    age,
    sex,
    have_chest_pain,
    coronary_calcium_score,
    output = c("grouping", "numeric", "percentage"),
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_chest_pain_no = c("no"),
    label_have_chest_pain_yes = c("yes"),
    label_have_chest_pain_unknown = c(NA, NaN),
    error_call = rlang::caller_env()
    )
{
  check_if_positive(x = age, allow_na = TRUE,
                   error_call = error_call)
  check_if_integer(x = age, allow_na = TRUE,
                   error_call = error_call)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_sex_unknown,
    error_call = error_call
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
      harmonise_label_unknown = NA,
      error_call = error_call
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

  check_if_non_negative(x = coronary_calcium_score, allow_na = TRUE)
  check_if_integer(x = coronary_calcium_score, allow_na = TRUE)

  output <- output |>
    rlang::arg_match(error_call = error_call)

  age_group <- dplyr::case_when(
    age < 40 ~ "< 40",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    age >= 70 ~ ">= 70",
    .default = NA
  )

  cacs_group <- dplyr::case_when(
    coronary_calcium_score == 0 ~ "0",
    dplyr::between(coronary_calcium_score, 1, 99) ~ "1-99",
    dplyr::between(coronary_calcium_score, 100, 399) ~ "100-399",
    dplyr::between(coronary_calcium_score, 400, 999) ~ "400-999",
    coronary_calcium_score >= 1000 ~ ">= 1000",
    .default = NA
  )  

  ptp_percentage_group <- dplyr::case_when(
    age_group == "< 40" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 1.7 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 13.9,
    age_group == "< 40" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 33.3,
    age_group == "< 40" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 53.9,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 77.6,

    age_group == "40-49" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 2.2 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 17.7,
    age_group == "40-49" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 39.8,
    age_group == "40-49" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 60.8,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 82.1,
    
    age_group == "50-59" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 2.9 ,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 21.8,
    age_group == "50-59" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 46.2,
    age_group == "50-59" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 66.8,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 85.6,  

    age_group == "60-69" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 2.3 ,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 18.1,
    age_group == "60-69" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 40.5,
    age_group == "60-69" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 61.5,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 82.5, 

    age_group == ">= 70" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 2.2 ,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 17.5,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 39.6,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 60.5,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 82.0,

    age_group == "< 40" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 3.4 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 24.9,
    age_group == "< 40" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 50.5,
    age_group == "< 40" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 70.5,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 87.6,

    age_group == "40-49" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 4.4 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 30.5,
    age_group == "40-49" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 57.5,
    age_group == "40-49" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 76.0,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 90.4,
    
    age_group == "50-59" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 5.7,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 36.3,
    age_group == "50-59" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 63.7,
    age_group == "50-59" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 80.4,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 92.4,  

    age_group == "60-69" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 4.6,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 31.1,
    age_group == "60-69" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 58.2,
    age_group == "60-69" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 76.5,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 90.6, 

    age_group == ">= 70" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 4.4,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 30.3,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 57.2,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 75.8,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 90.3,   
    
    age_group == "< 40" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 1.0 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 9.4,
    age_group == "< 40" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 27.9,
    age_group == "< 40" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 48.7,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 60.2,

    age_group == "40-49" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 1.4 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 12.1,
    age_group == "40-49" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 33.9,
    age_group == "40-49" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 55.6,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 66.7,
    
    age_group == "50-59" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 1.8 ,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 15.2,
    age_group == "50-59" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 40.0,
    age_group == "50-59" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 62.0,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 72.2,  

    age_group == "60-69" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 1.4 ,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 12.4,
    age_group == "60-69" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 34.6,
    age_group == "60-69" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 56.4,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 67.4, 

    age_group == ">= 70" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 1.3 ,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 12.0,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 33.7,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 55.4,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 66.5,    

    age_group == "< 40" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 2.2 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 18.3,
    age_group == "< 40" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 45.5,
    age_group == "< 40" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 67.1,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 76.5,

    age_group == "40-49" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 2.9 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 22.9,
    age_group == "40-49" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 52.5,
    age_group == "40-49" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 73.0,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 81.2,

    age_group == "50-59" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 3.7 ,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 27.8,
    age_group == "50-59" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 58.9,
    age_group == "50-59" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 77.8,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 84.8,
    
    age_group == "60-69" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 3.0 ,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 23.4,
    age_group == "60-69" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 53.2,
    age_group == "60-69" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 73.6,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 81.6,  

    age_group == ">= 70" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 2.8 ,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 22.7,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 52.2,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 72.8,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 81.0,

    .default = NA
  )

  if (isTRUE(output %in% c("numeric", "grouping"))) {

    if (isTRUE(output == "numeric")) {
      return(ptp_percentage_group)
    }

    ptp_percentage_group <- dplyr::case_when(
      ptp_percentage_group < 15 ~ "low",
      ptp_percentage_group >= 15 & ptp_percentage_group <= 50 ~ "intermediate",
      ptp_percentage_group > 50 ~ "high"

    )

    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- ptp_percentage_group |> 
      round_to_nearest_digit(digits = 1) |> 
      formatC(digits = 1, format = "f") |> 
      stringr::str_c("%")
    return(ptp_percentage_group)
  }

}

#' @title Calculate 2023 Miller PTP for obstructive CAD (stenosis ≥ 50% in left main vessel or ≥ 70% in non-left main vessel)
#' @description This function returns a patient's
#' pre-test Probability (PTP) of obstructive
#' coronary artery disease (CAD) based on 2023 Miller et. al. likelihood table
#' likelihood table for CAD (stenosis ≥ 50% in left main vessel or ≥ 70% in non-left main vessel)
#' @inheritParams rlang::args_error_context
#' @inheritParams calculate_miller_2023_vessel_50_cad_ptp
#' @return An integer, percentage or category representing the patient's PTP for obstructive CAD
#' based on 2023 Miller et. al. likelihood table for CAD defined as stenosis ≥ 50% in left main vessel
#' or ≥ 70% in non-left main vessel.
#' See parameter option \code{output} for more information.
#' @details The predictive model is based on 2055
#' patients from 3 multinational sites. They are
#' Henry Ford Hospital (n=853), 
#' Ottawa Heart Institute (n=808), and 
#' University Hospital Zurich (n=394).
#' @examples
#' # 40 female with cardiac chest pain and coronary calcium score of 0
#' calculate_miller_2023_lm_50_non_lm_70_cad_ptp(
#'   age = 40,
#'   sex = "female",
#'   have_chest_pain = "yes",
#'   coronary_calcium_score = 0,
#'   output = "percentage"
#' )
#'
#' @rdname calculate_miller_2023_lm_50_non_lm_70_cad_ptp
#' @export
calculate_miller_2023_lm_50_non_lm_70_cad_ptp <- function(
    age,
    sex,
    have_chest_pain,
    coronary_calcium_score,
    output = c("grouping", "numeric", "percentage"),
    label_sex_male = c("male"),
    label_sex_female = c("female"),
    label_sex_unknown = c(NA, NaN),
    label_have_chest_pain_no = c("no"),
    label_have_chest_pain_yes = c("yes"),
    label_have_chest_pain_unknown = c(NA, NaN),
    error_call = rlang::caller_env()
    )
{
  check_if_positive(x = age, allow_na = TRUE,
                   error_call = error_call)
  check_if_integer(x = age, allow_na = TRUE,
                   error_call = error_call)

  check_if_two_categories_are_mutually_exclusive(
    label_sex_male,
    label_sex_female,
    label_sex_unknown,
    error_call = error_call
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
      harmonise_label_unknown = NA,
      error_call = error_call
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

  check_if_non_negative(x = coronary_calcium_score, allow_na = TRUE)
  check_if_integer(x = coronary_calcium_score, allow_na = TRUE)

  output <- output |>
    rlang::arg_match(error_call = error_call)

  age_group <- dplyr::case_when(
    age < 40 ~ "< 40",
    dplyr::between(age, 40, 49) ~ "40-49",
    dplyr::between(age, 50, 59) ~ "50-59",
    dplyr::between(age, 60, 69) ~ "60-69",
    age >= 70 ~ ">= 70",
    .default = NA
  )

  cacs_group <- dplyr::case_when(
    coronary_calcium_score == 0 ~ "0",
    dplyr::between(coronary_calcium_score, 1, 99) ~ "1-99",
    dplyr::between(coronary_calcium_score, 100, 399) ~ "100-399",
    dplyr::between(coronary_calcium_score, 400, 999) ~ "400-999",
    coronary_calcium_score >= 1000 ~ ">= 1000",
    .default = NA
  )  

  ptp_percentage_group <- dplyr::case_when(
    age_group == "< 40" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 1.2 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 6.3,
    age_group == "< 40" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 23.2,
    age_group == "< 40" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 37.3,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 62.4,

    age_group == "40-49" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 1.7 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 8.6,
    age_group == "40-49" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 29.7,
    age_group == "40-49" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 45.3,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 69.9,
    
    age_group == "50-59" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 1.2 ,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 6.3,
    age_group == "50-59" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 23.2,
    age_group == "50-59" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 37.2,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 62.4,  

    age_group == "60-69" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 1.3 ,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 6.7,
    age_group == "60-69" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 24.4,
    age_group == "60-69" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 38.8,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 63.9, 

    age_group == ">= 70" & cacs_group == "0"       & sex == "male" & have_chest_pain == "no" ~ 0.9 ,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "no" ~ 4.9 ,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "no" ~ 18.8,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "no" ~ 31.3,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "no" ~ 56.1,

    age_group == "< 40" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 2.5 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 12.3,
    age_group == "< 40" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 38.6,
    age_group == "< 40" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 55.3,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 77.6,

    age_group == "40-49" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 3.4 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 16.3,
    age_group == "40-49" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 46.8,
    age_group == "40-49" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 63.3,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 82.8,
    
    age_group == "50-59" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 2.5,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 12.2,
    age_group == "50-59" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 38.6,
    age_group == "50-59" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 55.2,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 77.5,  

    age_group == "60-69" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 2.6,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 13.0,
    age_group == "60-69" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 40.2,
    age_group == "60-69" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 56.9,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 78.7, 

    age_group == ">= 70" & cacs_group == "0"       & sex == "male" & have_chest_pain == "yes" ~ 1.9,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "male" & have_chest_pain == "yes" ~ 9.7,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "male" & have_chest_pain == "yes" ~ 32.6,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "male" & have_chest_pain == "yes" ~ 48.7,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "male" & have_chest_pain == "yes" ~ 72.6,   
    
    age_group == "< 40" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 0.5 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 4.3 ,
    age_group == "< 40" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 19.8,
    age_group == "< 40" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 22.0,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 39.9,

    age_group == "40-49" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 0.7 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 5.9 ,
    age_group == "40-49" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 25.7,
    age_group == "40-49" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 28.2,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 48.1,
    
    age_group == "50-59" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 0.5 ,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 4.3 ,
    age_group == "50-59" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 19.8,
    age_group == "50-59" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 22.0,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 39.8,  

    age_group == "60-69" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 0.5 ,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 4.6 ,
    age_group == "60-69" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 20.9,
    age_group == "60-69" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 23.1,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 41.4, 

    age_group == ">= 70" & cacs_group == "0"       & sex == "female" & have_chest_pain == "no" ~ 0.4 ,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "no" ~ 3.3 ,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "no" ~ 15.9,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "no" ~ 17.8,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "no" ~ 33.7,    

    age_group == "< 40" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 1.1 ,
    age_group == "< 40" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 9.2 ,
    age_group == "< 40" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 35.7,
    age_group == "< 40" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 38.8,
    age_group == "< 40" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 59.9,

    age_group == "40-49" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 1.6 ,
    age_group == "40-49" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 12.3,
    age_group == "40-49" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 43.7,
    age_group == "40-49" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 46.9,
    age_group == "40-49" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 67.6,

    age_group == "50-59" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 1.1 ,
    age_group == "50-59" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 9.1 ,
    age_group == "50-59" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 35.6,
    age_group == "50-59" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 38.7,
    age_group == "50-59" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 59.8,
    
    age_group == "60-69" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 1.2 ,
    age_group == "60-69" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 9.7 ,
    age_group == "60-69" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 37.2,
    age_group == "60-69" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 40.3,
    age_group == "60-69" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 61.4,  

    age_group == ">= 70" & cacs_group == "0"       & sex == "female" & have_chest_pain == "yes" ~ 0.9 ,
    age_group == ">= 70" & cacs_group == "1-99"    & sex == "female" & have_chest_pain == "yes" ~ 7.2 ,
    age_group == ">= 70" & cacs_group == "100-399" & sex == "female" & have_chest_pain == "yes" ~ 29.9,
    age_group == ">= 70" & cacs_group == "400-999" & sex == "female" & have_chest_pain == "yes" ~ 32.7,
    age_group == ">= 70" & cacs_group == ">= 1000" & sex == "female" & have_chest_pain == "yes" ~ 53.4,

    .default = NA
  )

  if (isTRUE(output %in% c("numeric", "grouping"))) {

    if (isTRUE(output == "numeric")) {
      return(ptp_percentage_group)
    }

    ptp_percentage_group <- dplyr::case_when(
      ptp_percentage_group < 15 ~ "low",
      ptp_percentage_group >= 15 & ptp_percentage_group <= 50 ~ "intermediate",
      ptp_percentage_group > 50 ~ "high"

    )

    return(ptp_percentage_group)
  }

  if (isTRUE(output == "percentage")) {
    ptp_percentage_group <- ptp_percentage_group |> 
      round_to_nearest_digit(digits = 1) |> 
      formatC(digits = 1, format = "f") |> 
      stringr::str_c("%")
    return(ptp_percentage_group)
  }

}