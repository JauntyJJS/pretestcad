#' @title Calculate 2020 Winther Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2020 Winther et. al. basic model (Basic_PTP).
#' @param age Input integer to indicate the age of the patient.
#' @param sex Input integer 0 or 1 to indicate the sex of the patient.
#' \itemize{
#'   \item 0 stands for Female
#'   \item 1 stands for Male
#' }
#' @param chest_pain Input integer 1 to 3 to indicate the chest pain
#' characteristics of the patient.
#' \itemize{
#'   \item 1 stands for the patient having typical chest pain.
#'   \item 2 stands for the patient having atypical chest pain or dyspnea.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#' @return A numeric value representing the patient's PTP for obstructive CAD
#' based on the 2020 Winther et. al. basic model (Basic_PTP).
#' @details The predictive model is based on > 40000 symptomatic
#' patients from 2008 to 2017 from 13 hospitals in Western Denmark. These
#' patients are registered under the Western Denmark Heart Registry (WDHR).
#'
#' @examples
#' # 40 year old Male with typical chest pain
#' calculate_winther_2020_basic_ptp(
#'     age = 40,
#'     sex = 1,
#'     chest_pain = 1
#' )
#'
#' # 40 year old Male with non-anginal chest pain
#' calculate_winther_2020_basic_ptp(
#'     age = 40,
#'     sex = 1,
#'     chest_pain = 3
#' )
#' @rdname calculate_winther_2020_basic_ptp
#' @export
calculate_winther_2020_basic_ptp <- function(
    age,
    sex,
    chest_pain
  )
{
  has_non_anginal_chest_pain <- dplyr::case_when(
    chest_pain %in% c(1, 2) ~ 0,
    chest_pain == 3 ~ 1
  )

  has_typical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(2, 3) ~ 0,
    chest_pain == 1 ~ 1
  )

  winther_2020_basic_ptp <- 1 /
    (1 + exp(-(-7.0753 +
              ( 1.2308 * sex) +
              ( 0.0642 * age) +
              ( 2.2501 * has_typical_chest_pain) +
              (-0.5095 * has_non_anginal_chest_pain) +
              (-0.0191 * age * has_typical_chest_pain)
    )
    )
    )

  return(winther_2020_basic_ptp)
}
