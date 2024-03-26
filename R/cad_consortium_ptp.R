#' @title Calculate 2012 CAD2 Basic PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2012 CAD Consortium 2 (CAD2) basic model.
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
#'   \item 2 stands for the patient having atypical chest pain.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#' @return A numeric value or percentage representing the patient's PTP for obstructive CAD
#' based on the 2012 CAD Consortium 2 (CAD2) basic model.
#' @details The predictive model used to create the guidelines are based on
#' patients from 18 hospitals in Europe and the United States.
#'
#' The 2012 CAD Consortium 2 (CAD2) PTP models are as follows:
#'
#' \if{html}{\figure{cad2_2012.jpg}{alt="2012 CAD Consortium 2 pre-test probabilities of obstructive coronary artery disease model."}}
#' \emph{CAD Consortium 2 (CAD2) pre-test probabilities (PTP) of obstructive coronary artery disease model taken from}
#' \href{https://doi.org/10.1136/bmj.e3485}{\emph{Tessa Genders et. al.}} from
#' \href{https://www.bmj.com/}{\emph{British Medical Journal}} \emph{is licensed under}
#' \href{http://creativecommons.org/licenses/by/4.0}{\emph{CC BY 4.0}}.
#' @examples
#' # 40 year old female with typical chest pain
#' calculate_cad2_2012_basic_ptp(
#'     age = 40,
#'     sex = 0,
#'     chest_pain = 1
#' )
#' @rdname calculate_cad2_2012_basic_ptp
#' @export
calculate_cad2_2012_basic_ptp <- function(
    age,
    sex,
    chest_pain
  )
{

  has_atypical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(1, 3) ~ 0,
    chest_pain == 2 ~ 1
  )

  has_typical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(2, 3) ~ 0,
    chest_pain == 1 ~ 1
  )

  cad2_2012_basic_ptp <- 1 /
    (1 + exp(-(-6.917 +
               (0.063 * age) +
               (1.358 * sex) +
               (0.658 * has_atypical_chest_pain) +
               (1.975 * has_typical_chest_pain)
    )
    )
    )

  return(cad2_2012_basic_ptp)

}

#' @title Calculate 2012 CAD2 Clinical PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2012 CAD Consortium 2 (CAD2) clinical model.
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
#'   \item 2 stands for the patient having atypical chest pain.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#' @param has_diabetes Input integer 0 or 1 to indicate if the patient
#' has diabetes.
#' \itemize{
#'   \item 0 stands for not having diabetes.
#'   \item 1 stands for having diabetes.
#' }
#' @param has_hypertension Input integer 0 or 1 to indicate if the patient
#' has hypertension.
#' \itemize{
#'   \item 0 stands for not having hypertension.
#'   \item 1 stands for having hypertension.
#' }
#' @param has_dyslipidemia Input integer 0 or 1 to indicate if the patient
#' has dyslipidemia.
#' \itemize{
#'   \item 0 stands for not having dyslipidemia.
#'   \item 1 stands for having dyslipidemia.
#' }
#' @param has_smoking_history Input integer 0 or 1 to indicate if the patient
#' has a smoking history (current or past smoker).
#' \itemize{
#'   \item 0 stands for not having a smoking history (non-smoker).
#'   \item 1 stands for having a smoking history (current or past smoker).
#' }
#' @return A numeric value or percentage representing the patient's PTP for obstructive CAD
#' based on the 2012 CAD Consortium 2 (CAD2) clinical model.
#' @details The predictive model used to create the guidelines are based on
#' patients from 18 hospitals in Europe and the United States.
#'
#' The 2012 CAD Consortium 2 (CAD2) PTP models are as follows:
#'
#' \if{html}{\figure{cad2_2012.jpg}{alt="2012 CAD Consortium 2 pre-test probabilities of obstructive coronary artery disease model."}}
#' \emph{CAD Consortium 2 (CAD2) pre-test probabilities (PTP) of obstructive coronary artery disease model taken from}
#' \href{https://doi.org/10.1136/bmj.e3485}{\emph{Tessa Genders et. al.}} from
#' \href{https://www.bmj.com/}{\emph{British Medical Journal}} \emph{is licensed under}
#' \href{http://creativecommons.org/licenses/by/4.0}{\emph{CC BY 4.0}}.
#' @examples
#' # 40 year old female with typical chest pain,
#' # diabetes but no hypertension, dyslipidemia
#' # and a non-smoker
#' calculate_cad2_2012_clinical_ptp(
#'     age = 40,
#'     sex = 0,
#'     chest_pain = 1,
#'     has_diabetes = 1,
#'     has_hypertension = 0,
#'     has_dyslipidemia = 0,
#'     has_smoking_history = 0
#'
#' )
#' @rdname calculate_cad2_2012_clinical_ptp
#' @export
calculate_cad2_2012_clinical_ptp <- function(
    age,
    sex,
    chest_pain,
    has_diabetes,
    has_hypertension,
    has_dyslipidemia,
    has_smoking_history
)
{

  has_atypical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(1, 3) ~ 0,
    chest_pain == 2 ~ 1
  )

  has_typical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(2, 3) ~ 0,
    chest_pain == 1 ~ 1
  )

  cad2_2012_clinical_ptp <- 1 /
    (1 + exp(-(-7.539 +
              (0.062 * age) +
              (1.332 * sex) +
              (0.633 * has_atypical_chest_pain) +
              (1.998 * has_typical_chest_pain) +
              (0.828 * has_diabetes) +
              (0.338 * has_hypertension) +
              (0.422 * has_dyslipidemia) +
              (0.461 * has_smoking_history) +
              (-0.402 * has_diabetes * has_typical_chest_pain)
    )
    )
    )

  return(cad2_2012_clinical_ptp)

}

#' @title Calculate 2012 CAD2 Clinical and CCS PTP for obstructive CAD
#' @description This function returns a patient's
#' pre-test probability (PTP) of obstructive
#' coronary artery disease based on the
#' 2012 CAD Consortium 2 (CAD2) clinical and coronary calcium score (CCS) model.
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
#'   \item 2 stands for the patient having atypical chest pain.
#'   \item 3 stands for the patient having non-anginal or non-specific chest pain.
#' }
#' @param has_diabetes Input integer 0 or 1 to indicate if the patient
#' has diabetes.
#' \itemize{
#'   \item 0 stands for not having diabetes.
#'   \item 1 stands for having diabetes.
#' }
#' @param has_hypertension Input integer 0 or 1 to indicate if the patient
#' has hypertension.
#' \itemize{
#'   \item 0 stands for not having hypertension.
#'   \item 1 stands for having hypertension.
#' }
#' @param has_dyslipidemia Input integer 0 or 1 to indicate if the patient
#' has dyslipidemia.
#' \itemize{
#'   \item 0 stands for not having dyslipidemia.
#'   \item 1 stands for having dyslipidemia.
#' }
#' @param has_smoking_history Input integer 0 or 1 to indicate if the patient
#' has a smoking history (current or past smoker).
#' \itemize{
#'   \item 0 stands for not having a smoking history (non-smoker).
#'   \item 1 stands for having a smoking history (current or past smoker).
#' }
#' @param coronary_calcium_score Input positive numeric to indicate the
#' total coronary calcium score of the patient.
#' @return A numeric value or percentage representing the patient's PTP for obstructive CAD
#' based on the 2012 CAD Consortium 2 (CAD2) clinical and coronary calcium score (CCS) model.
#' @details The predictive model used to create the guidelines are based on
#' patients from 18 hospitals in Europe and the United States.
#'
#' The 2012 CAD Consortium 2 (CAD2) PTP models are as follows:
#'
#' \if{html}{\figure{cad2_2012.jpg}{alt="2012 CAD Consortium 2 pre-test probabilities of obstructive coronary artery disease model."}}
#' \emph{CAD Consortium 2 (CAD2) pre-test probabilities (PTP) of obstructive coronary artery disease model taken from}
#' \href{https://doi.org/10.1136/bmj.e3485}{\emph{Tessa Genders et. al.}} from
#' \href{https://www.bmj.com/}{\emph{British Medical Journal}} \emph{is licensed under}
#' \href{http://creativecommons.org/licenses/by/4.0}{\emph{CC BY 4.0}}.
#' @examples
#' # 40 year old female with typical chest pain,
#' # diabetes but no hypertension, dyslipidemia,
#' # a non-smoker and a coronary calcium score of 0
#' calculate_cad2_2012_clinical_ccs_ptp(
#'     age = 40,
#'     sex = 0,
#'     chest_pain = 1,
#'     has_diabetes = 1,
#'     has_hypertension = 0,
#'     has_dyslipidemia = 0,
#'     has_smoking_history = 0,
#'     coronary_calcium_score = 0
#'
#' )
#' @rdname calculate_cad2_2012_clinical_ccs_ptp
#' @export
calculate_cad2_2012_clinical_ccs_ptp <- function(
    age,
    sex,
    chest_pain,
    has_diabetes,
    has_hypertension,
    has_dyslipidemia,
    has_smoking_history,
    coronary_calcium_score
)
{

  has_atypical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(1, 3) ~ 0,
    chest_pain == 2 ~ 1
  )

  has_typical_chest_pain <- dplyr::case_when(
    chest_pain %in% c(2, 3) ~ 0,
    chest_pain == 1 ~ 1
  )

  log_transformed_ccs <- log(coronary_calcium_score + 1)

  cad2_2012_clinical_ccs_ptp <- 1 /
    (1 + exp(-(-5.975 +
              (0.011 * age) +
              (0.786 * sex) +
              (0.718 * has_atypical_chest_pain) +
              (2.024 * has_typical_chest_pain) +
              (0.658 * has_diabetes) +
              (0.235 * has_hypertension) +
              (0.185 * has_dyslipidemia) +
              (0.207 * has_smoking_history) +
              (0.577 * log_transformed_ccs) +
              (-0.780 * has_diabetes * has_typical_chest_pain)
    )
    )
    )

  return(cad2_2012_clinical_ccs_ptp)

}
