% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcs_ptp.R
\name{calculate_dcs_1993_severe_cad_ptp}
\alias{calculate_dcs_1993_severe_cad_ptp}
\title{Calculate 1993 Duke Clinical Score for Severe CAD}
\usage{
calculate_dcs_1993_severe_cad_ptp(
  age,
  sex,
  chest_pain_type,
  have_progressive_angina,
  have_nocturnal_angina,
  have_peripheral_vascular_disease,
  have_cerebrovascular_disease,
  have_carotid_bruits,
  have_hypertension,
  have_dyslipidemia,
  have_diabetes,
  have_q_waves,
  have_st_t_changes,
  frequency_of_angina_pain_per_week,
  duration_of_cad_symptoms_year,
  max_na_risk_factor_index = 0,
  max_na_pain_index = 0,
  max_na_vascular_disease_index = 0,
  max_frequency_of_angina_pain_per_week = 35
)
}
\arguments{
\item{age}{Input integer value to indicate the age of the patient.}

\item{sex}{Input characters (female, male) to indicate the sex of the patient.
\itemize{
  \item female
  \item male
}}

\item{chest_pain_type}{Input characters (typical, atypical, nonanginal)
to indicate the chest pain characteristics of the patient.
\itemize{
  \item typical stands for the patient having typical chest pain.
  \item atypical stands for the patient having atypical chest pain.
  \item nonanginal stands for the patient having nonanginal or non-specific chest pain.
}}

\item{have_progressive_angina}{Input characters (no, yes) to indicate if the patient
has progressive angina.
\itemize{
  \item no stands for not having progressive angina.
  \item yes stands for having progressive angina.
}}

\item{have_nocturnal_angina}{Input characters (no, yes) to indicate if the patient
has nocturnal angina.
\itemize{
  \item no stands for not having nocturnal angina.
  \item yes stands for having nocturnal angina.
}}

\item{have_peripheral_vascular_disease}{Input characters (no, yes) to indicate if the patient
has peripheral vascular disease.
\itemize{
  \item no stands for not having peripheral vascular disease.
  \item yes stands for having peripheral vascular disease.
}}

\item{have_cerebrovascular_disease}{Input characters (no, yes) to indicate if the patient
has cerebrovascular disease.
\itemize{
  \item no stands for not having cerebrovascular disease.
  \item yes stands for having cerebrovascular disease.
}}

\item{have_carotid_bruits}{Input characters (no, yes) to indicate if the patient
has carotid bruits.
\itemize{
  \item no stands for not having carotid bruits.
  \item yes stands for having carotid bruits.
}}

\item{have_hypertension}{Input characters (no, yes) to indicate if the patient
has hypertension.
\itemize{
  \item no stands for not having hypertension.
  \item yes stands for having hypertension.
}}

\item{have_dyslipidemia}{Input characters (no, yes) to indicate if the patient
has dyslipidemia.
\itemize{
  \item no stands for not having dyslipidemia.
  \item yes stands for having dyslipidemia.
}}

\item{have_diabetes}{Input characters (no, yes) to indicate if the patient
has diabetes.
\itemize{
  \item no stands for not having diabetes.
  \item yes stands for having diabetes.
}}

\item{have_q_waves}{Input characters (no, yes) to indicate if the patient
has Q waves on electrocardiogram (ECG).
\itemize{
  \item no stands for the patient not having Q waves on ECG.
  \item yes stands for the patient having Q waves on ECG.
}}

\item{have_st_t_changes}{Input characters (no, yes) to indicate if the patient
has ST-T changes on electrocardiogram (ECG).
\itemize{
  \item no stands for the patient not having ST-T changes on ECG.
  \item yes stands for the patient having ST-T changes on ECG.
}}

\item{frequency_of_angina_pain_per_week}{Input integer to indicate the patient's
frequency of angina per week.}

\item{duration_of_cad_symptoms_year}{Input integer to indicate the duration of
coronary artery disease symptoms in years.}

\item{max_na_risk_factor_index}{Input integer 0 to 3 to indicate the maximum number of
missing risk factors to tolerate before outputting an \code{NA}.
Default: 0}

\item{max_na_pain_index}{Input integer 0 to 5 to indicate the maximum number of
missing symptoms to tolerate before outputting an \code{NA}.
Default: 0}

\item{max_na_vascular_disease_index}{Input integer 0 to 3 to indicate the maximum number of
missing disease history to tolerate before outputting an \code{NA}.
Default: 0}

\item{max_frequency_of_angina_pain_per_week}{Input non-negative integer to
indicate the maximum frequency angina per week to tolerate before outputting an \code{NA}.
In the Duke Clinical Score 1993 paper, the maximum value is set as 35.
Default: 35}
}
\value{
A numeric value representing the patient's PTP for severe
(>75\% luminal diameter narrowing of all three major coronary arteries
or of the left main coronary artery) CAD
based on the 1993 Duke Clinical Score.
}
\description{
This function returns a patient's
pre-test probability (PTP) of severe (>75\% luminal diameter narrowing
of all three major coronary arteries or of the left main coronary artery)
coronary artery disease based on the
1993 Duke Clinical Score.
}
\details{
The predictive model is based on
patients referred for cardiac catheterisation between 1969 and 1983.
}
\examples{
# 40 year old female with typical chest pain for one year,
# She has progressive angina but no nocturnal angina.
# Angina pain lasted at most five times a week.
# She has peripheral vascular and cerebrovascular disease.
# She has hypertension but has no dyslipidemia and not diabetic.
# She has Q waves and ST-T changes on ECG.

calculate_dcs_1993_severe_cad_ptp(
    age = 40,
    sex = "female",
    chest_pain_type = "typical",
    have_progressive_angina = "yes",
    have_nocturnal_angina = "no",
    have_peripheral_vascular_disease = "yes",
    have_cerebrovascular_disease = "yes",
    have_carotid_bruits = "no",
    have_hypertension = "yes",
    have_dyslipidemia = "no",
    have_diabetes = "no",
    have_q_waves = "yes",
    have_st_t_changes = "yes",
    frequency_of_angina_pain_per_week = 5,
    duration_of_cad_symptoms_year = 1,
)
}
