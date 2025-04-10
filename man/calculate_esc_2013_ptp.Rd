% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esc_2013_conference_ptp.R
\name{calculate_esc_2013_ptp}
\alias{calculate_esc_2013_ptp}
\title{Calculate ESC 2013 PTP for obstructive CAD}
\usage{
calculate_esc_2013_ptp(
  age,
  sex,
  chest_pain_type,
  output = c("numeric", "percentage")
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

\item{output}{Input text to indicate the how pre-test
probability results be expressed
Default: c("numeric", "percentage")
\itemize{
  \item numeric means the PTP will be expressed as an integer probability (0-100).
  \item percentage means the PTP will be expressed as percentage text (0-100\%).
}}
}
\value{
An integer or percentage representing the patient's PTP for obstructive CAD
based on the ESC 2013 guidelines.
}
\description{
This function returns a patient's
pre-test Probability (PTP) of obstructive
coronary artery disease (CAD) based on the
European Society of Cardiology (ESC) 2013 guidelines.
}
\details{
The predictive model used to create the guidelines are based on
the journal A clinical prediction rule for the diagnosis of coronary artery disease:
validation, updating, and extension by 2011 Genders et. al.
}
\examples{
# 35 year old female with typical chest pain
calculate_esc_2013_ptp(
    age = 35,
    sex = "female",
    chest_pain_type = "typical",
    output = "percentage"
)

# 65 year old male with nonanginal chest pain
calculate_esc_2013_ptp(
    age = 65,
    sex = "male",
    chest_pain_type = "nonanginal",
    output = "percentage"
)
}
