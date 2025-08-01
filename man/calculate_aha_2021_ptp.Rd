% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aha_2021_conference_ptp.R
\name{calculate_aha_2021_ptp}
\alias{calculate_aha_2021_ptp}
\title{Calculate AHA/ACC 2021 PTP for obstructive CAD}
\usage{
calculate_aha_2021_ptp(
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
}
\arguments{
\item{age}{Input integer value to indicate the age of the patient in years.}

\item{sex}{The value of variable in the parameters \code{label_sex_male},
\code{label_sex_female} and \code{label_sex_unknown}.}

\item{have_dyspnoea}{The value of variable in the parameters \code{label_have_dyspnoea_no},
\code{label_have_dyspnoea_yes} and \code{label_have_dyspnoea_unknown}.}

\item{have_chest_pain}{The value of variable in the parameters \code{label_have_chest_pain_no},
\code{label_have_chest_pain_yes} and \code{label_have_chest_pain_unknown}.}

\item{output}{Input text to indicate the how pre-test
probability results be expressed
Default: c("grouping", "numeric", "percentage")
\itemize{
  \item grouping means the PTP will be expressed as Low, Intermediate and High.
  \itemize{
     \item very low if PTP is less than 5\%.
     \item low if PTP is in between 5\% to 15\%.
     \item intermediate if PTP is in between 15\% to 50\%.
     \item high if PTP is more than 50\%.
  }
  \item numeric means the PTP will be expressed as an integer probability (0-100).
  \item percentage means the PTP will be expressed as percentage text (0-100\%).
}}

\item{label_sex_male}{Label(s) for definition(s) of male sex.
Default: \code{c("male")}}

\item{label_sex_female}{Label(s) for definition(s) of female sex.
Default: \code{c("female")}}

\item{label_sex_unknown}{Label(s) for definition(s) of missing sex.
Default: \code{c(NA, NaN)}}

\item{label_have_dyspnoea_no}{Label(s) for patient having no dyspnoea symptoms.
Default: \code{c("no")}}

\item{label_have_dyspnoea_yes}{Label(s) for patient having dyspnoea symptoms.
Default: \code{c("yes")}}

\item{label_have_dyspnoea_unknown}{Label(s) for patient having unknown dyspnoea symptoms.
Default: \code{c(NA, NaN)}}

\item{label_have_chest_pain_no}{Label(s) for patient not having chest pain symptoms.
Default: \code{c("no")}}

\item{label_have_chest_pain_yes}{Label(s) for patient having chest pain symptoms.
Default: \code{c("yes")}}

\item{label_have_chest_pain_unknown}{Label(s) for patient with unknown chest pain symptoms.
Default: \code{c(NA, NaN)}}
}
\value{
An integer, percentage or category representing the patient's PTP for obstructive CAD
based on the AHA/ACC 2021 guidelines.
See parameter option \code{output} for more information.
}
\description{
This function returns a patient's
pre-test Probability (PTP) of obstructive
coronary artery disease (CAD) based on the
American Heart Association/American College of Cardiology (AHA/ACC) 2021 guidelines.
}
\details{
The predictive model used to create the guidelines are based on
patients from European countries with low cardiovascular disease (CVD) risk.

If the patient has both dyspnoea and a particular
chest pain type (typical, atypical, nonanginal),
The chest pain type will take precedence over dyspnoea
}
\examples{
# 35 year old female with chest pain
calculate_aha_2021_ptp(
    age = 35,
    sex = "female",
    have_dyspnoea = "no",
    have_chest_pain = "yes",
    output = "percentage"
)

# 75 year old male with only dyspnoea
calculate_aha_2021_ptp(
    age = 75,
    sex = "male",
    have_dyspnoea = "yes",
    have_chest_pain = "no",
    output = "percentage"
)
}
