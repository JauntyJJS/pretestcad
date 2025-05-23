% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmrs_ptp.R
\name{calculate_prms_2017_ptp}
\alias{calculate_prms_2017_ptp}
\title{Calculate 2017 PROMISE Minimal-Risk Score for obstructive CAD}
\usage{
calculate_prms_2017_ptp(
  age,
  sex,
  hdl_mg_dl,
  is_minority_ethnicity,
  have_diabetes,
  have_hypertension,
  have_dyslipidemia,
  have_smoking_history,
  have_family_history,
  have_stress_symptoms = NA
)
}
\arguments{
\item{age}{Input numeric value to indicate the age of the patient.}

\item{sex}{Input characters (female, male) to indicate the sex of the patient.
\itemize{
  \item female
  \item male
}}

\item{hdl_mg_dl}{Input positive numeric value to indicate the
patient's high-density lipoprotein (HDL) in \eqn{mg/dL}.}

\item{is_minority_ethnicity}{Input characters (no, yes) to indicate if the patient
is from a racial or minority ethnicity
(or patient is not a non-Hispanic/Latino White).
\itemize{
  \item no stands for patient is a non-Hispanic/Latino White.
  \item yes stands for patient is not a non-Hispanic/Latino White. E.g. Blacks, Asians, etc.
}}

\item{have_diabetes}{Input characters (no, yes) to indicate if the patient
has diabetes.
\itemize{
  \item no stands for not having diabetes.
  \item yes stands for having diabetes.
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

\item{have_smoking_history}{Input characters (no, yes) to indicate if the patient
has a smoking history (current or past smoker).
\itemize{
  \item no stands for not having a smoking history (non-smoker).
  \item yes stands for having a smoking history (current or past smoker).
}}

\item{have_family_history}{Input characters (no, yes) to indicate if the patient
has a family history of CAD.
\itemize{
  \item no stands for not having a family history of CAD.
  \item yes stands for having a family history of CAD.
}}

\item{have_stress_symptoms}{Input characters (no, yes) to indicate if the patient
has symptoms related to physical or mental stress. It can be set
to \code{NA} if the patient results are inconclusive or have not taken any stress test
such as exercise treadmill testing, stress echocardiography, or stress
nuclear imaging.
\itemize{
  \item no stands for no symptoms (negative results) related to physical or mental stress.
  \item yes stands for having symptoms (positive results) related to physical or mental stress.
  \item \code{NA} stands for inconclusive results or patient has not taken any stress test
}
Default: \code{NA}}
}
\value{
A numeric value representing the patient's minimal risk
score for obstructive CAD based on the 2017 PROMISE Minimal-Risk Score.
}
\description{
This function returns
a symptomatic (have chest pain or dyspnoea) patient's
minimal risk score for obstructive
coronary artery disease based on the
2017 PROMISE Minimal-Risk Score.
}
\details{
The predictive model is based on CCTA images from 4632
patients in the Prospective Multicenter imaging Study for Evaluation
of Chest Pain (PROMISE) trial.
}
\examples{
# 50 year old white female with chest pain
# a medical history of hypertension, and a
# high-density lipoprotein cholesterol level of 70 mg/dL
calculate_prms_2017_ptp(
    age = 50,
    sex = "female",
    hdl_mg_dl = 70,
    is_minority_ethnicity = "no",
    have_diabetes = "no",
    have_hypertension = "yes",
    have_dyslipidemia = "no",
    have_smoking_history = "no",
    have_family_history = "no",
    have_stress_symptoms = "no"
)

# 40 year old non-white male with chest pain
# a medical history of diabetes, unknown stress symptoms and a
# high-density lipoprotein cholesterol level of 70 mg/dL
calculate_prms_2017_ptp(
    age = 40,
    sex = "male",
    hdl_mg_dl = 70,
    is_minority_ethnicity = "yes",
    have_diabetes = "yes",
    have_hypertension = "no",
    have_dyslipidemia = "no",
    have_smoking_history = "no",
    have_family_history = "no",
    have_stress_symptoms = NA
)
}
