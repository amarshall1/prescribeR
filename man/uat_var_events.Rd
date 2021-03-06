% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_at_time.R
\name{uat_var_events}
\alias{uat_var_events}
\title{Use at Time Point, Individual Patient Date Ranges}
\usage{
uat_var_events(df, df2, drug, flatten = FALSE,
  patient_id_col = "patient_id", drug_id_col = "drug_id",
  presc_date_col = "presc_date_x", ev_date_1_col = "ev_date_1",
  ev_date_2_col = "ev_date_2", date_format)
}
\arguments{
\item{df}{a data frame containing prescribing records to be analysed -
records must contain at least a patient ID, drug ID, prescription date}

\item{df2}{a data frame containing event data to be analysed, records must
contain at least two event dates (\code{ev_date_1} and \code{ev_date_2})}

\item{drug}{a string containing a drug ID to be used to limit the prescribing
data to the drug(s) of interest, accepts regular expressions}

\item{flatten}{logical, if TRUE the function will only count one record/
prescription per drug ID per date}

\item{patient_id_col}{a string, the name of the column in \code{df} and
\code{df2} containing the patient IDs}

\item{drug_id_col}{a string, the name of the column in \code{df} containing
the drug IDs}

\item{presc_date_col}{a string, the name of the column in \code{df}
containing}

\item{ev_date_1_col}{a string, the name of the column in \code{df2}
containing the first event dates}

\item{ev_date_2_col}{a string, the name of the column in \code{df2}
containing the second event dates if present}

\item{date_format}{a string, the format of the dates contained in \code{df}
and \code{df2}}
}
\value{
a data frame containing a list of patient IDs for patients who have
  at least one prescription matching the criteria, the number of
  prescriptions within the defined date window and the date of the first
  matching prescription
}
\description{
Determines which patients have been exposed to the drug(s) of interest within
a timeframe of interest. This function determines exposure by checking for
prescription dates between two patient-specific event dates.
}
\examples{
uat_var_events(synth_presc, df2 = synth_events, drug = "ATORVASTATIN",
drug_id_col = "approved_name", presc_date_col = "presc_date",
ev_date_1_col = "event_1", ev_date_2_col = "event_2")

}
