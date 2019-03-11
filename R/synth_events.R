#' Example medical event records for 100 patients
#'
#' A dataset containing synthetic medical events records for the 100 patients
#' covered by the synth_presc dataset. Each row contains a patient ID and up to
#' two event dates (with ICD-10 codes). Structured to allow easy use within the
#' use at time point functions within this package which require individual
#' event dates.
#'
#' @format A data frame with 100 rows and 5 variavles
#' @description \describe{ \item{patient_id}{a unique patient identifier}
#'   \item{ev_date_1}{the date of the patients' first event, if applicable}
#'   \item{ev_code_1}{the ICD-10 code assigned to the first event}
#'   \item{ev_date_2}{the date of the patients' first event, if applicable}
#'   \item{ev_code_2}{the ICD-10 code assigned to the second event}}
#'
"synth_events"
