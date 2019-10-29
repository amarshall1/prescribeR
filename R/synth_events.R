#' Example medical event records for 100 patients
#'
#' A dataset containing synthetic medical events records for the 100 patients
#' covered by the synth_presc dataset. Each row contains a patient ID, start and
#' end of follow-up dates and up to two event dates
#'
#' @format A data frame with 100 rows and 5 variavles
#' @description \describe{ \item{patient_id}{a unique patient identifier}
#'   \item{start_date}{the start date of follow-up (the date of their first
#'   prescription in synth_presc)} \item{end_date}{the end date of follow-up}
#'   \item{event_1}{the date of the patient's first event, if applicable}
#'   \item{event_2}{the date of the patient's second event, if applicable}}
#'
"synth_events"
