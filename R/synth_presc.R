#' Example prescribing records for 100 patients
#'
#' A dataset containing synthetic prescribing records for 100 patients, modelled
#' on a subset of the structure of Scottish Prescribing Information System (PIS)
#' data. Each row represents an individual prescription for a drug on a given
#' date, and contains a patient identifier, the date of the prescription, the
#' approved name and British National Formulary categories for the drug, the
#' quantity and strength dispensed and the number of Defined Daily Doses
#' dispensed.
#'
#' @format A data frame with 1478 rows and 13 variables
#' @description \describe{ \item{patient_id}{a unique patient identifier}
#'   \item{presc_date}{the date the drug was prescribed}
#'   \item{approved_name}{the approved name of the prescribable item}
#'   \item{qty_dispensed}{the number of units prescribed}
#'   \item{item_strength}{the item strength and unit of measurement}
#'   \item{bnf_chapter}{the chapter of the BNF in which the drug appears}
#'   \item{bnf_section}{the section of the BNF in which the drug appears}
#'   \item{bnf_subsection}{the sub-section of the BNF in which the drug appears}
#'   \item{bnf_paragraph}{the paragraph of the BNF in which the drug appears}
#'   \item{bnf_item_code}{a 15 digit code - the first seven digits detail the
#'   BNF categories and the last eight digits represent the medicinal product,
#'   form, strength and generic equivalent} \item{ddd_conversion}{a factor by
#'   which the quantity should be divided to give the number of Defined Daily
#'   Doses, based on strength} \item{ddd_dispensed}{the number of Defined Daily
#'   Doses dispensed based on the quantity and strength} \item{qty_per_day}{the
#'   prescriber's instruction for how many units should be taken per day} }
#'
"synth_presc"
