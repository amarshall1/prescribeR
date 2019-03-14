#' Determine Ever Use of Drug(s) of Interest
#'
#' Determines which patients within a data frame have been exposed to the drug
#' of interest, based on having at least the desired number of prescriptions
#' matching the drug identifier of interest, with option to also provide counts
#' and dates of the first matching prescription.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a paitent ID, drug ID and a prescription date
#' @param drug a string corresponding to a drug identifier to be matched
#' @param summary logical, if TRUE the corresponding prescription counts and
#'   first prescription dates will be returned alongside patient IDs
#' @param threshold a number representing the minimum number of prescriptions
#'   that must be present for the patient to be considered exposed
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing patient IDs who match the selected criteria,
#'   and if \code{summary} is TRUE the corresponding prescription counts and
#'   dates
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{ever_use(synth_presc, drug = "CITALOPRAM", drug_id_col = "approved_name")}
#' \code{ever_use(synth_presc, drug = "212000", summary = TRUE, threshold = 10,
#' drug_id_col = "bnf_paragraph", presc_date_col = "presc_date",
#' date_format = "%d/%m/%Y")}
#'

ever_use <- function(df, drug, summary = FALSE, threshold = 1,
                     patient_id_col = "patient_id", drug_id_col = "drug_id",
                     presc_date_col = "presc_date_x", date_format){
  tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
                        presc_date = presc_date_col, date_format = date_format)
  ever1 <- tidy_df %>%
    dplyr::filter(grepl(drug, drug_id)) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(n_presc = n(),
              first_presc = min(presc_date_x)) %>%
    dplyr::filter(n_presc >= threshold)
  if(summary == TRUE){
    ever_result <- ever1
    return(ever_result)
  } else {
    ever_result <- ever1 %>%
      dplyr::select(patient_id)
    return(ever_result)
  }
}
