#' Determine Ever Use of Drug(s) of Interest
#'
#' Determines which patients within a data frame have been exposed to the drug
#' of interest, based on having at least the desired number of prescriptions
#' matching the drug identifier of interest, with option to also provide counts
#' and dates of the first matching prescription.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a paitent ID, drug ID and a prescription date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param flatten logical, if TRUE the function will only count one record/
#'   prescription per drug ID per date
#' @param threshold a number representing the minimum number of prescriptions
#'   that must be present for the patient to be considered exposed
#' @param summary logical, if TRUE the corresponding prescription counts and
#'   first prescription dates will be returned alongside patient IDs
#' @param return_all logical, if TRUE the output will contain all patients with
#'   a flag indicating if they were exposed or not, and if FALSE will only
#'   return data for patients who meet the exposure threshold
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
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @examples
#' ever_use(synth_presc, drug = "CITALOPRAM", drug_id_col = "approved_name",
#' presc_date_col = "presc_date")
#' ever_use(synth_presc, drug = "212000", summary = TRUE, threshold = 10,
#' flatten = TRUE, drug_id_col = "bnf_paragraph",
#' presc_date_col = "presc_date", date_format = "%Y-%m-%d")
#'

ever_use <- function(df, drug, flatten = FALSE, threshold = 1, summary = FALSE, return_all = FALSE,
                     patient_id_col = "patient_id", drug_id_col = "drug_id",
                     presc_date_col = "presc_date_x", date_format){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, date_format = date_format)
  ids <- tidy_df %>%
    dplyr::select(.data$patient_id) %>%
    dplyr::distinct()
  ever1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  if(flatten == TRUE){
    ever1 <- ever1 %>%
      dplyr::distinct(.data$patient_id, .data$drug_id, .data$presc_date_x)
  }
  ever1 <- ever1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::summarise(n_presc = dplyr::n(),
                     first_presc = min(.data$presc_date_x)) %>%
    dplyr::mutate(exposed = ifelse(.data$n_presc >= threshold, 1, 0))
  if(return_all == FALSE){
    ever1 <- ever1 %>%
      dplyr::filter(.data$exposed == 1)
  } else if(return_all == TRUE){
    ever1 <- dplyr::left_join(ids, ever1, by = "patient_id")
    ever1$n_presc[is.na(ever1$n_presc)] <- 0
    ever1$exposed[is.na(ever1$exposed)] <- 0
  }
  if(summary == TRUE){
    return(ever1)
  } else if(summary == FALSE){
    ever1 <- ever1 %>%
      dplyr::select(.data$patient_id, .data$exposed)
    return(ever1)
  }
}



