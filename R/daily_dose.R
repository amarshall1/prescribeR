#' Calculate Cumulative Daily Dose
#'
#' Calculates the cumulative number of daily doses of a drug of interest
#' dispensed for each patient. Results can be limited to only patients who have
#' been prescribed more than a desired threshold number of daily doses.
#'
#' @param df a data frame containing prescribing records  to be analysed -
#'   records must contain at least a paitent ID, drug ID, a prescription date
#'   and the number of daily doses disepnsed
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param threshold a number representing the minimum number of daily doses that
#'   must be present for the patient to be considered exposed
#' @param patient_id_col a string, the name of the column in \code{df} containing
#'   the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing the
#'   drug IDs
#' @param presc_date_col a string, the name of the column in \code{df} containing
#'   the prescption date
#' @param dd_disp_col a string, the name of the column in \code{df} containing the
#'   number of daily doses dispensed
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing patient IDs and the corresponding number of
#'   prescriptions, number of daily doses dispensed and the date of the first
#'   prescription of the drug of interest
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' dd_sum(synth_presc, drug = "CITALOPRAM", threshold = 500,
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' dd_disp_col = "ddd_dispensed", date_format = "%Y-%m-%d")
#'
dd_sum <- function(df,
                   drug,
                   threshold = 1,
                   patient_id_col = "patient_id",
                   drug_id_col = "drug_id",
                   presc_date_col = "presc_date_x",
                   dd_disp_col = "dd_disp",
                   date_format) {
  tidy_df <-
    tidy_presc(
      df,
      patient_id_col = patient_id_col,
      drug_id_col = drug_id_col,
      presc_date_col = presc_date_col,
      dd_disp_col = dd_disp_col,
      date_format = date_format
    )
  dd1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  dd1 <- dd1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::summarise(
      n_presc = dplyr::n(),
      total_dds = sum(.data$dd_disp),
      first_presc = min(.data$presc_date_x)
    ) %>%
    dplyr::filter(.data$total_dds >= threshold)
  return(dd1)
}



#' Calculate Prescription Durations
#'
#' Calculates the duration in days of individual prescriptions based on the
#' number of daily doses dispensed. A multiplication factor is applied to allow
#' for use in cases where, for example an assumption of less or more than 1 DDD
#' per day is required
#'
#' @param df a data frame containing prescribing records  to be analysed -
#'   records must contain at least a paitent ID, drug ID, a prescription date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param dd_factor a number, a multiplication factor applied to the dd_disp
#'   field to calculate prescription duration
#' @param patient_id_col a string, the name of the column in \code{df} containing
#'   the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing the
#'   drug IDs
#' @param presc_date_col a string, the name of the column in \code{df} containing
#'   the prescption date
#' @param dd_disp_col a string, the name of the column in \code{df} containing the
#'   number of daily doses dispensed
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a modified version of \code{df} with records for the drug of interest
#'   with the \code{duration} field added
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' dd_duration(synth_presc, drug = "212000",
#' drug_id_col = "bnf_paragraph", presc_date_col = "presc_date",
#' dd_disp_col = "ddd_dispensed", date_format = "%Y-%m-%d")
#'
dd_duration <- function(df,
                        drug,
                        dd_factor = 1,
                        patient_id_col = "patient_id",
                        drug_id_col = "drug_id",
                        presc_date_col = "presc_date_x",
                        dd_disp_col = "dd_disp",
                        date_format) {
  tidy_df <-
    tidy_presc(
      df,
      patient_id_col = patient_id_col,
      drug_id_col = drug_id_col,
      presc_date_col = presc_date_col,
      dd_disp_col = dd_disp_col,
      date_format = date_format
    )
  dd1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  dd1 <- dd1 %>%
    dplyr::mutate(
      duration = floor(.data$dd_disp * dd_factor),
      end_date = .data$presc_date_x + floor(.data$dd_disp * dd_factor)
    )
}


#' Calculate Number of Prescribed Daily Doses Dispensed
#'
#' Calculates the number of Prescribed Daily Doses (PDDs) dispensed based on
#' individual patient instructions on how many tablets should be taken per day,
#' and the number of tablets dispensed, and adds this as a new column to the
#' data frame.
#'
#' @param df a data frame containing prescribing records  to be analysed -
#'   records must contain at least a paitent ID, drug ID, a quantity dispensed
#'   and a quantity per day instruction
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param qty_disp_col a string, the name of the column in \code{df} containing
#'   the quantity of drug dispensed
#' @param qty_per_day_col a string, the name of the column in \code{df}
#'   containing the quantity of drug to be taken per day
#'
#' @return a modified version of \code{df} containing records for the drug of
#'   interest with the \code{dd_disp} field added
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' calculate_pdd(synth_presc, drug = "ATORVASTATIN",
#' drug_id_col = "approved_name", qty_disp_col = "qty_dispensed")
#'
calculate_pdd <- function(df,
                          drug,
                          drug_id_col = "drug_id",
                          qty_disp_col = "qty_disp",
                          qty_per_day_col = "qty_per_day") {
  tidy_df <- tidy_presc(
    df,
    drug_id_col = drug_id_col,
    qty_disp_col = qty_disp_col,
    qty_per_day_col = qty_per_day_col
  )
  dd1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  dd1 <- dd1 %>%
    dplyr::mutate(pdd_disp = .data$qty_disp / .data$qty_per_day)
  return(dd1)
}
