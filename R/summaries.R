# 1.3 - Data summaries

#' Prescription and Patient Counts and Date Range
#'
#' Function produces a high-level summary of a data frame, containin the number
#' of patients and prescriptions in the data and the date range of the
#' prescriptions. Can either be used to obtain a summary of all prescriptions,
#' or limited to a drug or drugs of interest
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, a drug identifier and a
#'   prescription date
#' @param drug a string corresponding to a drug identifier to be matched
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing the number of prescriptions, number of
#'   patients and the first and last prescription dates for the chosen data
#'   frame
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{presc_data_summary(synth_presc, drug_id_col = "approved_name", presc_date_col = "presc_date", date_format = "%d/%m/%Y")}
#' \code{presc_data_summary(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name", presc_date_col = "presc_date", date_format = "%d/%m/%Y")}
#'
presc_data_summary <- function(df, drug = "*", patient_id_col = "patient_id",
                     drug_id_col = "drug_id", presc_date_col = "presc_date",
                     date_format){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, date_format = date_format)
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, drug_id))
  result <- summ1 %>%
    dplyr::summarise(n_presc = n(),
              n_pat = dplyr::n_distinct(patient_id),
              date_first = min(presc_date_x),
              date_last = max(presc_date_x))
  return(result)
}


#' Produce a List of the Most Commonly Prescribed Drugs
#'
#' Function produces a list of the most commonly prescribed drugs and the number
#' of prescriptions for each, based on unique drug IDs. The number of results
#' returned can be adjusted.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a drug identifier
#' @param rank the number of drugs to be included in the list
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#'
#' @return a data frame containing a list of drug IDs and corresponding number
#'   of prescriptions
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{presc_top_drugs(synth_presc, drug_id_col = "approved_name")}
#' \code{presc_top_drugs(synth_presc, rank = 5, drug_id_col = "bnf_item_code")}
#'
presc_top_drugs <- function(df, drug = "*", rank = 10, drug_id_col = "drug_id"){
  tidy_df <- tidy_presc(df, drug_id_col = drug_id_col)
  freq <- tidy_df %>%
    dplyr::filter(grepl(drug, drug_id))
  freq <- freq %>%
    dplyr::group_by(drug_id) %>%
    dplyr::summarise(n_presc = n())
  freq <- freq %>%
    dplyr::top_n(rank, n_presc) %>%
    dplyr::arrange(desc(n_presc))
  return(freq)
}


#' Prescriptions Per Patient Summary Statistics
#'
#' Calculates the mean, median, minimum and maximum numbers of prescriptions per
#' patient within the data - either for all drugs, or for those matching the
#' specified ID
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a drug identifier
#' @param drug a string corresponding to a drug identifier to be matched
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#'
#' @return a dat aframe containing the mean, median, minimum and maximum number
#'   of prescriptions per patient
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{presc_per_patient(synth_presc, drug_id_col = "approved_name")}
#' \code{presc_per_patient(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name")}
#'
presc_per_patient <- function(df, drug = "*", patient_id_col = "patient_id",
                     drug_id_col = "drug_id"){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col)
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, drug_id))
  summ2 <- summ1 %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(n_presc = n())
  result <- summ2 %>%
    dplyr::summarise(mean = mean(n_presc),
              median = median(n_presc),
              min = min(n_presc),
              max = max(n_presc))
  return(result)
}


#' Prescription Time Trends
#'
#' Function creates a breakdown of the number of prescriptions matching a chosen
#' drug identifier over time - either broken down by month, quarter, semester or
#' year.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a drug identifier and a prescription date
#' @param drug a string corresponding to a drug identifier to be matched
#' @param group a string corresponding to the desired grouping for results -
#'   either month ("M"), quarter ("Q"), semester ("S") or year ("Y")
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param dd_disp_col a string, the name of the column in \code{df} containing
#'   the number of daily doses dispensed
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing the selected time intervals and number of
#'   prescriptions per interval
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{presc_by_time(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name", presc_date_col = "presc_date", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")}
#' \code{presc_by_time(synth_presc, drug = "212000", group = "M", drug_id_col = "bnf_paragraph", presc_date_col = "presc_date", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")}
#'
presc_by_time <- function(df, drug = "*", group = "Y",
                     drug_id_col = "drug_id", presc_date_col = "presc_date",
                     dd_disp_col = "dd_disp", date_format){
  tidy_df <- tidy_presc(df, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, dd_disp_col = dd_disp_col,
                        date_format = date_format)
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, drug_id)) %>%
    dplyr::mutate(month = lubridate::month(presc_date_x),
           year = lubridate::year(presc_date_x),
           quarter = lubridate::quarter(presc_date_x, with_year = TRUE),
           semester = lubridate::semester(presc_date_x, with_year = TRUE)) %>%
    dplyr::select(presc_date_x, month, year, quarter, semester, dd_disp)
  if(group == "Y"){
    result <- summ1 %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n_presc = n(),
                ddds_disp = sum(dd_disp))
  } else if (group == "M"){
    result <- summ1 %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarise(n_presc = n(),
                ddds_disp = sum(dd_disp))
  } else if (group == "S"){
    result <- summ1 %>%
      dplyr::group_by(semester) %>%
      dplyr::summarise(n_presc = n(),
                ddds_disp = sum(dd_disp))
  } else if (group == "Q"){
    result <- summ1 %>%
      dplyr::group_by(quarter) %>%
      dplyr::summarise(n_presc = n(),
                ddds_disp = sum(dd_disp))
  }
  return(result)
}
