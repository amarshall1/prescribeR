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
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
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
#' @importFrom rlang .data
#'
#' @examples
#' presc_data_summary(synth_presc, drug_id_col = "approved_name",
#' presc_date_col = "presc_date", date_format = "%Y-%m-%d")
#' presc_data_summary(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name",
#' presc_date_col = "presc_date", date_format = "%Y-%m-%d")
#'
presc_data_summary <- function(df, drug = "*", patient_id_col = "patient_id",
                     drug_id_col = "drug_id", presc_date_col = "presc_date_x",
                     date_format){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, date_format = date_format)
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  result <- summ1 %>%
    dplyr::summarise(n_presc = dplyr::n(),
              n_pat = dplyr::n_distinct(.data$patient_id),
              date_first = min(.data$presc_date_x),
              date_last = max(.data$presc_date_x))
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
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#'
#' @return a data frame containing a list of drug IDs and corresponding number
#'   of prescriptions
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' presc_top_drugs(synth_presc, drug_id_col = "approved_name")
#' presc_top_drugs(synth_presc, rank = 5, drug_id_col = "bnf_item_code")
#'
presc_top_drugs <- function(df, drug = "*", rank = 10, drug_id_col = "drug_id"){
  tidy_df <- tidy_presc(df, drug_id_col = drug_id_col)
  freq <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  freq <- freq %>%
    dplyr::group_by(.data$drug_id) %>%
    dplyr::summarise(n_presc = dplyr::n())
  freq <- freq %>%
    dplyr::top_n(rank, .data$n_presc) %>%
    dplyr::arrange(dplyr::desc(.data$n_presc))
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
#' @importFrom rlang .data
#'
#' @examples
#' presc_per_patient(synth_presc, drug_id_col = "approved_name")
#' presc_per_patient(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name")
#'
presc_per_patient <- function(df, drug = "*", patient_id_col = "patient_id",
                     drug_id_col = "drug_id"){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col)
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  summ2 <- summ1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::summarise(n_presc = dplyr::n())
  result <- summ2 %>%
    dplyr::summarise(mean = mean(.data$n_presc),
              median = stats::median(.data$n_presc),
              min = min(.data$n_presc),
              max = max(.data$n_presc))
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
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param group a string corresponding to the desired grouping for results -
#'   either month ("M"), quarter ("Q"), semester ("S") or year ("Y")
#' @param flatten logical, if TRUE the function will only count one record/
#'   prescription per patient per drug ID per date
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing the selected time intervals and the
#'   corresponding number of prescriptions and patients with at least 1
#'   prescription during the interval
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' presc_by_time(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name",
#' presc_date_col = "presc_date", date_format = "%Y-%m-%d")
#' presc_by_time(synth_presc, drug = "212000", group = "M", drug_id_col = "bnf_paragraph",
#' presc_date_col = "presc_date", date_format = "%Y-%m-%d")
#'
presc_by_time <- function(df, drug = "*", group = "Y", flatten = FALSE,
                     drug_id_col = "drug_id", patient_id_col = "patient_id",
                     presc_date_col = "presc_date_x", date_format){
  tidy_df <- tidy_presc(df, drug_id_col = drug_id_col,
                        patient_id_col = patient_id_col, presc_date_col = presc_date_col,
                        date_format = date_format)
  if(flatten == TRUE){
    tidy_df <- tidy_df %>%
      dplyr::distinct(.data$patient_id, .data$presc_date, .data$drug_id)
  }
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id)) %>%
    dplyr::mutate(presc_month = lubridate::month(.data$presc_date_x),
             presc_year = lubridate::year(.data$presc_date_x),
             presc_quarter = lubridate::quarter(.data$presc_date_x, with_year = TRUE),
             presc_semester = lubridate::semester(.data$presc_date_x, with_year = TRUE)) %>%
    dplyr::select(.data$patient_id, .data$presc_date_x, .data$presc_month,
                  .data$presc_year, .data$presc_quarter, .data$presc_semester)
  if(group == "Y"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_year) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id))
  } else if (group == "M"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_year, .data$presc_month) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id))
  } else if (group == "S"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_semester) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id))
  } else if (group == "Q"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_quarter) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id))
  }
  return(result)
}


#' Prescription Time Trends with Daily Doses Dispensed
#'
#' Function generates a breakdown of the number of daily doses dispensed from
#' prescriptions matching a chosen drug identifier over time - either broken
#' down by month, quarter, semester or year.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a drug identifier and a prescription date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param group a string corresponding to the desired grouping for results -
#'   either month ("M"), quarter ("Q"), semester ("S") or year ("Y")
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param dd_disp_col a string, the name of the column in \code{df} containing
#'   the number of daily doses dispensed
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing the selected time intervals and the
#'   corresponding number of prescriptions and patients with at least 1
#'   prescription during the interval
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' presc_by_time_dd(synth_presc, drug = "SIMVASTATIN",
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' dd_disp_col = "ddd_dispensed", date_format = "%Y-%m-%d")
#'
presc_by_time_dd <- function(df, drug = "*", group = "Y",
                          drug_id_col = "drug_id", patient_id_col = "patient_id",
                          presc_date_col = "presc_date_x", dd_disp_col = "dd_disp",
                          date_format){
  tidy_df <- tidy_presc(df, drug_id_col = drug_id_col, patient_id_col = patient_id_col,
                        presc_date_col = presc_date_col, dd_disp_col = dd_disp_col,
                        date_format = date_format)
  summ1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id)) %>%
    dplyr::mutate(presc_month = lubridate::month(.data$presc_date_x),
                  presc_year = lubridate::year(.data$presc_date_x),
                  presc_quarter = lubridate::quarter(.data$presc_date_x, with_year = TRUE),
                  presc_semester = lubridate::semester(.data$presc_date_x, with_year = TRUE)) %>%
    dplyr::select(.data$patient_id, .data$presc_date_x, .data$dd_disp, .data$presc_month,
                  .data$presc_year, .data$presc_quarter, .data$presc_semester)
  if(group == "Y"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_year) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id),
                       total_dds = sum(.data$dd_disp))
  } else if (group == "M"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_year, .data$presc_month) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id),
                       total_dds = sum(.data$dd_disp))
  } else if (group == "S"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_semester) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id),
                       total_dds = sum(.data$dd_disp))
  } else if (group == "Q"){
    result <- summ1 %>%
      dplyr::group_by(.data$presc_quarter) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       n_patients = dplyr::n_distinct(.data$patient_id),
                       total_dds = sum(.data$dd_disp))
  }
  return(result)
}
