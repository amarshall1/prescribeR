#' Standardise Prescribing Data Field Names and Formats
#'
#' Adjusts the field names and data formats of prescribing data to create a data
#' frame in a format suited for use with the other functions in this package -
#' mostly called internally by the other functions in this package, but can be
#' used directly  by the user to reduce the number of arguments which need to be
#' passed to other functions as these standardised values are inluded as the
#' default values in their code.
#'
#' @param df a data frame containing prescribing records
#' @param patient_id a string, the name of the column in \code{df} containing
#'   the patient IDs
#' @param drug_id a string, the name of the column in \code{df} containing the
#'   drug IDs
#' @param presc_date a string, the name of the column in \code{df} containing
#'   the prescption date
#' @param dd_disp a string, the name of the column in \code{df} containing the
#'   number of daily doses dispensed
#' @param qty_disp a string, the name of the column in \code{df} containing the
#'   quantity of drug dispensed
#' @param qty_per_day a string, the name of the column in \code{df} containing
#'   the quantity of drug to be taken per day
#' @param dates_format a string, the format of the dates in \code{df}
#'
#' @return a data frame with standardised field names and formats
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{tidy_synth <- tidy_presc(df = synth_presc, drug_id = "approved_name",
#'  dd_disp = "ddd_dispensed", qty_disp = "qty_dispensed",
#'  dates_format = "%d/%m/%Y")}
#' \code{tidy_synth <- tidy_presc(synth_presc, patient_id = "patient_id",
#' drug_id = "approved_name", presc_date = "presc_date",
#' dd_disp = "ddd_dispensed", qty_disp = "qty_dispensed",
#' qty_per_day = "qty_per_day", dates_format = "%d/%m/%Y")}
#'
tidy_presc <- function(df, patient_id = NULL, drug_id = NULL, presc_date = NULL,
                       dd_disp = NULL, qty_disp = NULL,
                       qty_per_day = NULL, dates_format){
  df1 <- df %>%
    dplyr::rename_(.dots = setNames(drug_id, "drug_id"))
  df1$drug_id <- as.character(df1$drug_id)
    if(!is.null(patient_id)){
      df1 <- df1 %>%
        dplyr::rename_(.dots = setNames(patient_id, "patient_id"))
      df1$patient_id <- as.character(df1$patient_id)
    }
    if(!is.null(presc_date)){
      df1 <- df1 %>%
        dplyr::rename_(.dots = setNames(presc_date, "presc_date"))
      df1$presc_date <- as.Date(df1$presc_date, format = dates_format)
    }
    if(!is.null(dd_disp)){
      df1 <- df1 %>%
        dplyr::rename_(.dots = setNames(dd_disp, "dd_disp"))
      df1$dd_disp <- as.numeric(df1$dd_disp)
    }
    if(!is.null(qty_disp)){
      df1 <- df1 %>%
        dplyr::rename_(.dots = setNames(qty_disp, "qty_disp"))
      df1$qty_disp <- as.numeric(df1$qty_disp)
    }
    if(!is.null(qty_per_day)){
      df1 <- df1 %>%
        dplyr::rename_(.dots = setNames(qty_per_day, "qty_per_day"))
      df1$qty_per_day <- as.numeric(df1$qty_per_day)
    }
  return(df1)
}


#' Standardise Medical Event Dates
#'
#' Adjusts the field names and data formats of medical event data to create a
#' data frame in a format suited for use with the use at time point functions in
#' this package. Called internally within the functions which make use of
#' medical event data, but can be used directly by the user to reduce the number
#' of arguments which need to be passed to other functions as these standardised
#' values are inluded as the default values in their code.
#'
#' @param df a data frame containing at least patient ID's and one corresponding
#'   medical event date
#' @param patient_id a string, the name of the column in df containing the
#'   patient IDs
#' @param ev_date_1 a string, the name of the column containing the first event
#'   dates
#' @param ev_code_1 a string, the name of the column containing the medical code
#'   for the first event
#' @param ev_date_2 a string, the name of the column containing the second event
#'   dates if present
#' @param ev_code_2 a string, the name of the column containing the medical code
#'   for the second event
#' @param dates_format a string, giving the format of the dates used in
#'   \code{df}
#'
#' @return a data frame with standardised field names and formats
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#'
tidy_events <- function(df, patient_id, ev_date_1, ev_code_1 = NULL,
                        ev_date_2 = NULL, ev_code_2 = NULL, dates_format){
  df1 <- df %>%
    dplyr::rename_(.dots = setNames(patient_id, "patient_id")) %>%
    dplyr::rename_(.dots = setNames(ev_date_1, "ev_date_1"))
  df1$patient_id <- as.character(df1$patient_id)
  df1$ev_date_1 <- as.Date(df1$ev_date_1, format = dates_format)
  if(!is.null(ev_code_1)){
    df1 <- df1 %>% dplyr::rename_(.dots = setNames(ev_code_1, "ev_code_1"))
    df1$ev_code_1 <- as.character(df1$ev_code_1)
  }
  if(!is.null(ev_date_2)){
    df1 <- df1 %>% dplyr::rename_(.dots = setNames(ev_date_2, "ev_date_2"))
    df1$ev_date_2 <- as.Date(df1$ev_date_2, format = dates_format)
  }
  if(!is.null(ev_code_2)){
    df1 <- df1 %>% dplyr::rename_(.dots = setNames(ev_code_2, "ev_code_2"))
    df1$ev_code_2 <- as.character(df1$ev_code_2)
  }
  return(df1)
}
