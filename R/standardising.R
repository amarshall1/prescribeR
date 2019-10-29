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
#' @param patient_id_col a string, the name of the column in \code{df} containing
#'   the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing the
#'   drug IDs
#' @param presc_date_col a string, the name of the column in \code{df} containing
#'   the prescption date
#' @param dd_disp_col a string, the name of the column in \code{df} containing the
#'   number of daily doses dispensed
#' @param qty_disp_col a string, the name of the column in \code{df} containing the
#'   quantity of drug dispensed
#' @param qty_per_day_col a string, the name of the column in \code{df} containing
#'   the quantity of drug to be taken per day
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame with standardised field names and formats
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' tidy_synth <- tidy_presc(df = synth_presc, drug_id_col = "approved_name",
#'  dd_disp_col = "ddd_dispensed", qty_disp_col = "qty_dispensed",
#'  date_format = "%Y-%m-%d")
#' tidy_synth <- tidy_presc(synth_presc, patient_id_col = "patient_id",
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' dd_disp_col = "ddd_dispensed", qty_disp_col = "qty_dispensed",
#' qty_per_day_col = "qty_per_day")
#'
tidy_presc <- function(df, patient_id_col = NULL, drug_id_col = NULL,
                           presc_date_col = NULL, dd_disp_col = NULL,
                           qty_disp_col = NULL, qty_per_day_col = NULL, date_format){
  df1 <- df
  if(!is.null(patient_id_col)){
    df1 <- df1 %>%
      dplyr::rename(patient_id = patient_id_col)
    df1$patient_id <- as.character(df1$patient_id)
  }
  if(!is.null(drug_id_col)){
    df1 <- df1 %>%
      dplyr::rename(drug_id = drug_id_col)
    df1$drug_id <- as.character(df1$drug_id)
  }
  if(!is.null(presc_date_col)){
    df1 <- df1 %>%
      dplyr::rename(presc_date_x = presc_date_col)
    df1$presc_date_x <- as.Date(df1$presc_date_x, format = date_format)
  }
  if(!is.null(dd_disp_col)){
    df1 <- df1 %>%
      dplyr::rename(dd_disp = dd_disp_col)
    df1$dd_disp <- as.numeric(df1$dd_disp)
  }
  if(!is.null(qty_disp_col)){
    df1 <- df1 %>%
      dplyr::rename(qty_disp = qty_disp_col)
    df1$qty_disp <- as.numeric(df1$qty_disp)
  }
  if(!is.null(qty_per_day_col)){
    df1 <- df1 %>%
      dplyr::rename(qty_per_day = qty_per_day_col)
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
#' @param patient_id_col a string, the name of the column in df containing the
#'   patient IDs
#' @param ev_date_1_col a string, the name of the column containing the first
#'   event dates
#' @param ev_date_2_col a string, the name of the column containing the second
#'   event dates if present
#' @param date_format a string, giving the format of the dates used in \code{df}
#'
#' @return a data frame with standardised field names and formats
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' tidy_ev <- tidy_events(synth_events, patient_id_col = "patient_id",
#' ev_date_1_col = "event_1", date_format = "%Y-%m-%d")
#'
tidy_events <- function(df, patient_id_col, ev_date_1_col,
                        ev_date_2_col = NULL, date_format){
  df1 <- df %>%
    dplyr::rename(patient_id = patient_id_col) %>%
    dplyr::rename(ev_date_1 = ev_date_1_col)
  df1$patient_id <- as.character(df1$patient_id)
  df1$ev_date_1 <- as.Date(df1$ev_date_1, format = date_format)
  if(!is.null(ev_date_2_col)){
    df1 <- df1 %>%
      dplyr::rename(ev_date_2 = ev_date_2_col)
    df1$ev_date_2 <- as.Date(df1$ev_date_2, format = date_format)
  }
  return(df1)
}
