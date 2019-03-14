#' Use at Time Point, Fixed Date Range
#'
#' Determines which patients have been exposed to the drug(s) of interest within
#' a timeframe of interest. This function applies the same timeframe to all
#' prescriptions - by default it considers all prescriptions before or after the
#' date provided, but a number of days can be entered to give a more specific
#' timeframe.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, drug ID and prescription date
#' @param drug a string corresponding to a drug identifier to be matched
#' @param date_1 a string containing a date to base the follow-up window on
#' @param timeframe a number representing the length of the follow-up window. If
#'   0 as by default any prescriptions before or after (depending on the value
#'   of \code{forward}) the date entered in \code{date_1} will be considered
#' @param forward a logical, if TRUE the value of \code{timeframe} will be added
#'   to the value of \code{date_1} to generate an end date for the follow-up
#'   window. If FALSE, the value will be subtracted to generate a start date
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing a list of patient IDs for patients who have
#'   at least one prescription matching the criteria, the number of
#'   prescriptions within the defined date window and the date of the first
#'   matching prescription
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{uat_fixed(synth_presc, drug = "ATORVASTATIN", date_1 = "01/07/2020", drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#' \code{uat_fixed(synth_presc, drug = "SIMVASTATIN", date_1 = "01/01/2021", timeframe = 180, forward = FALSE, drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#'
uat_fixed <- function(df, drug, date_1, timeframe = 0, forward = TRUE,
                      patient_id_col = "patient_id", drug_id_col = "drug_id",
                      presc_date_col = "presc_date_x", date_format){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, date_format = date_format)
  uat1 <- tidy_df %>%
    dplyr::filter(grepl(drug, drug_id))
  date_1 <- as.Date(date_1, format = date_format)
  if((forward == TRUE) && (timeframe == 0)){
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x >= date_1)
  } else if ((forward == FALSE) && (timeframe == 0)){
    uat1 <- uat1 %>%
      dplyr::filter(date_1 >= presc_date_x)
  } else if ((forward == TRUE) && (timeframe > 0)){
    date_2 <- date_1 + timeframe
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x >= date_1 & presc_date_x <=date_2)
  } else if ((forward == FALSE) && (timeframe > 0)){
    date_2 <- date_1 - timeframe
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x <= date_1 & presc_date_x >= date_2)
  }
  uat_result <- uat1 %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(n_presc = n(),
              first_presc = min(presc_date_x))
  return(uat_result)
}

#' Use At Time Point, Fixed Range From Patient Event Date
#'
#' Determines which patients have been exposed to the drug(s) of interest within
#' a timeframe of interest. This function applies the same timeframe to
#' patient-specific event dates. By default it considers all prescriptions
#' before or after the event date, but a number of days can be entered to give a
#' more specific timeframe.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, drug ID, prescription date
#' @param df2 a data frame containing event dates - should contain at least
#'   patient IDs and an event date, can also include event codes
#' @param drug a string corresponding to a drug identifier to be matched
#' @param timeframe a number representing the length of the follow-up window. If
#'   0 as by default any prescriptions before or after (depending on the value
#'   of \code{forward}) the patient's event date, \code{presc_date}, will be
#'   considered
#' @param forward a logical, if TRUE the value of \code{timeframe} will be added
#'   to the value of \code{presc_date} to generate an end date for the follow-up
#'   window. If FALSE, the value will be subtracted to generate a start date
#' @param patient_id_col a string, the name of the column in \code{df} and
#'   \code{df2} containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing
#' @param ev_date_col a string, the name of the column in \code{df2} containing
#'   the event dates
#' @param ev_code_col a string, the name of the column in \code{df2} containing
#'   the event codes if present
#' @param date_format a string, the format of the dates contained in \code{df}
#'   and \code{df2}
#' @return data frame containing a list of patient IDs for patients who have at
#'   least one prescription matching the criteria, the number of prescriptions
#'   within the defined date window and the date of the first matching
#'   prescription
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{uat_fixed_events(df = synth_presc, df2 = synth_events, drug = "SIMVASTATIN", forward = TRUE, drug_id_col = "approved_name", presc_date_col = "presc_date", date_formate = "%d/%m/%Y")}
#'
#'
uat_fixed_events <- function(df, df2, drug, timeframe = 0, forward = TRUE,
                             patient_id_col = "patient_id", drug_id_col = "drug_id",
                             presc_date_col = "presc_date_x",
                             ev_date_col = "ev_date_1", ev_code_col = NULL,
                             date_format){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, date_format = date_format)
  tidy_ev <- tidy_events(df2, patient_id_col = patient_id_col, ev_date_1_col = ev_date_col,
                         ev_code_1_col = ev_code_col, date_format = date_format)
  uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
  uat1 <- dplyr::filter(uat1, !is.na(ev_date_1))
  uat1 <- uat1 %>%
    dplyr::filter(grepl(drug, drug_id))
  if((forward == TRUE) && (timeframe == 0)){
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x >= ev_date_1)
  } else if ((forward == FALSE) && (timeframe == 0)){
    uat1 <- uat1 %>%
      dplyr::filter(ev_date_1 >= presc_date_x)
  } else if ((forward == TRUE) && (timeframe > 0)){
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x >= ev_date_1 & presc_date_x <= ev_date_1 + timeframe)
  } else if ((forward == FALSE) && (timeframe > 0)){
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x <= ev_date_1 & presc_date_x >= ev_date_1 - timeframe)
  }
  uat_result <- uat1 %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(n_presc = n(),
              first_presc = min(presc_date_x))
  return(uat_result)
}

#' Use at Time Point, Individual Patient Date Ranges
#'
#' Determines which patients have been exposed to the drug(s) of interest within
#' a timeframe of interest. This function determines exposure by checking for
#' prescription dates between two patient-specific event dates.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, drug ID, prescription date and
#'   two event dates (\code{ev_date_1} and \code{ev_date_2})
#' @param drug a string corresponding to a drug identifier to be matched
#' @param forward a logical, if TRUE the follow-up window will start with
#'   \code{ev_date_1} as the start date and \code{ev_date_2} as the end date. If
#'   FALSE, the reverse will be true
#' @param patient_id_col a string, the name of the column in \code{df} and
#'   \code{df2} containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing
#' @param ev_date_col_1 a string, the name of the column in \code{df2}
#'   containing the first event dates
#' @param ev_code_col_1 a string, the name of the column in \code{df2}
#'   containing the first event codes if present
#' @param ev_date_col_2 a string, the name of the column in \code{df2}
#'   containing the second event dates if present
#' @param ev_code_col_2 a string, the name of the column in \code{df2}
#'   containing the second event codes if present
#' @param date_format a string, the format of the dates contained in \code{df}
#'   and \code{df2}
#'
#' @return a data frame containing a list of patient IDs for patients who have
#'   at least one prescription matching the criteria, the number of
#'   prescriptions within the defined date window and the date of the first
#'   matching prescription
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \code{uat_var_events(synth_presc, df2 = synth_events, drug = "ATORVASTATIN", drug_id_col = "approved_name", presc_date_col = "presc_date", forward = TRUE)}
#'
uat_var_events <- function(df, df2, drug, forward = TRUE,
                           patient_id_col = "patient_id", drug_id_col = "drug_id",
                           presc_date_col = "presc_date_x",
                           ev_date_1_col = "ev_date_1", ev_code_1_col = NULL,
                           ev_date_2_col = "ev_date_2", ev_code_2_col = NULL,
                           date_format){
  tidy_df <- tidy_presc(df, patient_id_col = patient_id_col, drug_id_col = drug_id_col,
                        presc_date_col = presc_date_col, date_format = date_format)
  tidy_ev <- tidy_events(df2, patient_id = patient_id_col,
                         ev_date_1_col = ev_date_1_col, ev_code_1_col = ev_code_1_col,
                         ev_date_2_col = ev_date_2_col, ev_code_2_col = ev_code_2_col,
                         date_format = date_format)
    uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
  uat1 <- dplyr::filter(uat1, !is.na(ev_date_1) & !is.na(ev_date_2))
  uat1 <- uat1 %>%
    dplyr::filter(grepl(drug, drug_id))
  if(forward == TRUE){
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x > ev_date_1 & presc_date_x < ev_date_2)
  } else {
    uat1 <- uat1 %>%
      dplyr::filter(presc_date_x > ev_date_2 & presc_date_x < ev_date_1)
  }
  uat_result <- uat1 %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(n_presc = n(),
              first_presc = min(presc_date_x))
  return(uat_result)
}
