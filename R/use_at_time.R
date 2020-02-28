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
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param date_1 a string containing a date to base the follow-up window on
#' @param timeframe a number representing the length of the follow-up window. If
#'   0 as by default any prescriptions before or after (depending on the value
#'   of \code{forward}) the date entered in \code{date_1} will be considered
#' @param flatten logical, if TRUE the function will only count one record/
#'   prescription per drug ID per date
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
#' @importFrom rlang .data
#'
#' @examples
#' uat_fixed(synth_presc, drug = "ATORVASTATIN", date_1 = "01/07/2020",
#' drug_id_col = "approved_name", presc_date_col = "presc_date", date_format = "%Y-%m-%d")
#' uat_fixed(synth_presc, drug = "SIMVASTATIN", date_1 = "01/01/2021", timeframe = 180,
#' forward = FALSE, drug_id_col = "approved_name", presc_date_col = "presc_date",
#' date_format = "%Y-%m-%d")
#'
uat_fixed <-
  function(df,
           drug,
           date_1,
           flatten = FALSE,
           timeframe = 0,
           forward = TRUE,
           patient_id_col = "patient_id",
           drug_id_col = "drug_id",
           presc_date_col = "presc_date_x",
           date_format) {
    tidy_df <-
      tidy_presc(
        df,
        patient_id_col = patient_id_col,
        drug_id_col = drug_id_col,
        presc_date_col = presc_date_col,
        date_format = date_format
      )
    uat1 <- tidy_df %>%
      dplyr::filter(grepl(drug, .data$drug_id))
    if (flatten == TRUE) {
      uat1 <- uat1 %>%
        dplyr::distinct(.data$patient_id, .data$drug_id, .data$presc_date_x)
    }
    date_1 <- as.Date(date_1, format = date_format)
    if ((forward == TRUE) && (timeframe == 0)) {
      uat1 <- uat1 %>%
        dplyr::filter(.data$presc_date_x >= date_1)
    } else if ((forward == FALSE) && (timeframe == 0)) {
      uat1 <- uat1 %>%
        dplyr::filter(date_1 >= .data$presc_date_x)
    } else if ((forward == TRUE) && (timeframe > 0)) {
      date_2 <- date_1 + timeframe
      uat1 <- uat1 %>%
        dplyr::filter(.data$presc_date_x >= date_1 &
                        .data$presc_date_x <= date_2)
    } else if ((forward == FALSE) && (timeframe > 0)) {
      date_2 <- date_1 - timeframe
      uat1 <- uat1 %>%
        dplyr::filter(.data$presc_date_x <= date_1 &
                        .data$presc_date_x >= date_2)
    }
    uat_result <- uat1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       first_presc = min(.data$presc_date_x))
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
#' @param df2 a data frame containing event dates - must contain at least
#'   patient IDs and an event date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param flatten logical, if TRUE the function will only count one record/
#'   prescription per drug ID per date
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
#'   containing the date the drug was prescribed
#' @param ev_date_col a string, the name of the column in \code{df2} containing
#'   the event dates
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
#' @importFrom rlang .data
#'
#' @examples
#' uat_fixed_events(df = synth_presc, df2 = synth_events, drug = "SIMVASTATIN", forward = TRUE,
#' drug_id_col = "approved_name", presc_date_col = "presc_date", ev_date_col = "event_1",
#' date_format = "%Y-%m-%d")
#'
uat_fixed_events <-
  function(df,
           df2,
           drug,
           flatten = FALSE,
           timeframe = 0,
           forward = TRUE,
           patient_id_col = "patient_id",
           drug_id_col = "drug_id",
           presc_date_col = "presc_date_x",
           ev_date_col = "ev_date_1",
           date_format) {
    tidy_df <-
      tidy_presc(
        df,
        patient_id_col = patient_id_col,
        drug_id_col = drug_id_col,
        presc_date_col = presc_date_col,
        date_format = date_format
      )
    tidy_ev <-
      tidy_presc(
        df2,
        patient_id_col = patient_id_col,
        ev_date_1_col = ev_date_col,
        date_format = date_format
      )
    uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
    uat1 <- dplyr::filter(uat1,!is.na(.data$ev_date_1))
    uat1 <- uat1 %>%
      dplyr::filter(grepl(drug, .data$drug_id))
    if (flatten == TRUE) {
      uat1 <- uat1 %>%
        dplyr::distinct(.data$patient_id, .data$drug_id, .data$presc_date_x)
    }
    if ((forward == TRUE) && (timeframe == 0)) {
      uat1 <- uat1 %>%
        dplyr::filter(.data$presc_date_x >= .data$ev_date_1)
    } else if ((forward == FALSE) && (timeframe == 0)) {
      uat1 <- uat1 %>%
        dplyr::filter(.data$ev_date_1 >= .data$presc_date_x)
    } else if ((forward == TRUE) && (timeframe > 0)) {
      uat1 <- uat1 %>%
        dplyr::filter(
          .data$presc_date_x >= .data$ev_date_1 &
            .data$presc_date_x <= .data$ev_date_1 + timeframe
        )
    } else if ((forward == FALSE) && (timeframe > 0)) {
      uat1 <- uat1 %>%
        dplyr::filter(
          .data$presc_date_x <= .data$ev_date_1 &
            .data$presc_date_x >= .data$ev_date_1 - timeframe
        )
    }
    uat_result <- uat1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::summarise(n_presc = dplyr::n(),
                       first_presc = min(.data$presc_date_x))
    return(uat_result)
  }

#' Use at Time Point, Individual Patient Date Ranges
#'
#' Determines which patients have been exposed to the drug(s) of interest within
#' a timeframe of interest. This function determines exposure by checking for
#' prescription dates between two patient-specific event dates.
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, drug ID, prescription date
#' @param df2 a data frame containing event data to be analysed, records must
#'   contain at least two event dates (\code{ev_date_1} and \code{ev_date_2})
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param flatten logical, if TRUE the function will only count one record/
#'   prescription per drug ID per date
#' @param patient_id_col a string, the name of the column in \code{df} and
#'   \code{df2} containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing
#' @param ev_date_1_col a string, the name of the column in \code{df2}
#'   containing the first event dates
#' @param ev_date_2_col a string, the name of the column in \code{df2}
#'   containing the second event dates if present
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
#' @importFrom rlang .data
#'
#' @examples
#' uat_var_events(synth_presc, df2 = synth_events, drug = "ATORVASTATIN",
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' ev_date_1_col = "event_1", ev_date_2_col = "event_2")
#'
uat_var_events <- function(df,
                           df2,
                           drug,
                           flatten = FALSE,
                           patient_id_col = "patient_id",
                           drug_id_col = "drug_id",
                           presc_date_col = "presc_date_x",
                           ev_date_1_col = "ev_date_1",
                           ev_date_2_col = "ev_date_2",
                           date_format) {
  tidy_df <-
    tidy_presc(
      df,
      patient_id_col = patient_id_col,
      drug_id_col = drug_id_col,
      presc_date_col = presc_date_col,
      date_format = date_format
    )
  tidy_ev <- tidy_presc(
    df2,
    patient_id_col = patient_id_col,
    ev_date_1_col = ev_date_1_col,
    ev_date_2_col = ev_date_2_col,
    date_format = date_format
  )
  uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
  uat1 <-
    dplyr::filter(uat1,!is.na(.data$ev_date_1) &
                    !is.na(.data$ev_date_2))
  uat1 <- uat1 %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  if (flatten == TRUE) {
    uat1 <- uat1 %>%
      dplyr::distinct(.data$patient_id, .data$drug_id, .data$presc_date_x)
  }
    uat1 <- uat1 %>%
      dplyr::filter(.data$presc_date_x > .data$ev_date_1 &
                      .data$presc_date_x < .data$ev_date_2)
  uat_result <- uat1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::summarise(n_presc = dplyr::n(),
                     first_presc = min(.data$presc_date_x))
  return(uat_result)
}


#' Determine New vs. Ongoing Use of a Drug of Interest, Fixed Start Date
#'
#' Classifies patients as new or ongoing users of the drug(s) of interest based
#' on whether or not they have prescriptions within a lookback period based on a
#' standardised start date for all patients
#'
#' @param df a data frame containing prescription records to be analysed -
#'   records must contain at least a patient ID, drug ID and prescription date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param start_date a string containing the date to start follow-up - any
#'   prescriptions before this date are used to indicate ongoing use
#' @param timeframe a number representing the length, in days, of the timeframe
#'   before the start date to check for previous prescriptions
#' @param return_all a logical, if TRUE the function will return data for all
#'   patients as well as flags indicating if they were exposed during follow-up
#'   and if they are new users. If FALSE, returns only new users exposed during
#'   follow-up
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescription date
#' @param date_format a string, the format of the dates contained in \code{df}
#'   and the start_date argument
#'
#' @return A data frame containing patient IDs, flags indicating if the patient
#'   was a new user at the start date and if they were exposed during follow-up,
#'   and where applicable the date of the first prescription during follow-up
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' new_users_fixed(synth_presc, drug = "SIMVASTATIN", start_date = "2021-01-01", timeframe = 90,
#' presc_date_col = "presc_date", drug_id_col = "approved_name")
new_users_fixed <-
  function(df,
           drug,
           start_date = NULL,
           timeframe = 0,
           return_all = TRUE,
           patient_id_col = "patient_id",
           drug_id_col = "drug_id",
           presc_date_col = "presc_date_x",
           date_format) {
    tidy_df <-
      tidy_presc(
        df,
        patient_id_col = patient_id_col,
        drug_id_col = drug_id_col,
        presc_date_col = presc_date_col,
        date_format = date_format
      )
    ids <- tidy_df %>%
      dplyr::select(.data$patient_id) %>%
      dplyr::distinct()
    tidy_df <- tidy_df %>%
      dplyr::filter(grepl(drug, .data$drug_id))
    start_date <- as.Date(start_date, format = date_format)
    if (timeframe == 0) {
      uat1 <- tidy_df %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::filter(.data$presc_date_x < start_date) %>%
        dplyr::distinct(.data$patient_id) %>%
        dplyr::mutate(new_user = 0)
    } else if (timeframe != 0) {
      uat1 <- tidy_df %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::filter(.data$presc_date_x < start_date &
                        .data$presc_date_x > start_date - timeframe) %>%
        dplyr::distinct(.data$patient_id) %>%
        dplyr::mutate(new_user = 0)
    }
    uat2 <- tidy_df %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::filter(.data$presc_date_x >= start_date) %>%
      dplyr::summarise(first_presc = min(.data$presc_date_x)) %>%
      dplyr::mutate(exposed = 1)
    uat1 <- dplyr::left_join(ids, uat1, by = "patient_id")
    uat1 <- dplyr::left_join(uat1, uat2, by = "patient_id")
    uat1$new_user[is.na(uat1$new_user) & uat1$exposed == 1] <- 1
    uat1$exposed[is.na(uat1$exposed)] <- 0
    if (return_all == FALSE) {
      uat1 <- uat1 %>%
        dplyr::filter(.data$exposed == 1 & .data$new_user == 1)
      return(uat1)
    } else {
      return(uat1)
    }
  }


#' Determine New vs. Ongoing Use of a Drug of Interest, Individual Start Date
#'
#' Classifies patients as new or ongoing users of the drug(s) of interest based
#' on whether or not they have prescriptions within a set number of days of a
#' patient-specific follow-up start date
#'
#' @param df a data frame containing prescription records to be analysed -
#'   records must contain at least a patient ID, drug ID and prescription date
#' @param df2 a data frame containing dates to be used as patient specific start
#'   dates - needs to contain at least patient IDs and event dates
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param timeframe a number representing the length, in days, of the timeframe
#'   before the start date to check for previous prescriptions
#' @param return_all a logical, if TRUE the function will return data for all
#'   patients as well as flags indicating if they were exposed during follow-up
#'   and if they are new users. If FALSE, returns only new users exposed during
#'   follow-up
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescription date
#' @param ev_date_col a string, the name of the column in \code{df2}
#'   containing the start dates
#' @param date_format a string, the format of the dates contained in \code{df}
#'   and  \code{df2}
#'
#' @return A data frame containing patient IDs, flags indicating if the patient
#'   was a new user at the start date and if they were exposed during follow-up,
#'   and where applicable the date of the first prescription during follow-up
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' new_users_var(synth_presc, df2 = synth_events,
#' drug = "SIMVASTATIN", timeframe = 90,
#' presc_date_col = "presc_date", drug_id_col = "approved_name",
#' ev_date_col = "start_date")

new_users_var <-
  function(df,
           df2,
           drug,
           timeframe = 0,
           return_all = TRUE,
           patient_id_col = "patient_id",
           drug_id_col = "drug_id",
           presc_date_col = "presc_date_x",
           ev_date_col = "ev_date_1",
           date_format) {
    tidy_df <-
      tidy_presc(
        df,
        patient_id_col = patient_id_col,
        drug_id_col = drug_id_col,
        presc_date_col = presc_date_col,
        date_format = date_format
      )
    tidy_ev <-
      tidy_presc(
        df2,
        patient_id_col = patient_id_col,
        ev_date_1_col = ev_date_col,
        date_format = date_format
      )
    tidy_df <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
    tidy_df <- dplyr::filter(tidy_df,!is.na(.data$ev_date_1))
    ids <- tidy_df %>%
      dplyr::select(.data$patient_id) %>%
      dplyr::distinct()
    tidy_df <- tidy_df %>%
      dplyr::filter(grepl(drug, .data$drug_id))
    if (timeframe == 0) {
      uat1 <- tidy_df %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::filter(.data$presc_date_x < .data$ev_date_1) %>%
        dplyr::distinct(.data$patient_id) %>%
        dplyr::mutate(new_user = 0)
    } else if (timeframe != 0) {
      uat1 <- tidy_df %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::filter(
          .data$presc_date_x < .data$ev_date_1 &
            .data$presc_date_x > .data$ev_date_1 - timeframe
        ) %>%
        dplyr::distinct(.data$patient_id) %>%
        dplyr::mutate(new_user = 0)
    }
    uat2 <- tidy_df %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::filter(.data$presc_date_x >= .data$ev_date_1) %>%
      dplyr::summarise(first_presc = min(.data$presc_date_x)) %>%
      dplyr::mutate(exposed = 1)
    uat1 <- dplyr::left_join(ids, uat1, by = "patient_id")
    uat1 <- dplyr::left_join(uat1, uat2, by = "patient_id")
    uat1$new_user[is.na(uat1$new_user) & uat1$exposed == 1] <- 1
    uat1$exposed[is.na(uat1$exposed)] <- 0
    if (return_all == FALSE) {
      uat1 <- uat1 %>%
        dplyr::filter(.data$exposed == 1 & .data$new_user == 1)
      return(uat1)
    } else {
      return(uat1)
    }
  }

#' Define Recentness of Exposure at Event Date
#'
#' Classifies patients as current or past users of the drug(s) of interest at an
#' event date based on a user-defined time window, and calculates the number of
#' prescriptions that fall within and outwith the recent use window.
#'
#' @param df a data frame containing prescription records to be analysed;
#'   records must contain at least a patient ID, drug ID and prescription date
#' @param df2 a data frame containing medical event data to be analysed; records
#'   must contain at least a patient ID and event date, maximum 1 event per
#'   patient
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param timeframe a number, subtracted from the event date to define the start
#'   of the rexcent use period
#' @param patient_id_col a string, the name of the column in \code{df} and
#'   \code{df2} containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the date the drug was prescribed
#' @param ev_date_col a string, the name of the column in \code{df2} containing
#'   the event dates
#' @param date_format a string, the format of the dates contained in \code{df}
#'   and \code{df2}
#'
#' @return a data frame containing patient IDs, the number of prescriptions in
#'   each period and the use classification at the event date
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' uat_recent(df = synth_presc, df2 = synth_events, drug = "OMEPRAZOLE", timeframe = 30,
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' ev_date_col = "event_1", date_format = "%Y-%m-%d")

#'
uat_recent <- function(df,
                       df2,
                       drug,
                       timeframe,
                       patient_id_col = "patient_id",
                       drug_id_col = "drug_id",
                       presc_date_col = "presc_date_x",
                       ev_date_col = "ev_date_1",
                       date_format) {
  tidy_df <-
    tidy_presc(
      df,
      patient_id_col = patient_id_col,
      drug_id_col = drug_id_col,
      presc_date_col = presc_date_col,
      date_format = date_format
    )
  tidy_ev <-
    tidy_presc(
      df2,
      patient_id_col = patient_id_col,
      ev_date_1_col = ev_date_col,
      date_format = date_format
    )
  uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
  uat1 <- dplyr::filter(uat1,!is.na(.data$ev_date_1))
  uat1 <- uat1 %>%
    dplyr::filter(grepl(drug, .data$drug_id)) %>%
    dplyr::filter(.data$ev_date_1 >= .data$presc_date_x)
  uat1 <- uat1 %>%
    dplyr::mutate(
      current_flag = dplyr::if_else((
        .data$presc_date_x <= .data$ev_date_1 &
          .data$presc_date_x >= (.data$ev_date_1 - timeframe)
      ),
      1,
      0
      ),
      past_flag = dplyr::if_else(.data$presc_date_x < (.data$ev_date_1 - timeframe), 1 , 0)
    )
  uat1 <- uat1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::summarise(
      current_use_flag = max(.data$current_flag),
      n_current = sum(.data$current_flag),
      past_use_flag = max(.data$past_flag),
      n_past = sum(.data$past_flag)
    )
  uat1 <- uat1 %>%
    dplyr::mutate(use_at_event = dplyr::if_else(.data$current_use_flag == 1, "current", "past"))
}


#' Two Prescriptions Within a Desired Timeframe
#'
#' Classifies patients as exposed or unexposed based on whether they received
#' two prescriptions for the drug of interest within a desired number of days of
#' each other
#'
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, drug ID and prescription date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param timeframe a number representing the desired maximum gap between
#'   prescriptions
#' @param return_all logical, if TRUE function returns data for all patient IDs
#'   with a flag indicating which patients did and did not meet the threshold
#'   for exposure
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing the patient IDs and first two prescription
#'   dates for patients who meet the definition of exposure
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' uat_gap(df = synth_presc, drug = "SIMVASTATIN", timeframe = 14,
#' patient_id_col = "patient_id", drug_id_col = "approved_name",
#' presc_date_col = "presc_date", date_format = "%Y-%m-%d")

uat_gap <- function(df,
                    drug,
                    timeframe,
                    return_all = FALSE,
                    patient_id_col = "patient_id",
                    drug_id_col = "drug_id",
                    presc_date_col = "presc_date_x",
                    date_format) {
  tidy_df <-
    tidy_presc(
      df,
      patient_id_col = patient_id_col,
      drug_id_col = drug_id_col,
      presc_date_col = presc_date_col,
      date_format = date_format
    )
  ids <- tidy_df %>%
    dplyr::select(.data$patient_id) %>%
    dplyr::distinct()
  df1 <- tidy_df %>%
    dplyr::filter(grepl(drug, .data$drug_id))
  df1 <- df1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::arrange(.data$patient_id, .data$presc_date_x) %>%
    dplyr::mutate(difference = c(0, diff(.data$presc_date_x)))
  df1 <- df1 %>%
    dplyr::mutate(flag = ifelse(dplyr::lead(.data$difference) <= timeframe, 1, 0))
  df1$flag[is.na(df1$flag)] <- 0
  df1 <- df1 %>%
    dplyr::mutate(exposed = max(.data$flag)) %>%
    dplyr::filter(.data$exposed == 1)
  df2 <- df1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::filter(.data$flag == 1) %>%
    dplyr::summarise(presc_date_x = min(.data$presc_date_x)) %>%
    dplyr::mutate(presc1 = 1)
  df1 <-
    dplyr::left_join(df1, df2, by = c("patient_id", "presc_date_x"))
  df1$presc1[is.na(df1$presc1)] <- 0
  df1 <- df1 %>%
    dplyr::mutate(presc2 = ifelse(dplyr::lag(.data$presc1) == 1, 1, 0))
  df1$presc2[is.na(df1$presc2)] <- 0
  df2 <- df1 %>%
    dplyr::filter(.data$presc1 == 1 | .data$presc2 == 1) %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::summarise(presc1 = min(.data$presc_date_x),
                     presc2 = max(.data$presc_date_x))
  if (return_all == TRUE) {
    df2 <- df2 %>%
      dplyr::mutate(exposed = 1)
    df2 <- dplyr::left_join(ids, df2, by = "patient_id") %>%
      dplyr::select(.data$patient_id,
                    .data$exposed,
                    .data$presc1,
                    .data$presc2)
    df2$exposed[is.na(df2$exposed)] <- 0
  }
  return(df2)
}




#' Use Windows Consecutive Time Windows
#'
#' Function divides follow-up time into evenly sized windows of a user-defined
#' length and defines patient exposure to the drug(s) of interest based on the
#' presence or absence of prescriptions within the windows
#'
#'
#' @param df a data frame containing prescribing records to be analysed -
#'   records must contain at least a patient ID, drug ID, prescription date
#' @param df2 a data frame containing event data to be analysed, records must
#'   contain at least two event dates (\code{ev_date_1} and \code{ev_date_2})
#'   representing the start and end dates of follow-up for each patient
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param individual logical - if TRUE prescriptions are split into windows
#'   based on individual values of ev_date_1 and ev_date_2. If FALSE,
#'   prescriptions are split into the same windows across all patients, based on
#'   the maximum value of ev_date_1 and the minimum value of ev_date_2
#' @param timeframe a number representing the desired length of each window in
#'   days
#' @param return_all logical - if TRUE, all prescription windows are returned
#'   for all patients including windows where the patient did not have any
#'   prescriptions for the drug(s) of interest. If FALSE, only windows where
#'   patients had at least 1 prescription are returned
#' @param patient_id_col a string, the name of the column in \code{df} and
#'   \code{df2} containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing
#' @param ev_date_1_col a string, the name of the column in \code{df2}
#'   containing the start of follow-up dates
#' @param ev_date_2_col a string, the name of the column in \code{df2}
#'   containing the second event dates if present
#' @param date_format a string, the format of the dates contained in \code{df}
#'   and \code{df2}
#'
#' @return A data frame containing a sequence of date groups for each patient
#'   and the number of prescriptions the patient had during each window; if
#'   return_all is TRUE a flag indicates windows during which the patient was
#'   exposed (exposed = 1) and unexposed (exposed = 0)
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' uat_windows(df = synth_presc, df2 = synth_events, drug = "SIMVASTATIN",
#' individual = TRUE, timeframe = 90, return_all = TRUE,
#' patient_id_col = "patient_id", drug_id_col = "approved_name",
#' presc_date_col = "presc_date",
#' ev_date_1_col = "event_1", ev_date_2_col = "event_2", date_format = "%Y-%m-%d")


uat_windows <-
  function(df,
           df2,
           drug,
           individual = FALSE,
           timeframe,
           return_all = FALSE,
           patient_id_col = "patient_id",
           drug_id_col = "drug_id",
           presc_date_col = "presc_date_x",
           ev_date_1_col = "ev_date_1",
           ev_date_2_col = "ev_date_2",
           date_format) {
    tidy_df <-
      tidy_presc(
        df,
        patient_id_col = patient_id_col,
        drug_id_col = drug_id_col,
        presc_date_col = presc_date_col,
        date_format = date_format
      )
    tidy_ev <- tidy_presc(
      df2,
      patient_id_col = patient_id_col,
      ev_date_1_col = ev_date_1_col,
      ev_date_2_col = ev_date_2_col,
      date_format = date_format
    )
    uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
    uat1 <-
      dplyr::filter(uat1,!is.na(.data$ev_date_1) &
                      !is.na(.data$ev_date_2))
    uat1 <- uat1 %>%
      dplyr::filter(grepl(drug, .data$drug_id))
    if (individual == TRUE) {
      uat1 <- uat1 %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::mutate(date_group = cut(
          .data$presc_date_x,
          seq(
            min(.data$ev_date_1),
            max(.data$ev_date_2) + timeframe,
            by = timeframe
          )
        )
        %>% as.Date)
    } else if (individual == FALSE) {
      uat1 <- uat1 %>%
        dplyr::mutate(date_group = cut(.data$presc_date_x,
                                       seq(
                                         min(.data$ev_date_1),
                                         max(.data$ev_date_2),
                                         by = timeframe
                                       ))
                      %>% as.Date)
    }
    uat1 <- uat1 %>%
      dplyr::filter(!is.na(.data$date_group))
    uat1 <- uat1 %>%
      dplyr::group_by(.data$patient_id, .data$date_group) %>%
      dplyr::summarise(n_presc = dplyr::n())
    if (return_all == TRUE) {
      uat1 <- uat1 %>%
        dplyr::group_by(.data$patient_id) %>%
        tidyr::complete(date_group = seq.Date(
          from = min(.data$date_group),
          to = max(.data$date_group),
          by = timeframe
        ))
      uat1$n_presc[is.na(uat1$n_presc)] <- 0
      uat1 <- uat1 %>%
        dplyr::mutate(
          end_date = .data$date_group + timeframe - 1,
          exposed = ifelse(.data$n_presc > 0, 1, 0)
        ) %>%
        dplyr::select(
          .data$patient_id,
          .data$date_group,
          .data$end_date,
          .data$exposed,
          .data$n_presc
        )
    } else if (return_all == FALSE){
      uat1 <- uat1 %>%
        dplyr::mutate(
          end_date = .data$date_group + timeframe - 1) %>%
        dplyr::select(
          .data$patient_id,
          .data$date_group,
          .data$end_date,
          .data$n_presc
        )
    }
    return(uat1)
  }
