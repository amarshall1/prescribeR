#' #' Standardise Prescribing Data Field Names and Formats
#' #'
#' #' Adjusts the field names and data formats of prescribing data to create a data
#' #' frame in a format suited for use with the other functions in this package -
#' #' mostly called internally by the other functions in this package, but can be
#' #' used directly  by the user to reduce the number of arguments which need to be
#' #' passed to other functions as these standardised values are inluded as the
#' #' default values in their code.
#' #'
#' #' @param df a data frame containing prescribing records
#' #' @param patient_id a string, the name of the column in \code{df} containing
#' #'   the patient IDs
#' #' @param drug_id a string, the name of the column in \code{df} containing the
#' #'   drug IDs
#' #' @param presc_date a string, the name of the column in \code{df} containing
#' #'   the prescption date
#' #' @param dd_disp a string, the name of the column in \code{df} containing the
#' #'   number of daily doses dispensed
#' #' @param qty_disp a string, the name of the column in \code{df} containing the
#' #'   quantity of drug dispensed
#' #' @param qty_per_day a string, the name of the column in \code{df} containing
#' #'   the quantity of drug to be taken per day
#' #' @param dates_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame with standardised field names and formats
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{tidy_synth <- tidy_presc(df = synth_presc, drug_id = "approved_name",
#' #'  dd_disp = "ddd_dispensed", qty_disp = "qty_dispensed",
#' #'  dates_format = "%d/%m/%Y")}
#' #' \code{tidy_synth <- tidy_presc(synth_presc, patient_id = "patient_id",
#' #' drug_id = "approved_name", presc_date = "presc_date",
#' #' dd_disp = "ddd_dispensed", qty_disp = "qty_dispensed",
#' #' qty_per_day = "qty_per_day", dates_format = "%d/%m/%Y")}
#' #'
#' tidy_presc <- function(df, patient_id = NULL, drug_id = NULL, presc_date = NULL,
#'                        dd_disp = NULL, qty_disp = NULL,
#'                        qty_per_day = NULL, dates_format){
#'   df1 <- df %>%
#'     dplyr::rename_(.dots = setNames(drug_id, "drug_id"))
#'   df1$drug_id <- as.character(df1$drug_id)
#'   if(!is.null(patient_id)){
#'     df1 <- df1 %>%
#'       dplyr::rename_(.dots = setNames(patient_id, "patient_id"))
#'     df1$patient_id <- as.character(df1$patient_id)
#'   }
#'   if(!is.null(presc_date)){
#'     df1 <- df1 %>%
#'       dplyr::rename_(.dots = setNames(presc_date, "presc_date"))
#'     df1$presc_date <- as.Date(df1$presc_date, format = dates_format)
#'   }
#'   if(!is.null(dd_disp)){
#'     df1 <- df1 %>%
#'       dplyr::rename_(.dots = setNames(dd_disp, "dd_disp"))
#'     df1$dd_disp <- as.numeric(df1$dd_disp)
#'   }
#'   if(!is.null(qty_disp)){
#'     df1 <- df1 %>%
#'       dplyr::rename_(.dots = setNames(qty_disp, "qty_disp"))
#'     df1$qty_disp <- as.numeric(df1$qty_disp)
#'   }
#'   if(!is.null(qty_per_day)){
#'     df1 <- df1 %>%
#'       dplyr::rename_(.dots = setNames(qty_per_day, "qty_per_day"))
#'     df1$qty_per_day <- as.numeric(df1$qty_per_day)
#'   }
#'   return(df1)
#' }
#'
#'
#' #' Standardise Medical Event Dates
#' #'
#' #' Adjusts the field names and data formats of medical event data to create a
#' #' data frame in a format suited for use with the use at time point functions in
#' #' this package. Called internally within the functions which make use of
#' #' medical event data, but can be used directly by the user to reduce the number
#' #' of arguments which need to be passed to other functions as these standardised
#' #' values are inluded as the default values in their code.
#' #'
#' #' @param df a data frame containing at least patient ID's and one corresponding
#' #'   medical event date
#' #' @param patient_id a string, the name of the column in df containing the
#' #'   patient IDs
#' #' @param ev_date_1 a string, the name of the column containing the first event
#' #'   dates
#' #' @param ev_code_1 a string, the name of the column containing the medical code
#' #'   for the first event
#' #' @param ev_date_2 a string, the name of the column containing the second event
#' #'   dates if present
#' #' @param ev_code_2 a string, the name of the column containing the medical code
#' #'   for the second event
#' #' @param dates_format a string, giving the format of the dates used in
#' #'   \code{df}
#' #'
#' #' @return a data frame with standardised field names and formats
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #'
#' #'
#' tidy_events <- function(df, patient_id, ev_date_1, ev_code_1 = NULL,
#'                         ev_date_2 = NULL, ev_code_2 = NULL, dates_format){
#'   df1 <- df %>%
#'     dplyr::rename_(.dots = setNames(patient_id, "patient_id")) %>%
#'     dplyr::rename_(.dots = setNames(ev_date_1, "ev_date_1"))
#'   df1$patient_id <- as.character(df1$patient_id)
#'   df1$ev_date_1 <- as.Date(df1$ev_date_1, format = dates_format)
#'   if(!is.null(ev_code_1)){
#'     df1 <- df1 %>% dplyr::rename_(.dots = setNames(ev_code_1, "ev_code_1"))
#'     df1$ev_code_1 <- as.character(df1$ev_code_1)
#'   }
#'   if(!is.null(ev_date_2)){
#'     df1 <- df1 %>% dplyr::rename_(.dots = setNames(ev_date_2, "ev_date_2"))
#'     df1$ev_date_2 <- as.Date(df1$ev_date_2, format = dates_format)
#'   }
#'   if(!is.null(ev_code_2)){
#'     df1 <- df1 %>% dplyr::rename_(.dots = setNames(ev_code_2, "ev_code_2"))
#'     df1$ev_code_2 <- as.character(df1$ev_code_2)
#'   }
#'   return(df1)
#' }
#'
#' # 1.3 - Data summaries
#'
#' #' Prescription and Patient Counts and Date Range
#' #'
#' #' Function produces a high-level summary of a data frame, containin the number
#' #' of patients and prescriptions in the data and the date range of the
#' #' prescriptions. Can either be used to obtain a summary of all prescriptions,
#' #' or limited to a drug or drugs of interest
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a patient ID, a drug identifier and a
#' #'   prescription date
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param patient_id_col a string, the name of the column in \code{df}
#' #'   containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing the prescption date
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing the number of prescriptions, number of
#' #'   patients and the first and last prescription dates for the chosen data
#' #'   frame
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{presc_data_summary(synth_presc, drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#' #' \code{presc_data_summary(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#' #'
#' presc_data_summary <- function(df, drug = "*", patient_id_col = "patient_id",
#'                                drug_id_col = "drug_id", presc_date_col = "presc_date",
#'                                date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dates_format = date_format)
#'   summ1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   result <- summ1 %>%
#'     dplyr::summarise(n_presc = n(),
#'                      n_pat = dplyr::n_distinct(patient_id),
#'                      date_first = min(presc_date),
#'                      date_last = max(presc_date))
#'   return(result)
#' }
#'
#'
#' #' Produce a List of the Most Commonly Prescribed Drugs
#' #'
#' #' Function produces a list of the most commonly prescribed drugs and the number
#' #' of prescriptions for each, based on unique drug IDs. The number of results
#' #' returned can be adjusted.
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a drug identifier
#' #' @param rank the number of drugs to be included in the list
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #'
#' #' @return a data frame containing a list of drug IDs and corresponding number
#' #'   of prescriptions
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{presc_top_drugs(synth_presc, drug_id_col = "approved_name")}
#' #' \code{presc_top_drugs(synth_presc, rank = 5, drug_id_col = "bnf_item_code")}
#' #'
#' presc_top_drugs <- function(df, rank = 10, drug_id_col = "drug_id"){
#'   tidy_df <- tidy_presc(df, drug_id = drug_id_col)
#'   freq <- tidy_df %>%
#'     dplyr::group_by(drug_id) %>%
#'     dplyr::summarise(n_presc = n())
#'   freq <- freq %>%
#'     dplyr::top_n(rank, n_presc) %>%
#'     dplyr::arrange(desc(n_presc))
#'   return(freq)
#' }
#'
#'
#' #' Prescriptions Per Patient Summary Statistics
#' #'
#' #' Calculates the mean, median, minimum and maximum numbers of prescriptions per
#' #' patient within the data - either for all drugs, or for those matching the
#' #' specified ID
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a drug identifier
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param patient_id_col a string, the name of the column in \code{df}
#' #'   containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #'
#' #' @return a dat aframe containing the mean, median, minimum and maximum number
#' #'   of prescriptions per patient
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{presc_per_patient(synth_presc, drug_id_col = "approved_name")}
#' #' \code{presc_per_patient(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name")}
#' #'
#' presc_per_patient <- function(df, drug = "*", patient_id_col = "patient_id",
#'                               drug_id_col = "drug_id"){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col)
#'   summ1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   summ2 <- summ1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::summarise(n_presc = n())
#'   result <- summ2 %>%
#'     dplyr::summarise(mean = mean(n_presc),
#'                      median = median(n_presc),
#'                      min = min(n_presc),
#'                      max = max(n_presc))
#'   return(result)
#' }
#'
#'
#' #' Prescription Time Trends
#' #'
#' #' Function creates a breakdown of the number of prescriptions matching a chosen
#' #' drug identifier over time - either broken down by month, quarter, semester or
#' #' year.
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a drug identifier and a prescription date
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param group a string corresponding to the desired grouping for results -
#' #'   either month ("M"), quarter ("Q"), semester ("S") or year ("Y")
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing the prescption date
#' #' @param dd_disp_col a string, the name of the column in \code{df} containing
#' #'   the number of daily doses dispensed
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing the selected time intervals and number of
#' #'   prescriptions per interval
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{presc_by_time(synth_presc, drug = "SIMVASTATIN", drug_id_col = "approved_name", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")}
#' #' \code{presc_by_time(synth_presc, drug = "212000", group = "M", drug_id_col = "bnf_paragraph", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")}
#' #'
#' presc_by_time <- function(df, drug = "*", group = "Y",
#'                           drug_id_col = "drug_id", presc_date_col = "presc_date",
#'                           dd_disp_col = "dd_disp", date_format){
#'   tidy_df <- tidy_presc(df, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dd_disp = dd_disp_col,
#'                         dates_format = date_format)
#'   summ1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id)) %>%
#'     dplyr::mutate(month = lubridate::month(presc_date),
#'                   year = lubridate::year(presc_date),
#'                   quarter = lubridate::quarter(presc_date, with_year = TRUE),
#'                   semester = lubridate::semester(presc_date, with_year = TRUE)) %>%
#'     dplyr::select(presc_date, month, year, quarter, semester, dd_disp)
#'   if(group == "Y"){
#'     result <- summ1 %>%
#'       dplyr::group_by(year) %>%
#'       dplyr::summarise(n_presc = n(),
#'                        ddds_disp = sum(dd_disp))
#'   } else if (group == "M"){
#'     result <- summ1 %>%
#'       dplyr::group_by(year, month) %>%
#'       dplyr::summarise(n_presc = n(),
#'                        ddds_disp = sum(dd_disp))
#'   } else if (group == "S"){
#'     result <- summ1 %>%
#'       dplyr::group_by(semester) %>%
#'       dplyr::summarise(n_presc = n(),
#'                        ddds_disp = sum(dd_disp))
#'   } else if (group == "Q"){
#'     result <- summ1 %>%
#'       dplyr::group_by(quarter) %>%
#'       dplyr::summarise(n_presc = n(),
#'                        ddds_disp = sum(dd_disp))
#'   }
#'   return(result)
#' }
#'
#' #' Determine Ever Use of Drug(s) of Interest
#' #'
#' #' Determines which patients within a data frame have been exposed to the drug
#' #' of interest, based on having at least the desired number of prescriptions
#' #' matching the drug identifier of interest, with option to also provide counts
#' #' and dates of the first matching prescription.
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a paitent ID, drug ID and a prescription date
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param summary logical, if TRUE the corresponding prescription counts and
#' #'   first prescription dates will be returned alongside patient IDs
#' #' @param threshold a number representing the minimum number of prescriptions
#' #'   that must be present for the patient to be considered exposed
#' #' @param patient_id_col a string, the name of the column in \code{df}
#' #'   containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing the prescption date
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing patient IDs who match the selected criteria,
#' #'   and if \code{summary} is TRUE the corresponding prescription counts and
#' #'   dates
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{ever_use(synth_presc, drug = "CITALOPRAM", drug_id_col = "approved_name")}
#' #' \code{ever_use(synth_presc, drug = "212000", summary = TRUE, threshold = 10,
#' #' drug_id_col = "bnf_paragraph", date_format = "%d/%m/%Y")}
#' #'
#'
#' ever_use <- function(df, drug, summary = FALSE, threshold = 1,
#'                      patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                      presc_date_col = "presc_date", date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dates_format = date_format)
#'   ever1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id)) %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::summarise(n_presc = n(),
#'                      first_presc = min(presc_date)) %>%
#'     dplyr::filter(n_presc >= threshold)
#'   if(summary == TRUE){
#'     ever_result <- ever1
#'     return(ever_result)
#'   } else {
#'     ever_result <- ever1 %>%
#'       dplyr::select(patient_id)
#'     return(ever_result)
#'   }
#' }
#'
#' #' Use at Time Point, Fixed Date Range
#' #'
#' #' Determines which patients have been exposed to the drug(s) of interest within
#' #' a timeframe of interest. This function applies the same timeframe to all
#' #' prescriptions - by default it considers all prescriptions before or after the
#' #' date provided, but a number of days can be entered to give a more specific
#' #' timeframe.
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a patient ID, drug ID and prescription date
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param date_1 a string containing a date to base the follow-up window on
#' #' @param timeframe a number representing the length of the follow-up window. If
#' #'   0 as by default any prescriptions before or after (depending on the value
#' #'   of \code{forward}) the date entered in \code{date_1} will be considered
#' #' @param forward a logical, if TRUE the value of \code{timeframe} will be added
#' #'   to the value of \code{date_1} to generate an end date for the follow-up
#' #'   window. If FALSE, the value will be subtracted to generate a start date
#' #' @param patient_id_col a string, the name of the column in \code{df}
#' #'   containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing the prescption date
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing a list of patient IDs for patients who have
#' #'   at least one prescription matching the criteria, the number of
#' #'   prescriptions within the defined date window and the date of the first
#' #'   matching prescription
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{uat_fixed(synth_presc, drug = "ATORVASTATIN", date_1 = "01/07/2020", drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#' #' \code{uat_fixed(synth_presc, drug = "SIMVASTATIN", date_1 = "01/01/2021", timeframe = 180, forward = FALSE, drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#' #'
#' uat_fixed <- function(df, drug, date_1, timeframe = 0, forward = TRUE,
#'                       patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                       presc_date_col = "presc_date", date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dates_format = date_format)
#'   uat1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   date_1 <- as.Date(date_1, format = date_format)
#'   if((forward == TRUE) && (timeframe == 0)){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date >= date_1)
#'   } else if ((forward == FALSE) && (timeframe == 0)){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(date_1 >= presc_date)
#'   } else if ((forward == TRUE) && (timeframe > 0)){
#'     date_2 <- date_1 + timeframe
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date >= date_1 & presc_date <=date_2)
#'   } else if ((forward == FALSE) && (timeframe > 0)){
#'     date_2 <- date_1 - timeframe
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date <= date_1 & presc_date >= date_2)
#'   }
#'   uat_result <- uat1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::summarise(n_presc = n(),
#'                      first_presc = min(presc_date))
#'   return(uat_result)
#' }
#'
#' #' Use At Time Point, Fixed Range From Patient Event Date
#' #'
#' #' Determines which patients have been exposed to the drug(s) of interest within
#' #' a timeframe of interest. This function applies the same timeframe to
#' #' patient-specific event dates. By default it considers all prescriptions
#' #' before or after the event date, but a number of days can be entered to give a
#' #' more specific timeframe.
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a patient ID, drug ID, prescription date
#' #' @param df2 a data frame containing event dates - should contain at least
#' #'   patient IDs and an event date, can also include event codes
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param timeframe a number representing the length of the follow-up window. If
#' #'   0 as by default any prescriptions before or after (depending on the value
#' #'   of \code{forward}) the patient's event date, \code{presc_date}, will be
#' #'   considered
#' #' @param forward a logical, if TRUE the value of \code{timeframe} will be added
#' #'   to the value of \code{presc_date} to generate an end date for the follow-up
#' #'   window. If FALSE, the value will be subtracted to generate a start date
#' #' @param patient_id_col a string, the name of the column in \code{df} and
#' #'   \code{df2} containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing
#' #' @param ev_date_col a string, the name of the column in \code{df2} containing
#' #'   the event dates
#' #' @param ev_code_col a string, the name of the column in \code{df2} containing
#' #'   the event codes if present
#' #' @param date_format a string, the format of the dates contained in \code{df}
#' #'   and \code{df2}
#' #' @return data frame containing a list of patient IDs for patients who have at
#' #'   least one prescription matching the criteria, the number of prescriptions
#' #'   within the defined date window and the date of the first matching
#' #'   prescription
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{}
#' #' \code{}
#' #'
#' uat_fixed_events <- function(df, df2, drug, timeframe = 0, forward = TRUE,
#'                              patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                              presc_date_col = "presc_date",
#'                              ev_date_col = "ev_date_1", ev_code_col = NULL,
#'                              date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dates_format = date_format)
#'   tidy_ev <- tidy_events(df2, patient_id = patient_id_col, ev_date_1 = ev_date_col,
#'                          ev_code_1 = ev_code_col, dates_format = date_format)
#'   uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
#'   uat1 <- dplyr::filter(uat1, !is.na(ev_date_1))
#'   uat1 <- uat1 %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   if((forward == TRUE) && (timeframe == 0)){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date >= ev_date_1)
#'   } else if ((forward == FALSE) && (timeframe == 0)){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(ev_date_1 >= presc_date)
#'   } else if ((forward == TRUE) && (timeframe > 0)){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date >= ev_date_1 & ev_date_1 <= ev_date_1 + timeframe)
#'   } else if ((forward == FALSE) && (timeframe > 0)){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date <= ev_date_1 & presc_date >= ev_date_1 - timeframe)
#'   }
#'   uat_result <- uat1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::summarise(n_presc = n(),
#'                      first_presc = min(presc_date))
#'   return(uat_result)
#' }
#'
#' #' Use at Time Point, Individual Patient Date Ranges
#' #'
#' #' Determines which patients have been exposed to the drug(s) of interest within
#' #' a timeframe of interest. This function determines exposure by checking for
#' #' prescription dates between two patient-specific event dates.
#' #'
#' #' @param df a data frame containing prescribing records to be analysed -
#' #'   records must contain at least a patient ID, drug ID, prescription date and
#' #'   two event dates (\code{ev_date_1} and \code{ev_date_2})
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param forward a logical, if TRUE the follow-up window will start with
#' #'   \code{ev_date_1} as the start date and \code{ev_date_2} as the end date. If
#' #'   FALSE, the reverse will be true
#' #' @param patient_id_col a string, the name of the column in \code{df} and
#' #'   \code{df2} containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing
#' #' @param ev_date_col_1 a string, the name of the column in \code{df2}
#' #'   containing the first event dates
#' #' @param ev_code_col_1 a string, the name of the column in \code{df2}
#' #'   containing the first event codes if present
#' #' @param ev_date_col_2 a string, the name of the column in \code{df2}
#' #'   containing the second event dates if present
#' #' @param ev_code_col_2 a string, the name of the column in \code{df2}
#' #'   containing the second event codes if present
#' #' @param date_format a string, the format of the dates contained in \code{df}
#' #'   and \code{df2}
#' #'
#' #' @return a data frame containing a list of patient IDs for patients who have
#' #'   at least one prescription matching the criteria, the number of
#' #'   prescriptions within the defined date window and the date of the first
#' #'   matching prescription
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{}
#' #'
#' uat_var_events <- function(df, df2, drug, forward = TRUE,
#'                            patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                            presc_date_col = "presc_date",
#'                            ev_date_1_col = "ev_date_1", ev_code_1_col = NULL,
#'                            ev_date_2_col = "ev_date_2", ev_code_2_col = NULL,
#'                            date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dates_format = date_format)
#'   tidy_ev <- tidy_events(df2, patient_id = patient_id_col,
#'                          ev_date_1 = ev_date_1_col, ev_code_1 = ev_code_1_col,
#'                          ev_date_2 = ev_date_2_col, ev_code_2 = ev_code_2_col,
#'                          dates_format = date_format)
#'   uat1 <- dplyr::left_join(tidy_df, tidy_ev, by = "patient_id")
#'   uat1 <- dplyr::filter(uat1, !is.na(ev_date_1) & !is.na(ev_date_2))
#'   uat1 <- uat1 %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   if(forward == TRUE){
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date > ev_date_1 & presc_date < ev_date_2)
#'   } else {
#'     uat1 <- uat1 %>%
#'       dplyr::filter(presc_date > ev_date_2 & presc_date < ev_date_1)
#'   }
#'   uat_result <- uat1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::summarise(n_presc = n(),
#'                      first_presc = min(presc_date))
#'   return(uat_result)
#' }
#'
#' #' Calculate Cumulative Daily Dose
#' #'
#' #' Calculates the cumulative number of daily doses of a drug of interest
#' #' dispensed for each patient. Results can be limited to only patients who have
#' #' been prescribed more than a desired threshold number of daily doses.
#' #'
#' #' @param df a data frame containing prescribing records  to be analysed -
#' #'   records must contain at least a paitent ID, drug ID, a prescription date
#' #'   and the number of daily doses disepnsed
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param threshold a number representing the minimum number of daily doses that
#' #'   must be present for the patient to be considered exposed
#' #' @param patient_id_col a string, the name of the column in \code{df} containing
#' #'   the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing the
#' #'   drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df} containing
#' #'   the prescption date
#' #' @param dd_disp_col a string, the name of the column in \code{df} containing the
#' #'   number of daily doses dispensed
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing patient IDs and the corresponding number of
#' #'   prescriptions, number of daily doses dispensed and the date of the first
#' #'   prescription of the drug of interest
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{}
#' #' \code{}
#' #'
#' dd_sum <- function(df, drug, threshold = 1,
#'                    patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                    presc_date_col = "presc_date", dd_disp_col = "dd_disp",
#'                    date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dd_disp = dd_disp_col,
#'                         dates_format = date_format)
#'   dd1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   dd1 <- dd1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::summarise(n_presc = n(),
#'                      total_dds = sum(dd_disp),
#'                      first_presc = min(presc_date)) %>%
#'     dplyr::filter(total_dds >= threshold)
#'   return(dd1)
#' }
#'
#' #Still need to add a date range restriction to this function - fixed date ranges
#' #or individual date ranges?
#'
#'
#' #' Calculate Prescription Durations
#' #'
#' #' Calculates the duration in days of individual prescriptions based on the
#' #' number of daily doses dispensed. A multiplication factor is applied to allow
#' #' for use in cases where, for example an assumption of less or more than 1 DDD
#' #' per day is required
#' #'
#' #' @param df a data frame containing prescribing records  to be analysed -
#' #'   records must contain at least a paitent ID, drug ID, a prescription date
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param dd_factor a number, a multiplication factor applied to the dd_disp
#' #'   field to calculate prescription duration
#' #' @param patient_id a string, the name of the column in \code{df} containing
#' #'   the patient IDs
#' #' @param drug_id a string, the name of the column in \code{df} containing the
#' #'   drug IDs
#' #' @param presc_date a string, the name of the column in \code{df} containing
#' #'   the prescption date
#' #' @param dd_disp a string, the name of the column in \code{df} containing the
#' #'   number of daily doses dispensed
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a modified version of \code{df} with records for the drug of interest
#' #'   with the \code{duration} field added
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{}
#' #' \code{}
#' #'
#' dd_duration <- function(df, drug, dd_factor = 1,
#'                         patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                         presc_date_col = "presc_date", dd_disp_col = "dd_disp",
#'                         date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dd_disp = dd_disp_col,
#'                         dates_format = date_format)
#'   dd1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   dd1 <- dd1 %>%
#'     dplyr::mutate(duration = floor(dd_disp * dd_factor),
#'                   end_date = presc_date + floor(dd_disp * dd_factor))
#' }
#'
#'
#' #' Calculate Number of Prescribed Daily Doses Dispensed
#' #'
#' #' Calculates the number of Prescribed Daily Doses (PDDs) dispensed based on
#' #' individual patient instructions on how many tablets should be taken per day,
#' #' and the number of tablets dispensed, and adds this as a new column to the
#' #' data frame.
#' #'
#' #' @param df a data frame containing prescribing records  to be analysed -
#' #'   records must contain at least a paitent ID, drug ID, a quantity dispensed
#' #'   and a quantity per day instruction
#' #' @param drug a string corresponding to a drug identifier to be matched
#'
#' #' @param drug_id_col a string, the name of the column in \code{df} containing the
#' #'   drug IDs
#' #' @param qty_disp_col a string, the name of the column in \code{df} containing the
#' #'   quantity of drug dispensed
#' #' @param qty_per_day_col a string, the name of the column in \code{df} containing
#' #'   the quantity of drug to be taken per day
#' #'
#' #' @return a modified version of \code{df} containing records for the drug of
#' #'   interest with the \code{dd_disp} field added
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{}
#' #'
#' calculate_pdd <- function(df, drug, patient_id_col = "patient_id",
#'                           drug_id_col = "drug_id", qty_disp_col = "qty_disp",
#'                           qty_per_day_col = "qty_per_day"){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         qty_disp = qty_disp_col, qty_per_day = qty_per_day_col)
#'   dd1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   dd1 <- dd1 %>%
#'     dplyr::mutate(pdd_disp = qty_disp/qty_per_day)
#'   return(dd1)
#' }
#'
#'
#' #' Determine Drug Persistence with Refill Gap Only
#' #'
#' #' This function determines periods of persistent use of the drug of interest
#' #' based on the gaps between prescriptions. Gaps which exceed the selected
#' #' allowable gap length result in discontinuation points, and the function
#' #' returns patient ID's and periods of exposure that are over the selected
#' #' minimum length.
#' #'
#' #' @param df a data frame containing prescribing records  to be analysed -
#' #'   records must contain at least a paitent ID, drug ID and a prescription date
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param gap a number, the length of the allowable gap in days between two
#' #'   prescriptions before the patient is considered to have discontinued use of
#' #'   the drug
#' #' @param threshold a number, the minimum length of period of exposure, in days,
#' #'   to be included in the results
#' #' @param patient_id_col a string, the name of the column in \code{df}
#' #'   containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing the prescption date
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing patient IDs and corresponding periods of
#' #'   exposure (consisting of first and last prescriptions, number of
#' #'   prescriptions and lengths of exposure) which match the chosen refill gap
#' #'   and minimum length
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{refill_gap(synth_presc, drug = "SIMVASTATIN", gap = 30, drug_id_col = "approved_name", date_format = "%d/%m/%Y")}
#' #' \code{refill_gap(synth_presc, drug = "103050", gap = 30, threshold = 60, drug_id_col = "bnf_paragraph", date_format = "%d/%m/%Y")}
#' #'
#' refill_gap <- function(df, drug, gap, threshold = 0,
#'                        patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                        presc_date_col = "presc_date", date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dates_format = date_format)
#'   pers1 <- tidy_df %>%
#'     dplyr::filter(grepl(drug, drug_id))
#'   pers1 <- pers1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::arrange(patient_id, presc_date) %>%
#'     dplyr::mutate(difference = c(0, diff(presc_date)))
#'   pers1 <- dplyr::mutate(pers1, terminated = ifelse(difference > gap, 1, 0))
#'   pers1 <- pers1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::mutate(period = cumsum(terminated)) %>%
#'     dplyr::select(patient_id, presc_date, difference, terminated, period)
#'   pers1 <- pers1 %>%
#'     dplyr::group_by(patient_id, period) %>%
#'     dplyr::summarise(first_presc = min(presc_date),
#'                      last_presc = max(presc_date),
#'                      n_presc = n(),
#'                      length_of_exposure = (max(presc_date) - min(presc_date)) + gap)
#'   if(threshold == 0){
#'     return(pers1)
#'   }
#'   else if(threshold != 0){
#'     pers2 <- pers1 %>%
#'       dplyr::filter(length_of_exposure >= threshold)
#'     return(pers2)
#'   }
#' }
#'
#'
#' #' Determine Drug Persistence with Refill Gap and Coverage
#' #'
#' #' This function determines periods of persistent use of the drug of interest
#' #' based on the gaps between prescriptions, taking into account the coverage of
#' #' those prescriptions. Prescription duration is defined based on the number of
#' #' daily doses dispensed, and there is the option to carry stockpiled
#' #' medications from one prescription over to the next . Gaps which exceed the
#' #' selected allowable gap length result in discontinuation points, and the
#' #' function returns patient ID's and periods of exposure that are over the
#' #' selected minimum length.
#' #'
#' #' @param df a data frame containing prescribing records  to be analysed -
#' #'   records must contain at least a paitent ID, drug ID, a prescription date
#' #'   and a number of daily doses dispensed
#' #' @param drug a string corresponding to a drug identifier to be matched
#' #' @param gap a number, the length of the allowable gap in days between two
#' #'   prescriptions before the patient is considered to have discontinued use of
#' #'   the drug
#' #' @param dd_factor a number, a multiplication factor applied to the dd_disp
#' #'   field to calculate prescription duration - used, for example, if using an
#' #'   assumption of 0.5 DDDs per day
#' #' @param threshold a number, the minimum length of period of exposure, in days,
#' #'   to be included in the results
#' #' @param stockpile a logical, if TRUE the function will carry over leftover
#' #'   medication from one prescription period to the next when determining
#' #'   duration
#' #' @param patient_id_col a string, the name of the column in \code{df}
#' #'   containing the patient IDs
#' #' @param drug_id_col a string, the name of the column in \code{df} containing
#' #'   the drug IDs
#' #' @param presc_date_col a string, the name of the column in \code{df}
#' #'   containing the prescption date
#' #' @param dd_disp_col a string, the name of the column in \code{df} containing
#' #'   the number of daily doses dispensed
#' #' @param date_format a string, the format of the dates in \code{df}
#' #'
#' #' @return a data frame containing patient IDs and corresponding periods of
#' #'   exposure (consisting of first and last prescriptions, number of
#' #'   prescriptions and lengths of exposure) which match the chosen refill gap
#' #'   and minimum length
#' #'
#' #'
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' \code{refill_gap_dd(refill_gap_dd(synth_presc, drug = "OMEPRAZOLE", gap = 30, drug_id_col = "approved_name", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y"))}
#' #' \code{refill_gap_dd(synth_presc, drug = "CITALOPRAM", gap = 30, dd_factor = 1, threshold = 60, stockpile = TRUE, drug_id_col = "approved_name", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")}
#' #'
#' refill_gap_dd <- function(df, drug, gap, dd_factor = 1,
#'                           threshold = 0, stockpile = FALSE,
#'                           patient_id_col = "patient_id", drug_id_col = "drug_id",
#'                           presc_date_col = "presc_date", dd_disp_col = "dd_disp",
#'                           date_format){
#'   tidy_df <- tidy_presc(df, patient_id = patient_id_col, drug_id = drug_id_col,
#'                         presc_date = presc_date_col, dd_disp = dd_disp_col,
#'                         dates_format = date_format)
#'   pers1 <- tidy_df %>%
#'     dd_duration(drug = drug, dd_factor = dd_factor)
#'   if(stockpile == FALSE){
#'     pers1 <- pers1 %>%
#'       dplyr::group_by(patient_id) %>%
#'       dplyr::arrange(patient_id, presc_date) %>%
#'       dplyr::mutate(difference = presc_date - lag(end_date))
#'     pers1$difference[is.na(pers1$difference)] <- 0
#'   } else if(stockpile == TRUE){
#'     pers1 <- pers1 %>%
#'       dplyr::group_by(patient_id) %>%
#'       dplyr::arrange(patient_id, presc_date) %>%
#'       dplyr::mutate(stockpile = ifelse((lag(end_date) > presc_date),
#'                                        (lag(end_date) - presc_date), 0))
#'     pers1$stockpile[is.na(pers1$stockpile)] <- 0
#'     pers1 <- pers1 %>%
#'       dplyr::mutate(end_date = presc_date + (duration + stockpile)) %>%
#'       dplyr::mutate(difference = presc_date - lag(end_date))
#'     pers1$difference[is.na(pers1$difference)] <- 0
#'   }
#'   pers1 <- dplyr::mutate(pers1, terminated = ifelse(difference > gap, 1, 0))
#'   pers1 <- pers1 %>%
#'     dplyr::group_by(patient_id) %>%
#'     dplyr::mutate(period = cumsum(terminated)) %>%
#'     dplyr::select(patient_id, presc_date, end_date, dd_disp, difference, terminated, period)
#'   pers1 <- pers1 %>%
#'     dplyr::group_by(patient_id, period) %>%
#'     dplyr::summarise(first_presc = min(presc_date),
#'                      last_presc = max(presc_date),
#'                      end_date = max(end_date + gap),
#'                      n_presc = n(),
#'                      length_of_exposure = (max(end_date) - min(presc_date)) + gap)
#'   if(threshold == 0){
#'     return(pers1)
#'   } else if(threshold != 0){
#'     pers2 <- pers1 %>%
#'       dplyr::filter(length_of_exposure >= threshold)
#'     return(pers2)
#'   }
#' }
