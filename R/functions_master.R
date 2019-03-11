# #1.2.1 - Standardising field names
# tidy_names <- function(df, patient_id, drug_id, date_x, dd_disp=NULL){
#   df1 <- df %>% rename_(.dots = setNames(patient_id, "patient_id")) %>%
#     rename_(.dots = setNames(drug_id, "drug_id")) %>%
#     rename_(.dots = setNames(date_x, "date_x"))
#     if(!is.null(dd_disp)){
#       df1 <- df1 %>% rename_(.dots = setNames(dd_disp, "dd_disp"))
#     }
#   return(df1)
# }
#
# #1.2.2 - Standardising data formats
# tidy_formats <- function(df, dd_disp = FALSE){
#   df$patient_id <- as.character(df$patient_id)
#   df$drug_id <- as.character(df$drug_id)
#   df$date_x <- as.Date(df$date_x)
#     if(dd_disp != FALSE){
#       df$dd_disp <- as.numeric(df$dd_disp)
#       }
#   return(df)
# }
#
# # Standardising field names - event dates
# tidy_event_names <- function(df, patient_id, ev_date_1, ev_date_2 = NULL){
#   df1 <- df %>% rename_(.dots = setNames(patient_id, "patient_id")) %>%
#     rename_(.dots = setNames(ev_date_1, "ev_date_1"))
#   if(!is.null(ev_date_2)){
#     df1 <- df1 %>% rename_(.dots = setNames(ev_date_2, "ev_date_2"))
#   }
#   return(df1)
# }
#
# # Standardising data formats - event dates
# tidy_event_formats <- function(df, ev_date_2 = FALSE, date_format){
#   df$patient_id <- as.character(df$patient_id)
#   df$ev_date_1 <- as.Date(df$ev_date_1, format = date_format)
#   if(ev_date_2 != FALSE){
#     df$ev_date_2 <- as.Date(df$ev_date_2, format = date_format)
#   }
#   return(df)
# }
#
# # Join event dates to prescription data
# ev_date_join <- function(df1, df2, ev_date_2 = FALSE){
#       if(ev_date_2 == FALSE){
#         df2 <- df2 %>%
#           select(patient_id, ev_date_1)
#       } else if (ev_date_2 == TRUE){
#         df2 <- df2 %>%
#           select(patient_id, ev_date_1, ev_date_2)
#       }
#     df3 <- left_join(df1, df2, by = "patient_id")
#     return(df3)
# }
#
#
# # 1.3 - Data summaries
# # 1.3.1 - Prescription and patient counts and date range of data
# summary1 <- function(df, drug = "*"){
#   summ1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   result <- summ1 %>%
#     summarize(n_presc = n(),
#               n_pat = n_distinct(patient_id),
#               date_first = min(date_x),
#               date_last = max(date_x))
#   return(result)
# }
#
# # 1.3.2 - Most commonly prescribed drugs
# summary2 <- function(df, rank = 10){
#   freq <- df %>%
#     group_by(drug_id) %>%
#     summarise(n_presc = n())
#   freq <- freq %>%
#     top_n(rank, n_presc) %>%
#     arrange(desc(n_presc))
#   return(freq)
# }
#
# # 1.3.3 - Mean, median and range prescriptions per patient
# summary3 <- function(df, drug = "*"){
#   summ1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   summ2 <- summ1 %>%
#     group_by(patient_id) %>%
#     summarize(n_presc = n())
#   result <- summ2 %>%
#     summarise(mean = mean(n_presc),
#               median = median(n_presc),
#               min = min(n_presc),
#               max = max(n_presc))
#   return(result)
# }
#
# # 1.3.4 - Prescription time trends
# summary4 <- function(df, drug = "*", group = "Y"){
#   summ1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id)) %>%
#     mutate(month = month(date_x),
#            year = year(date_x),
#            quarter = quarter(date_x, with_year = TRUE),
#            semester = semester(date_x, with_year = TRUE)) %>%
#     select(date_x, month, year, quarter, semester)
#   if(group == "Y"){
#     result <- summ1 %>%
#       group_by(year) %>%
#       summarise(n_presc = n())
#   } else if (group == "M"){
#     result <- summ1 %>%
#       group_by(year, month) %>%
#       summarise(n_presc = n())
#   } else if (group == "S"){
#     result <- summ1 %>%
#       group_by(semester) %>%
#       summarise(n_presc = n())
#   }else if (group == "Q"){
#     result <- summ1 %>%
#       group_by(quarter) %>%
#       summarise(n_presc = n())
#   }
# }
#
# # 1.4 - Ever use
# everuse <- function(df, drug, summary = FALSE, threshold = 1){
#   ever1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id)) %>%
#     group_by(patient_id) %>%
#     summarise(n_presc = n(),
#               first_presc = min(date_x)) %>%
#     dplyr::filter(n_presc >= threshold)
#   if(summary == TRUE){
#     ever_result <- ever1
#     return(ever_result)
#   } else {
#     ever_result <- ever1 %>%
#       select(patient_id)
#     return(ever_result)
#   }
#
# }
#
# #1.5 Use at time point, fixed date range
# uat_fixed <- function(df, drug, date_1, timeframe = 0, forward = TRUE){
#   uat1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   date_1 <- as.Date(date_1)
#   if((forward == TRUE) && (timeframe == 0)){
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x >= date_1)
#   } else if ((forward == FALSE) && (timeframe == 0)){
#     uat1 <- uat1 %>%
#       dplyr::filter(date_1 >= date_x)
#   } else if ((forward == TRUE) && (timeframe > 0)){
#     date_2 <- date_1 + timeframe
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x >= date_1 & date_x <=date_2)
#   } else if ((forward == FALSE) && (timeframe > 0)){
#     date_2 <- date_1 - timeframe
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x <= date_1 & date_x >= date_2)
#   }
#   uat_result <- uat1 %>%
#     group_by(patient_id) %>%
#     summarise(n_presc = n(),
#               first_presc = min(date_x))
#   return(uat_result)
# }
#
# #1.5 Use at time point, fixed range from individual dates
# uat_fixed_events <- function(df, drug, timeframe = 0, forward = TRUE){
#   uat1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   if((forward == TRUE) && (timeframe == 0)){
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x >= ev_date_1)
#   } else if ((forward == FALSE) && (timeframe == 0)){
#     uat1 <- uat1 %>%
#       dplyr::filter(ev_date_1 >= date_x)
#   } else if ((forward == TRUE) && (timeframe > 0)){
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x >= ev_date_1 & ev_date_1 <= ev_date_1 + timeframe)
#   } else if ((forward == FALSE) && (timeframe > 0)){
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x <= ev_date_1 & date_x >= ev_date_1 - timeframe)
#   }
#   uat_result <- uat1 %>%
#     group_by(patient_id) %>%
#     summarise(n_presc = n(),
#               first_presc = min(date_x))
#   return(uat_result)
# }
#
# #1.5 Use at time point, individual date ranges
# uat_var_events <- function(df, drug, forward = TRUE){
#   uat1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   if(forward == TRUE){
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x > ev_date_1 & date_x < ev_date_2)
#   } else {
#     uat1 <- uat1 %>%
#       dplyr::filter(date_x > ev_date_2 & date_x < ev_date_1)
#   }
#   uat_result <- uat1 %>%
#     group_by(patient_id) %>%
#     summarise(n_presc = n(),
#               first_presc = min(date_x))
#   return(uat_result)
# }
#
# #1.6 Daily dose, cumulative daily dose
# dd_sum <- function(df, drug, threshold = 1){
#   dd1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   dd1 <- dd1 %>%
#     group_by(patient_id) %>%
#     summarise(n_presc = n(),
#               total_dds = sum(dd_disp),
#               first_presc = min(date_x)) %>%
#     dplyr::filter(total_dds >= threshold)
# }
#
# #1.6 Daily dose, durations
# dd_duration <- function(df, drug, dd_factor = 1){
#   dd1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   dd1 <- dd1 %>%
#     mutate(duration = dd_disp * dd_factor,
#            end_date = date_x + (dd_disp * dd_factor))
# }
#
# #1.6 Daily dose, duration from individual dosing instructions
# #Prescribed Daily Doses dispensed = number of tablets dispensed / number of tablets per day
#
#
# #1.7 Persistence, refill gap only
# refill_gap <- function(df, drug, gap, threshold = 0){
#   pers1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   pers1 <- pers1 %>%
#     group_by(patient_id) %>%
#     arrange(patient_id, date_x) %>%
#     mutate(difference = c(0, diff(date_x)))
#   pers1 <- mutate(pers1, terminated = ifelse(difference > gap, 1, 0))
#   pers1 <- pers1 %>%
#     group_by(patient_id) %>%
#     mutate(period = cumsum(terminated)) %>%
#     select(patient_id, date_x, difference, terminated, period)
#   pers1 <- pers1 %>%
#     group_by(patient_id, period) %>%
#     summarise(first_presc = min(date_x),
#               last_presc = max(date_x),
#               n_presc = n(),
#               length_of_exposure = (max(date_x) - min(date_x)) + gap)
#   if(point == 0){
#     return(pers1)
#   }
#   else if(point != 0){
#     pers2 <- pers1 %>%
#       dplyr::filter(length_of_exposure >= point)
#     return(pers2)
#   }
# }
#
# #1.7 Persistence, refill gap with coverage
# refill_gap_dd <- function(df, drug, gap, dd_factor = 1, threshold = 0){
#   pers1 <- df %>%
#     dplyr::filter(grepl(drug, drug_id))
#   pers1 <- pers1 %>%
#     mutate(duration = dd_disp * dd_factor,
#            end_date = date_x + (dd_disp * dd_factor))
#   pers1 <- pers1 %>%
#     group_by(patient_id) %>%
#     arrange(patient_id, date_x) %>%
#     mutate(difference = date_x - lag(end_date))
#   pers1$difference[is.na(pers1$difference)] <- 0
#   pers1 <- mutate(pers1, terminated = ifelse(difference > gap, 1, 0))
#   pers1 <- pers1 %>%
#     group_by(patient_id) %>%
#     mutate(period = cumsum(terminated)) %>%
#     select(patient_id, date_x, end_date, dd_disp, difference, terminated, period)
#   pers1 <- pers1 %>%
#     group_by(patient_id, period) %>%
#     summarise(first_presc = min(date_x),
#               last_presc = max(date_x),
#               end_date = max(end_date + gap),
#               n_presc = n(),
#               length_of_exposure = (max(end_date) - min(date_x)) + gap)
#   if(point == 0){
#     return(pers1)
#   }
#   else if(point != 0){
#     pers2 <- pers1 %>%
#       dplyr::filter(length_of_exposure >= point)
#     return(pers2)
#   }
# }
#
# #1.8 Adherence
# #MPR = number of days covered during period (dd_disp) / length of period
# #adherence_mpr
#
#
