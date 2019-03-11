# #Function usage examples for thesis chapter
# #Standardisation
# synth_presc_tidy <- tidy_presc(synth_presc, patient_id = "patient_id", drug_id = "approved_name", presc_date = "presc_date", dd_disp = "ddd_dispensed", qty_disp = "qty_dispensed", qty_per_day = "qty_per_day", dates_format = "%d/%m/%Y")
# test1 <- head(synth_presc)
# write.csv(test1, file = "synth_presc_head.csv")
#
# synth_events_tidy <- tidy_events(df = synth_events, patient_id = "patient_id", ev_date_1 = "ev_date_1", ev_code_1 = "ev_code_1", ev_date_2 = "ev_date_2", ev_code_2 = "ev_code_2", dates_format = "%d/%m/%Y")
#
#
# #Data summaries
# test1 <- presc_data_summary(synth_presc_tidy, date_format = "%d/%m/%Y")
# test1 <- presc_data_summary(synth_presc, drug = "SIMVASTATIN", patient_id_col = "patient_id", drug_id_col = "approved_name", presc_date_col = "presc_date", date_format = "%d/%m/%Y")
#
# test1 <- presc_top_drugs(synth_presc_tidy)
# test1 <- presc_top_drugs(synth_presc, rank = 5, drug_id_col = "bnf_item_code")
#
# test1 <- presc_per_patient(synth_presc_tidy, drug = "CITALOPRAM")
# test1 <- presc_per_patient(synth_presc, drug = "STATIN$", patient_id_col = "patient_id", drug_id_col = "approved_name")
#
# test1 <- presc_by_time(synth_presc_tidy, drug = "ATORVASTATIN", group = "Y")
# test1 <- presc_by_time(synth_presc, drug = "212", group = "Q", drug_id_col = "bnf_section", presc_date_col = "presc_date", dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")
#
#
# #Ever use
# test1 <- ever_use(synth_presc_tidy, drug = "OMEPRAZOLE")
# test1 <- ever_use(synth_presc_tidy, drug = "OMEPRAZOLE", summary = TRUE, threshold = 5)
# test1 <- ever_use(synth_presc, drug = "212000", summary = TRUE, threshold = 10, patient_id_col = "patient_id", drug_id_col =  "bnf_paragraph",  presc_date_col = "presc_date", date_format = "%d/%m/%Y")
# write.csv(test1, "test1.csv")
#
#
# #Use at time point
# test1 <- uat_fixed(synth_presc_tidy, drug = "CITALOPRAM", date_1 = "01/01/2021", date_format = "%d/%m/%Y")
# test1 <- uat_fixed(synth_presc_tidy, drug = "CITALOPRAM", date_1 = "01/01/2021", timeframe = 365, forward = FALSE, date_format = "%d/%m/%Y")
#
# test1 <- uat_fixed_events(df = synth_presc_tidy, df2 = synth_events_tidy, drug = "SIMVASTATIN")
# test2 <- uat_fixed_events(df = synth_presc_tidy, df2 = synth_events_tidy, drug = "SIMVASTATIN", timeframe = 180, forward = FALSE)

# test3 <- uat_var_events(synth_presc_tidy, df2 = synth_events_tidy, drug = "SIMVASTATIN", forward = TRUE)
#
# write.csv(test1, "test1.csv")
# write.csv(test2, "test2.csv")
# write.csv(test3, "test3.csv")

# #daily dose
# test1 <- dd_sum(synth_presc_tidy, drug = "ATORVASTATIN", threshold = 100, date_format = "%d/%m/%Y")
# test1 <- dd_duration(synth_presc_tidy, drug = "CITALOPRAM", dd_factor = 0.5)
# test1 <- calculate_pdd(synth_presc_tidy, drug = "SIMVASTATIN")
#
# #Persistence
# test1 <- refill_gap(synth_presc_tidy, drug = "SIMVASTATIN", gap = 30)
# test1 <- refill_gap(synth_presc_tidy, drug = "SIMVASTATIN", gap = 30, threshold = 60)
#
# test1 <- refill_gap_dd(synth_presc_tidy, drug = "SIMVASTATIN", gap = 30, dd_factor = 1)
# test1 <- refill_gap_dd(synth_presc_tidy, drug = "SIMVASTATIN", gap = 30, dd_factor = 1, stockpile = TRUE)
# write.csv(test1, "test1.csv")
#
#
