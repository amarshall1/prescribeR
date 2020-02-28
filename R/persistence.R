#' Determine Drug Persistence with Refill Gap Only
#'
#' This function determines periods of persistent use of the drug of interest
#' based on the gaps between prescriptions. Gaps which exceed the selected
#' allowable gap length result in discontinuation points, and the function
#' returns patient ID's and periods of exposure that are over the selected
#' minimum length.
#'
#' @param df a data frame containing prescribing records  to be analysed -
#'   records must contain at least a paitent ID, drug ID and a prescription date
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param gap a number, the length of the allowable gap in days between two
#'   prescriptions before the patient is considered to have discontinued use of
#'   the drug
#' @param threshold a number, the minimum length of period of exposure, in days,
#'   to be included in the results
#' @param summary a logical, if true the function will return a summary of all
#'   exposure periods instead of the individual periods
#' @param patient_id_col a string, the name of the column in \code{df}
#'   containing the patient IDs
#' @param drug_id_col a string, the name of the column in \code{df} containing
#'   the drug IDs
#' @param presc_date_col a string, the name of the column in \code{df}
#'   containing the prescption date
#' @param date_format a string, the format of the dates in \code{df}
#'
#' @return a data frame containing patient IDs and corresponding periods of
#'   exposure (consisting of first and last prescriptions, number of
#'   prescriptions and lengths of exposure) which match the chosen refill gap
#'   and minimum length
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' refill_gap(synth_presc, drug = "SIMVASTATIN", gap = 30,
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' date_format = "%d/%m/%Y")
#' refill_gap(synth_presc, drug = "103050", gap = 30, threshold = 60, summary = TRUE,
#' drug_id_col = "bnf_paragraph", presc_date_col = "presc_date",
#' date_format = "%d/%m/%Y")
#'
refill_gap <-
  function(df,
           drug,
           gap,
           threshold = 0,
           summary = FALSE,
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
    pers1 <- tidy_df %>%
      dplyr::filter(grepl(drug, .data$drug_id))
    pers1 <- pers1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::arrange(.data$patient_id, .data$presc_date_x) %>%
      dplyr::mutate(difference = c(0, diff(.data$presc_date_x)))
    pers1 <-
      dplyr::mutate(pers1, terminated = ifelse(.data$difference > gap, 1, 0))
    pers1 <- pers1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::mutate(period = cumsum(.data$terminated)) %>%
      dplyr::select(
        .data$patient_id,
        .data$presc_date_x,
        .data$difference,
        .data$terminated,
        .data$period
      )
    pers1 <- pers1 %>%
      dplyr::group_by(.data$patient_id, .data$period) %>%
      dplyr::summarise(
        first_presc = min(.data$presc_date_x),
        last_presc = max(.data$presc_date_x),
        end_date = max(.data$presc_date_x) + gap,
        n_presc = dplyr::n(),
        duration = (max(.data$presc_date_x) - min(.data$presc_date_x)) + gap
      )
    if (threshold == 0 & summary == FALSE) {
      return(pers1)
    } else if (threshold != 0 & summary == FALSE) {
      pers2 <- pers1 %>%
        dplyr::filter(.data$duration >= threshold)
      return(pers2)
    } else if (threshold == 0 & summary == TRUE) {
      pers2 <- pers1 %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::summarise(
          n_periods = dplyr::n(),
          total_n_presc = sum(.data$n_presc),
          first_presc = min(.data$first_presc),
          last_presc = max(.data$last_presc),
          total_length = sum(.data$duration)
        )
    } else if (threshold != 0 & summary == TRUE) {
      pers2 <- pers1 %>%
        dplyr::group_by(.data$patient_id) %>%
        dplyr::summarise(
          n_periods = dplyr::n(),
          total_n_presc = sum(.data$n_presc),
          first_presc = min(.data$first_presc),
          last_presc = max(.data$last_presc),
          total_length = sum(.data$duration)
        )
      pers2 <- pers2 %>%
        dplyr::filter(.data$total_length >= threshold)
    }
  }



#' Determine Drug Persistence with Refill Gap and Coverage
#'
#' This function determines periods of persistent use of the drug of interest
#' based on the gaps between prescriptions, taking into account the coverage of
#' those prescriptions. Prescription duration is defined based on the number of
#' daily doses dispensed, and there is the option to carry stockpiled
#' medications from one prescription over to the next . Gaps which exceed the
#' selected allowable gap length result in discontinuation points, and the
#' function returns patient ID's and periods of exposure that are over the
#' selected minimum length.
#'
#' @param df a data frame containing prescribing records  to be analysed -
#'   records must contain at least a paitent ID, drug ID, a prescription date
#'   and a number of daily doses dispensed
#' @param drug a string containing a drug ID to be used to limit the prescribing
#'   data to the drug(s) of interest, accepts regular expressions
#' @param gap a number, the length of the allowable gap in days between two
#'   prescriptions before the patient is considered to have discontinued use of
#'   the drug
#' @param dd_factor a number, a multiplication factor applied to the dd_disp
#'   field to calculate prescription duration - used, for example, if using an
#'   assumption of 0.5 DDDs per day
#' @param threshold a number, the minimum length of period of exposure, in days,
#'   to be included in the results
#' @param stockpile a logical, if TRUE the function will carry over leftover
#'   medication from one prescription period to the next when determining
#'   duration
#' @param summary a logical, if TRUE the function will return a summary of all
#'   exposure periods instead of the individual periods
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
#' @return a data frame containing patient IDs and corresponding periods of
#'   exposure (consisting of first and last prescriptions, number of
#'   prescriptions and lengths of exposure) which match the chosen refill gap
#'   and minimum length
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{refill_gap_dd(synth_presc, drug = "SIMVASTATIN", gap = 30, stockpile = TRUE,
#' drug_id_col = "approved_name", presc_date_col = "presc_date",
#' dd_disp_col = "ddd_dispensed", date_format = "%d/%m/%Y")}
#'
refill_gap_dd <- function(df,
                          drug,
                          gap,
                          dd_factor = 1,
                          threshold = 0,
                          stockpile = FALSE,
                          summary = FALSE,
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
  pers1 <- tidy_df %>%
    dd_duration(drug = drug, dd_factor = dd_factor)
  if (stockpile == FALSE) {
    pers1 <- pers1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::arrange(.data$patient_id, .data$presc_date_x) %>%
      dplyr::mutate(difference = as.numeric(.data$presc_date_x - dplyr::lag(.data$end_date)))
    pers1$difference[is.na(pers1$difference)] <- 0
  } else if (stockpile == TRUE) {
    pers1 <- pers1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::arrange(.data$patient_id, .data$presc_date_x) %>%
      dplyr::mutate(stockpile = dplyr::if_else((dplyr::lag(.data$end_date) > .data$presc_date_x),
                                               as.numeric(dplyr::lag(.data$end_date) - .data$presc_date_x),
                                               0
      ))
    pers1$stockpile[is.na(pers1$stockpile)] <- 0
    pers1 <- pers1 %>%
      dplyr::mutate(end_date = .data$presc_date_x + (.data$duration + .data$stockpile)) %>%
      dplyr::mutate(difference = as.numeric(.data$presc_date_x - dplyr::lag(.data$end_date)))
    pers1$difference[is.na(pers1$difference)] <- 0
  }
  pers1 <-
    dplyr::mutate(pers1, terminated = dplyr::if_else(.data$difference > gap, 1, 0))
  pers1 <- pers1 %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::mutate(period = cumsum(.data$terminated)) %>%
    dplyr::select(
      .data$patient_id,
      .data$presc_date_x,
      .data$end_date,
      .data$dd_disp,
      .data$difference,
      .data$terminated,
      .data$period
    )
  pers1 <- pers1 %>%
    dplyr::group_by(.data$patient_id, .data$period) %>%
    dplyr::summarise(
      first_presc = min(.data$presc_date_x),
      last_presc = max(.data$presc_date_x),
      end_date = max(.data$end_date) + gap,
      n_presc = dplyr::n(),
      duration = as.numeric(max(.data$end_date) - min(.data$presc_date_x)) + gap
    )
  if (threshold == 0 & summary == FALSE) {
    return(pers1)
  } else if (threshold != 0 & summary == FALSE) {
    pers2 <- pers1 %>%
      dplyr::filter(.data$duration >= threshold)
    return(pers2)
  } else if (threshold == 0 & summary == TRUE) {
    pers2 <- pers1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::summarise(
        n_periods = dplyr::n(),
        total_n_presc = sum(.data$n_presc),
        first_presc = min(.data$first_presc),
        last_presc = max(.data$last_presc),
        end_of_exposure = max(.data$end_date),
        total_length = sum(.data$duration)
      )
  } else if (threshold != 0 & summary == TRUE) {
    pers2 <- pers1 %>%
      dplyr::group_by(.data$patient_id) %>%
      dplyr::summarise(
        n_periods = dplyr::n(),
        total_n_presc = sum(.data$n_presc),
        first_presc = min(.data$first_presc),
        last_presc = max(.data$last_presc),
        end_of_exposure = max(.data$end_date),
        total_length = sum(.data$duration)
      )
    pers2 <- pers2 %>%
      dplyr::filter(.data$total_length >= threshold)
  }
}
