#' Calculate Change from Previous Year
#'
#' This function calculates the change in `values_num` from the previous year
#' for each `LA and Regions` group in the provided data.
#' The result is a data frame where each row corresponds to the second row of
#' each group when sorted in descending order by `Years`.
#' The `values_num` column in the result contains the change from the
#' previous year,
#' and the `Years` column is set to "Change from previous year".
#'
#' @param data A data frame that contains at least the columns
#' `LA and Regions`, `Years`, and `values_num`.
#' `LA and Regions` and `Years` should be of type character or factor,
#' and `values_num` should be numeric.
#'
#' @return A data frame with the same columns as the input data frame,
#' but only the second row of each group when sorted in descending
#' order by `Years`.
#' The `values_num` column contains the change from the previous year,
#' and the `Years` column is set to "Change from previous year".
#' If a group has only one row in the input data,
#' it will not appear in the output.
#'
calculate_change_from_prev_yr <- function(data) {
  data |>
    dplyr::group_by(`LA and Regions`) |>
    dplyr::arrange(`LA and Regions`,
      desc(Years_num),
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      values_num = dplyr::lag(values_num) - values_num,
      Years = "Change from previous year"
    ) |>
    dplyr::filter(dplyr::row_number() == 2)
}


#' Calculate Trend Based on Change Since Previous Value
#'
#' This function determines the trend (increase, decrease, or no trend)
#' based on the change since the previous value.
#'
#' @param change_since_prev Numeric vector representing the change since
#' the previous value.
#'
#' @return A character vector indicating the trend: "Increase", "Decrease",
#' "No trend", or `NA` if the input is missing.
#'
calculate_trend <- function(change_since_prev) {
  dplyr::case_when(
    is.na(change_since_prev) ~ NA_character_,
    change_since_prev > 0 ~ "Increase",
    change_since_prev < 0 ~ "Decrease",
    TRUE ~ "No trend"
  )
}


#' Calculate Quartile Band Based on Indicator Value
#'
#' This function assigns a quartile band (A, B, C, D) to an indicator value
#' based on its position within specified quartile bands.
#'
#' @param indicator_val Numeric value representing the indicator to be
#' classified.
#' @param quartile_bands A named numeric vector specifying the
#' quartile band thresholds.
#'
#' @return A character vector indicating the quartile band: "A", "B", "C", "D",
#' or "Error" if the value does not fit within the provided ranges.
#'
calculate_quartile_band <- function(indicator_val, quartile_bands) {
  dplyr::case_when(
    is.na(indicator_val) ~ NA_character_,
    (indicator_val >= quartile_bands[["0%"]]) &
      (indicator_val <= quartile_bands[["25%"]]) ~ "A",
    (indicator_val > quartile_bands[["25%"]]) &
      (indicator_val <= quartile_bands[["50%"]]) ~ "B",
    (indicator_val > quartile_bands[["50%"]]) &
      (indicator_val <= quartile_bands[["75%"]]) ~ "C",
    (indicator_val > quartile_bands[["75%"]]) &
      (indicator_val <= quartile_bands[["100%"]]) ~ "D",
    TRUE ~ "Error"
  )
}


#' Get Cell Colour for Quartile Band Based on Polarity
#'
#' This function retrieves the cell colour associated with a specific quartile
#' band and polarity from a given data frame.
#'
#' @param polarity_colours A data frame that maps quartile bands and
#' polarity to specific cell colours.
#' @param table_stats A data frame containing the current table statistics,
#' including columns for "Polarity" and "Quartile Banding".
#'
#' @return A character vector representing the cell colour corresponding
#' to the given polarity and quartile band.
#'
get_quartile_band_cell_colour <- function(polarity_colours, table_stats) {
  polarity_colours |>
    dplyr::filter(
      polarity == table_stats$Polarity,
      quartile_band == table_stats$`Quartile Banding`
    ) |>
    dplyr::pull(cell_colour)
}


#' Calculate Rank Based on Numeric Values
#'
#' This function calculates the rank of numeric values within a data frame,
#' handling ties and missing values appropriately.
#'
#' @param filtered_data A data frame containing the numeric values to be ranked.
#'
#' @return A data frame with an additional column for the calculated rank.
#'
calculate_rank <- function(filtered_data) {
  filtered_data |>
    dplyr::mutate(
      rank = dplyr::case_when(
        is.na(values_num) ~ NA,
        TRUE ~ rank(values_num, ties.method = "min", na.last = TRUE)
      )
    )
}
