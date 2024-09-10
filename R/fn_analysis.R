#' Calculate Change from Previous Year
#'
#' This function calculates the change in `values_num` from the previous year
#' for each `LA and Regions` group in the provided data.
#' It computes the difference
#' between the `values_num` of the current year and the previous year, ensuring
#' that the difference is only calculated if both the current and previous years
#' are consecutive and neither of the `values_num` values is `NA`. If the years
#' are not consecutive or if either `values_num` is `NA`, the result is `NA`.
#'
#' The result is a data frame where each row corresponds to the second row of
#' each group when sorted in descending order by `Years_num`. The `values_num`
#' column in the result contains the change from the previous year (or `NA`
#' where appropriate), and the `Years` column is set to
#' "Change from previous year".
#'
#' @param data A data frame that contains at least the columns
#' `LA and Regions`, `Years`, `Years_num`, and `values_num`.
#' `LA and Regions` should be of type character or factor,
#' `Years` should be of type character or factor,
#' `Years_num` should be numeric (representing the year),
#' and `values_num` should be numeric (representing the value for that year).
#'
#' @return A data frame with the same columns as the input data frame,
#' but only the second row of each group when sorted in descending o
#' rder by `Years_num`.
#' The `values_num` column contains the change from the previous year, or `NA`
#' if the years are not consecutive or if either `values_num` is `NA`.
#' The `Years` column is set to "Change from previous year".
#' Groups with only one row in the input data or where the year difference
#' is not exactly one will not appear in the output.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   `LA and Regions` = c(
#'     "Region1", "Region1", "Region1",
#'     "Region2", "Region2"
#'   ),
#'   Years = c("2023", "2022", "2021", "2023", "2022"),
#'   Years_num = c(2023, 2022, 2021, 2023, 2022),
#'   values_num = c(100, 90, 80, 200, 195)
#' )
#' calculate_change_from_prev_yr(data)
#' }
#' #
calculate_change_from_prev_yr <- function(data) {
  data |>
    dplyr::group_by(`LA and Regions`) |>
    dplyr::arrange(`LA and Regions`,
      desc(Years_num),
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      values_num = ifelse(
        dplyr::lag(Years_num) - Years_num != 1,
        NA,
        dplyr::lag(values_num) - values_num
      ),
      Years = "Change from previous year"
    ) |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::ungroup() |>
    as.data.frame()
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
  # Check if change_since_prev is empty or has a length greater than one
  if (length(change_since_prev) == 0 || length(change_since_prev) > 1) {
    warning(
      "The change_since_prev value looks wrong: ",
      "length is either 0 or greater than 1."
    )
  }

  dplyr::case_when(
    change_since_prev == 0 ~ "No change",
    change_since_prev > 0 ~ "Increase",
    change_since_prev < 0 ~ "Decrease",
    TRUE ~ NA_character_
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
calculate_quartile_band <- function(indicator_val, quartile_bands, indicator_polarity) {
  # Check if all required quartile bands are present
  required_bands <- c("0%", "25%", "50%", "75%", "100%")
  missing_bands <- setdiff(required_bands, names(quartile_bands))

  # Check for missing quartile bands
  if (length(missing_bands) > 0) {
    warning("Quartile bands are missing: ", paste(missing_bands, collapse = ", "))
    return(rep("Error", length(indicator_val)))
  }

  # Check indicator value is present
  if (length(indicator_val) == 0) {
    warning("Indicator value is empty; returning an empty character vector.")
    return(character(0))
  }

  # Set the Quartile Band (dependent on polarity)
  if (indicator_polarity == "Low") {
    quartile_band <- dplyr::case_when(
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
  } else if (indicator_polarity == "High") {
    quartile_band <- dplyr::case_when(
      is.na(indicator_val) ~ NA_character_,
      (indicator_val >= quartile_bands[["0%"]]) &
        (indicator_val <= quartile_bands[["25%"]]) ~ "D",
      (indicator_val > quartile_bands[["25%"]]) &
        (indicator_val <= quartile_bands[["50%"]]) ~ "C",
      (indicator_val > quartile_bands[["50%"]]) &
        (indicator_val <= quartile_bands[["75%"]]) ~ "B",
      (indicator_val > quartile_bands[["75%"]]) &
        (indicator_val <= quartile_bands[["100%"]]) ~ "A",
      TRUE ~ "Error"
    )
  } else {
    quartile_band <- "Not applicable"
  }

  if (quartile_band %notin% c("A", "B", "C", "D", "Not applicable", NA_character_)) {
    warning("Unexpected Quartile Banding")
  }

  quartile_band
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
  all_polarities <- c("High", "Low", "-", NA)
  valid_polarities <- c("High", "Low")
  all_quartiles <- c("A", "B", "C", "D", "Error", "Not applicable", NA_character_)
  valid_quartiles <- c("A", "B", "C", "D")

  # Check if polarity is not unexpected value
  if (!table_stats$Polarity %in% all_polarities) {
    warning("Unexpected polarity value: ", table_stats$Polarity)
    return(NULL)
  }

  # Check if Quartile band is unexpected value
  if (!table_stats$`Quartile Banding` %in% all_quartiles) {
    warning("Unexpected Quartile Banding value: ", table_stats$`Quartile Banding`)
    return(NULL)
  }

  # Check if Quartile Band is unexpected if polarity is valid
  if (!table_stats$`Quartile Banding` %in% valid_quartiles && table_stats$Polarity %in% valid_polarities) {
    warning("Unexpected Quartile Banding (with valid polarity): ", table_stats$`Quartile Banding`)
    return(NULL)
  }

  # Filter the polarity_colours based on the given conditions
  matching_colour <- polarity_colours |>
    dplyr::filter(
      (is.na(polarity) & is.na(table_stats$Polarity)) |
        (polarity == table_stats$Polarity),
      quartile_band == table_stats$`Quartile Banding`
    ) |>
    dplyr::pull(cell_colour)

  return(matching_colour)
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
