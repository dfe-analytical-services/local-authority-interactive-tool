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
  if (any(is.na(data$Years_num))) {
    warning(
      paste0(
        "Missing year found in the change from previous year ",
        "calculation, may cause incorrect value for indicator: "
      )
    )
  }

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
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::ungroup() |>
    as.data.frame()
}


#' Calculate Quartile Bands for Indicator Values
#'
#' This function calculates the quartile band for a given set of indicator
#' values based on specified quartile bands and the polarity of the indicator.
#' The function supports both "Low" and "High" polarity, which affects how the
#' bands are assigned.
#'
#' @param indicator_val A numeric vector of indicator values to be
#' categorised into quartile bands.
#' @param quartile_bands A named vector or list containing quartile bands with
#' names "0%", "25%", "50%", "75%", and "100%".
#' @param indicator_polarity A string indicating the polarity of the indicator.
#' Should be either "Low" or "High".
#'
#' @return A character vector with quartile bands assigned ("A", "B", "C", "D")
#' based on the indicator values,
#' or "Error" if the input is invalid or missing quartile bands.
#' If the indicator value is NA, it returns NA_character_.
#' If the indicator value is empty, it returns an empty character vector.
#'
#' @examples
#' # Example usage
#' calculate_quartile_band(
#'   c(5, 10, 15),
#'   c(
#'     "0%" = 0, "25%" = 10, "50%" = 20,
#'     "75%" = 30, "100%" = 40
#'   ),
#'   "Low"
#' )
#' calculate_quartile_band(
#'   c(5, 10, 15),
#'   c(
#'     "0%" = 0, "25%" = 10, "50%" = 20,
#'     "75%" = 30, "100%" = 40
#'   ),
#'   "High"
#' )
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
  if (indicator_polarity %in% "Low") {
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
  } else if (indicator_polarity %in% "High") {
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
get_quartile_band_cell_colour <- function(data_polarity, data_quartile_band) {
  all_polarities <- c("High", "Low", "-", NA)
  valid_polarities <- c("High", "Low")
  all_quartiles <- c("A", "B", "C", "D", "Error", "Not applicable", NA_character_)
  valid_quartiles <- c("A", "B", "C", "D")

  polarity <- data_polarity[1]
  quartile_band <- data_quartile_band[1]

  # Check if polarity is not unexpected value
  if (!polarity %in% all_polarities) {
    warning("Unexpected polarity value: ", polarity)
    return(NULL)
  }

  # Check if Quartile band is unexpected value
  if (!quartile_band %in% all_quartiles) {
    warning("Unexpected Quartile Banding value: ", quartile_band)
    return(NULL)
  }

  # Set cell colour based on Quartile Banding
  matching_colour <- dplyr::case_when(
    quartile_band == "A" & polarity %in% valid_polarities ~ "#00703c",
    quartile_band == "D" & polarity %in% valid_polarities ~ "#d4351c",
    TRUE ~ "none"
  )

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
calculate_rank <- function(filtered_data, indicator_polarity) {
  filtered_data |>
    dplyr::mutate(
      rank = dplyr::case_when(
        is.na(values_num) ~ NA,
        # Rank in descending order
        indicator_polarity == "High" ~ rank(-values_num, ties.method = "min", na.last = TRUE),
        # Rank in ascending order
        indicator_polarity == "Low" ~ rank(values_num, ties.method = "min", na.last = TRUE)
      )
    )
}
