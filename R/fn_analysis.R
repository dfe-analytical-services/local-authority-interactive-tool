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
      Years = "Change from previous year",
      Values = NA_character_,
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
calculate_quartile_band <- function(indicator_val,
                                    quartile_bands,
                                    indicator_polarity,
                                    no_show_qb = FALSE) {
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
  if (no_show_qb) {
    quartile_band <- "-"
  } else if (indicator_polarity %in% "Low") {
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
    quartile_band <- "-"
  }

  if (quartile_band %notin% c("A", "B", "C", "D", "-", NA_character_)) {
    warning("Unexpected Quartile Banding")
  }

  # Clean NA values based on polarity
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
  all_quartiles <- c("A", "B", "C", "D", "Error", "-", "", NA_character_)
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
    quartile_band == "A" & polarity %in% valid_polarities ~ get_gov_green(),
    quartile_band == "D" & polarity %in% valid_polarities ~ get_gov_red(),
    TRUE ~ "none"
  )

  matching_colour
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
  # Check if filtered_data is empty and emit a warning
  if (nrow(filtered_data) == 0) {
    warning("The filtered data frame is empty; returning an empty result.")
    return(filtered_data |> dplyr::mutate(rank = numeric(0)))
  }

  # Proceed with ranking if data is not empty
  filtered_data |>
    dplyr::mutate(
      rank = dplyr::case_when(
        indicator_polarity %notin% c("High", "Low") ~ "-",
        indicator_polarity %in% c("High", "Low") & is.na(values_num) ~ NA,
        # Rank in descending order
        indicator_polarity == "High" ~ as.character(rank(-values_num, ties.method = "min", na.last = TRUE)),
        # Rank in ascending order
        indicator_polarity == "Low" ~ as.character(rank(values_num, ties.method = "min", na.last = TRUE))
      )
    )
}


#' Filter All LA Data Based on Local Authorities
#'
#' This function filters the All LA data to retain only the rows where the
#' `LA and Regions` column matches any of the provided `la_names`. The
#' resulting data is then arranged in alphabetical order based on
#' `LA and Regions`.
#'
#' @param data A data frame containing a column named `LA and Regions` to filter.
#' @param la_names A character vector of local authority or region names to
#'   filter the data by.
#'
#' @return A data frame filtered to include only rows where the
#'   `LA and Regions` column matches the names in `la_names`. The output
#'   is arranged alphabetically by `LA and Regions`.
#'
#' @importFrom dplyr filter arrange
#'
#' @examples
#' # Assuming `df` is your data frame and `la_names_vec` is a vector of LA names
#' filtered_data <- filter_la_data_all_la(df, la_names_vec)
#'
filter_la_data_all_la <- function(data, la_names, geog_colname) {
  data |>
    dplyr::filter(`LA and Regions` %in% la_names) |>
    dplyr::arrange(`LA and Regions`) |>
    dplyr::rename("LA" = `LA and Regions`)
}


#' Filter All LA Data Based on Regions
#'
#' This function filters the All LA data to exclude rows where the
#' `LA and Regions` column matches any of the provided `la_names`. It also
#' removes rows where the `LA and Regions` value is either "London (Inner)" or
#' "London (Outer)" and all remaining columns (except `LA Number` and
#' `LA and Regions`) contain only `NA` values. After filtering, the function
#' adds a blank `Rank` column and arranges the data by `LA Number`.
#'
#' @param data A data frame containing columns `LA Number` and `LA and Regions`.
#' @param la_names A character vector of local authority names to exclude from
#'   the data.
#'
#' @return A data frame filtered to exclude rows matching the specified
#'   local authority names and rows with "London (Inner)" or "London (Outer)"
#'   where the remaining columns contain only `NA` values. The output includes
#'   a blank `Rank` column and is arranged by `LA Number`.
#'
#' @importFrom dplyr filter mutate arrange select
#'
#' @examples
#' # Assuming `df` is your data frame and `la_names_vec` is a vector of LA names
#' filtered_region_data <- filter_region_data_all_la(df, la_names_vec)
#'
filter_region_data_all_la <- function(data, la_names) {
  data |>
    dplyr::filter(
      `LA and Regions` %notin% la_names,
      !(`LA and Regions` %in% c("London (Inner)", "London (Outer)") &
        rowSums(!is.na(dplyr::select(
          data, -c(`LA Number`, `LA and Regions`)
        ))) == 0)
    ) |>
    dplyr::mutate(Rank = "") |>
    dplyr::arrange(`LA Number`) |>
    dplyr::rename("Region" = `LA and Regions`)
}


#' Filter BDS for Selected Indicators
#'
#' This function filters a given BDS dataset to include only the rows that
#' match the specified indicators. It removes any rows where the year value
#' is missing (NA). The filtered dataset can be used for further analysis
#' or visualisation of selected indicators.
#'
#' @param bds_data A data frame containing BDS data, including a column
#'                 for indicators (`Measure`) and years (`Years`).
#' @param selected_indicators A character vector of indicator names that
#'                            should be retained in the filtered dataset.
#' @return A data frame containing only the rows from `bds_data` that
#'         correspond to the specified indicators and have non-missing
#'         year values.

filter_bds_for_indicators <- function(bds_data, selected_indicators) {
  bds_data |>
    dplyr::filter(
      Measure %in% selected_indicators
    )
}


#' Get Local Authorities in Selected Regions
#'
#' This function filters a dataset of geographical data to return unique
#' local authority names based on the selected regions. It is useful for
#' narrowing down local authorities relevant to specific regions.
#'
#' @param data_geog A data frame containing geographical data, including
#'                  local authority names and corresponding region codes.
#' @param selected_regions A vector of region identifiers to filter the
#'                         local authorities. Only those local authorities
#'                         that belong to the selected regions will be
#'                         returned.
#' @return A vector of unique local authority names that are part of the
#'         specified regions.
#'
get_las_in_regions <- function(data_geog, selected_regions) {
  data_geog |>
    dplyr::filter(GOReg %in% selected_regions) |>
    pull_uniques("LA Name")
}


#' Get Regions for Selected Local Authorities
#'
#' This function filters a dataset of geographical data to return unique
#' region identifiers associated with selected local authorities. It helps
#' in identifying the regions relevant to a specified set of local
#' authorities.
#'
#' @param data_geog A data frame containing geographical data, including
#'                  local authority names and their corresponding region
#'                  codes.
#' @param selected_las A vector of local authority names to filter the
#'                      regions. Only the regions associated with the
#'                      specified local authorities will be returned.
#' @return A vector of unique region identifiers that correspond to the
#'         selected local authorities.
#'
get_la_region <- function(data_geog, selected_las) {
  data_geog |>
    dplyr::filter(`LA Name` %in% selected_las) |>
    pull_uniques("GOReg")
}


#' Get Statistical Neighbors for Selected Local Authorities
#'
#' This function retrieves the statistical neighbors for a given set of
#' local authorities from a dataset containing statistical neighbor
#' information. It filters the dataset to include only the selected local
#' authorities and returns their associated statistical neighbors.
#'
#' @param data_stat_n A data frame containing statistical neighbor data,
#'                     which includes local authority names and their
#'                     corresponding statistical neighbor identifiers.
#' @param selected_las A vector of local authority names for which
#'                      statistical neighbors should be retrieved. The
#'                      function will return neighbors associated with
#'                      these local authorities.
#' @return A vector of unique statistical neighbor identifiers linked to
#'         the specified local authorities.
#'
get_la_stat_neighbrs <- function(data_stat_n, selected_las) {
  data_stat_n |>
    dplyr::filter(`LA Name` %in% selected_las) |>
    pull_uniques("LA Name_sn")
}



#' Get Distinct and Separated Unique Values from a Data Frame Column
#'
#' This helper function retrieves distinct values from a specified column
#' in a data frame, separates them into individual rows if they are
#' concatenated, and trims any whitespace. This is particularly useful
#' for extracting and formatting unique entries for dropdowns or filters
#' in a Shiny application.
#'
#' @param data A data frame from which unique values will be extracted.
#' @param column The name of the column from which to retrieve unique
#'               values. The column can contain concatenated entries
#'               that will be separated into individual values.
#' @return A vector of unique values from the specified column, with any
#'         whitespace trimmed and values separated into individual entries.
#'
get_query_table_values <- function(data, column) {
  data |>
    dplyr::distinct({{ column }}) |>
    tidyr::separate_rows({{ column }}, sep = ",<br>") |>
    dplyr::mutate({{ column }} := trimws({{ column }})) |>
    pull_uniques(as.character(substitute(column)))
}


#' Filter data based on topic selection
#'
#' @param data A data frame or tibble to filter.
#' @param topic_column The name of the column containing topic values (as a string).
#' @param selected_topics A vector of selected topics from the user input.
#' @return A filtered data frame or tibble based on the topic selection.
filter_by_topic <- function(data, topic_column, selected_topics) {
  # Check if selected topics are all selected or empty (return whole df if so)
  if (is.null(selected_topics) || any(selected_topics %in% c("All Topics", ""))) {
    # Return data ordered alphabetically by "Measure", with letters first
    alphabet_ordered <- data |>
      order_alphabetically(.data$Measure)
    return(alphabet_ordered)
  }

  # Filter by selected topic
  dplyr::filter(data, .data[[topic_column]] %in% selected_topics)
}
