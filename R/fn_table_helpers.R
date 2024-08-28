#' Pivot Indicator Data to Long Format
#'
#' This function pivots indicator data from wide format to long format.
#'
#' @param data_ind A data frame containing the indicator data in wide format.
#' @param indicator_colname A character string representing the name of the indicator column.
#'
#' @return A data frame containing the indicator data in long format.
#' @export
#'
#' @examples
#' data_ind <- data.frame(
#'   `Short Desc` = c("Desc1", "Desc2"),
#'   line = c(1, 2),
#'   `Data item` = c("Item1", "Item2"),
#'   Years = c("2020", "2021"),
#'   `Local Authority 1` = c(10, 20),
#'   `Local Authority 2` = c(30, 40)
#' )
#' indicator_colname <- "IndicatorValue"
#' pivot_indicator_data_long(data_ind, indicator_colname)
pivot_indicator_data_long <- function(data_ind, indicator_colname) {
  data_ind |>
    dplyr::filter(!is.na(as.Date(Years, format = "%Y"))) |>
    tidyr::pivot_longer(
      cols = -c(`Short Desc`, line, `Data item`, Years),
      names_to = "local_authority",
      values_to = indicator_colname
    )
}


#' Create a Table of Statistical Neighbours for an Indicator
#'
#' This function creates a table of statistical neighbours for a given indicator and local authority.
#'
#' @param data_indicator A data frame containing the indicator data.
#' @param data_sn A data frame containing the statistical neighbours data.
#' @param indicator_colname A character string representing the name of the indicator column.
#' @param la A character string representing the name of the local authority. Defaults to "Barking and Dagenham".
#'
#' @return A data frame containing the statistical neighbours table for the specified indicator and local authority.
#' @export
#'
#' @examples
#' data_indicator <- data.frame(
#'   local_authority = c("Barking and Dagenham", "Neighbour1", "Neighbour2"),
#'   Years = c("2020", "2020", "2020"),
#'   IndicatorValue = c(10, 20, 30)
#' )
#' data_sn <- data.frame(
#'   `LA Name` = c("Barking and Dagenham", "Barking and Dagenham"),
#'   `LA Name_sn` = c("Neighbour1", "Neighbour2")
#' )
#' indicator_colname <- "IndicatorValue"
#' create_sn_table(data_indicator, data_sn, indicator_colname)
create_sn_table <- function(data_indicator,
                            data_sn,
                            indicator_colname,
                            la = "Barking and Dagenham") {
  # Selected LA
  selected_la <- la

  # Selected LA statistical neighbours
  df_selected_sns <- data_sn |>
    dplyr::filter(`LA Name` == selected_la)

  # Most recent year data available
  max_year <- data_indicator |>
    dplyr::pull(Years) |>
    max()

  # Indicator stat neigh table
  table_prod <- data_indicator |>
    dplyr::filter(local_authority %in% c(df_selected_sns$`LA Name_sn`, selected_la)) |>
    dplyr::select(-c(`Short Desc`, line, `Data item`)) |>
    dplyr::mutate(indicator = unlist(lapply(
      !!rlang::sym(indicator_colname),
      dfeR::pretty_num,
      dp = 3
    ))) |>
    tidyr::pivot_wider(
      id_cols = local_authority,
      names_from = Years,
      values_from = indicator
    ) |>
    dplyr::rename("Local Authority" = "local_authority") |>
    dplyr::arrange(!!rlang::sym(max_year))

  table_prod
}


#' Filter data based on LA and Regions
#'
#' This function filters a data frame based on the provided
#' Local Authority (LA) and Regions.
#' It can also return a specific column from the filtered data.
#'
#' @param data A data frame that contains the data to be filtered.
#' @param filter_col A character vector specifying the LA and Regions to be
#' included in the filtered data.
#' @param latest A logical value indicating whether to filter the data
#' based on the latest year.
#'               If TRUE, only rows where Years is equal to the maximum
#'               year are included.
#'               If FALSE, all rows are included. Default is FALSE.
#' @param pull_col A character string specifying a column to be returned
#' from the filtered data.
#'                If NA, the entire data frame is returned. Default is NA.
#'
#' @return If pull_col is NA, a data frame containing the filtered data
#' is returned.
#'         If pull_col is not NA, a vector containing the values of the
#'         specified column from the filtered data is returned.
#'
#' @examples
#' \dontrun{
#' filter_la_regions(
#'   data = df, filter_col = c("LA1", "Region1"),
#'   latest = TRUE, pull_col = "Population"
#' )
#' }
#'
filter_la_regions <- function(data, filter_col, latest = FALSE, pull_col = NA) {
  # Filter LA & Regions
  result <- data |>
    dplyr::filter(`LA and Regions` %in% filter_col)

  # Slice max Year if latest is TRUE
  if (latest) {
    result <- result |>
      dplyr::slice_max(Years)
  }

  # Return df or col
  if (!is.na(pull_col)) {
    result <- result |>
      dplyr::pull(pull_col)
  }

  result
}


#' Format Numeric Columns with Pretty Numbers
#'
#' This function formats numeric columns in a data frame using the
#' `dfeR::pretty_num` function.
#' It allows you to specify which columns to include or exclude from formatting.
#'
#' @param data A data frame containing the columns to be formatted.
#' @param include_columns A character vector specifying which columns to
#' include for formatting. If `NULL` (default), all numeric columns
#' will be considered.
#' @param exclude_columns A character vector specifying which columns
#' to exclude from formatting.
#' If `NULL` (default), no columns will be excluded.
#' The `include_columns` parameter takes precedence over `exclude_columns`.
#' @param ... Additional arguments passed to `dfeR::pretty_num`,
#' such as `dp` for decimal places.
#'
#' @return A data frame with the specified numeric columns
#' formatted using `dfeR::pretty_num`.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   a = c(1.234, 5.678, 9.1011),
#'   b = c(10.1112, 20.1314, 30.1516),
#'   c = c("A", "B", "C")
#' )
#'
#' # Apply formatting to all numeric columns
#' pretty_num_table(df, dp = 2)
#'
#' # Apply formatting to selected columns
#' pretty_num_table(df, include_columns = c("a"), dp = 2)
#'
#' # Apply formatting to all numeric columns except specified ones
#' pretty_num_table(df, exclude_columns = c("b"), dp = 2)
#'
pretty_num_table <- function(data,
                             include_columns = NULL,
                             exclude_columns = NULL,
                             ...) {
  # Determine the columns to include or exclude
  if (!is.null(include_columns)) {
    cols_to_include <- include_columns
  } else if (!is.null(exclude_columns)) {
    cols_to_include <- setdiff(names(data)[sapply(data, is.numeric)], exclude_columns)
  } else {
    cols_to_include <- names(data)[sapply(data, is.numeric)]
  }

  # Apply the pretty_num function across the selected columns
  data |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(cols_to_include),
      ~ sapply(., dfeR::pretty_num, ...)
    ))
}


#' DFE Reactable Function
#'
#' This function creates a reactable table from a given data frame.
#' The table has several predefined settings:
#' it has highlighting enabled, no borders,
#' no sort icon, a font size of 16px, and a default column definition
#' with a specific header class and HTML enabled.
#'
#' @param data A data frame.
#' The data frame to be displayed as a reactable table.
#'
#' @return A reactable object representing the input data frame.
#'
dfe_reactable <- function(data, ...) {
  reactable::reactable(
    data,
    highlight = TRUE,
    borderless = TRUE,
    showSortIcon = FALSE,
    style = list(fontSize = "16px"),
    defaultColDef = reactable::colDef(
      headerClass = "bar-sort-header",
      html = TRUE
    ),
    ...
  )
}


#' Create a Statistics Table for Local Authorities and Regions
#'
#' This function creates a data frame that summarizes key statistics for
#' local authorities (LAs) and regions.
#' It includes columns for LA numbers, names, trends, changes, rankings,
#' quartile bandings, and other relevant metrics.
#'
#' @param main_table A data frame containing the main data with information
#' on local authorities.
#' @param selected_la A character vector specifying the selected local
#' authorities and regions to be included in the table.
#' @param trend A character vector representing the trend for each local
#' authority (e.g., "Increase", "Decrease").
#' @param change_since_prev A numeric vector indicating the change from the
#' previous year for each local authority.
#' @param rank A numeric vector representing the national rank of each
#' local authority.
#' @param quartile A character vector indicating the quartile band
#' ("A", "B", "C", or "D") for each local authority.
#' @param quartile_bands A named numeric vector specifying the thresholds for
#' each quartile band.
#' @param indicator_polarity A character vector indicating the polarity of the
#' indicator (e.g., "low", "high").
#'
#' @return A data frame with formatted columns summarizing the statistics
#' for the selected local authorities and regions.
#'
create_stats_table <- function(
    main_table,
    selected_la,
    trend,
    change_since_prev,
    rank,
    quartile,
    quartile_bands,
    indicator_polarity) {
  data.frame(
    "LA Number" = main_table |>
      filter_la_regions(selected_la, pull_col = "LA Number"),
    "LA and Regions" = selected_la,
    "Trend" = trend,
    "Change from previous year" = change_since_prev,
    "Latest National Rank" = rank,
    "Quartile Banding" = quartile,
    "(A) Up to and including" = quartile_bands[["25%"]],
    "(B) Up to and including" = quartile_bands[["50%"]],
    "(C) Up to and including" = quartile_bands[["75%"]],
    "(D) Up to and including" = quartile_bands[["100%"]],
    "Polarity" = indicator_polarity,
    check.names = FALSE
  ) |>
    pretty_num_table(dp = 1)
}


# Creating indicator polarity cell colour dataframe
polarity_colours_df <- function() {
  # Define the possible values for each column
  polarity_options <- c(NA, "-", "Low", "High")
  quartile_band_options <- c("A", "B", "C", "D")
  cell_colour_options <- c("red", "green", "none")

  # Create all combinations of polarity and quartile band
  polarity_colours <- expand.grid(
    polarity = polarity_options,
    quartile_band = quartile_band_options,
    stringsAsFactors = FALSE
  )

  # Initialize cell_colour column with "none"
  polarity_colours$cell_colour <- "none"

  # Apply the conditions to determine the cell colour
  polarity_colours$cell_colour <- with(polarity_colours, ifelse(
    (is.na(polarity) | polarity == "-") | (quartile_band == "B" | quartile_band == "C"),
    "none", ifelse(
      (quartile_band == "A" & polarity == "Low"),
      "green", ifelse(
        (quartile_band == "D" & polarity == "Low"),
        "red", ifelse(
          (quartile_band == "A" & polarity == "High"),
          "red", ifelse(
            (quartile_band == "D" & polarity == "High"),
            "green",
            "none"
          )
        )
      )
    )
  ))

  polarity_colours
}
