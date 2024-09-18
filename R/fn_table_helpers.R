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
  if (nrow(data) < 1) {
    warning("Dataframe seems empty")
  }

  # Filter LA & Regions
  result <- data |>
    dplyr::filter(`LA and Regions` %in% filter_col)

  if (nrow(result) < 1) {
    warning("Filter value doesn't exist in LA and Regions")
  }

  # Slice max Year if latest is TRUE
  if (latest) {
    result <- result |>
      dplyr::slice_max(Years, na_rm = TRUE)
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
  if (nrow(data) < 1) {
    warning("Data seems to be empty")
  }

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
      ~ sapply(., pretty_num, ...)
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
  # Generate the reactable
  reactable::reactable(
    data,
    highlight = TRUE,
    borderless = TRUE,
    showSortIcon = FALSE,
    style = list(fontSize = "16px"),
    defaultColDef = reactable::colDef(
      headerClass = "bar-sort-header",
      html = TRUE,
      na = "NA"
    ),
    ...
  )
}


# Create column alignment definitions
align_reactable_cols <- function(data, exclude = NULL) {
  # Right-align columns that contain numeric values or are all NA, unless excluded
  column_defs <- lapply(names(data), function(col) {
    # Check if the column contains numeric values (using regex)
    contains_numeric <- any(grepl("[0-9]", as.character(data[[col]])))

    # Check if all values in the column are NA
    all_na <- all(is.na(data[[col]]))

    # Exclude columns that are explicitly mentioned in the `exclude` argument
    if ((contains_numeric || all_na) && !(col %in% exclude)) {
      # Right-align columns that contain numbers or are all NA
      reactable::colDef(
        align = "right",
        headerClass = "bar-sort-header",
        html = TRUE,
        na = "NA"
      )
    } else {
      # Default left-alignment for other columns
      reactable::colDef(
        align = "left",
        headerClass = "bar-sort-header",
        html = TRUE,
        na = "NA"
      )
    }
  })

  # Create a named list for the colDef argument
  names(column_defs) <- names(data)

  column_defs
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
  la_number <- main_table |>
    filter_la_regions(selected_la, pull_col = "LA Number")

  if (any(is.na(c(selected_la, la_number)))) {
    warning("Suprise NA value in stats table")
  }

  # Create the ranking and Quartile Banding based on polarity
  rank_quartile_band_values <- if (indicator_polarity %in% "Low") {
    list(
      "Latest National Rank" = rank,
      "Quartile Banding" = quartile,
      "(A) Up to and including" = quartile_bands[["25%"]],
      "(B) Up to and including" = quartile_bands[["50%"]],
      "(C) Up to and including" = quartile_bands[["75%"]],
      "(D) Up to and including" = quartile_bands[["100%"]]
    )
  } else if (indicator_polarity %in% "High") {
    list(
      "Latest National Rank" = rank,
      "Quartile Banding" = quartile,
      "(D) Up to and including" = quartile_bands[["25%"]],
      "(C) Up to and including" = quartile_bands[["50%"]],
      "(B) Up to and including" = quartile_bands[["75%"]],
      "(A) Up to and including" = quartile_bands[["100%"]]
    )
  } else {
    list(
      "Latest National Rank" = "Not applicable",
      "Quartile Banding" = "Not applicable",
      "(A) Up to and including" = "-",
      "(B) Up to and including" = "-",
      "(C) Up to and including" = "-",
      "(D) Up to and including" = "-"
    )
  }

  stats_table <- data.frame(
    "LA Number" = la_number,
    "LA and Regions" = selected_la,
    "Trend" = trend,
    "Change from previous year" = change_since_prev,
    "Polarity" = indicator_polarity,
    check.names = FALSE
  ) |>
    cbind(rank_quartile_band_values)

  stats_table
}


#' Highlight a selected row in a reactable
#'
#' This function applies a specific style to a row if the value in the
#' "LA and Regions" column matches the selected area.
#'
#' @param index The index of the row (automatically passed by `reactable`).
#' @param data A data frame containing the table data.
#' @param selected_area The specific area to highlight in the
#' "LA and Regions" column.
#'
#' @return A list of CSS styles to apply to the row (if the condition matches),
#' otherwise NULL.
#'
#' @examples
#' \dontrun{
#' # Use in a reactable table
#' dfe_reactable(
#'   region_la_table,
#'   rowStyle = function(index) {
#'     highlight_selected_row(index, region_la_table, selected_area)
#'   }
#' )
#' }
highlight_selected_row <- function(index, data, selected_area) {
  if (data[index, "LA and Regions"] == selected_area) {
    list(
      color = "#6BACE6",
      fontWeight = "bold"
    )
  }
}



get_indicator_dps <- function(data_full) {
  data_full |>
    pull_uniques("dps") |>
    as.numeric()
}
