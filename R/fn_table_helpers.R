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
      ~ sapply(., dfeR::pretty_num, ...)
    ))
}


#' Create a Reactable Table
#'
#' This function generates a customisable reactable table from the provided
#' data. It enhances the display with features such as highlighting,
#' borderless design, and adjustable font size.
#'
#' The function applies default column definitions, including styling for
#' headers and handling of missing values. Additional parameters can be
#' passed for further customisation of the reactable.
#'
#' @param data A data frame or tibble that contains the data to be displayed
#'   in the reactable table.
#' @param ... Additional arguments to customise the reactable table, such as
#'   column definitions, styling options, or any other reactable parameters.
#'
#' @return A reactable object that can be rendered in a Shiny application or
#'   a static HTML document.
#'
#' @examples
#' # Example usage:
#' dfe_reactable(data)
#'
dfe_reactable <- function(data, ...) {
  # Generate the reactable
  reactable::reactable(
    data,
    highlight = TRUE,
    borderless = TRUE,
    showSortIcon = FALSE,
    resizable = TRUE,
    fullWidth = TRUE,
    defaultColDef = reactable::colDef(
      headerClass = "bar-sort-header",
      html = TRUE,
      na = "NA",
      minWidth = 65,
      align = "left"
    ),
    ...
  )
}




#' Check if a column contains numeric or NA values
#'
#' This function checks whether a column contains numeric values, is entirely
#' composed of `NA` values, or includes string representations of missing
#' numeric values (e.g., a hyphen `"-"`). It returns `TRUE` if any of these
#' conditions are met, and `FALSE` otherwise.
#' Mainly for aligning cols in reactable()
#'
#' @param col_data A vector or column of data to check.
#'
#' @return A logical value indicating whether the column contains numeric
#' values, all `NA` values, or string representations of missing numeric values.
#'
#' @examples
#' is_numeric_or_na(c(1, 2, NA))
#' is_numeric_or_na(c("-", "-", "-"))
#' is_numeric_or_na(c("text", "text"))
is_numeric_or_na <- function(col_data) {
  contains_numeric <- any(grepl("[0-9]", as.character(col_data)))
  all_na <- all(is.na(col_data))
  str_na_num_col <- any(grepl("^(-|Not applicable)$", as.character(col_data)))
  contains_numeric || all_na || str_na_num_col
}


# Helper function to format numeric columns
format_reactable_num_col <- function(col, indicator_dps) {
  reactable::colDef(
    align = "right",
    headerClass = "bar-sort-header",
    html = TRUE,
    na = "NA",
    sortable = TRUE,
    sortNALast = TRUE,
    cell = function(value) {
      dfeR::pretty_num(value, dp = indicator_dps)
    }
  )
}

# Helper function to format categorical columns
format_reactable_cat_col <- function() {
  reactable::colDef(
    align = "right",
    headerClass = "bar-sort-header",
    html = TRUE,
    na = "NA",
    sortable = TRUE,
    sortNALast = TRUE
  )
}

# Helper function to set minimum column widths
set_custom_default_col_widths <- function(...) {
  list(
    `LA Number` = set_min_col_width(80),
    `LA and Regions` = set_min_col_width(100),
    `Change from previous year` = set_min_col_width(90),
    ...
  )
}


# Main function to format numeric and categorical columns
format_num_reactable_cols <- function(data, indicator_dps, num_exclude = NULL, categorical = NULL) {
  formatted_cols <- lapply(names(data), function(col) {
    col_data <- data[[col]]
    if (is_numeric_or_na(col_data) && (col %notin% c(num_exclude, categorical))) {
      # Format numeric columns
      format_reactable_num_col(col, indicator_dps)
    } else if (col %in% categorical) {
      # Format categorical columns
      format_reactable_cat_col()
    }
  }) |>
    setNames(names(data))

  formatted_cols
}



#' Create a Statistics Table
#'
#' This function generates a statistics table for a specified local authority
#' (LA), including relevant metrics such as trends, changes from the previous
#' year, rankings, and quartile banding based on the indicator's polarity.
#'
#' The function filters the main data table for the selected LA and creates
#' a summary table that combines key statistics with appropriate ranking and
#' quartile banding values. It handles cases where the indicator polarity
#' affects how quartiles are assigned.
#'
#' @param main_table A data frame containing the main data used for filtering
#'   and calculating statistics.
#' @param selected_la A character string representing the selected local
#'   authority for which to generate the statistics table.
#' @param trend A numeric value or character string indicating the trend of the
#'   selected authority's indicator.
#' @param change_since_prev A numeric value representing the change in the
#'   indicator from the previous year.
#' @param rank A numeric value representing the latest national rank of the
#'   selected local authority.
#' @param quartile A character string indicating the quartile of the selected
#'   authority based on the indicator.
#' @param quartile_bands A named list or vector containing the quartile
#'   boundaries for the indicator.
#' @param indicator_polarity A character string indicating the polarity of the
#'   indicator (e.g., "Low" or "High").
#'
#' @return A data frame summarising the statistics for the selected local
#'   authority, including the LA number, name, trend, change from the previous
#'   year, polarity, ranking, and quartile banding.
#'
#' @examples
#' # Example usage:
#' stats_table <- build_la_stats_table(
#'   main_table, "Barking and Dagenham",
#'   trend, change_since_prev, rank,
#'   quartile, quartile_bands, "Low"
#' )
#'
build_la_stats_table <- function(
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


#' Build a formatted statistics table for regions
#'
#' This function creates a data frame containing statistics for local
#' authorities (LAs) and regions, and formats it using the `pretty_num_table`
#' function.
#'
#' @param region_stats_la_num A vector of Local Authority numbers.
#' @param region_stats_name A vector of names corresponding to the
#' Local Authorities and regions.
#' @param region_trend A vector indicating the trend for each Local Authority.
#' @param region_stats_change A vector showing the change from the previous
#' year for each Local Authority.
#' @param filtered_bds A data frame used to determine the decimal places for
#' formatting.
#'
#' @return A formatted statistics table with specific columns and pretty
#' number formatting.
#'
#' @examples
#' build_region_stats_table(
#'   c("LA1", "LA2"), c("Region A", "Region B"),
#'   c("Up", "Down"), c(10, -5), some_filtered_data
#' )
#'
build_region_stats_table <- function(la_number,
                                     area_name,
                                     trend,
                                     change_since_prev,
                                     filtered_bds) {
  data.frame(
    "LA Number" = la_number,
    "LA and Regions" = area_name,
    "Trend" = trend,
    "Change from previous year" = change_since_prev,
    check.names = FALSE
  ) |>
    pretty_num_table(
      dp = get_indicator_dps(filtered_bds),
      exclude_columns = c("LA Number", "Trend")
    )
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


#' Extract Unique Indicator Data Points
#'
#' This function retrieves unique data points for the specified indicator
#' from the provided dataset. It specifically pulls the "dps" column,
#' ensuring that the values are returned as numeric.
#'
#' The function utilises a helper function to extract unique values from
#' the specified column in the dataset, converting them to numeric for
#' further analysis.
#'
#' @param data_full A data frame or tibble containing the full dataset
#'   from which the unique indicator data points are to be extracted.
#'
#' @return A numeric vector of unique data points from the "dps" column of
#'   the dataset.
#'
#' @examples
#' # Example usage:
#' dps_values <- get_indicator_dps(data_full)
#'
get_indicator_dps <- function(data_full) {
  data_full |>
    pull_uniques("dps") |>
    as.numeric()
}


#' Render Trend Icons Based on Value
#'
#' This function determines and renders a trend icon based on the given
#' numeric value. It categorises the value into three trends: upward,
#' downward, or stable, and returns the corresponding icon for use in
#' Shiny applications.
#'
#' The function handles NA values appropriately, returning an NA for any
#' missing data. Positive values are represented with an upward arrow,
#' negative values with a downward arrow, and a value of zero with
#' horizontal arrows indicating stability.
#'
#' @param value A numeric value that indicates the trend to be rendered.
#'   This value can be positive, negative, zero, or NA.
#'
#' @return A Shiny icon object corresponding to the trend represented by
#'   the input value, or NA if the value is missing.
#'
#' @examples
#' # Example usage:
#' icon <- trend_icon_renderer(5) # Returns an upward arrow icon
#'
trend_icon_renderer <- function(value) {
  trend_icon <- dplyr::case_when(
    is.na(value) ~ NA,
    value > 0 ~ "arrow-up",
    value < 0 ~ "arrow-down",
    value == 0 ~ "arrows-left-right",
    TRUE ~ NA
  )

  # Render output
  if (is.na(trend_icon)) {
    NA
  } else {
    shiny::icon(trend_icon)
  }
}


#' Define Quartile Banding Column with Background Color
#'
#' This function creates a column definition for a reactable table that
#' applies background colors to cells based on their quartile banding.
#' It utilises the `reactablefmtr` package to style cells according to the
#' specified quartile band and polarity of the data.
#'
#' The function calls another helper function to determine the appropriate
#' background color for each cell based on the values in the `Polarity`
#' and `Quartile Banding` columns of the input data.
#'
#' @param data A data frame or tibble containing at least two columns:
#'   `Polarity` and `Quartile Banding`, which are used to determine the
#'   background color for each cell in the quartile banding column.
#'
#' @return A `colDef` object from the `reactablefmtr` package that defines
#'   the styling for the quartile banding column, including the background
#'   color settings.
#'
#' @examples
#' # Example usage:
#' col_def <- quartile_banding_col_def(data)
#' reactable::reactable(data, defaultColDef = col_def)
#'
quartile_banding_col_def <- function(data) {
  # Return the colDef object with the background color applied
  reactablefmtr::cell_style(
    data = data,
    background_color = get_quartile_band_cell_colour(
      data$Polarity,
      data$`Quartile Banding`
    )
  )
}


#' Sets a minimum column width for Reactable tables to prevent text wrapping.
#'
#' @param min_width A numeric value specifying the minimum width of the column
#'   in pixels. Default is set to 60 pixels.
#'
#' @return A `colDef` object from the `reactable` package that applies the
#'   specified minimum width to a column. This helps maintain a consistent
#'   appearance in tables and prevents cell content from wrapping onto
#'   additional lines.
#'
#' @examples
#' # Example usage in a Reactable table definition
#' reactable::reactable(
#'   data = my_data,
#'   columns = list(
#'     column1 = set_min_col_width(80),
#'     column2 = set_min_col_width(100)
#'   )
#' )
#'
set_min_col_width <- function(min_width = 60) {
  reactable::colDef(
    minWidth = min_width,
  )
}



# Updated function to handle an entire column for sorting
sort_by_numeric_value <- function(df, column_name) {
  # Helper function to convert string to numeric
  convert_to_numeric <- function(x) {
    x <- str_replace_all(x, ",", "") # Remove commas
    if (str_detect(x, "billion")) {
      as.numeric(str_replace(x, " billion", "")) * 1e9
    } else if (str_detect(x, "million")) {
      as.numeric(str_replace(x, " million", "")) * 1e6
    } else {
      as.numeric(x)
    }
  }

  # Apply the conversion function using sapply and sort the data frame
  df %>%
    mutate(numeric_value = sapply(.data[[column_name]], convert_to_numeric)) %>% # Apply to the specified column
    arrange(numeric_value) %>% # Sort by the numeric column
    select(-numeric_value) # Optionally remove the numeric column
}


# Helper function to convert strings with numbers and units (e.g., million, billion) to numeric
convert_to_numeric <- function(x) {
  x <- stringr::str_replace_all(x, ",", "") # Remove commas

  if (stringr::str_detect(x, "billion")) {
    as.numeric(stringr::str_replace(x, " billion", "")) * 1e9 # Convert billion
  } else if (stringr::str_detect(x, "million")) {
    as.numeric(stringr::str_replace(x, " million", "")) * 1e6 # Convert million
  } else {
    as.numeric(x) # Convert plain numbers
  }
}

# Function to sort a vector of mixed numeric and labeled values
sort_numeric_vector <- function(vec) {
  # Convert the vector to numeric values using sapply
  numeric_vec <- sapply(vec, convert_to_numeric)

  # Sort the original vector based on the numeric values
  sorted_indices <- order(numeric_vec, na.last = TRUE) # Get indices for sorting
  sorted_vec <- vec[sorted_indices] # Sort original vector based on indices

  sorted_vec # Return the sorted vector
}
