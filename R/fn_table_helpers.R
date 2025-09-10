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

  # Filter LA & Regions (order by filter_col order)
  result <- data |>
    dplyr::filter(`LA and Regions` %in% filter_col) |>
    dplyr::arrange(factor(`LA and Regions`, levels = filter_col))

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


#' Determine Decimal Places for Large Numeric Values
#'
#' This helper function calculates the appropriate number of decimal places
#' based on the value's magnitude. Values smaller than 1 million use the
#' supplied default decimal places. For values over 1 million or 1 billion,
#' decimal places are conditionally applied if the value normalised by a
#' million or billion is not divisible by 10.
#'
#' @param value A single numeric value.
#' @param dp Integer. The default number of decimal places for values
#'   over 1 million or 1 billion.
#'
#' @return An integer indicating the number of decimal places to use.
#' @examples
#' determine_decimal_places(999, dp = 2) # Returns 2
#' determine_decimal_places(1234567, dp = 3) # Returns 3
#' determine_decimal_places(10000000, dp = 2) # Returns 0
#' determine_decimal_places(5000000000, dp = 3) # Returns 3
#' @export
determine_decimal_places <- function(value, dp = 0) {
  if (is.na(value)) {
    return(dp)
  } else if (abs(value) >= 1e9) {
    # For values over 1 billion, check divisibility by 10 after dividing by 1 billion
    if ((value / 1e9) %% 10 != 0) {
      return(3)
    } else {
      return(0)
    }
  } else if (abs(value) >= 1e6) {
    # For values between 1 million and 1 billion,
    # check divisibility by 10 after dividing by 1 million
    if ((value / 1e6) %% 10 != 0) {
      return(3)
    } else {
      return(0)
    }
  } else {
    # For values less than 1 million, use the default decimal places
    return(dp)
  }
}


#' Format Large Numeric Values with Conditional Decimal Places
#'
#' This function formats numeric values, applying specific rules for values
#' greater than 1 million or 1 billion. Numbers smaller than 1 million use
#' the user-supplied default decimal places. Decimal places for larger values
#' are applied only if the value normalised by a million or billion is not
#' divisible by 10.
#'
#' @param x A numeric vector to be formatted.
#' @param dp Integer. The default number of decimal places for values
#'   over 1 million or 1 billion. Default is 3.
#' @param ... Additional arguments passed to `dfeR::pretty_num`.
#'
#' @return A character vector with formatted numeric values.
#' @examples
#' pretty_num_large(c(999, 1000000, 1234567), dp = 2)
#' pretty_num_large(c(5000000000, 9876543210), dp = 3)
#' @export
pretty_num_large <- function(x, dp = 0, ...) {
  # Determine decimal places for each value
  decimal_places <- sapply(x, determine_decimal_places, dp = dp)

  # Format the numbers using dfeR::pretty_num
  dfeR::pretty_num(x, dp = decimal_places, ...)
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
  # Check if data is empty
  if (nrow(data) < 1) {
    warning("Data seems to be empty. Returning unmodified.")
    return(data)
  }

  # Determine numeric columns to process
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  cols_to_include <- if (!is.null(include_columns)) {
    include_columns
  } else if (!is.null(exclude_columns)) {
    setdiff(numeric_cols, exclude_columns)
  } else {
    numeric_cols
  }

  # Apply formatting to selected columns
  data <- data |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(cols_to_include),
      ~ sapply(., pretty_num_large, ...)
    ))

  data
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
    language = reactable::reactableLang(
      searchPlaceholder = "Search table..."
    ),
    theme = reactable::reactableTheme(
      searchInputStyle = list(
        float = "right",
        width = "25%",
        marginBottom = "10px",
        padding = "5px",
        fontSize = "14px",
        border = "1px solid #ccc",
        borderRadius = "5px"
      )
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
  contains_numeric || all_na
}


#' Format Numeric Column for Reactable
#'
#' A helper function to format numeric columns in a `reactable` table with
#' custom alignment, headers, NA handling, and sorting.
#'
#' @param col The column to format.
#' @param indicator_dps Integer. The number of decimal places for formatting.
#'
#' @return A `reactable::colDef` object with customised numeric formatting.
#'
#' @details Formats numeric columns to align right, sets NA as "NA", and applies
#'   custom decimal precision based on `indicator_dps`. Values are sorted with
#'   NAs appearing last.
#'
#' @examples
#' format_reactable_num_col(my_column, indicator_dps = 2)
#'
format_reactable_num_col <- function(col, indicator_dps) {
  reactable::colDef(
    align = "right",
    headerClass = "bar-sort-header",
    html = TRUE,
    na = "NA",
    sortable = TRUE,
    sortNALast = TRUE,
    cell = function(value) {
      ifelse(
        is.nan(value),
        "",
        pretty_num_large(value, dp = indicator_dps)
      )
    }
  )
}


#' Format Categorical Column for Reactable
#'
#' A helper function to format categorical columns in a `reactable` table with
#' custom alignment, headers, and NA handling.
#'
#' @return A `reactable::colDef` object with customised categorical formatting.
#'
#' @details Formats categorical columns to align right, sets NA as "NA", and
#'   sorts values with NAs appearing last.
#'
#' @examples
#' format_reactable_cat_col()
#'
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


#' Set Minimum Column Widths for Reactable Columns
#'
#' A helper function to set minimum column widths for specified columns in a
#' `reactable` table.
#'
#' @param ... Additional column width settings passed as a list.
#' @return A list of column definitions with minimum width settings applied.
#'
#' @details This function applies minimum widths to specific columns to ensure
#'   consistent table layout. Additional column definitions can be specified
#'   via `...`.
#'
#' @examples
#' set_custom_default_col_widths()
#'
set_custom_default_col_widths <- function(...) {
  list(
    `LA Number` = set_min_col_width(80),
    `LA and Regions` = set_min_col_width(100),
    `Change from previous year` = set_min_col_width(90),
    ...
  )
}


#' Format Numeric and Categorical Columns for Reactable
#'
#' A main function to apply specific formatting to numeric and categorical
#' columns within a `reactable` table.
#'
#' @param data A dataframe containing the data to be displayed in the table.
#' @param indicator_dps Integer specifying the decimal places for numeric
#'   formatting.
#' @param num_exclude Optional; character vector of column names to exclude
#'   from numeric formatting.
#' @param categorical Optional; character vector of column names to format
#'   as categorical columns.
#' @return A named list of column definitions with the appropriate formatting
#'   applied for each column in the table.
#'
#' @details This function applies specific formatting to numeric and
#'   categorical columns in a `reactable` table based on the data type and
#'   column names. Numeric columns not in `num_exclude` or `categorical` are
#'   formatted using `format_reactable_num_col`, while categorical columns are
#'   formatted with `format_reactable_cat_col`.
#'
#' @examples
#' format_num_reactable_cols(data,
#'   indicator_dps = 2, num_exclude = "ID",
#'   categorical = c("Category")
#' )
#'
format_num_reactable_cols <- function(data,
                                      indicator_dps,
                                      num_exclude = NULL,
                                      categorical = NULL) {
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
    indicator_dps,
    indicator_polarity,
    no_show_qb) {
  # Get LA number
  la_number <- main_table |>
    filter_la_regions(selected_la, pull_col = "LA Number")

  n_rows <- max(length(selected_la), length(la_number), 1)

  # Helper to safely fill vectors
  safe_fill <- function(x, len, na_value) {
    if (length(x) == 0 || all(is.na(x))) {
      rep(na_value, len)
    } else {
      rep(x, length.out = len)
    }
  }

  # Apply safe filling
  la_number <- safe_fill(la_number, n_rows, NA_integer_)
  selected_la <- safe_fill(selected_la, n_rows, NA_character_)
  trend <- safe_fill(trend, n_rows, NA_real_)
  change_since_prev <- safe_fill(change_since_prev, n_rows, NA_real_)
  rank <- safe_fill(rank, n_rows, NA_integer_)
  quartile <- safe_fill(quartile, n_rows, NA_character_)
  indicator_polarity <- safe_fill(indicator_polarity, n_rows, NA_character_)

  # Round quartile bands
  round_qbs <- round(quartile_bands, indicator_dps)
  qb_adj <- 10^(-indicator_dps)

  # Function to make a band label
  make_band <- function(lo, hi) {
    lo <- ifelse(is.na(lo), "-", lo)
    hi <- ifelse(is.na(hi), "-", hi)
    paste0(lo, " to ", hi)
  }

  # Quartile band columns
  polarity_case <- if (is.na(indicator_polarity[1])) "none" else tolower(indicator_polarity[1])

  if (polarity_case == "low") {
    bands <- list(
      "Latest National Rank" = rank,
      "Quartile Banding" = quartile,
      "A" = make_band(round_qbs[["0%"]], round_qbs[["25%"]]),
      "B" = make_band(round_qbs[["25%"]] + qb_adj, round_qbs[["50%"]]),
      "C" = make_band(round_qbs[["50%"]] + qb_adj, round_qbs[["75%"]]),
      "D" = make_band(round_qbs[["75%"]] + qb_adj, round_qbs[["100%"]])
    )
  } else if (polarity_case == "high") {
    bands <- list(
      "Latest National Rank" = rank,
      "Quartile Banding" = quartile,
      "A" = make_band(round_qbs[["100%"]], round_qbs[["75%"]] + qb_adj),
      "B" = make_band(round_qbs[["75%"]], round_qbs[["50%"]] + qb_adj),
      "C" = make_band(round_qbs[["50%"]] + qb_adj, round_qbs[["25%"]] + qb_adj),
      "D" = make_band(round_qbs[["25%"]], round_qbs[["0%"]])
    )
  } else {
    bands <- list(
      "Latest National Rank" = safe_fill(rank, n_rows, NA_integer_),
      "Quartile Banding" = safe_fill(quartile, n_rows, NA_character_),
      "No Quartiles" = rep("-", n_rows)
    )
  }

  # Apply no_show_qb logic
  if (isTRUE(no_show_qb)) {
    bands <- modifyList(bands, list(
      "Quartile Banding" = rep("-", n_rows),
      "No Quartiles" = rep("Data range is too small.", n_rows)
    ))
  }

  # Convert all band entries to flat character vectors
  bands <- lapply(bands, function(x) {
    if (is.null(x)) {
      rep("-", n_rows)
    } else if (is.list(x)) {
      unlist(x, use.names = FALSE)
    } else {
      x
    }
  })

  # Build main stats table
  stats_table <- data.frame(
    "LA Number" = la_number,
    "LA and Regions" = selected_la,
    "Trend" = trend,
    "Change from previous year" = change_since_prev,
    "Polarity" = indicator_polarity,
    check.names = FALSE
  )

  # Bind everything together safely
  stats_table <- dplyr::bind_cols(stats_table, as.data.frame(bands, check.names = FALSE))

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
    "Polarity" = pull_uniques(filtered_bds, "Polarity"),
    check.names = FALSE
  )
}


#' Create a Stats Table for Statistical Neighbour page
#'
#' @param stat_n_diff Statistical Neighbour dataframe inc change since prev year.
#' @param la_and_regions A vector for LA and region names.
#' @param trend A vector representing the trend.
#' @param change_prev A vector representing the change from previous year.
#' @param national_rank A vector for national ranking (with space padding).
#' @param quartile_band A vector for quartile banding (with space padding).
#' @param polarity A vector representing the polarity.
#' @param pull_col The name of the column to pull in filtering "LA Number".
#'
#' @return A data frame containing stats table for LA with national statistics.
#' @export

build_sn_stats_table <- function(
    stat_n_diff,
    la_and_regions,
    trend,
    change_prev,
    national_rank,
    quartile_band,
    polarity,
    pull_col = "LA Number") {
  # Helper to safely pad or replace missing values
  safe_fill <- function(x, len, na_value) {
    if (length(x) == 0 || all(is.na(x))) {
      rep(na_value, len)
    } else {
      # If shorter than needed, recycle
      rep(x, length.out = len)
    }
  }

  # Number of rows in the output table
  n_rows <- length(la_and_regions)

  data.frame(
    "LA Number" = safe_fill(
      stat_n_diff |> filter_la_regions(la_and_regions, pull_col = pull_col),
      n_rows,
      NA_integer_
    ),
    "LA and Regions" = safe_fill(la_and_regions, n_rows, NA_character_),
    "Trend" = safe_fill(trend, n_rows, NA_real_),
    "Change from previous year" = safe_fill(change_prev, n_rows, NA_real_),
    "Latest National Rank" = safe_fill(national_rank, n_rows, NA_character_),
    "Quartile Banding" = safe_fill(quartile_band, n_rows, NA_character_),
    "Polarity" = safe_fill(polarity, n_rows, NA_character_),
    check.names = FALSE
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
highlight_selected_row <- function(index, data, selected_area = NULL, geog_col = "LA and Regions") {
  la_region <- data[index, geog_col]

  # Handle missing values first
  if (is.na(la_region)) {
    return(list()) # Default styling for rows with missing "LA and Regions" value
  }

  # Check if the row matches the selected area
  if (!is.null(selected_area) && la_region == selected_area) {
    return(list(
      color = get_selected_la_colour(),
      fontWeight = "bold"
    ))
  }

  # Check if the row is for "England"
  if (la_region == "England") {
    return(list(
      color = get_england_colour(),
      fontWeight = "bold"
    ))
  }

  # Default styling for all other rows
  list()
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
    as.numeric() |>
    max()
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
    shiny::icon(trend_icon, style = "font-size: xxx-large;")
  }
}


#' Determine Background and Text Colour Based on Trend Polarity and Value
#'
#' This function returns a list with both background and text colours based on
#' the trend value and its associated polarity. It helps visually indicate
#' favourable and unfavourable trends, with white text for readability
#' on coloured backgrounds.
#'
#' @param value Numeric, the trend value to evaluate.
#' @param polarity Character, indicates if higher or lower values are favourable.
#' Accepts "High" for favourable high values or "Low" for favourable low values.
#'
#' @return A list with `background` and `text` elements. The `background` is set
#' to green (`get_gov_green()`) for favourable trends, red (`get_gov_red()`) for
#' unfavourable trends,
#' or `"none"` if either `value` or `polarity` is `NA`.
#' The `color` is `"white"` when the background is coloured,
#' and `"black"` otherwise.
#'
#' @examples
#' get_trend_style(5, "High") # Returns green background, white text
#' get_trend_style(-3, "Low") # Returns green background, white text
#' get_trend_style(3, "Low") # Returns red background, white text
#'
get_trend_colour <- function(value, polarity) {
  if (is.na(polarity) || is.na(value) || polarity == "-") {
    return(list(color = "black"))
  }

  # Colours to set trend arrow
  red_colour <- get_gov_red()
  green_colour <- get_gov_green()

  # Apply colour dependent on polarity and vlaue
  trend_colour <- dplyr::case_when(
    polarity == "Low" & value < 0 ~ green_colour,
    polarity == "Low" & value > 0 ~ red_colour,
    polarity == "High" & value > 0 ~ green_colour,
    polarity == "High" & value < 0 ~ red_colour,
    TRUE ~ "black"
  )

  list(color = trend_colour)
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
  qb_color <- get_quartile_band_cell_colour(
    data$Polarity,
    data$`Quartile Banding`
  )

  # Setting text colour based on whether cell background has colour
  if (qb_color != "none") {
    text_colour <- "white"
  } else {
    text_colour <- NA
  }

  list(
    background = qb_color,
    textAlign = "right",
    color = text_colour
  )
}


#' Format National Rank Display
#'
#' @description Formats the national rank value for display in a table. This
#' function returns an empty string for NA values, replaces -1 with "-", and
#' otherwise displays the value as-is.
#'
#' @param rank_value Numeric or character. The national rank value to be
#' formatted.
#'
#' @return A character string: "" if NA, "-" if -1, or the original value.
#'
#' @examples
#' format_national_rank(NA) # Returns ""
#' format_national_rank(-1) # Returns "-"
#' format_national_rank(42) # Returns "42"
#'
#' @export
format_national_rank <- function(rank_value) {
  dplyr::case_when(
    is.na(rank_value) ~ "-", # Display empty string for NA
    rank_value == -1 ~ "-", # Replace -1 with "-"
    TRUE ~ as.character(rank_value) # Display value as-is
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


#' Get Geographic Selection Based on User Input
#'
#' This function processes user input to generate a selection of
#' geographic units based on various criteria, including the selection
#' of local authorities (LAs), regions, and statistical neighbors. It
#' updates the selection vector to include additional descriptions based
#' on user choices, ensuring that selections reflect the user's
#' preferences while excluding unnecessary duplicates.
#'
#' @param input A reactive input object containing user selections
#'              related to geographic units, such as LAs and regions.
#' @param la_names_bds A character vector of local authority names
#'                      used for filtering and validation of selections.
#' @param region_names_bds A character vector of region names to
#'                         determine if regional selections are valid.
#' @param stat_n_geog A data frame that contains geographic
#'                     relationships, particularly for identifying
#'                     statistical neighbors of LAs.
#' @return A character vector representing the final geographic
#'         selection based on the input criteria, including options
#'         for "LAs in [Region]", statistical neighbors, and "All LAs"
#'         or "All Regions" as applicable.
#'
get_geog_selection <- function(input, la_names_bds, region_names_bds, stat_n_geog) {
  # Initialise an empty vector to store the results
  selection <- input$geog

  # If Region LAs are selected, add "LAs in [Region]" and exclude those LAs
  if (input$la_group == "region_las") {
    # LAs in same region as selected LA
    selected_las <- intersect(input$geog, la_names_bds)
    selected_la_regions <- get_la_region(stat_n_geog, selected_las)

    # Only add Region LAs if they are LAs
    if (length(selected_las) > 0) {
      selection <- c(setdiff(selection, selected_las), paste0("LAs in ", selected_la_regions))
    }
  }

  # If LA statistical neighbours selected add "[LA] statistical neighbours"
  if (isTRUE(input$la_group == "la_stat_ns")) {
    selected_las <- intersect(input$geog, la_names_bds)

    # Only add stat neighbours if they are LAs
    if (length(selected_las) > 0) {
      selection <- c(setdiff(selection, selected_las), paste0(selected_las, " statistical neighbours"))
    }
  }

  # If all LAs are selected, add "All LAs" and exclude all other LA terms
  if (input$la_group == "all_las") {
    selection <- c(setdiff(selection, la_names_bds), "All LAs") |>
      (\(x) x[!grepl("^LAs in ", x)])() |>
      (\(x) x[!grepl(" statistical neighbours$", x)])()
  }

  # If all regions are selected, add "All Regions" and exclude region_names_bds
  if (input$inc_regions) {
    selection <- c(setdiff(selection, region_names_bds), "All Regions")
  }

  # If include England is selected, add "England"
  if (input$inc_england) {
    selection <- c(setdiff(selection, "England"), "England")
  }

  # Return the final selection
  selection
}


#' HTML Column Definition Helper
#'
#' This helper function creates a column definition for use with the
#' `reactable` package that enables HTML rendering within the column.
#' This allows for more flexible formatting options for cell content,
#' such as including styled text or images. It simplifies the process
#' of defining columns that require HTML output.
#'
#' @return A column definition object for `reactable` with HTML rendering
#'         enabled, which can be used in a `reactable` table to display
#'         HTML content in specified columns.
#'
html_col_def <- function() {
  reactable::colDef(
    html = TRUE
  )
}


#' Truncate Cell with Tooltip Hover
#'
#' This function creates a div element that displays truncated text with an
#' overflow ellipsis effect. When the user hovers over the truncated text,
#' a tooltip is displayed showing the full text. This is useful for
#' enhancing user experience by providing additional context without
#' cluttering the interface.
#'
#' @param text A string containing the text to be displayed in the cell.
#' @param tooltip A string containing the tooltip text that will appear
#'        when the user hovers over the truncated text.
#' @return A div element containing the truncated text and tooltip, styled
#'         for proper display and interaction.
#'
truncate_cell_with_hover <- function(text, tooltip) {
  div(
    style = "cursor: info;
             white-space: nowrap;
             overflow: hidden;
             text-overflow: ellipsis;",
    tippy::tippy(text = text, tooltip = tooltip)
  )
}


#' Create a Tooltip with a FontAwesome Icon
#'
#' Generates a tooltip that displays a specified message when hovering over
#' a FontAwesome icon. The tooltip and icon can be customised using parameters
#' for text, style, and class.
#'
#' @param tooltip_text A character string specifying the tooltip text to display.
#' @param icon_class A character string specifying the FontAwesome class for the
#'   icon. Default is `"fas fa-question-circle"`.
#' @param icon_style A character string specifying the CSS styling for the icon.
#'   Default is `"color: #5694ca; padding-right: 7px; cursor: help; font-size: 1.2em;"`.
#' @param ... Additional arguments passed to `bslib::tooltip` for further
#'   customisation.
#'
#' @return An HTML element containing a FontAwesome icon with an attached tooltip.
#'
#' @examples
#' # Create a tooltip with default icon and style
#' create_tooltip_icon("Hover to see the tooltip")
#'
#' # Customise the icon style
#' create_tooltip_icon(
#'   "Hover to see the tooltip",
#'   icon_style = "color: red; font-size: 1.5em;"
#' )
#'
#' # Pass additional options to the tooltip
#' create_tooltip_icon(
#'   "Tooltip with placement",
#'   placement = "bottom"
#' )
#'
create_tooltip_icon <- function(
    tooltip_text,
    icon_class = "fas fa-info-circle",
    icon_style = "color: #5694ca; padding-right: 7px; padding-left: 7px; cursor: help;",
    ...) {
  bslib::tooltip(
    htmltools::tags$span(
      htmltools::tags$i(
        class = icon_class,
        style = icon_style
      )
    ),
    shiny::HTML(tooltip_text),
    options = list(customClass = "gov-tooltip"),
    ...
  )
}


#' Add a Tooltip to a Reactable Column Header
#'
#' Creates an interactive tooltip for a reactable column header with an
#' accompanying FontAwesome icon. The tooltip displays specified text when the
#' user hovers over the icon. The function also formats the header content for
#' better alignment and appearance.
#'
#' @param value A character string specifying the main text to display in the
#'   column header.
#' @param tooltip_text A character string specifying the tooltip text to display
#'   when hovering over the icon.
#' @param ... Additional arguments passed to `create_tooltip_icon` for further
#'   customisation of the tooltip or icon.
#'
#' @return A character string containing HTML for the styled column header with
#'   an embedded tooltip icon.
#'
#' @examples
#' # Add a tooltip to a column header
#' add_tooltip_to_reactcol("Trend", "Based on change from previous year")
#'
#' # Customise the tooltip placement
#' add_tooltip_to_reactcol(
#'   "Latest Rank",
#'   "Rank 1 is the top rank",
#'   placement = "bottom"
#' )
#'
add_tooltip_to_reactcol <- function(value, tooltip_text, ...) {
  as.character(
    div(
      style = "rt-th rt-th-resizable rt-align-right bar-sort-header",
      value,
      create_tooltip_icon(tooltip_text, ...)
    )
  )
}


#' Replace "NaN" values with empty strings in columns starting with "2"
#'
#' This function takes a data frame and performs the following actions:
#' 1. Converts all columns starting with "2" to character type.
#' 2. Replaces any "NaN" string values with an empty string ("").
#'
#' @param data A data frame to be processed.
#'
#' @return A data frame with columns starting with "2" converted to character
#' type, and any "NaN" values replaced with empty strings.
#'
#' @examples
#' # Assuming `df` is a data frame with columns starting with "2"
#' result <- replace_nan_with_empty(df)
#'
replace_nan_with_empty <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("2"),
        .fns = ~ as.character(.x),
        .names = "{.col}"
      ),
      dplyr::across(
        .cols = dplyr::starts_with("2"),
        .fns = ~ ifelse(.x == "NaN", "", .x)
      )
    )
}
