# -----------------------------------------------------------------------------
# This is the helper file, filled with lots of helpful functions!
#
# It is commonly used as an R script to store custom functions used through the
# app to keep the rest of the app code easier to read.
# -----------------------------------------------------------------------------

# Expandable function with input validation
expandable <- function(input_id, label, contents) {
  # Input validation
  if (!is.character(input_id) || length(input_id) != 1) {
    stop("input_id must be a single string.")
  }
  if (!is.character(label) || length(label) != 1) {
    stop("label must be a single string.")
  }
  if (!is.character(contents) && !inherits(contents, "shiny.tag")) {
    stop("contents must be a string or a shiny.tag object.")
  }

  # Create the expandable element
  gov_details <- shiny::tags$details(
    class = "govuk-details", id = input_id,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )

  return(gov_details)
}


# Value box function ----------------------------------------------------------
# fontsize: can be small, medium or large
value_box <- function(value, subtitle, icon = NULL,
                      color = "blue", width = 4,
                      href = NULL, fontsize = "medium") {
  validate_color(color)
  if (!is.null(icon) && !inherits(icon, "shiny.tag")) {
    stop("icon must be a shiny.tag object")
  }

  box_content <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      p(value, id = paste0("vboxhead-", fontsize)),
      p(subtitle, id = paste0("vboxdetail-", fontsize))
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href)) {
    box_content <- a(href = href, box_content)
  }

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    box_content
  )
}

# Valid colours for value box -------------------------------------------------
valid_colors <- c("blue", "dark-blue", "green", "orange", "purple", "white")

# Validate that only valid colours are used -----------------------------------
validate_color <- function(color) {
  if (color %in% valid_colors && !is.null(color)) {
    return(TRUE)
  }

  stop(
    "Invalid color: ", ifelse(is.null(color), "NULL", color), ". Valid colors are: ",
    paste(valid_colors, collapse = ", "), "."
  )
}

# GSS colours -----------------------------------------------------------------
# Current GSS colours for use in charts. These are taken from the current
# guidance here:
# https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/
# Note the advice on trying to keep to a maximum of 4 series in a single plot
# AF colours package guidance here: https://best-practice-and-impact.github.io/afcolours/
suppressMessages(
  gss_colour_pallette <- afcolours::af_colours("categorical", colour_format = "hex", n = 4)
)


#' Set CSS Style Sheet --------------------------------------------------------
#'
#' This function generates an HTML `head` tag that includes a
#' link to a specified CSS stylesheet.
#' It can be used to dynamically set or include a CSS file in a Shiny
#' application or any other HTML-based interface that utilises R.
#'
#' @param css_filename A character string specifying the path or
#' URL to the CSS file.
#' This should be a relative or absolute path to a `.css` file or a URL
#' pointing to an external stylesheet.
#'
#' @return A `tags$head` object containing a `link` tag that references
#' the specified CSS file.
#' This object can be directly included in the UI definition of a
#' Shiny application.
#'
#' @details
#' When included in the UI of a Shiny app, it instructs the web browser
#' to load and apply the specified CSS stylesheet.
#'
#' This function is useful when you need to modularise the
#' inclusion of stylesheets, especially in applications where the CSS file
#'  might change dynamically or needs to be set programmatically.
#'
set_css_style_sheet <- function(css_filename) {
  if (!grepl(".css", css_filename)) {
    stop("This doesn't look like a css file")
  }

  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = css_filename
    )
  )
}


#' Check if Not All Elements are NA
#'
#' This function checks if not all elements in a vector are `NA`.
#'
#' @param x A vector of any type.
#'
#' @return A logical value: `TRUE` if not all elements are `NA`,
#' otherwise `FALSE`.
#'
#' @examples
#' # Check if not all elements are NA
#' not_all_na(c(NA, 1, NA))
#' # [1] TRUE
#'
#' not_all_na(c(NA, NA, NA))
#' # [1] FALSE
#'
#' @export
not_all_na <- function(x) {
  !all(is.na(x))
}


#' Negated %in% Operator
#'
#' This function provides a negated version of the `%in%` operator,
#' allowing you to check if elements are not in a specified set.
#'
#' @param x A vector of values to be checked.
#' @param table A vector of values to be compared against.
#'
#' @return A logical vector indicating if the elements of
#' `x` are not in `table`.
#'
#' @examples
#' # Check if elements are not in the specified set
#' 1 %notin% c(2, 3, 4)
#' # [1] TRUE
#'
#' "a" %notin% c("b", "c", "d")
#' # [1] TRUE
#'
#' 2 %notin% c(1, 2, 3)
#' # [1] FALSE
#'
#' @export
`%notin%` <- Negate(`%in%`)


#' Extract Non-Empty Column Names
#'
#' This function extracts the non-empty column names from a dataframe.
#'
#' @param x A dataframe.
#' @return A character vector containing non-empty column names.
#' @examples
#' df <- data.frame(col1 = c(1, 2, 3), "col2" = c(4, 5, 6), col3 = c(7, 8, 9))
#' colnames(df)[2] <- ""
#' non_empty_colnames(df)
#' df |>
#'   {
#'     \(.) dplyr::select(., dplyr::all_of(non_empty_colnames(.)))
#'   }()
#' @export
non_empty_colnames <- function(x) {
  colnames(x)[nzchar(colnames(x))]
}


#' Replace Multiple Spaces with a Single Space
#'
#' This function takes a character vector and replaces any occurrence of
#' multiple whitespace characters with a single space.
#'
#' @param x A character vector.
#'
#' @return A character vector with multiple spaces replaced by a single space.
#'
#' @examples
#' # Replace multiple spaces with a single space in a character vector
#' clean_spaces("This  is   a    test.")
#' # [1] "This is a test."
#'
#' @export
clean_spaces <- function(x) {
  gsub("\\s+", " ", x)
}


#' Pull Unique Values from a Column in a Data Frame
#'
#' This function extracts a specified column from a data frame,
#' and then returns the unique values in that column.
#'
#' @param data A data frame.
#' @param col A character string specifying the column name
#' from which to pull unique values.
#'
#' @return A vector of unique values from the specified column.
#' @examples
#' data <- data.frame(
#'   "A" = c(1, 2, 2, 3, 3, 3),
#'   "B" = c("a", "b", "b", "c", "c", "c")
#' )
#' pull_uniques(data, "A")
#' pull_uniques(data, "B")
#'
#' @export
pull_uniques <- function(data, col) {
  data |>
    dplyr::pull(col) |>
    unique()
}


#' Filter a data frame and pull out a specific column
#'
#' @param data A data frame to filter.
#' @param filter_col A character string specifying the column to filter on.
#' @param filter_var A character string specifying the value to filter for
#' in the filter_col.
#' @param pull_col A character string specifying the column to pull out
#' after filtering.
#' @return A vector containing the values of the pull_col from the
#' filtered rows of the data frame.
#' @examples
#' filter_and_pull(mtcars, "cyl", 6, "mpg")
#'
filter_and_pull <- function(data, filter_col, filter_var, pull_col) {
  data |>
    dplyr::filter(get(filter_col) == filter_var) |>
    dplyr::pull(pull_col)
}


#' Get metadata for a specific indicator
#'
#' @param data A data frame containing the data.
#' @param input_indicator A character string specifying the indicator
#' to get metadata for.
#' @param metadata A character string specifying the metadata to get.
#' @param date A logical value indicating whether to convert the
#' metadata to a date.
#' Default is FALSE.
#' @return A character string containing the metadata for the
#' specified indicator.
#' If date is TRUE and the metadata is a number,
#' the metadata is converted to a date and returned as a character string.
#' @examples
#' get_metadata(mtcars, "mpg", "cyl", date = FALSE)
#'
get_metadata <- function(data, input_indicator, metadata) {
  metadata_output <- data |>
    filter_and_pull("Measure", input_indicator, metadata) |>
    unique()

  if (length(metadata_output) < 1) {
    warning("No matching metadata for ", metadata)
    metadata_output <- "No matching metadata"
  }

  if (grepl("^[0-9]+$", metadata_output)) {
    metadata_output |>
      as.numeric() |>
      as.Date(origin = "1899-12-30") |>
      format("%B %Y") |>
      as.character()
  } else {
    metadata_output
  }
}


#' Mute Warnings and Messages, but Allow cat Output
#'
#' This function uses `spsUtil::quiet` to suppress warnings and messages
#' while still allowing printed `cat` output to appear.
#'
#' @param input A function or code block to be executed where warnings
#' and messages are suppressed, but `cat` outputs are allowed.
#'
#' @return The result of the executed function or expression.
#'
#' @details This is a wrapper around the `spsUtil::quiet` function, with
#'   predefined parameters to mute warnings and messages while allowing
#'   `cat` output to be displayed.
#'
#' @examples
#' \dontrun{
#' mute_cat({
#'   cat("This will be printed\n")
#'   message("This message will be suppressed")
#'   warning("This warning will be suppressed")
#' })
#' }
#'
#' @export
mute_cat <- function(input) {
  spsUtil::quiet(input, print_cat = TRUE, warning = FALSE, message = FALSE)
}


#' Determine Appropriate London Region
#'
#' This function determines which London region to use for reporting.
#' Some indicators may not be provided at the Inner or Outer London level,
#' so the function defaults to "London" if no data is available for a
#' specific region.
#'
#' @param region A character string representing the region
#' (e.g., "London", "London (Inner)", or "London (Outer)").
#' @param filtered_bds A data frame containing the filtered dataset with
#' region-specific values.
#'
#' @return A character string indicating either the input `region` or "London"
#' if all values for the input region are missing (`NA`).
#'
#' @examples
#' # Example usage:
#' clean_ldn_region("London (Inner)", filtered_bds)
#'
#' @export
clean_ldn_region <- function(region, filtered_bds) {
  # Return early if the region doesn't start with "London"
  if (!grepl("^London", region)) {
    return(region)
  }

  # Extract values for the given region
  ldn_values <- filtered_bds |>
    dplyr::filter(`LA and Regions` == region) |>
    dplyr::pull(values_num)

  # Return "London" if all values are NA, otherwise return the original region
  if (all(is.na(ldn_values))) {
    return("London")
  } else {
    return(region)
  }
}


#' Retrieve AF Colours without Warning Message
#'
#' This function retrieves the "focus" color palette from the
#' `afcolours` package and suppresses the informational message that typically
#' accompanies it.
#'
#' The suppressed message advises that the palette should only be used
#' to highlight specific elements to help users understand the information.
#'
#' @return A character vector of hex color codes representing the
#' "focus" colour palette.
#'
#' @importFrom afcolours af_colours
#' @examples
#' # Retrieve the "focus" palette without the message
#' af_colours_focus()
#'
#' @export
af_colours_focus <- function() {
  withCallingHandlers(
    afcolours::af_colours(type = "focus"),
    message = function(m) {
      if (grepl(
        paste0(
          "This palette should only be used to highlight specific ",
          "elements to help users understand the information."
        ),
        m$message
      )) {
        invokeRestart("muffleMessage")
      }
    }
  )
}



get_geog_selection <- function(input, la_names_bds, region_names_bds, stat_n_geog) {
  # Initialize an empty vector to store the results
  selection <- input$geog_input

  # If Region LAs are selected, add "LAs in [Region]" and exclude those LAs
  if (input$la_groups == "region_las") {
    # LAs in same region as selected LA
    selected_las <- intersect(input$geog_input, la_names_bds)
    selected_la_regions <- get_la_region(stat_n_geog, selected_las)

    selection <- c(setdiff(selection, selected_las), paste0("LAs in ", selected_la_regions))
  }

  # If LA statistical neighbours selected add "[LA] statistical neighbours"
  if (isTRUE(input$la_groups == "la_stat_ns")) {
    selected_las <- intersect(input$geog_input, la_names_bds)
    selection <- c(setdiff(selection, selected_las), paste0(selected_las, " statistical neighbours"))
  }

  # If all LAs are selected, add "All LAs" and exclude all other LA terms
  if (input$la_groups == "all_las") {
    selection <- c(setdiff(selection, la_names_bds), "All LAs") |>
      (\(x) x[!grepl("^LAs in ", x)])() |>
      (\(x) x[!grepl(" statistical neighbours$", x)])()
  }

  # If all regions are selected, add "All Regions" and exclude region_names_bds
  if (input$all_regions) {
    selection <- c(setdiff(selection, region_names_bds), "All Regions")
  }

  # Return the final selection
  selection
}


# Function to get min and max years from the data
get_min_max_years <- function(data) {
  query_num_years <- data |>
    dplyr::pull(Years_num)

  max_year <- query_num_years |> max(na.rm = TRUE)
  min_year <- query_num_years |> min(na.rm = TRUE)

  return(list(min_year = min_year, max_year = max_year))
}

# Function to create a data frame for all years
create_years_df <- function(min_year, max_year) {
  data.frame(Years_num = seq(min_year, max_year))
}

# Function to filter BDS for selected indicators
filter_bds_for_indicators <- function(bds_data, selected_indicators) {
  bds_data |>
    dplyr::filter(
      Measure %in% selected_indicators,
      !is.na(Years)
    )
}

# Function to check if all year suffixes are the same
check_year_suffix_consistency <- function(data) {
  data |>
    pull_uniques("Years") |>
    substring(5) |>
    stringr::str_replace_all("\\d", "") |>
    {
      \(x) all(x == x[1])
    }()
}

# Function to sort year columns with full names preserved
sort_year_columns <- function(full_query_data) {
  full_query_year_cols <- names(full_query_data)[grepl("^\\d{4}", names(full_query_data))]

  full_query_year_cols |>
    purrr::set_names() |>
    purrr::map_chr(~ stringr::str_sub(.x, 1, 4)) |>
    sort() |>
    names()
}
