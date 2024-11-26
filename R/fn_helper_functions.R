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

  # Warning if any entry in metadata_output appears to be a 5-digit number
  if (any(grepl("^\\d{5}$", metadata_output))) {
    warning("Detected a 5-digit numeric entry in metadata_output,
            which may represent a misformatted date (e.g., Excel numeric).")

    # Convert 5-digit number to Date if it matches the Excel date format
    metadata_output <- metadata_output |>
      as.numeric() |>
      as.Date(origin = "1899-12-30") |>
      format("%B %Y") |>
      as.character()
  }

  metadata_output
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


#' Retrieve AF Colours Without Warning Message
#'
#' Retrieves AF colours while suppressing a specific warning message about
#' limiting categories when using the categorical palette.
#'
#' @return A vector of colours from the `afcolours` package.
#'
#' @details This function retrieves the colour palette from `afcolours::af_colours`
#' while silencing the message advising to limit categories to four. The
#' message is suppressed using `withCallingHandlers` to improve usability
#' without affecting the palette content.
#'
#' @examples
#' colours <- get_af_colours()
#'
get_af_colours <- function() {
  withCallingHandlers(
    afcolours::af_colours(),
    message = function(m) {
      if (grepl(
        paste0(
          "It is best practice to limit to four categories when using the ",
          "categorical palette so the chart does not become too cluttered."
        ),
        m$message
      )) {
        invokeRestart("muffleMessage")
      }
    }
  )
}


#' Get Colour for Local Authority Focus
#'
#' Returns a hex colour for highlighting the selected local authority in
#' tables or plots.
#'
#' @return A character string with the hex colour code for LA focus.
#' @examples
#' get_la_focus_colour()
#' @export
get_la_focus_colour <- function() {
  get_af_colours()[4] # "#5694ca"
}


#' Get Colour for England Highlight
#'
#' Retrieves a colour from the afcolours package palette for highlighting
#' "England" rows in tables or plots.
#'
#' @return A character string with the hex colour code for England highlight.
#' @examples
#' get_england_colour()
#' @export
get_england_colour <- function() {
  get_af_colours()[3]
}


#' Retrieve Available Colours for Plotting
#'
#' This function provides a filtered set of colours from the AF colours palette,
#' excluding the specific colours reserved for the focus group and "England."
#'
#' @return A vector of hex colour codes from the AF colours palette, with
#'   colours reserved for the focus group and "England" removed.
#' @seealso [get_af_colours()] for the original AF colour palette,
#'   [get_la_focus_colour()] for the focus group colour, and
#'   [get_england_colour()] for the "England" colour.
#' @examples
#' clean_colours <- get_clean_af_colours()
#'
#' # Use clean_colours for general plotting, excluding reserved colours
#'
get_clean_af_colours <- function() {
  setdiff(get_af_colours(), c(
    get_la_focus_colour(),
    get_england_colour(),
    "#3D3D3D"
  ))
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


#' Get Minimum and Maximum Years from Data
#'
#' This function extracts the minimum and maximum years from a specified
#' numeric column of year values within a data frame. It handles
#' missing values appropriately and returns the results in a list for
#' easy access.
#'
#' @param data A data frame that contains a numeric column representing
#'              year values, typically labeled as "Years_num".
#' @return A list containing two elements: `min_year`, the minimum year
#'         value found in the data, and `max_year`, the maximum year
#'         value. Both values are numeric and NA is handled appropriately.
#'
get_min_max_years <- function(data) {
  query_num_years <- data |>
    dplyr::pull(Years_num)

  max_year <- query_num_years |> max(na.rm = TRUE)
  min_year <- query_num_years |> min(na.rm = TRUE)

  list(min_year = min_year, max_year = max_year)
}


#' Create Data Frame for All Years
#'
#' This function generates a data frame containing a sequence of year
#' values from a specified minimum year to a maximum year. The resulting
#' data frame includes a single column named "Years_num" that holds
#' the year values in order.
#'
#' @param min_year The starting year of the sequence, which should be
#'                 numeric.
#' @param max_year The ending year of the sequence, which should also be
#'                 numeric.
#' @return A data frame with one column, `Years_num`, containing all
#'         year values from `min_year` to `max_year`, inclusive.
#'
create_years_df <- function(min_year, max_year) {
  data.frame(Years_num = seq(min_year, max_year))
}


#' Check Year Suffix Consistency
#'
#' This function verifies whether all entries in the specified dataset have
#' consistent suffixes for each year. It extracts the first four characters
#' from the year strings to identify the year and the remaining characters
#' as the suffix. The function ensures that for each unique year, there is
#' only one unique suffix present.
#'
#' @param data A data frame containing a column `Years`, which includes year
#'              values with potential suffixes.
#' @return A logical value indicating whether all years have consistent
#'         suffixes (`TRUE`) or not (`FALSE`).
#'
check_year_suffix_consistency <- function(data) {
  data |>
    pull_uniques("Years") |>
    (\(years_list) {
      # Extract the first four characters as the year and remaining as the suffix
      years <- substring(years_list, 1, 4)
      suffixes <- substring(years_list, 5)

      # For each unique year, check if there is more than one unique suffix
      all(
        sapply(unique(years), function(year) {
          # Get the suffixes for the current year and check for consistency
          unique_suffixes <- unique(suffixes[years == year])
          length(unique_suffixes) <= 1
        })
      )
    })()
}


#' Sort Year Columns
#'
#' This function identifies and sorts the columns of a data frame that
#' represent years, while preserving their full names. It searches for
#' column names that start with a four-digit year (e.g., 2020) and
#' returns them in ascending order based on the year, maintaining
#' their original names in the process.
#'
#' @param full_query_data A data frame that may contain year columns
#'                        named with the format of four-digit years.
#' @return A character vector of sorted column names representing years,
#'         preserving the original names.
#'
sort_year_columns <- function(full_query_data) {
  full_query_year_cols <- names(full_query_data)[grepl("^\\d{4}", names(full_query_data))]

  full_query_year_cols |>
    purrr::set_names() |>
    purrr::map_chr(~ stringr::str_sub(.x, 1, 4)) |>
    sort() |>
    names()
}


#' Rename Columns with Year
#'
#' This function renames the columns of a data frame by extracting the
#' first four digits from the column names if they start with a four-digit
#' year. If a column name does not begin with four digits, it retains
#' its original name. This is useful for simplifying column names in
#' data frames that contain year-based columns while keeping other
#' column names intact.
#'
#' @param df A data frame whose column names may include four-digit years
#'            at the start.
#' @return A data frame with renamed columns where applicable, with four-digit
#'         years extracted as the new column names.
#'
rename_columns_with_year <- function(df) {
  # Use `gsub` to extract the first 4 digits if they start the column name
  new_names <- sapply(names(df), function(col) {
    if (grepl("^\\d{4}", col)) {
      # Extract the first 4 digits
      return(substr(col, start = 1, stop = 4))
    } else {
      # Return the original name if it doesn't start with 4 digits
      return(col)
    }
  })

  # Rename columns with the modified names
  colnames(df) <- new_names
  df
}


#' @title Insert Line Breaks at Full Words
#' @description Adds line breaks to a string after full words to ensure that
#' each line stays within a specified maximum length.
#' @param text A character string to insert line breaks into.
#' @param max_length Maximum length of each line in characters. Default is 20.
#' @return A character string with line breaks after full words.
#' @details The function splits the input string into words, then builds each
#' line by appending words until the maximum length is reached. If adding a word
#' would exceed the line length, a line break is added before that word.
#' @examples
#' add_line_breaks("This is an example of a long text that needs breaks.", 15)
#'
add_line_breaks <- function(text, max_length = 20) {
  words <- strsplit(text, " ")[[1]]
  lines <- c()
  current_line <- ""

  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 <= max_length) {
      current_line <- paste(current_line,
        word,
        sep = if (nchar(current_line) > 0) " " else ""
      )
    } else {
      lines <- c(lines, current_line)
      current_line <- word
    }
  }
  c(lines, current_line) |> paste(collapse = "\n")
}


#' Add a GOV.UK styled spinner to a UI element
#'
#' This function wraps a UI element with a spinner from the `shinycssloaders`
#' package, with a GOV.UK style and custom size. The spinner is displayed
#' while the UI element is loading, and its height is adjusted dynamically
#' based on the size parameter.
#'
#' @param ui_element A UI element (e.g., `reactableOutput`, `plotOutput`, etc.)
#'   to be wrapped with a spinner.
#' @param size A numeric value (default is 1) to specify the size of the spinner.
#'   The height of the spinner will scale based on this value (multiplied by 250px).
#'
#' @return A UI element wrapped in a spinner.
#'
#' @examples
#' # Wrap a reactable table with a spinner
#' with_gov_spinner(reactable::reactableOutput("la_table"))
#'
#' # Wrap a plot with a larger spinner
#' with_gov_spinner(plotOutput("la_plot"), size = 2)
#'
with_gov_spinner <- function(ui_element, spinner_type = 6, size = 1) {
  shinycssloaders::withSpinner(
    ui_element,
    type = spinner_type,
    color = "#1d70b8",
    size = size,
    proxy.height = paste0(250 * size, "px")
  )
}



update_topic_label <- function(
    indicator_input,
    topic_input,
    topic_indicator_data,
    topic_label_id = "topic_label") {
  shiny::observeEvent(c(indicator_input(), topic_input()), {
    indicator <- indicator_input()
    topic <- topic_input()

    # Default topic label
    label <- "Topic:"

    # Update label if conditions are met
    if (!is.null(indicator) && indicator != "" &&
      (topic %in% c("", "All Topics"))) {
      # Get related topic(s)
      related_topics <- topic_indicator_data |>
        dplyr::filter(.data$Measure == indicator) |>
        pull_uniques("Topic")

      # Create label, combining topics if multiple exist
      if (length(related_topics) > 0) {
        label <- paste0(
          "Topic:&nbsp;&nbsp;(",
          paste0(related_topics, collapse = " / "),
          ")"
        )
      }
    }

    # Update the HTML element with the new label
    shinyjs::html(topic_label_id, label)
  })
}
