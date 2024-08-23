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
#' df |> {\(.) dplyr::select(., dplyr::all_of(non_empty_colnames(.))) }()
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
#' data <- data.frame("A" = c(1, 2, 2, 3, 3, 3),
#' "B" = c("a", "b", "b", "c", "c", "c"))
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
    filter_and_pull("Measure", input_indicator, metadata)

  if (grepl("^[0-9]+$", metadata_output)) {
    metadata_output |>
      as.numeric() |>
      as.Date(origin = "1899-12-30") |>
      as.character()
  } else {
    metadata_output
  }
}

