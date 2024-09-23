#' Shared Folder Path
#'
#' This variable holds the path to the 'LAIT - modernisation' teams channel folder.
#' The folder is synchronised and located in the user's local system.
#'
shared_folder <- paste0(
  r"(C:\Users\jtufts\Department for Education\LAIT modernisation - General)",
  r"(\LAIT Modernisation 2024\Information for App Development)"
)


#' Clean SNP Column Names
#'
#' This function cleans the column names of a given data frame. Specifically, it adds numbers to 'SNP' columns
#' based on the numbers extracted from 'SN' columns.
#'
#' @param data A data frame that contains columns with names starting with 'SN' and 'SNP'.
#'
#' @return A data frame with cleaned column names.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(SN1 = c(1, 2), SNP = c(3, 4), SN2 = c(5, 6), SNP = c(7, 8))
#' clean_snp_colnames(data)
#' }
#'
#' @export
clean_snp_colnames <- function(data) {
  col_names <- colnames(data)

  # Logical vectors to identify "SN" and "SNP" columns
  sn_cols <- which(grepl("^SN\\d+$", col_names))
  snp_cols <- which(grepl("^SNP", col_names))

  if (length(sn_cols) < 1) {
    stop(
      paste0(
        "SN columns do not seem to be in the right format e.g., SNx",
        "where x is a number"
      )
    )
  }

  if (length(snp_cols) < 1) {
    stop(
      paste0(
        "SNP columns do not seem to be in the right format e.g., SNPx",
        "where x can be anything"
      )
    )
  }

  # Extract the numbers from "SN" columns
  sn_numbers <- gsub("^SN", "", col_names[sn_cols])

  # Clean column names vector
  clean_col_names <- col_names

  # Assign new names to "SNP" columns using extracted "SN" column numbers
  clean_col_names[snp_cols] <- paste0("SNP", sn_numbers)

  # Assign the new column names to the dataframe
  colnames(data) <- clean_col_names

  data
}


#' Create Measure Key
#'
#' This function creates a new column 'measure_key' in the given data frame. The 'measure_key' is created by
#' concatenating the 'Topic' and 'Measure_short' columns, replacing spaces with underscores, and converting
#' the resulting string to lowercase.
#'
#' @param data A data frame that contains the columns 'Topic' and 'Measure_short'.
#'
#' @return A data frame with an additional 'measure_key' column.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(Topic = c("Topic1", "Topic2"), Measure_short = c("Measure1", "Measure2"))
#' create_measure_key(data)
#' }
#'
#' @export
create_measure_key <- function(data) {
  data |>
    dplyr::mutate(
      measure_key = tolower(gsub(" ", "_", paste(Topic, Measure_short))),
      .after = Measure_short
    )
}
