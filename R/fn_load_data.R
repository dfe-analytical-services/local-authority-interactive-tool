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


#' Generates a CSV or Excel file from the provided data and stores the
#' export file path in a reactive values object.
#'
#' @param local A reactive values object containing a `data` attribute, which
#'   holds the data to be exported, and an `export_file` attribute to store
#'   the file path of the generated export.
#' @param ext_input A character string indicating the desired file format.
#'   It should be either "CSV (Up to 5.47 MB)" for CSV format or any other
#'   string for Excel format (XLSX).
#'
#' @return NULL. This function modifies the `local` reactive values object to
#'   store the path of the generated export file.
#'
#' @details The function creates a temporary file in the system's
#'   temporary directory. If the user selects CSV format, it writes the
#'   data to a CSV file. If the user selects Excel format, it writes
#'   the data to an Excel file with automatic column widths.
#'
#' @examples
#' # Example of using generate_csv in a download handler
#' generate_csv(local_values, input$file_type)
#'
generate_csv <- function(local, ext_input) {
  # Set extension from user input
  if (ext_input == "CSV (Up to 5.47 MB)") {
    # Create a temporary file path for the CSV export
    out <- tempfile(fileext = ".csv")

    # Write the CSV file to the temporary path
    write.csv(local$data, file = out, row.names = FALSE)
  } else {
    out <- tempfile(fileext = ".xlsx")
    openxlsx::write.xlsx(local$data, file = out, colWidths = "Auto")
  }

  # Store the file path in the reactive values object
  local$export_file <- out
}


#' Creates a download handler for exporting data in either CSV or Excel
#' format from a Shiny application.
#'
#' @param local A reactive values object containing the path to the
#'   export file in `local$export_file`.
#' @param ext_input A reactive expression that returns the file format
#'   selected by the user (either "CSV (Up to 5.47 MB)" or another value
#'   for Excel format).
#' @param table_name_prefix A character string used as a prefix for the
#'   download filename, typically indicating the content of the data.
#'
#' @return A Shiny `downloadHandler` that provides the necessary
#'   functionality for file downloading.
#'
#' @details The filename for the downloaded file is generated based on
#'   the user's selected format and includes the current date. A notification
#'   is displayed to the user while the file is being generated, particularly
#'   for Excel files that may take longer to create. The contents of the
#'   file are copied from the temporary export file stored in `local`.
#'
#' @examples
#' # Example of creating a download handler for a CSV or Excel export
#' output$download <- create_download_handler(local_values, reactive({
#'   input$file_type
#' }), "Data")
#'
create_download_handler <- function(local, ext_input, table_name_prefix) {
  downloadHandler(
    filename = function() {
      if (ext_input() == "CSV (Up to 5.47 MB)") {
        paste0(table_name_prefix, "-", Sys.Date(), ".csv")
      } else {
        paste0(table_name_prefix, "-", Sys.Date(), ".xlsx")
      }
    },
    content = function(file) {
      # Added a basic pop up notification as the Excel file can take time to generate
      pop_up <- shiny::showNotification("Generating download file", duration = NULL)
      # Copy the CSV from local$export_file to the file being downloaded
      file.copy(local$export_file, file)
      on.exit(shiny::removeNotification(pop_up), add = TRUE)
    }
  )
}
