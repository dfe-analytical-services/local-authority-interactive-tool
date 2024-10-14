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


#' Generate Downloadable File Based on Data and File Type
#'
#' This function generates a temporary file for download based on the provided
#' data and file type. Supported file types include CSV, XLSX, PNG, and HTML.
#' The function writes the data to the corresponding file format, and returns
#' the file path for download.
#'
#' @param data A dataset or object to be saved, which can be a data frame or
#'        a reactive list containing specific data for charts or widgets.
#' @param file_type A string specifying the desired file type for download.
#'        Supported types are: "csv", "xlsx", "png", and "html". This string
#'        is matched case-insensitively.
#'
#' @return A character string representing the path to the generated file.
#'         The file will be saved temporarily with the appropriate extension
#'         based on the provided `file_type`.
#'
#' @details
#' The function handles multiple file types as follows:
#' - For `"csv"`, it writes the data to a CSV file without row names.
#' - For `"xlsx"`, it saves the data as an Excel file with auto column widths.
#' - For `"png"`, it saves a ggplot object to a PNG file with specified size.
#' - For `"html"`, it saves an HTML widget to an HTML file.
#' If an unsupported file type is provided, the function returns an error.
#'
#' @examples
#' \dontrun{
#' # Save a data frame as a CSV file
#' generate_download_file(mtcars, "csv")
#'
#' # Save a ggplot chart as PNG
#' plot_data <- list(png = ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point())
#' generate_download_file(plot_data, "png")
#' }
#'
generate_download_file <- function(data, file_type) {
  out <- tempfile(fileext = dplyr::case_when(
    grepl("csv", file_type, ignore.case = TRUE) ~ ".csv",
    grepl("xlsx", file_type, ignore.case = TRUE) ~ ".xlsx",
    grepl("png", file_type, ignore.case = TRUE) ~ ".png",
    grepl("html", file_type, ignore.case = TRUE) ~ ".html",
    TRUE ~ "Error"
  ))

  if (grepl("csv", file_type, ignore.case = TRUE)) {
    write.csv(data, file = out, row.names = FALSE)
  } else if (grepl("xlsx", file_type, ignore.case = TRUE)) {
    openxlsx::write.xlsx(data, file = out, colWidths = "Auto")
  } else if (grepl("png", file_type, ignore.case = TRUE)) {
    ggplot2::ggsave(filename = out, plot = data, width = 8.5, height = 6)
  } else if (grepl("html", file_type, ignore.case = TRUE)) {
    htmlwidgets::saveWidget(widget = data, file = out)
  }

  out
}


#' Create Download Handler for Shiny Application
#'
#' This function creates a `downloadHandler` for use in a Shiny app, allowing
#' users to download a file in the specified format. The file name is generated
#' dynamically based on the provided extension input and table name prefix.
#'
#' @param export_file A string representing the file path of the file to be
#'        downloaded. This should be a pre-generated file ready for download.
#' @param ext_input A reactive expression that returns the file type selected
#'        by the user. Supported types are: "xlsx", "csv", "png", and "html".
#'        The file extension is matched case-insensitively.
#' @param table_name_prefix A reactive expression returning a vector of strings
#'        that represent parts of the file name. These are concatenated with
#'        hyphens (`-`) to form the base of the download file name.
#'
#' @return A `downloadHandler` object for use in the server function of a
#'         Shiny app. This handler will allow the user to download a file
#'         with the desired format and name.
#'
#' @details
#' The filename is generated based on the table name prefix and current date,
#' with the appropriate file extension determined by the user's selection.
#' Supported extensions are: `.xlsx`, `.csv`, `.png`, and `.html`. The content
#' of the file is copied from `export_file`, and a notification is shown to
#' indicate that the file is being generated.
#'
#' @examples
#' \dontrun{
#' # Create a download handler for a CSV file
#' create_download_handler("path/to/file.csv", reactive("csv"), reactive("data-table"))
#' }
#'
create_download_handler <- function(local) {
  downloadHandler(
    filename = function() {
      file_ext <- dplyr::case_when(
        grepl("xlsx", local$file_type, ignore.case = TRUE) ~ ".xlsx",
        grepl("csv", local$file_type, ignore.case = TRUE) ~ ".csv",
        grepl("png", local$file_type, ignore.case = TRUE) ~ ".png",
        grepl("html", local$file_type, ignore.case = TRUE) ~ ".html",
        TRUE ~ "Error"
      )
      paste0(paste(local$file_name, collapse = "-"), "-", Sys.Date(), file_ext)
    },
    content = function(file) {
      pop_up <- shiny::showNotification("Generating download file", duration = NULL)
      file.copy(local$export_file, file)
      on.exit(shiny::removeNotification(pop_up), add = TRUE)
    }
  )
}


#' Generate a Radio Button Input for File Type Selection
#'
#' This function creates a radio button input for selecting the download file
#' format in a Shiny application. The label, hint, and choices for the radio
#' button are dynamically generated based on the type of file being downloaded
#' (either a data table or plot).
#'
#' @param input_id A string representing the input ID for the radio button,
#'        which will be used to access the selected file type in the Shiny
#'        server logic.
#' @param file_type A string that specifies the type of file being downloaded.
#'        It can either be "table" (for downloading data tables) or any other
#'        string (for downloading plots). Defaults to "table".
#'
#' @return A `shinyGovstyle::radio_button_Input` object to be included in the
#'         Shiny UI, allowing the user to choose between available file formats
#'         for the download.
#'
#' @details
#' When the `file_type` is "table", the user will have the option to select
#' between "CSV" and "XLSX" file formats. For other file types, the user can
#' select between "PNG" and "HTML". The default selected option is "CSV" for
#' tables and "PNG" for plots. The hint label displayed below the input will
#' provide guidance based on the type of download.
#'
#' @examples
#' \dontrun{
#' # Generate file type selection for a table
#' file_type_input_btn("file_type", file_type = "table")
#'
#' # Generate file type selection for a plot
#' file_type_input_btn("file_type", file_type = "plot")
#' }
#'
file_type_input_btn <- function(input_id, file_type = "table") {
  shinyGovstyle::radio_button_Input(
    inputId = input_id,
    label = h2("Choose download file format"),
    hint_label = if (file_type == "table") {
      paste0(
        "This will download all data related to the providers and options selected.",
        " The XLSX format is designed for use in Microsoft Excel."
      )
    } else {
      paste0(
        "This will download the plots related to the options selected.",
        " The HTML format contains the interactive element."
      )
    },
    choices = if (file_type == "table") {
      c("CSV", "XLSX")
    } else {
      c("PNG", "HTML")
    },
    selected = if (file_type == "table") "CSV" else "PNG"
  )
}
