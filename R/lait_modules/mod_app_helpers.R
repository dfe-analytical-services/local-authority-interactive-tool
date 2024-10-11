# nolint start: object_name
#
#' Page Header UI Module
#'
#' Creates a UI output for rendering a page header dynamically.
#'
#' @param id The namespace ID for the module.
#'
#' @return A `uiOutput` object for displaying the page header.
#'
#' @examples
#' PageHeaderUI("header1")
#'
PageHeaderUI <- function(id) {
  ns <- shiny::NS(id) # Create a namespace function
  shiny::uiOutput(ns("page_header")) # Use uiOutput to render the header
}


#' Page Header Server Module
#'
#' Server logic for dynamically updating the page header based on input values.
#' If the `la_value` is not yet available, it will display a placeholder title.
#'
#' @param id The namespace ID for the module.
#' @param app_inputs A reactive input object that includes a `la()` function.
#' @param page_title A string containing the default page title to display.
#'
#' @return A rendered UI object displaying the page header, which includes
#' the LA value and the page title when available.
#'
#' @examples
#' PageHeaderServer("header1", app_inputs, "Dashboard")
#'
PageHeaderServer <- function(id, app_inputs, page_title) {
  moduleServer(id, function(input, output, session) {
    output$page_header <- shiny::renderUI({
      la_value <- app_inputs$la() # Get LA value

      # Display "Loading..." if la_value is NULL, otherwise show the complete title
      if (is.null(la_value)) {
        shiny::h1(paste0("Loading... ", page_title))
      } else {
        shiny::h1(paste0(la_value, " - ", page_title))
      }
    })
  })
}


#' Internal Link UI Function
#'
#' Creates an internal action link within a Shiny module. This link is
#' used to switch between tabs within a Shiny app.
#'
#' @param id Character string that serves as the namespace for the module.
#'
#' @return A UI element (action link) that can be clicked to switch tabs.
#'
InternalLinkUI <- function(id) {
  ns <- shiny::NS(id) # Namespace the module
  actionLink(ns("internal_link"), "LA Level page")
}


#' Internal Link Server Function
#'
#' Handles the server-side logic for switching tabs in a Shiny app using
#' the action link defined in `InternalLinkUI()`. It listens for the link's
#' click event and switches the tab accordingly.
#'
#' @param id Character string for namespacing the module.
#' @param tab_value Character string representing the value of the tab to
#' switch to.
#' @param parent_session Shiny session object from the parent server.
#' @param tabset_id Character string defining the ID of the tabset panel
#' (defaults to "navsetpillslist").
#'
#' @return None. This function is called for its side effects, which include
#' switching the active tab.
#'
InternalLinkServer <- function(id,
                               tab_value,
                               parent_session,
                               tabset_id = "navsetpillslist") {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$internal_link, {
      # Switch to the specified tab
      bslib::nav_select(
        id = tabset_id,
        selected = tab_value,
        session = parent_session
      )
    })
  })
}


#' UI function for creating a download button in a Shiny module.
#'
#' @param id A unique identifier for the module. This is used to namespace
#'   inputs and outputs in the UI.
#' @param download_label A label to customize the download button text.
#'   It will be prefixed with "Download ".
#'
#' @return A download button UI element that allows users to download data
#'   in a specified format.
#'
#' @examples
#' # Example usage in UI
#' Download_DataUI("download_data", "LA table")
#'
Download_DataUI <- function(id, download_label) {
  ns <- NS(id)

  shiny::downloadButton(
    ns("download"),
    label = paste0("Download ", download_label),
    class = "gov-uk-button",
    icon = NULL
  )
}


#' Server function for handling data downloads in a Shiny module.
#'
#' @param id A unique identifier for the module, matching the UI id.
#' @param file_type_input A reactive expression representing the selected
#'   file type for download (e.g., CSV or XLSX).
#' @param data_for_download A reactive expression that returns the data
#'   to be downloaded based on user inputs or selections.
#' @param download_name A string specifying the name of the download file
#'   (excluding the extension).
#'
#' @return A Shiny module server function that sets up the download
#'   functionality for the specified data and file type. The output
#'   includes a download handler that triggers when the user clicks the
#'   download button created in the UI function.
#'
#' @examples
#' # Example usage in server
#' Download_DataServer(
#'   "download_data", input$file_type,
#'   filtered_data(), "LA_data"
#' )
#'
Download_DataServer <- function(id, file_type_input, data_for_download, download_name) {
  moduleServer(id, function(input, output, session) {
    # Download tables
    # Store the table and export file in reactive values
    local <- reactiveValues(data = NULL, export_file = NULL)

    # Observe when input$file_type or all_la_table is updated and create relevant file
    observeEvent(list(file_type_input(), download_name()), {
      # LA table
      local$data <- data_for_download()

      generate_csv(local, file_type_input())
    })

    # Download handlers
    output$download <- create_download_handler(
      local,
      file_type_input,
      reactive(download_name())
    )
  })
}

# nolint end
