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
  ns <- shiny::NS(id)

  div(
    id = "page_header_spinner",
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("page_header")),
      type = 7,
      color = "#0b0c0c",
      size = 1,
      proxy.height = paste0(250 * 0.25, "px")
    )
  )
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


#' Download Data Server Module
#'
#' This function creates a server module for downloading data in various formats
#' (e.g., CSV, XLSX, PNG, HTML). The module observes changes in the file type
#' input and the data for download, generates the appropriate file based on the
#' selected format, and provides a download handler for the user to download
#' the file.
#'
#' @param id A string representing the module ID, used for namespacing
#'        UI and server logic of the download functionality.
#' @param file_type_input A reactive expression that returns the selected file
#'        type for the download (e.g., "CSV", "XLSX", "PNG", "HTML").
#' @param data_for_download A reactive expression that provides the data to be
#'        downloaded. The structure of the data should match the format expected
#'        by the file type (e.g., a data frame for tables or a ggplot object
#'        for images).
#' @param download_name A reactive expression that returns a character vector
#'        representing the name components to be concatenated for the downloaded
#'        file (e.g., "LA-Indicator-Local-Authority-View").
#'
#' @return A module server that handles file generation and download, enabling
#'         the user to download data in the desired format.
#'
#' @details
#' This module tracks changes to the file type input and the data, generating a
#' new file when either changes. It supports multiple file formats including:
#' - CSV and XLSX for tables
#' - PNG for plots
#' - HTML for interactive elements
#'
#' The file name for the download is generated by combining the elements
#' provided in `download_name()` and appending the current date and file extension.
#'
#' @examples
#' \dontrun{
#' # In server:
#' Download_DataServer(
#'   "data_download", reactive(input$file_type),
#'   reactive(my_data), reactive(c("Region", "Indicator"))
#' )
#' }
#'
Download_DataServer <- function(id, file_type_input, data_for_download, download_name) {
  moduleServer(id, function(input, output, session) {
    # Reactive values for storing file path
    local <- reactiveValues(export_file = NULL, data = NULL, plot_width = NULL, file_type = NULL, file_name = NULL)

    # Observe changes in file type or data and generate export file
    observeEvent(list(file_type_input(), data_for_download(), download_name()), {
      # Ensure inputs are not NULL
      req(file_type_input(), data_for_download(), download_name())

      # Setting parameters
      local$file_type <- file_type_input()
      local$file_name <- download_name()

      # For charts we need to pull the relevant object from the reactive list
      if (grepl("svg", local$file_type, ignore.case = TRUE)) {
        local$data <- data_for_download()$"svg"
        # Getting plot width from ggiraph obj ratio
        local$plot_width <- data_for_download()$"html"$x$ratio * 5
      } else if (grepl("html", local$file_type, ignore.case = TRUE)) {
        local$data <- data_for_download()$"html"
      } else {
        local$data <- data_for_download()
      }

      # Generate the file based on the selected file type
      local$export_file <- generate_download_file(local$data, local$file_type, local$plot_width)
    })

    # Download handler
    output$download <- create_download_handler(
      local
    )
  })
}



#' Download Chart Modal UI Module
#'
#' Creates a modal dialog for downloading a chart in different formats.
#'
#' This UI module generates a modal that allows users to select the file type
#' and download the chart. It includes a file type selection button and a
#' download button specific to the chart type.
#'
#' @param id A unique identifier for the modal instance.
#' @param chart_type A string representing the type of chart (e.g., "Line",
#'   "Bar") to include in the title and download button label.
#'
#' @return A modal dialog UI for downloading the specified chart type.
#'
#' @examples
#' DownloadChartModalUI("chart_modal", "Line")
DownloadChartModalUI <- function(id, chart_type) {
  ns <- NS(id) # Create a namespace
  shiny::modalDialog(
    title = paste0("Download ", chart_type, " Chart"),
    file_type_input_btn(ns("file_type"), file_type = "chart"),
    Download_DataUI(ns("chart_download"), paste0(chart_type, " chart")),
    easyClose = TRUE,
    footer = shiny::tagAppendAttributes(
      shiny::modalButton("Close"),
      class = "govuk-button--secondary"
    )
  )
}


#' Download Chart Button UI Module
#'
#' Creates a button that triggers the download modal for a chart.
#'
#' This UI module generates an action button that users can click to open a
#' modal dialog for downloading a chart. The button is styled and positioned
#' to appear to the right of the chart.
#'
#' @param id A unique identifier for the button instance.
#'
#' @return An action button UI element for triggering the download modal.
#'
#' @examples
#' DownloadChartBtnUI("download_btn")
DownloadChartBtnUI <- function(id) {
  ns <- NS(id)

  # Modal trigger button on the right
  shiny::actionButton(
    ns("open_modal"),
    label = "Download Chart",
    class = "govuk-button--secondary",
    style = "margin-left: 15px; align-self: flex-start;"
  )
}


#' Download Chart Button Server Module
#'
#' Handles the server-side logic for triggering the download modal.
#'
#' This server module listens for clicks on the download button and, upon
#' clicking, displays the corresponding modal dialog for chart downloads.
#'
#' @param id A unique identifier for the server module instance.
#' @param parent_id The ID of the parent module that will handle the modal
#'   display.
#' @param chart_type A string indicating the type of chart (e.g., "Line",
#'   "Bar") to include in the modal title and download button.
#'
#' @return None. Triggers a modal dialog for chart download upon button click.
#'
#' @examples
#' DownloadChartBtnServer("download_btn", "parent_module", "Line")
DownloadChartBtnServer <- function(id, parent_id, chart_type) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$open_modal, {
      shiny::showModal(DownloadChartModalUI(parent_id, chart_type))
    })
  })
}


#' Handle success and failure events for copying charts to clipboard
#'
#' This module listens for success or failure events triggered during
#' copying a chart to the clipboard. It then displays a toast message
#' indicating whether the chart was copied successfully or if the copy
#' operation failed.
#'
#' @param id A unique ID for the module server function. This is used to
#' identify the module when calling it from the UI or server function.
#'
#' @return No return value. The function will display a toast message
#' based on the outcome of the copy operation.
#'
CopyToClipboardPopUpServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input[["success"]], {
      shinyToastify::showToast(
        session,
        input,
        text = tags$span(
          style = "color: white; font-size: 20px;", "Chart copied!"
        ),
        type = "success",
        position = "top-center",
        autoClose = 1500,
        pauseOnFocusLoss = FALSE,
        draggable = FALSE,
        style = list(
          border = "2px solid black",
          boxShadow = "rgba(0, 0, 0, 0.56) 0px 22px 30px 4px"
        )
      )
    })

    observeEvent(input[["failure"]], {
      shinyToastify::showToast(
        session,
        input,
        text = tags$span(
          style = "color: white; font-size: 20px;", "Failed to copy chart!"
        ),
        type = "error",
        position = "top-center",
        autoClose = 1500,
        pauseOnFocusLoss = FALSE,
        draggable = FALSE,
        style = list(
          border = "2px solid black",
          boxShadow = "rgba(0, 0, 0, 0.56) 0px 22px 30px 4px"
        )
      )
    })
  })
}

# nolint end
