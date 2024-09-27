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

# nolint end
