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


# nolint end
