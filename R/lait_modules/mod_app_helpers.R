# Page Header UI Module
pageHeaderUI <- function(id) {
  ns <- shiny::NS(id) # Create a namespace function
  shiny::uiOutput(ns("page_header")) # Use uiOutput to render the header
}


# Page Header Server Module
pageHeaderServer <- function(id, app_inputs, page_title) {
  moduleServer(id, function(input, output, session) {
    output$page_header <- shiny::renderUI({
      la_value <- app_inputs$la() # Get LA value

      # Display "Loading..." if la_value is NULL, otherwise show the complete title
      if (is.null(la_value)) {
        shiny::h1(page_title) # Show just the page title initially
      } else {
        shiny::h1(paste0(la_value, " - ", page_title)) # Show full title once la_value is ready
      }
    })
  })
}
