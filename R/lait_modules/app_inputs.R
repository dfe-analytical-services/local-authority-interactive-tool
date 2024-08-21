#' Shiny Module UI for Displaying the App Inputs
#'
#' This function creates a Shiny UI module for displaying the app inputs.
#' The inputs include a select input for the local authority (LA) name,
#' the topic name, and the indicator name.
#' Each input is wrapped in a div with a well and a layout column for styling.
#'
#' @param id A unique ID that identifies the UI element
#' @return A div object that contains the UI elements for the module
#'
appInputsUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = "15rem", # Minimum width for each input box before wrapping
      shiny::selectInput(
        inputId = ns("la_name"),
        label = "LA:",
        choices = la_names_bds
      ),
      shiny::selectInput(
        inputId = ns("topic_name"),
        label = "Topic:",
        choices = metric_topics
      ),
      shiny::selectInput(
        inputId = ns("indicator_name"),
        label = "Indicator:",
        choices = metric_names
      )
    )
  )
}


#' Shiny Server Function for Handling the App Inputs
#'
#' This function creates a Shiny server module for handling the app inputs.
#' It observes the selected topic name and updates the choices for the
#' indicator name based on the selected topic.
#' It returns a list of reactive expressions for the selected LA name,
#' topic name, and indicator name.
#'
#' @param id A unique ID that identifies the server function
#' @return A list of reactive expressions for the app inputs,
#' including the selected LA name, topic name, and indicator name
#'
appInputsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Input ----------------------------------
    # Update Indicator dropdown for Topic selected
    shiny::observeEvent(input$topic_name, {

      # Get indicator choices for selected topic
      filtered_topic_bds <- bds_metrics |>
        dplyr::filter(
          Topic == input$topic_name
        ) |>
        pull_uniques("Measure")

      # Not used: Using the server to power to the dropdown for increased speed
      updateSelectInput(
        session = session,
        inputId = "indicator_name",
        label = "Indicator:",
        choices = filtered_topic_bds
      )
    })

    # Input outputs
    list(
      la = reactive({input$la_name}),
      topic = reactive({input$topic_name}),
      indicator = reactive({input$indicator_name})
    )
  })
}
