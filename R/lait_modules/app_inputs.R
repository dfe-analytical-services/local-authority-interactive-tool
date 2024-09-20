# nolint start: object_name
#
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


#' Shiny Server Function for Handling the App Inputs with Synchronization
#'
#' This function creates a Shiny server module for handling the app inputs
#' and synchronizing them across multiple pages.
#' It observes the selected topic name and updates the choices for the
#' indicator name based on the selected topic, and also updates the shared
#' reactive values to keep the inputs in sync between pages.
#'
#' @param id A unique ID that identifies the server function.
#' @param shared_values A `reactiveValues` object to store shared input values
#' that can be accessed and modified across different modules.
#' @return A list of reactive expressions for the app inputs, including
#' the selected LA name, topic name, and indicator name.
#'
appInputsServer <- function(id, shared_values) {
  moduleServer(id, function(input, output, session) {
    # Observe and synchronize LA input across pages
    observe({
      updateSelectInput(session, "la_name", selected = shared_values$la)
    })

    observe({
      updateSelectInput(session, "topic_name", selected = shared_values$topic)
    })

    observe({
      updateSelectInput(session, "indicator_name", selected = shared_values$indicator)
    })

    # Update Indicator dropdown for selected Topic
    shiny::observeEvent(input$topic_name, {
      # Get indicator choices for selected topic
      filtered_topic_bds <- bds_metrics |>
        dplyr::filter(.data$Topic == input$topic_name) |>
        pull_uniques("Measure")

      # Update the Indicator dropdown based on selected Topic
      updateSelectInput(
        session = session,
        inputId = "indicator_name",
        label = "Indicator:",
        choices = filtered_topic_bds
      )

      # Update the shared reactive value for the topic
      shared_values$topic <- input$topic_name
    })

    # Observe and synchronize LA input changes
    observeEvent(input$la_name, {
      shared_values$la <- input$la_name
    })

    # Observe and synchronize Indicator input changes
    observeEvent(input$indicator_name, {
      shared_values$indicator <- input$indicator_name
    })

    # Return reactive settings
    app_settings <- list(
      la = reactive({
        input$la_name
      }),
      topic = reactive({
        input$topic_name
      }),
      indicator = reactive({
        input$indicator_name
      })
    )

    return(app_settings)
  })
}

# nolint end
