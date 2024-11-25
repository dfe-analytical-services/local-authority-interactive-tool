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
      shiny::selectizeInput(
        inputId = ns("la_name"),
        label = "LA:",
        choices = la_names_bds
      ),
      shiny::selectInput(
        inputId = ns("topic_name"),
        label = "Topic:",
        choices = metric_topics,
        selected = NULL
      ),
      shiny::selectInput(
        inputId = ns("indicator_name"),
        label = "Indicator:",
        choices = metric_names
      )
    )
  )
}


#' Shiny Server Function for Handling the App Inputs with Synchronisation
#'
#' This function creates a Shiny server module for handling the app inputs
#' and synchronising them across multiple pages.
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
    # Debounce input values to prevent looping when inputs change quickly
    debounced_la_name <- shiny::debounce(reactive(input$la_name), 150)
    debounced_topic_name <- shiny::debounce(reactive(input$topic_name), 150)
    debounced_indicator_name <- shiny::debounce(reactive(input$indicator_name), 150)

    # Observe and synchronise LA input across pages
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
    shiny::observeEvent(debounced_topic_name(), {
      # Determine the filter condition for Topic
      topic_filter <- debounced_topic_name()

      # Get indicator choices for the selected topic or all topics if topic_filter is NA
      filtered_topic_bds <- bds_metrics |>
        dplyr::filter(if (!is.null(topic_filter)) .data$Topic == topic_filter else TRUE) |>
        pull_uniques("Measure")

      # Update the Indicator dropdown based on selected Topic
      shiny::updateSelectizeInput(
        session = session,
        inputId = "indicator_name",
        label = "Indicator:",
        choices = filtered_topic_bds
      )

      # Update the shared reactive value for the topic
      shared_values$topic <- debounced_topic_name()
    })

    # Observe and synchronise LA input changes
    observeEvent(debounced_la_name(), {
      shared_values$la <- debounced_la_name()
    })

    # Observe and synchronise Indicator input changes
    observeEvent(debounced_indicator_name(), {
      shared_values$indicator <- debounced_indicator_name()
    })

    # Return reactive settings
    app_settings <- list(
      la = reactive({
        debounced_la_name()
      }),
      topic = reactive({
        debounced_topic_name()
      }),
      indicator = reactive({
        debounced_indicator_name()
      })
    )

    return(app_settings)
  })
}

# nolint end
