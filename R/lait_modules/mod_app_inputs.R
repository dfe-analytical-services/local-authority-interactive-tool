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
    style = "overflow-y: visible; position: relative;",
    bslib::layout_column_wrap(
      width = "15rem", # Minimum width for each input box before wrapping
      shiny::selectizeInput(
        inputId = ns("la_name"),
        label = tags$label(
          "Local Authority:",
          create_tooltip_icon("Change selection by scrolling or typing.")
        ),
        choices = la_names_bds,
        options = list(
          placeholder = "Start typing or scroll to find a Local Authority...",
          plugins = list("clear_button")
        )
      ),
      shiny::selectizeInput(
        inputId = ns("topic_name"),
        label = tags$label(
          id = ns("topic_label"),
          "Topic:"
        ),
        choices = c("All Topics", metric_topics),
        selected = "All Topics",
        options = list(
          placeholder = "No topic selected, showing all indicators.",
          plugins = list("clear_button")
        )
      ),
      shiny::selectizeInput(
        inputId = ns("indicator_name"),
        label = "Indicator:",
        choices = metric_names,
        options = list(
          placeholder = "Start typing or scroll to find an indicator...",
          plugins = list("clear_button")
        )
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
appInputsServer <- function(id,
                            shared_values,
                            topic_indicator_full) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store the previous LA name
    previous_la_name <- reactiveVal(NULL)

    # Debounce input values to prevent looping when inputs change quickly
    debounced_la_name <- shiny::debounce(reactive(input$la_name), 150)
    debounced_topic_name <- shiny::debounce(reactive(input$topic_name), 150)
    debounced_indicator_name <- shiny::debounce(reactive(input$indicator_name), 150)

    # Update Indicator dropdown for selected Topic
    shiny::observeEvent(debounced_topic_name(),
      {
        # Save the currently selected indicator
        current_indicator <- debounced_indicator_name()

        # Determine the filter condition for Topic
        topic_filter <- debounced_topic_name()

        # Get indicator choices for selected topic
        # Include all rows if no topic is selected or "All Topics" is selected
        filtered_topic_bds <- topic_indicator_full |>
          filter_by_topic("Topic", topic_filter) |>
          pull_uniques("Measure")

        # Ensure the current indicator stays selected
        # Default to the first topic indicator if the current is not valid
        selected_indicator <- if (current_indicator %in% filtered_topic_bds) {
          current_indicator
        } else {
          filtered_topic_bds[1]
        }

        # Update the Indicator dropdown based on selected Topic
        shiny::updateSelectizeInput(
          session = session,
          inputId = "indicator_name",
          choices = filtered_topic_bds,
          selected = selected_indicator
        )

        # Update the shared reactive value for the topic
        shared_values$topic <- debounced_topic_name()
      },
      ignoreNULL = FALSE
    )

    # Prevent LA input from being empty by storing its previous value
    shiny::observeEvent(debounced_la_name(), {
      # Check if the LA name is NULL or empty
      la_name <- debounced_la_name()

      if ("" %notin% la_name && !is.null(la_name)) {
        # Update the reactive value with the current valid input
        previous_la_name(la_name)

        # Synchronise the shared reactive value
        shared_values$la <- la_name
      }
    })

    # Set dynamic topic label
    # (to display topic when not selected or all topics selected)
    update_topic_label(
      indicator_input = debounced_indicator_name,
      topic_input = debounced_topic_name,
      topic_indicator_data = topic_indicator_full,
      topic_label_id = "topic_label"
    )

    # Observe and synchronise Indicator input changes
    observeEvent(debounced_indicator_name(), {
      shared_values$indicator <- debounced_indicator_name()
    })

    # Synchronise inputs across pages:
    # LA
    observe({
      shiny::updateSelectizeInput(session, "la_name", selected = shared_values$la)
    })
    # Topic
    observe({
      shiny::updateSelectizeInput(session, "topic_name", selected = shared_values$topic)
    })
    # Indicator
    observe({
      shiny::updateSelectizeInput(session, "indicator_name", selected = shared_values$indicator)
    })

    # Return reactive settings
    app_settings <- list(
      la = reactive({
        previous_la_name()
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
