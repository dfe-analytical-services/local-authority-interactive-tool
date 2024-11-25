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
        label = tags$label(
          id = ns("la_label"),
          "Local Authority:"
        ),
        choices = la_names_bds,
        options = list(
          placeholder = "Select a Local Authority...",
          plugins = list("clear_button")
        )
      ),
      shiny::selectizeInput(
        inputId = ns("topic_name"),
        label = tags$label(
          id = ns("topic_label"),
          "Topic:"
        ),
        choices = c("All topics", metric_topics),
        selected = "All topics",
        options = list(
          placeholder = "No topic selected, showing all indicators.",
          plugins = list("clear_button"),
          dropdownParent = "body"
        )
      ),
      shiny::selectizeInput(
        inputId = ns("indicator_name"),
        label = tags$label(
          id = ns("indicator_label"),
          "Indicator:"
        ),
        choices = metric_names,
        options = list(
          placeholder = "Select an indicator...",
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
                            bds_metrics,
                            topic_indicator_full) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store the previous LA name
    previous_la_name <- reactiveVal(NULL)

    # Debounce input values to prevent looping when inputs change quickly
    debounced_la_name <- shiny::debounce(reactive(input$la_name), 150)
    debounced_topic_name <- shiny::debounce(reactive(input$topic_name), 150)
    debounced_indicator_name <- shiny::debounce(reactive(input$indicator_name), 150)

    # Observe and synchronise LA input across pages
    observe({
      # Check if the LA name is NULL or empty
      la_name <- debounced_la_name()

      if ("" %notin% la_name && !is.null(la_name)) {
        # Update the reactive value with the current valid input
        previous_la_name(la_name)

        # Synchronise the shared reactive value
        shared_values$la <- la_name
      }
    })

    observe({
      shiny::updateSelectizeInput(session, "topic_name", selected = shared_values$topic)
    })

    observe({
      shiny::updateSelectizeInput(session, "indicator_name", selected = shared_values$indicator)
    })

    # Update Indicator dropdown for selected Topic
    shiny::observeEvent(debounced_topic_name(),
      {
        # Save the currently selected indicator
        current_indicator <- debounced_indicator_name()

        # Determine the filter condition for Topic
        topic_filter <- debounced_topic_name()

        # Get indicator choices for selected topic
        # Include all rows if no topic is selected or "All topics" is selected
        filtered_topic_bds <- bds_metrics |>
          dplyr::filter(
            if (is.null(input$topic_name) ||
              "All topics" %in% input$topic_name ||
              "" %in% input$topic_name) {
              TRUE
            } else {
              .data$Topic %in% input$topic_name # Filter by selected topic(s)
            }
          ) |>
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

    indicator_label <- "Indicator:"
    topic_label <- "Topic:"
    la_label <- "LA:"

    # Dynamically update the Indicator label with the related topic
    shiny::observeEvent(debounced_indicator_name(), {
      indicator <- debounced_indicator_name()

      if (!is.null(indicator) && indicator != "") {
        # Find the topic related to the selected indicator
        related_topic <- topic_indicator_full |>
          dplyr::filter(.data$Measure == indicator) |>
          pull_uniques("Topic")

        # Use the first topic if multiple exist (unlikely but handled)
        if (length(related_topic) > 0) {
          indicator_label <- paste0(
            "Indicator:&nbsp;&nbsp;<span style='font-size: 0.85em;'>",
            "(Topic: ",
            paste0(related_topic, collapse = ", "),
            ")</span>"
          )
          topic_label <- paste0(
            "Topic:&nbsp;&nbsp<span style='font-size: 0.85em; color: #ffffff00;'>",
            "(Topic:&nbsp;iiiiiiii",
            paste0(related_topic, collapse = ", "),
            ")</span>"
          )
          la_label <- paste0(
            "Local Authority:<span style='font-size: 0.85em; color: #ffffff00;'>",
            "&nbsp;&nbsp;",
            paste0(related_topic, collapse = ", "),
            ")</span>"
          )
        }

        # Update the Indicator label to include the topic
        shiny::updateSelectizeInput(
          session = session,
          inputId = "indicator_name",
          selected = indicator
        )
      }

      # Setting custom indicator lable (to include topics related to)
      shinyjs::html(
        "indicator_label",
        indicator_label
      )
      shinyjs::html(
        "topic_label",
        topic_label
      )
      shinyjs::html(
        "la_label",
        la_label
      )

      shared_values$indicator <- indicator
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
