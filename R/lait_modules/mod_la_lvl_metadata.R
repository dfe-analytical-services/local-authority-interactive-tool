# nolint start: object_name
#
#' UI module for displaying metadata
#'
#' @param id A character string that is used as the namespace for the module's
#' input and output.
#' @return A UI element that displays the metadata.
#'
MetadataUI <- function(id) {
  ns <- NS(id)

  shinycssloaders::withSpinner(
    uiOutput(ns("metadata")),
    type = 7,
    color = "#0b0c0c",
    size = 0.6,
    proxy.height = "10px"
  )
}


#' Server module for fetching and rendering metadata
#'
#' @param id A character string that is used as the namespace for the module's
#' input and output.
#' @param indicator_input A reactive expression that returns the
#' current indicator.
#' @param data_metrics A data frame that contains the metrics data.
#' @param metadata_type A character string that specifies the type of
#' metadata to fetch.
#' @return A server-side module that fetches and renders the metadata.
#'
MetadataServer <- function(id, indicator_input, data_metrics, metadata_type) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store the previous metadata
    previous_metadata <- reactiveVal(NULL)

    output$metadata <- renderUI({
      # If indicator_input is NULL, return the previous metadata
      if (is.null(indicator_input()) || indicator_input() == "") {
        return(previous_metadata())
      }

      metadata <- data_metrics |>
        get_metadata(indicator_input(), metadata_type)

      if (grepl("link", metadata_type)) {
        label <- indicator_input()
        metadata <- dfeshiny::external_link(href = metadata, link_text = label)
      }

      # Collapse multiple newlines and limit <br> tags
      if (is.character(metadata)) {
        metadata <- gsub("\r\n|\n", "\n", metadata) # Normalize newlines
        metadata <- gsub("\n{2,}", "<br><br>", metadata) # Replace multiple newlines with a single <br><br>
        metadata <- gsub("\n", "", metadata) # Remove stray newlines
        metadata <- HTML(metadata)
      }

      # Update the previous metadata value
      previous_metadata(metadata)

      metadata
    })
  })
}


#' UI module for displaying metadata at the LA level
#'
#' @param id A character string that is used as the namespace for
#' the module's input and output.
#' @return A UI element that displays the LA level metadata.
#'
LA_LevelMetaUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_body(
        h3("Description:"),
        MetadataUI(ns("description")),
        h3("Methodology:"),
        MetadataUI(ns("methodology")),
        div(
          # Creates a flex container where the items are centered vertically
          style = "display: flex; align-items: baseline;",
          h3("Last Updated:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          MetadataUI(ns("last_update"))
        ),
        div(
          style = "display: flex; align-items: baseline;",
          h3("Next Updated:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          MetadataUI(ns("next_update"))
        ),
        div(
          style = "display: flex; align-items: baseline;",
          h3("Source:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          MetadataUI(ns("source"))
        )
      )
    )
  )
}


#' Server module for fetching and rendering metadata at the LA level
#'
#' @param id A character string that is used as the namespace for the
#' module's input and output.
#' @param indicator_input A reactive expression that returns the
#' current indicator.
#' @param data_metrics A data frame that contains the metrics data.
#' @return A server-side module that fetches and renders the LA level metadata.
LA_LevelMetaServer <- function(id, indicator_input, data_metrics) {
  moduleServer(id, function(input, output, session) {
    # Pass the indicator_input reactive expression itself (without calling it)
    output$description <- MetadataServer(
      "description",
      indicator_input,
      data_metrics,
      "Description"
    )

    output$methodology <- MetadataServer(
      "methodology",
      indicator_input,
      data_metrics,
      "Methodology"
    )

    output$last_update <- MetadataServer(
      "last_update",
      indicator_input,
      data_metrics,
      "Last Update"
    )

    output$next_update <- MetadataServer(
      "next_update",
      indicator_input,
      data_metrics,
      "Next Update"
    )

    output$source <- MetadataServer(
      "source",
      indicator_input,
      data_metrics,
      "Hyperlink(s)"
    )
  })
}

# nolint end
