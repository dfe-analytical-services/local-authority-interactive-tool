info_page_panel <- function() {
  shiny::tabPanel(
    "Information Page",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          shinyGovstyle::banner(
            "beta banner",
            "beta",
            paste0(
              "This page is in beta phase and we are still reviewing the content.
               We will provide a much more detailed user guide when the tool is
               published."
            )
          ),
          shiny::br(),
          h1("Local Authority Interactive Tool"),


          # Latest Updates =====================================================
          h2("Latest Updates"),
          LatestDataUpdateUI("latest_indicator_update"),
          br(),

          # Indicators metadata ===============================================
          h2("Indicator information"),
          IndicatorInfoTableUI("indicator_info_table"),


          # Guidance sources ===================================================
          h2("Guidance sources"),
          p("For example, here we'll add some of the key resources we draw
                  on to guide styling and visualisation...")
        )
      )
    )
  )
}


# Display Indicator Information table
IndicatorInfoTableUI <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("indicator_info_table"))
}


# Compute Indicator Information table
IndicatorInfoTableServer <- function(id, metrics_data) {
  moduleServer(id, function(input, output, session) {
    output$indicator_info_table <- reactable::renderReactable({
      # Select columns to show indicator information
      indicator_info <- metrics_data |>
        dplyr::select(
          Measure,
          Topic,
          `Last Update`,
          `Next Update`,
          `Data Owner (DO) /Supplier and Contact Details`,
          `Hyperlink(s)`
        ) |>
        # Convert to nice looking links
        dplyr::rowwise() |>
        dplyr::mutate(`Hyperlink(s) (opens in new tab)` = as.character(
          dfeshiny::external_link(
            href = `Hyperlink(s)`,
            link_text = Measure,
            add_warning = FALSE
          )
        )) |>
        dplyr::ungroup() |>
        order_alphabetically(Measure)

      # Output table
      dfe_reactable(
        indicator_info,
        columns = list(`Hyperlink(s)` = reactable::colDef(show = FALSE)),
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 25),
        compact = TRUE,
        searchable = TRUE
      )
    })
  })
}



LatestDataUpdateUI <- function(id) {
  ns <- NS(id)

  shinyGovstyle::noti_banner(
    inputId = ns("latest_update_indicator"),
    title_txt = "Latest updated indicator(s)",
    body_txt = as.character(
      shiny::tagList(
        shiny::p("These indicators were most recently updated:"),
        shiny::uiOutput(ns("latest_update_table"))
      )
    )
  )
}


LatestDataUpdateServer <- function(id, metrics_data) {
  moduleServer(id, function(input, output, session) {
    # Prepare the data
    latest_updated_indicator <- metrics_data |>
      dplyr::mutate(latest_update_date = as.Date(paste(`Last Update`, "01"),
        format = "%B %Y %d"
      )) |>
      dplyr::filter(latest_update_date == max(latest_update_date)) |>
      dplyr::select(Measure, `Last Update`)

    # Render the UI
    output$latest_update_table <- shiny::renderUI({
      # Create an HTML table with invisible styling
      htmltools::tags$table(
        style = "width: 100%; border-collapse: collapse;",
        # Iterate over each row in the data
        htmltools::tags$tbody(
          lapply(1:nrow(latest_updated_indicator), function(i) {
            htmltools::tags$tr(
              htmltools::tags$td(
                latest_updated_indicator$Measure[i],
                style = "text-align: left; padding: 5px;"
              ),
              htmltools::tags$td(
                latest_updated_indicator$`Last Update`[i],
                style = "text-align: right; padding: 5px;"
              )
            )
          })
        )
      )
    })
  })
}
