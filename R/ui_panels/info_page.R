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
          h1("Information Page"),
          p(
            "This page provides easy access to information on data updates,
              development changes and resources related to the Local Authority
              Interactive Tool (LAIT)."
          ),

          # Latest Updates =====================================================
          h2("Latest Updates"),
          p(
            "The section below highlights the latest updates related to
              both data and development.
              Please note that 'latest' updates may not always be time recent,
              so be sure to check the update dates."
          ),
          # Data updates -------------------------------------------------------
          LatestDataUpdateUI("latest_indicator_update"),
          br(),
          # Development updates ------------------------------------------------
          LatestDevUpdateUI("latest_dev_update"),
          br(),

          # Indicators metadata ===============================================
          h2("Indicator Information"),
          p(
            "The table below displays key metadata for each indicator.
              Use this to quickly locate and explore indicators of interest."
          ),
          p(
            "While this information is available in the metadata section of
              each indicator, it is consolidated here for convenience."
          ),
          IndicatorInfoTableUI("indicator_info_table"),
          br(),


          # Guidance sources ===================================================
          h2("Links to related or useful resources"),
          p("Below is a list of links to other resources that may be of use:")
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
          Topic,
          Measure,
          `Data Owner (DO) /Supplier and Contact Details`,
          `Last Update`,
          `Next Update`,
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
        # Adding reactableOutput for the table
        reactable::reactableOutput(ns("latest_update_table"))
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
      dplyr::select(Indicator = Measure, `Last Update`) |>
      order_alphabetically(Indicator)

    # Render the reactable table with scrollable rows
    output$latest_update_table <- reactable::renderReactable({
      dfe_reactable(
        latest_updated_indicator,
        pagination = FALSE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE,
        height = "220px",
        searchable = TRUE
      )
    })
  })
}




LatestDevUpdateUI <- function(id) {
  ns <- NS(id)

  shinyGovstyle::noti_banner(
    inputId = ns("latest_update_indicator"),
    title_txt = "Latest development updates",
    body_txt = as.character(
      shiny::tagList(
        shiny::p("There has been a development update..."),
        shiny::uiOutput(ns("latest_update_table")),
        shiny::br(),
        shiny::HTML(paste0(
          "Please visit the ",
          dfeshiny::external_link(
            href = "https://github.com/dfe-analytical-services/local-authority-interactive-tool",
            link_text = "LAIT GitHub",
            add_warning = TRUE
          ),
          " to find more information."
        ), )
      )
    )
  )
}


LatestDevUpdateServer <- function(id, dev_update_log) {
  moduleServer(id, function(input, output, session) {
    # Get most recent development update
    latest_dev_update <- dev_update_log |>
      dplyr::filter(Date == max(Date))

    # Render the UI
    output$latest_update_table <- shiny::renderUI({
      htmltools::tags$div(
        style = "width: 100%;",
        htmltools::tags$p(
          htmltools::tags$b("Type:"),
          paste(latest_dev_update$Type)
        ),
        htmltools::tags$p(
          htmltools::tags$b("Summary:"),
          paste(latest_dev_update$Summary)
        ),
        htmltools::tags$p(
          htmltools::tags$b("Details:"),
          htmltools::tags$br(),
          htmltools::tags$span(
            latest_dev_update$Details
          )
        ),
        htmltools::tags$p(
          htmltools::tags$b("Date updated:"),
          paste(latest_dev_update$Date)
        )
      )
    })
  })
}
