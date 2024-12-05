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
          p("The Department has developed the Local Authority Interactive Tool
              (LAIT) to provide easy access to a wide range of data related to
              children and young people sourced from various departments across
              government.The app is designed and maintained by the DFE's Regions
              Group LA Performance & Data (LAPD) Team."),
          p(
            "We might want to add some brief introductory text here alongside
              some links to different tabs within your dashboard. Here's an
              example of a link working:",
            InternalLinkUI("la_level_link")
          ),


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


# Module UI
IndicatorInfoTableUI <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("indicator_info_table"))
}

# Module Server
IndicatorInfoTableServer <- function(id, metrics_data) {
  moduleServer(
    id,
    function(input, output, session) {
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
          dplyr::mutate(
            `Hyperlink(s) (opens in new tab)` = as.character(dfeshiny::external_link(
              href = `Hyperlink(s)`,
              link_text = Measure,
              add_warning = FALSE
            ))
          ) |>
          dplyr::ungroup() |>
          order_alphabetically(Measure)

        # Output table
        dfe_reactable(
          indicator_info,
          columns = list(
            `Hyperlink(s)` = reactable::colDef(
              show = FALSE
            )
          ),
          defaultPageSize = 5,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(5, 10, 25),
          compact = TRUE,
          searchable = TRUE
        )
      })
    }
  )
}
