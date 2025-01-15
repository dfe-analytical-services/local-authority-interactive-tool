info_page_panel <- function() {
  shiny::tabPanel(
    "Updates and Sources",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          h1("Updates and Data Sources"),

          # Latest Updates =====================================================
          h2("Latest Updates"),
          p(
            "Please note that 'latest' updates may not always be time recent,
              so be sure to check the update dates."
          ),
          # Data updates -------------------------------------------------------
          LatestDataUpdateUI("latest_indicator_update"),
          br(),
          # Development updates ------------------------------------------------
          LatestDevUpdateUI("latest_dev_update"),
          br(),
          br(),

          # Indicators metadata ===============================================
          h2("Indicator Information"),
          p(
            "While this information is available in the metadata section of
              each indicator, it is consolidated here for convenience."
          ),
          IndicatorInfoTableUI("indicator_info_table"),
          br(),
          br(),


          # Guidance sources ===================================================
          h2(
            id = "useful_links_link",
            "Links to useful or related resources (opens in new tab)"
          ),
          UsefulLinksUI("useful_links")
        )
      )
    )
  )
}
