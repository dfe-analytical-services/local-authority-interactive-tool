example_tab_1_panel <- function() {
  shiny::tabPanel(
    "Example tab 1",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          width = 12,
          h1("Overall content title for this dashboard page"),
        ),
        # Expandable section --------------------------------------------------
        shiny::column(
          width = 12,
          expandable(
            input_id = "details", label = textOutput("dropdown_label"),
            contents =
              div(
                id = "div_a",
                # User selection dropdowns ------------------------------------
                shinyGovstyle::gov_row(
                  shiny::column(
                    width = 6,
                    shiny::selectizeInput("selectPhase",
                      "Select a school phase",
                      choices = choices_phase
                    )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::selectizeInput(
                      inputId = "selectArea",
                      label = "Choose an area:",
                      choices = choices_areas$area_name
                    )
                  ),
                  # Download button -------------------------------------------
                  shiny::column(
                    width = 12,
                    paste("Download the underlying data for this dashboard:"),
                    br(),
                    shiny::downloadButton(
                      outputId = "download_data",
                      label = "Download data",
                      icon = shiny::icon("download"),
                      class = "downloadButton"
                    )
                  )
                )
              )
          ),
        ),
        # Tabset under dropdowns ----------------------------------------------
        shiny::column(
          width = 12,
          shiny::tabsetPanel(
            id = "tabsetpanels",
            # Value boxes tab -------------------------------------------------
            shiny::tabPanel(
              "Valuebox example",
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  h2("Examples of producing value boxes in R-Shiny"),
                  shiny::fluidRow(
                    shiny::column(
                      width = 12,
                      shinydashboard::valueBoxOutput("box_balance_latest", width = 6),
                      shinydashboard::valueBoxOutput("box_balance_change", width = 6)
                    )
                  )
                )
              )
            ),
            # Timeseries tab --------------------------------------------------
            shiny::tabPanel(
              "Line chart example",
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  h2("An example line chart using ggplot and ggiraph"),
                  ggiraph::girafeOutput("lineRevBal", width = "100%", height = "100%")
                )
              )
            ),
            # Benchmarking tab ------------------------------------------------
            shiny::tabPanel(
              "Benchmarking example",
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  h2("An example bar chart using ggplot and ggiraph"),
                  p("This is the standard paragraph style for adding guiding
                    info around data content."),
                  # Bar chart for benchmarking --------------------------------
                  shiny::column(
                    width = 6,
                    girafeOutput("colBenchmark",
                      width = "100%", height = "100%"
                    )
                  ),
                  shiny::column(
                    width = 6,
                    div(
                      class = "well",
                      style = "min-height: 100%; height: 100%; overflow-y:
                      visible",
                      shiny::fluidRow(
                        # Benchmarking dropdown selection ---------------------
                        shiny::column(
                          width = 12,
                          shiny::selectizeInput("selectBenchLAs",
                            "Select benchmark local authorities",
                            choices = choices_las$area_name,
                            multiple = TRUE,
                            options = list(maxItems = 3)
                          )
                        )
                      )
                    ),
                    # Benchmarking table --------------------------------------
                    DT::dataTableOutput("tabBenchmark")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
