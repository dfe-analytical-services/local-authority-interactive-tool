# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)

# Load modules
list.files("R/lait_modules/", full.names = TRUE) |>
  purrr::walk(source)

# Main App UI
ui <- bslib::page_fillable(
  ## Other language dependencies ===============================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),
  tags$head(htmltools::includeScript("www/custom_js.js")),
  # Makes the remove button work
  reactable.extras::reactable_extras_dependency(),

  # Start of app ===============================================================
  titlePanel("Test Shiny App for Main Inputs"),
  sidebarLayout(
    sidebarPanel(
      Create_MainInputsUI("main_inputs")
    ),
    mainPanel(
      h4("Selected Inputs:"),
      verbatimTextOutput("selected_inputs")
    )
  )
)

# Main App Server
server <- function(input, output, session) {
  # Call the main inputs module
  main_inputs <- Create_MainInputsServer("main_inputs", bds_metrics)

  output$selected_inputs <- renderPrint({
    list(
      Geography = main_inputs$geog(),
      Topic = main_inputs$topic(),
      Indicator = main_inputs$indicator()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
