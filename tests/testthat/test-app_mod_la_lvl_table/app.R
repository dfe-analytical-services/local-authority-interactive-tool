# Load global
shinytest2::load_app_env(app_dir = here::here())
source(here::here("global.R"))

options(shiny.testmode = TRUE)

# Create minimal app
minimal_ui <- shiny::fluidRow(
  title = "Minimal app",

  # User inputs
  appInputsUI("la_level"),

  # Main table
  LA_LevelTableUI("la_table")
)

minimal_server <- function(input, output, session) {
  # User Inputs
  shared_values <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL
  )
  app_inputs <- appInputsServer("la_level", shared_values)

  # Main table
  la_main_tbl <- LA_LevelTableServer(
    "la_table",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Export values for use in UI tests
  exportTestValues(
    la_main_tbl = la_main_tbl()
  )
}

shinyApp(minimal_ui, minimal_server)
