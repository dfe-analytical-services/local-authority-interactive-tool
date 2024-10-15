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


# UI
ui_test <- bslib::page_fillable(
  ## Custom CSS ===============================================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),

  # Tab header ================================================================
  h1("Regional Level"),
  appInputsUI("la_level"),


  # Region multi chart
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "la_charts",
      StatN_MultiLineChartUI("multi_line"),
      StatN_MultiBarChartUI("multi_bar")
    )
  )
)

# Server
server_test <- function(input, output, session) {
  # Getting inputs  ===========================================================
  # Create a reactiveValues object to store shared input values across pages
  shared_values <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL,
    chart_line_input = NULL,
    chart_bar_input = NULL
  )

  # LA
  app_inputs <- appInputsServer("la_level", shared_values)

  # Region multi chart
  StatN_MultiLineChartServer(
    "multi_line",
    app_inputs,
    bds_metrics,
    stat_n_la,
    shared_values
  )

  StatN_MultiBarChartServer(
    "multi_bar",
    app_inputs,
    bds_metrics,
    stat_n_la,
    shared_values
  )
}


# Run app
shinyApp(ui_test, server_test)
