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
ui_mod <- bslib::page_fillable(
  ## Custom CSS ===============================================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),

  # Tab header ================================================================
  h1("All Local Authorities"),


  # Start of app  =============================================================
  appInputsUI("all_la_inputs"),

  # LA Level Table ------------------------------------------------------------
  AllLA_TableUI("all_la_table")
)


# Server
server_mod <- function(input, output, session) {
  # Getting inputs  ===========================================================
  # reactiveValues object to store shared input values across pages
  shared_values <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL,
    chart_line_input = NULL,
    chart_bar_input = NULL
  )

  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer(
    "all_la_inputs",
    shared_values,
    topic_indicator_full
  )


  # LA and Region table -------------------------------------------------------
  AllLA_TableServer(
    "all_la_table",
    app_inputs,
    bds_metrics,
    la_names_bds
  )
}


# Run app
shinyApp(ui_mod, server_mod)
