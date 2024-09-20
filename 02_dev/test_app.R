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

  # Start of app  =============================================================
  appInputsUI("region_level"),


  # Region multi chart
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "la_charts",
      Region_Multi_chartUI("region_multi_line")
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
    indicator = NULL
  )

  # LA
  la_app_inputs <- appInputsServer("la_level", shared_values)

  # Extract selected LA, Topic and Indicator
  region_app_inputs <- appInputsServer("region_level", shared_values)


  # Region multi chart
  Region_Multi_chartServer(
    "region_multi_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )
}


# Run app
shinyApp(ui_test, server_test)
