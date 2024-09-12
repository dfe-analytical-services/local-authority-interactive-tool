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
  h1("Regional Level"),


  # Start of app  =============================================================
  appInputsUI("region_inputs"),

  # Region LA Table ----------------------------------
  RegionLA_TableUI("la_table")
)


# Server
server_mod <- function(input, output, session) {
  # Getting inputs ----------------------------------
  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("region_inputs")


  # Region LA table ----------------------------------
  RegionLA_TableServer(
    "la_table",
    app_inputs,
    bds_metrics,
    stat_n_geog
  )
}


# Run app
shinyApp(ui_mod, server_mod)
