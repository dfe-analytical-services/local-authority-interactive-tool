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


  # Start of app  =============================================================
  appInputsUI("region_inputs"),
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
  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("region_inputs")


  Region_Multi_chartServer(
    "region_multi_line",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )
}


# Run app
shinyApp(ui_test, server_test)
