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
  AllLA_LATableUI("la_table"),

  # LA Stats Table ------------------------------------------------------------
  AllLA_RegionTableUI("la_stats"),
)


# Server
server_mod <- function(input, output, session) {
  # Getting inputs ------------------------------------------------------------
  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("all_la_inputs")


  # LA table ------------------------------------------------------------------
  AllLA_LATableServer(
    "la_table",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Region table  -------------------------------------------------------------
  AllLA_RegionTableServer(
    "la_stats",
    app_inputs,
    bds_metrics,
    stat_n_la
  )
}


# Run app
shinyApp(ui_mod, server_mod)
