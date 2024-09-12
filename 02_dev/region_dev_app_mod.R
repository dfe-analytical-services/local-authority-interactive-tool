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

  # Region LA Table -----------------------------------------------------------
  RegionLA_TableUI("la_table"),

  # Region Table --------------------------------------------------------------
  Region_TableUI("region_table"),

  # Region Stats Table --------------------------------------------------------
  Region_StatsTableUI("stats_table")
)


# Server
server_mod <- function(input, output, session) {
  # Getting inputs  ===========================================================
  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("region_inputs")

  # Region LA table -----------------------------------------------------------
  RegionLA_TableServer(
    "la_table",
    app_inputs,
    bds_metrics,
    stat_n_geog
  )

  # Region table --------------------------------------------------------------
  Region_TableServer(
    "region_table",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  Region_StatsTableServer(
    "stats_table",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )
}


# Run app
shinyApp(ui_mod, server_mod)
