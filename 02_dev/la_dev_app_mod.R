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
  shiny::includeCSS(here::here("www/dfe_shiny_gov_styles.css")),

  # Tab header ================================================================
  h1("Local Authority View"),


  # Start of app  =============================================================
  appInputsUI("la_inputs"),

  # LA Level Table ----------------------------------
  LA_LevelTableUI("la_table"),

  # LA Stats Table ----------------------------------
  LA_StatsTableUI("la_stats"),

  # LA Charts ----------------------------------
  LA_ChartUI("la_chart"),

  # LA Metadata ----------------------------------
  LA_LevelMetaUI("la_meta")
)


# Server
server_mod <- function(input, output, session) {
  # Getting inputs ----------------------------------
  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("la_inputs")


  # LA level table ----------------------------------
  LA_LevelTableServer(
    "la_table",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA stats table  ----------------------------------
  LA_StatsTableServer(
    "la_stats",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA line chart  ----------------------------------
  LA_LineChartServer(
    "la_chart",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA bar chart  ----------------------------------
  LA_BarChartServer(
    "la_chart",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA Meta
  LA_LevelMetaServer(
    "la_meta",
    app_inputs$indicator,
    metrics_clean
  )
}


# Run app
shinyApp(ui_mod, server_mod)
