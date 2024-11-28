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
  tags$head(htmltools::includeScript("www/custom_js.js")),
  shinyToastify::useShinyToastify(),
  shinyjs::useShinyjs(),

  # Tab header ================================================================
  h1("Local Authority View"),


  # Start of app  =============================================================
  appInputsUI("la_inputs"),

  # LA Level Table ----------------------------------
  LA_LevelTableUI("la_table"),

  # LA Stats Table ----------------------------------
  LA_StatsTableUI("la_stats"),

  # LA Charts ----------------------------------
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "la_charts",
      LA_LineChartUI("la_line_chart"),
      LA_BarChartUI("la_bar_chart")
    )
  ),

  # LA Metadata ----------------------------------
  LA_LevelMetaUI("la_meta")
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
    "la_inputs",
    shared_values,
    metrics_raw
  )

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
    stat_n_la,
    no_qb_indicators
  )

  # LA line chart  ----------------------------------
  LA_LineChartServer(
    "la_line_chart",
    app_inputs,
    bds_metrics,
    stat_n_la,
    covid_affected_data
  )

  # LA bar chart  ----------------------------------
  LA_BarChartServer(
    "la_bar_chart",
    app_inputs,
    bds_metrics,
    stat_n_la,
    covid_affected_data
  )

  # LA Meta
  LA_LevelMetaServer(
    "la_meta",
    app_inputs$indicator,
    metrics_clean
  )

  # Copy-to-clipboard pop-up notification
  CopyToClipboardPopUpServer("copy-to-clipboard")
}


# Run app
shinyApp(ui_mod, server_mod)
