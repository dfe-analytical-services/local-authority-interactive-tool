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

  # Tab header ================================================================
  h1("Regional Level"),


  # Start of app  =============================================================
  appInputsUI("region_inputs"),

  # Region tables =============================================================
  RegionLevel_TableUI("region_tables"),

  # Region charts =============================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "region_charts",
      Region_FocusLine_chartUI("region_focus_line"),
      Region_Multi_chartUI("region_multi_line")
    )
  )
)


# Server
server_mod <- function(input, output, session) {
  # Getting inputs  ===========================================================
  # reactiveValues object to store shared input values across pages
  shared_values <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL
  )

  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("region_inputs", shared_values)

  # Region tables =============================================================
  # Region LA table -----------------------------------------------------------
  RegionLA_TableServer(
    "region_tables",
    app_inputs,
    bds_metrics,
    stat_n_geog
  )

  # Region table --------------------------------------------------------------
  Region_TableServer(
    "region_tables",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region stats table --------------------------------------------------------
  Region_StatsTableServer(
    "stats_table_mod",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region charts =============================================================
  # Region focus line chart ---------------------------------------------------
  Region_FocusLine_chartServer(
    "region_focus_line",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region multi-choice line chart --------------------------------------------
  Region_Multi_chartServer(
    "region_multi_line",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )
}


# Run app
shinyApp(ui_mod, server_mod)
