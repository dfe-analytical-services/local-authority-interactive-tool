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
  h1("Statistical Neighbour Level"),


  # Start of app  =============================================================
  appInputsUI("stat_n_inputs"),

  # Region tables =============================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Statistical Neighbours"),
      # Statistical Neighbour LA SNs Table ----------------------------------
      StatN_LASNsTableUI("stat_n_sns_table"),
      # Statistical Neighbour LA Geog Compare Table -------------------------
      StatN_GeogCompTableUI("stat_n_comp_table")
    )
  ),
  div(
    class = "well",
    # Statistical Neighbour Statistics Table ------------------------------
    StatN_StatsTableUI("stat_n_stats_table")
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
  app_inputs <- appInputsServer("stat_n_inputs", shared_values)

  # Statistical Neighbour tables ==============================================
  # LA statistical neighbours table -------------------------------------------
  StatN_LASNsTableServer(
    "stat_n_sns_table",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA geographic comparison table --------------------------------------------
  StatN_GeogCompTableServer(
    "stat_n_comp_table",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Statistics Table ----------------------------------------------------------
  StatN_StatsTableServer(
    "stat_n_stats_table",
    app_inputs,
    bds_metrics,
    stat_n_la,
    la_names_bds
  )
}


# Run app
shinyApp(ui_mod, server_mod)
