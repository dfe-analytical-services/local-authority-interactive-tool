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

  # Region tables =============================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Regional Authorities"),
      bslib::card_body(
        # Region LA Table -------------------------------------------------------
        RegionLA_TableUI("la_table"),
        # Region Table ----------------------------------------------------------
        Region_TableUI("region_table"),
        # Region Stats Table ----------------------------------------------------
        Region_StatsTableUI("stats_table")
      )
    )
  ),

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
  # Extract selected LA, Topic and Indicator
  app_inputs <- appInputsServer("region_inputs")

  # Region tables =============================================================
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

  # Region stats table --------------------------------------------------------
  Region_StatsTableServer(
    "stats_table",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  # Region charts =============================================================
  # Region focus line chart ---------------------------------------------------
  Region_FocusLine_chartServer(
    "region_focus_line",
    app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  # Region multi-choice line chart --------------------------------------------
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
shinyApp(ui_mod, server_mod)
