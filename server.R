# -----------------------------------------------------------------------------
# This is the server file.
#
# Use it to create interactive elements like tables, charts and text for your
# app.
#
# Anything you create in the server file won't appear in your app until you call
# it in the UI file. This server script gives examples of plots and value boxes
#
# There are many other elements you can add in too, and you can play around with
# their reactivity. The "outputs" section of the shiny cheatsheet has a few
# examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # Bookmarking ===============================================================
  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  shiny::setBookmarkExclude(c(
    "cookies", "link_to_app_content_tab",
    "tabBenchmark_rows_current", "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected", "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected", "tabBenchmark_search",
    "tabBenchmark_rows_selected", "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A", "plotly_hover-A", "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts"
  ))

  shiny::observe({
    # Trigger this observer every time an input changes
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })

  shiny::onBookmarked(function(url) {
    shiny::updateQueryString(url)
  })

  # shiny::observe({
  #   if (input$navlistPanel == "Example tab 1") {
  #     shinytitle::change_window_title(
  #       session,
  #       paste0(
  #         site_title, " - ",
  #         input$selectPhase, ", ",
  #         input$selectArea
  #       )
  #     )
  #   } else {
  #     shinytitle::change_window_title(
  #       session,
  #       paste0(
  #         site_title, " - ",
  #         input$navlistPanel
  #       )
  #     )
  #   }
  # })

  # Cookies logic =============================================================
  output$cookie_status <- dfeshiny::cookie_banner_server(
    "cookie-banner",
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key,
    cookie_link_panel = "cookies_panel_ui"
  )

  dfeshiny::cookies_panel_server(
    id = "cookie-panel",
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  # ===========================================================================
  # Start of LAIT
  # ===========================================================================

  # ===========================================================================
  # LA Level Page
  # ===========================================================================

  # User Inputs ===============================================================
  la_app_inputs <- appInputsServer("la_level")

  # LA level tables ===========================================================
  # Main table
  la_main_tbl <- LA_LevelTableServer(
    "la_table",
    la_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Stats table
  LA_StatsTableServer(
    "la_stats",
    la_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA level charts ===========================================================
  # Line chart
  la_linechart <- LA_LineChartServer(
    "la_chart",
    la_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Bar chart
  la_barchart <- LA_BarChartServer(
    "la_chart",
    la_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA Metadata ===============================================================
  LA_LevelMetaServer(
    "la_meta",
    la_app_inputs$indicator,
    metrics_clean
  )

  # Export values for use in UI tests
  shiny::exportTestValues(
    la_main_tbl = la_main_tbl(),
    la_linechart = la_linechart(),
    la_barchart = la_barchart()
  )


  # ===========================================================================
  # Regional Level Page
  # ===========================================================================
  # User Inputs ===============================================================
  region_app_inputs <- appInputsServer("region_level")

  # Region tables =============================================================
  # Region LA table -----------------------------------------------------------
  RegionLA_TableServer(
    "region_la_table",
    region_app_inputs,
    bds_metrics,
    stat_n_geog
  )

  # Region table --------------------------------------------------------------
  Region_TableServer(
    "region_table",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  # Region stats table --------------------------------------------------------
  Region_StatsTableServer(
    "stats_table",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  # Region charts =============================================================
  # Region focus line chart ---------------------------------------------------
  Region_FocusLine_chartServer(
    "region_focus_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  # Region multi-choice line chart --------------------------------------------
  Region_Multi_chartServer(
    "region_multi_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    national_names_bds,
    region_names_bds
  )

  # Stop app ------------------------------------------------------------------
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}
