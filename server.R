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

  # Dynamically changes window title to be LAIT - page - LA - indicator
  # (Selected by user)
  shiny::observe({
    if (input$navsetpillslist %in% c("LA Level", "Regional Level")) {
      shinytitle::change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$navsetpillslist, ": ",
          la_app_inputs$la(), ", ",
          la_app_inputs$indicator()
        )
      )
    } else {
      shinytitle::change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$navsetpillslist
        )
      )
    }
  })

  # Cookies logic =============================================================
  output$cookie_status <- dfeshiny::cookies_banner_server(
    "cookie-banner",
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key,
    cookies_link_panel = "cookies_panel_ui"
  )

  dfeshiny::cookies_panel_server(
    id = "cookie-panel",
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  # ===========================================================================
  # Start of LAIT
  # ===========================================================================

  # reactiveValues object to store shared input values across pages
  shared_values <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL,
    chart_line_input = NULL,
    chart_bar_input = NULL
  )

  # ===========================================================================
  # LA Level Page
  # ===========================================================================

  # User Inputs ===============================================================
  la_app_inputs <- appInputsServer("la_inputs", shared_values)

  # Page header
  PageHeaderServer("la_header", la_app_inputs, "Local Authority View")

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
  region_app_inputs <- appInputsServer("region_inputs", shared_values)

  # Header
  PageHeaderServer("region_header", region_app_inputs, "Regional View")

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
    region_names_bds
  )

  # Region stats table --------------------------------------------------------
  Region_StatsTableServer(
    "stats_table",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region charts =============================================================
  # Region focus line chart ---------------------------------------------------
  Region_FocusLine_chartServer(
    "region_focus_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region multi-choice line chart --------------------------------------------
  Region_Multi_chartServer(
    "region_multi_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region Metadata ===========================================================
  LA_LevelMetaServer(
    "region_meta",
    region_app_inputs$indicator,
    metrics_clean
  )

  # ===========================================================================
  # Statistical Neighbour Level Page
  # ===========================================================================
  # User Inputs ===============================================================
  stat_n_app_inputs <- appInputsServer("stat_n_inputs", shared_values)

  # Header
  PageHeaderServer("stat_n_header", stat_n_app_inputs, "Statistical Neighbour View")

  # Statistical Neighbour tables ==============================================
  # LA statistical neighbours table -------------------------------------------
  StatN_LASNsTableServer(
    "stat_n_sns_table",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA geographic comparison table --------------------------------------------
  StatN_GeogCompTableServer(
    "stat_n_comp_table",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Statistics Table ----------------------------------------------------------
  StatN_StatsTableServer(
    "stat_n_stats_table",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    la_names_bds
  )

  # Statistical Neighbour charts ==============================================
  # Focus line chart ----------------------------------------------------------
  StatN_FocusLineChartServer(
    "stat_n_focus_line",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Multi-choice line chart ---------------------------------------------------
  StatN_MultiLineChartServer(
    "stat_n_multi_line",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    shared_values
  )

  # Focus bar chart -----------------------------------------------------------
  StatN_FocusBarChartServer(
    "stat_n_focus_bar",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Multi-choice bar chart ----------------------------------------------------
  StatN_MultiBarChartServer(
    "stat_n_multi_bar",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    shared_values
  )

  # Statistical Neighbour Metadata ============================================
  LA_LevelMetaServer(
    "stat_n_meta",
    stat_n_app_inputs$indicator,
    metrics_clean
  )

  # ===========================================================================
  # All LA Level Page
  # ===========================================================================
  # User Inputs ===============================================================
  all_la_app_inputs <- appInputsServer("all_la_inputs", shared_values)

  # Header
  PageHeaderServer("all_la_header", all_la_app_inputs, "All LA View")

  # All LA tables =============================================================
  # LA and Region table -------------------------------------------------------
  AllLA_TableServer(
    "all_la_table",
    all_la_app_inputs,
    bds_metrics,
    la_names_bds
  )

  # All LA Metadata ===========================================================
  LA_LevelMetaServer(
    "all_la_meta",
    all_la_app_inputs$indicator,
    metrics_clean
  )


  # User guide ================================================================
  InternalLinkServer(
    "la_level_link",
    "LA Level",
    session
  )

  # Stop app ------------------------------------------------------------------
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}
