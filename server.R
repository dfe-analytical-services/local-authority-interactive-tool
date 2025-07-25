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
  # Navigation ================================================================
  ## Main content left navigation ---------------------------------------------
  observeEvent(input$la_level, {
    bslib::nav_select("left_nav", selected = "la_level")
  })
  observeEvent(input$regional_level, {
    bslib::nav_select("left_nav", selected = "regional_level")
  })
  observeEvent(input$statistical_neighbour_level, {
    bslib::nav_select("left_nav", selected = "statistical_neighbour_level")
  })
  observeEvent(input$all_la_level, {
    bslib::nav_select("left_nav", selected = "all_la_level")
  })
  observeEvent(input$create_your_own, {
    bslib::nav_select("left_nav", selected = "create_your_own")
  })
  observeEvent(input$user_guide, {
    bslib::nav_select("left_nav", selected = "user_guide")
  })
  observeEvent(input$updates_and_sources, {
    bslib::nav_select("left_nav", selected = "updates_and_sources")
  })

  ## Footer links -------------------------------------------------------------
  observeEvent(input$dashboard, {
    bslib::nav_select("pages", "dashboard")
  })
  observeEvent(input$support, {
    bslib::nav_select("pages", "support")
  })
  observeEvent(input$accessibility_statement, {
    bslib::nav_select("pages", "accessibility_statement")
  })
  observeEvent(input$cookies_information, {
    bslib::nav_select("pages", "cookies_information")
  })

  ## Back links to main dashboard ---------------------------------------------
  observeEvent(input$support_to_dashboard, {
    bslib::nav_select("pages", "dashboard")
  })
  observeEvent(input$cookies_to_dashboard, {
    bslib::nav_select("pages", "dashboard")
  })
  observeEvent(input$accessibility_to_dashboard, {
    bslib::nav_select("pages", "dashboard")
  })

  # Bookmarking ===============================================================
  # This uses bookmarking to store input choices in the url.
  # All inputs are excluded by default, and inputs can be added explicitly
  # in the included_inputs variable below
  shiny::observe({
    # Include these inputs for bookmarking
    included_inputs <- c(
      "la_inputs-la_name",
      "la_inputs-indicator_name",
      "region_inputs-la_name",
      "region_inputs-indicator_name",
      "stat_n_inputs-la_name",
      "stat_n_inputs-indicator_name",
      "all_la_inputs-la_name",
      "all_la_inputs-indicator_name",
      "pages",
      "left_nav",
      "create_inputs-geog_input",
      "create_inputs-indicator",
      "create_inputs-la_group",
      "create_inputs-inc_regions",
      "create_inputs-inc_england",
      "year_range-year_range"
    )

    # Exclude all other inputs
    excluded_inputs <- setdiff(
      names(shiny::reactiveValuesToList(input)),
      included_inputs
    )

    # Set the excluded inputs for bookmarking
    shiny::setBookmarkExclude(excluded_inputs)
  })

  shiny::observe({
    # Trigger this observer every time an input changes
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })

  shiny::onBookmarked(function(url) {
    # Update the query string with the bookmark URL
    shiny::updateQueryString(url, mode = "replace")
  })


  # Update title ==============================================================
  # This changes the title based on the tab selections and is important for accessibility
  # If on the main dashboard it uses the active tab from left_nav, else it uses the page input
  # Define the lookup vector for titles
  nav_titles <- c(
    "la_level" = "LA Level",
    "regional_level" = "Regional Level",
    "statistical_neighbour_level" = "Statistical Neighbour Level",
    "all_la_level" = "All LA Level",
    "create_your_own" = "Create Your Own",
    "user_guide" = "User Guide",
    "updates_and_sources" = "Updates and Data Sources",
    "support" = "Support and Feedback",
    "accessibility_statement" = "Accessibility Statement",
    "cookies_information" = "Cookies Information",
    "dashboard" = "Dashboard"
  )

  shiny::observe({
    if (input$pages == "dashboard") {
      if (input$left_nav %in% c(
        "la_level", "regional_level",
        "statistical_neighbour_level", "all_la_level"
      )) {
        shinytitle::change_window_title(
          title = paste0(
            site_title,
            " - ",
            nav_titles[input$left_nav],
            ": ",
            la_app_inputs$la(),
            ", ",
            la_app_inputs$indicator()
          )
        )
      } else {
        shinytitle::change_window_title(
          title = paste0(
            site_title, " - ",
            nav_titles[input$left_nav]
          )
        )
      }
    } else {
      shinytitle::change_window_title(
        title = paste0(
          site_title, " - ",
          nav_titles[input$pages]
        )
      )
    }
  })

  # Cookies logic =============================================================
  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key,
    cookies_link_panel = "cookies_information",
    cookies_nav_id = "pages"
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key,
  )

  # ===========================================================================
  # Start of LAIT
  # ===========================================================================

  # reactiveValues object to store shared input values across pages
  shared_page_inputs <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL
  )

  # ===========================================================================
  # LA Level Page
  # ===========================================================================
  # User Inputs ===============================================================
  la_app_inputs <- appInputsServer(
    "la_inputs",
    shared_page_inputs,
    topic_indicator_full
  )

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
    stat_n_la,
    no_qb_indicators
  )

  # LA level charts ===========================================================
  # LA line chart
  la_linechart <- LA_LineChartServer(
    "la_line_chart",
    la_app_inputs,
    bds_metrics,
    stat_n_la,
    covid_affected_data
  )

  # LA bar chart
  la_barchart <- LA_BarChartServer(
    "la_bar_chart",
    la_app_inputs,
    bds_metrics,
    stat_n_la,
    covid_affected_data
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
  region_app_inputs <- appInputsServer(
    "region_inputs",
    shared_page_inputs,
    topic_indicator_full
  )

  # Header
  PageHeaderServer("region_header", region_app_inputs, "Regional View")

  # Region tables =============================================================
  # Region LA table -----------------------------------------------------------
  RegionLA_TableServer(
    "region_tables",
    region_app_inputs,
    bds_metrics,
    stat_n_geog
  )

  # Region table --------------------------------------------------------------
  Region_TableServer(
    "region_tables",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region stats table --------------------------------------------------------
  Region_StatsTableServer(
    "region_stats_mod",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds
  )

  # Region charts =============================================================
  # Shared inputs for Region multi-choice charts
  region_shared_inputs <- reactiveValues(
    chart_line_input = NULL,
    chart_bar_input = NULL
  )

  # Region focus line chart ---------------------------------------------------
  Region_FocusLineChartServer(
    "region_focus_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds,
    covid_affected_data
  )

  # Region multi-choice line chart --------------------------------------------
  Region_MultiLineChartServer(
    "region_multi_line",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds,
    region_shared_inputs,
    covid_affected_data
  )

  # Region focus bar chart ---------------------------------------------------
  Region_FocusBarChartServer(
    "region_focus_bar",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds,
    covid_affected_data
  )

  # Region multi-choice bar chart ---------------------------------------------
  Region_MultiBarChartServer(
    "region_multi_bar",
    region_app_inputs,
    bds_metrics,
    stat_n_geog,
    region_names_bds,
    region_shared_inputs,
    covid_affected_data
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
  stat_n_app_inputs <- appInputsServer(
    "stat_n_inputs",
    shared_page_inputs,
    topic_indicator_full
  )

  # Header
  PageHeaderServer("stat_n_header", stat_n_app_inputs, "Statistical Neighbour View")

  # Statistical Neighbour tables ==============================================
  # LA statistical neighbours table -------------------------------------------
  StatN_LASNsTableServer(
    "stat_n_tables",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # LA geographic comparison table --------------------------------------------
  StatN_GeogCompTableServer(
    "stat_n_tables",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Statistics Table ----------------------------------------------------------
  StatN_StatsTableServer(
    "stat_n_stats_mod",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    la_names_bds,
    no_qb_indicators
  )

  # Statistical Neighbour charts ==============================================
  # Shared inputs for Statistical Neighbour multi-choice charts
  stat_n_shared_inputs <- reactiveValues(
    chart_line_input = NULL,
    chart_bar_input = NULL
  )

  # Focus line chart ----------------------------------------------------------
  StatN_FocusLineChartServer(
    "stat_n_focus_line",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    covid_affected_data
  )

  # Multi-choice line chart ---------------------------------------------------
  StatN_MultiLineChartServer(
    "stat_n_multi_line",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    stat_n_shared_inputs,
    covid_affected_data
  )

  # Focus bar chart -----------------------------------------------------------
  StatN_FocusBarChartServer(
    "stat_n_focus_bar",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    covid_affected_data
  )

  # Multi-choice bar chart ----------------------------------------------------
  StatN_MultiBarChartServer(
    "stat_n_multi_bar",
    stat_n_app_inputs,
    bds_metrics,
    stat_n_la,
    stat_n_shared_inputs,
    covid_affected_data
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
  all_la_app_inputs <- appInputsServer(
    "all_la_inputs",
    shared_page_inputs,
    topic_indicator_full
  )

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

  # ===========================================================================
  # Create Your Own Page
  # ===========================================================================
  # User Inputs ===============================================================
  # Create own main inputs ----------------------------------------------------
  create_inputs <- Create_MainInputsServer(
    "create_inputs",
    topic_indicator_full
  )

  # Year range input ----------------------------------------------------------
  year_input <- YearRangeServer(
    "year_range",
    bds_metrics,
    create_inputs$indicator,
    create_inputs$clear_selections
  )

  # Logic to create own =======================================================
  # Geog Groupings ------------------------------------------------------------
  geog_groups <- GroupingInputServer(
    "geog_groups",
    create_inputs,
    la_names_bds,
    region_names_bds,
    stat_n_geog,
    stat_n_la
  )

  # Staging Table =============================================================
  # Filtering BDS for staging data --------------------------------------------
  staging_bds <- StagingBDSServer(
    "staging_bds",
    create_inputs,
    geog_groups,
    year_input,
    bds_metrics
  )

  # Build staging data --------------------------------------------------------
  staging_data <- StagingDataServer(
    "staging_data",
    create_inputs,
    staging_bds,
    region_names_bds,
    la_names_bds,
    stat_n_la
  )

  # Output staging table ------------------------------------------------------
  StagingTableServer(
    "staging_table",
    create_inputs,
    region_names_bds,
    la_names_bds,
    stat_n_la,
    geog_groups,
    year_input,
    bds_metrics
  )

  # Query Table ===============================================================
  # Building query data -------------------------------------------------------
  query_data <- QueryDataServer(
    "query_data",
    create_inputs,
    geog_groups,
    year_input,
    staging_data
  )

  # Output query table --------------------------------------------------------
  query_table <- QueryTableServer(
    "query_table",
    query_data
  )

  # Create Own Table ==========================================================
  CreateOwnTableServer(
    "create_own_table",
    query_table,
    bds_metrics
  )

  # Create Own Charts =========================================================
  # Line chart ----------------------------------------------------------------
  CreateOwnLineChartServer(
    "create_own_line",
    query_table,
    bds_metrics,
    covid_affected_data
  )

  # Bar chart -----------------------------------------------------------------
  CreateOwnBarChartServer(
    "create_own_bar",
    query_table,
    bds_metrics,
    covid_affected_data
  )


  # Updates and Data Sources ==================================================
  # Latest indicator update notification
  LatestDataUpdateServer(
    "latest_indicator_update",
    metrics_clean
  )

  # Latest development update
  LatestDevUpdateServer(
    "latest_dev_update",
    development_update_log
  )

  # Indicator information table
  IndicatorInfoTableServer(
    "indicator_info_table",
    metrics_clean
  )

  # Useful links
  UsefulLinksServer(
    "useful_links",
    useful_links
  )

  # Extras ====================================================================
  # Copy-to-clipboard pop-up notification
  CopyToClipboardPopUpServer("copy-to-clipboard")
}
