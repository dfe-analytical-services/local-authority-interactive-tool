# -----------------------------------------------------------------------------
# This is the ui file. Use it to call elements created in your server file into
# the app, and define where they are placed, and define any user inputs.
#
# Other elements like charts, navigation bars etc. are completely up to you to
# decide what goes in. However, every element should meet accessibility
# requirements and user needs.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for GOV.UK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#
# -----------------------------------------------------------------------------
ui <- function(input, output, session) {
  bslib::page_fillable(

    # Set application metadata ------------------------------------------------
    tags$head(HTML("<title>Local Authority Interactive Tool (LAIT)</title>")),
    tags$head(tags$link(rel = "shortcut icon", href = "dfefavicon.png")),
    tags$head(includeHTML(("google-analytics.html"))),
    shinytitle::use_shiny_title(),
    tags$html(lang = "en"),
    # Add meta description for search engines
    metathis::meta() |>
      metathis::meta_general(
        application_name = "Local Authority Interactive Tool (LAIT)",
        description = "Local Authority Interactive Tool (LAIT)",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "data tool",
        rating = "General",
        referrer = "no-referrer"
      ),

    # Custom disconnect function ----------------------------------------------
    # Variables used here are set in the global.R file
    dfeshiny::custom_disconnect_message(
      links = sites_list,
      publication_name = parent_pub_name,
      publication_link = parent_publication
    ),

    # Styling with CSS
    set_css_style_sheet("dfe_shiny_gov_style.css"),
    # Remove any gaps between elements
    gap = 0,

    # Load javascript dependencies --------------------------------------------
    shinyWidgets::useShinydashboard(),
    shinyjs::useShinyjs(),
    tags$head(htmltools::includeScript("www/custom_js.js")),
    reactable.extras::reactable_extras_dependency(),
    shinyToastify::useShinyToastify(),

    # Cookies -----------------------------------------------------------------
    # Setting up cookie consent based on a cookie recording the consent:
    # https://book.javascript-for-r.com/shiny-cookies.html
    dfeshiny::dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(
      "cookie-banner",
      "Local Authority Interactive Tool (LAIT)"
    ),

    # Header ------------------------------------------------------------------
    shinyGovstyle::header(
      main_text = "",
      main_link = "https://www.gov.uk/government/organisations/department-for-education",
      secondary_text = "Local Authority Interactive Tool (LAIT)",
      logo = "images/DfE_logo_landscape.png",
      logo_width = 150,
      logo_height = 32
    ),

    # Beta banner -------------------------------------------------------------
    shinyGovstyle::banner(
      "beta banner",
      "Beta",
      "This Dashboard is in beta phase and we are still reviewing performance
      and reliability."
    ),
    # Update message banner ---------------------------------------------------
    shinyGovstyle::banner(
      inputId = "update-msg-banner",
      type = "News",
      label = banner_update_msg
    ),

    # Start of app ============================================================

    # Nav panels --------------------------------------------------------------
    bslib::navset_pill_list(
      "",
      id = "navsetpillslist",
      widths = c(2, 10),
      well = FALSE,

      # =======================================================================
      # LA Level Page
      # =======================================================================
      bslib::nav_panel(
        shiny::hr(class = "mobile-only-hr"),
        title = "LA Level",
        value = "LA Level",

        # Tab header ==========================================================
        PageHeaderUI("la_header"),

        # User Inputs =========================================================
        appInputsUI("la_inputs"),

        # LA Tables ===========================================================
        # Main table
        LA_LevelTableUI("la_table"),

        # Stats table
        LA_StatsTableUI("la_stats"),

        # LA Charts ===========================================================
        div(
          class = "well",
          style = "overflow-y: visible;",
          bslib::navset_card_underline(
            id = "la_charts",
            LA_LineChartUI("la_line_chart"),
            LA_BarChartUI("la_bar_chart")
          )
        ),

        # LA Metadata =========================================================
        LA_LevelMetaUI("la_meta")
      ),

      # =======================================================================
      # Regional Level Page
      # =======================================================================
      bslib::nav_panel(
        shiny::hr(class = "mobile-only-hr"),
        title = "Regional Level",
        value = "Regional Level",

        # Tab header ==========================================================
        PageHeaderUI("region_header"),

        # User Inputs =========================================================
        appInputsUI("region_inputs"),

        # Region tables =======================================================
        RegionLevel_TableUI("region_tables"),

        # Region charts =======================================================
        div(
          class = "well",
          style = "overflow-y: visible;",
          bslib::navset_card_underline(
            id = "region_charts",
            Region_FocusLineChartUI("region_focus_line"),
            Region_MultiLineChartUI("region_multi_line"),
            Region_FocusBarChartUI("region_focus_bar"),
            Region_MultiBarChartUI("region_multi_bar")
          )
        ),

        # Region Metadata =====================================================
        LA_LevelMetaUI("region_meta")
      ),

      # =======================================================================
      # Statistical Neighbour Level Page
      # =======================================================================
      bslib::nav_panel(
        shiny::hr(class = "mobile-only-hr"),
        title = "Statistical Neighbour Level",
        value = "Statistical Neighbour Level",

        # Tab header ==========================================================
        PageHeaderUI("stat_n_header"),

        # User Inputs =========================================================
        appInputsUI("stat_n_inputs"),

        # Statistical Neighbour tables ========================================
        StatN_TablesUI("stat_n_tables"),

        # Statistical Neighbour charts ========================================
        div(
          class = "well",
          style = "overflow-y: visible;",
          bslib::navset_card_underline(
            id = "stat_n_charts",
            StatN_FocusLineChartUI("stat_n_focus_line"),
            StatN_MultiLineChartUI("stat_n_multi_line"),
            StatN_FocusBarChartUI("stat_n_focus_bar"),
            StatN_MultiBarChartUI("stat_n_multi_bar")
          )
        ),

        # Statistical Neighbour Metadata ======================================
        LA_LevelMetaUI("stat_n_meta")
      ),

      # =======================================================================
      # All LA Level Page
      # =======================================================================
      bslib::nav_panel(
        shiny::hr(class = "mobile-only-hr"),
        title = "All LA Level",
        value = "All LA Level",

        # Tab header ==========================================================
        PageHeaderUI("all_la_header"),

        # User Inputs =========================================================
        appInputsUI("all_la_inputs"),

        # All LA Tables =======================================================
        AllLA_TableUI("all_la_table"),

        # LA Metadata =========================================================
        LA_LevelMetaUI("all_la_meta")
      ),

      # =======================================================================
      # Create Your Own Page
      # =======================================================================
      bslib::nav_panel(
        title = "Create Your Own",
        value = "Create Your Own",
        # Full dataset notification banner
        full_data_on_github_noti(),
        # User Inputs =========================================================
        div(
          class = "well",
          style = "overflow-y: visible; padding: 1rem;",
          bslib::layout_column_wrap(
            Create_MainInputsUI("create_inputs")["Main choices"],
          ),
          bslib::layout_column_wrap(
            Create_MainInputsUI("create_inputs")["LA grouping"],
            Create_MainInputsUI("create_inputs")["Other grouping"],
            YearRangeUI("year_range"),
            Create_MainInputsUI("create_inputs")["Clear all current selections"]
          )
        ),

        # Tables ==============================================================
        # Staging table & Add selections btn ----------------------------------
        StagingTableUI("staging_table"),
        # Query table ---------------------------------------------------------
        QueryTableUI("query_table"),
        # Create own table ----------------------------------------------------
        CreateOwnTableUI("create_own_table"),
        # Charts ==============================================================
        div(
          class = "well",
          style = "overflow-y: visible;",
          h3(
            "Output Charts",
            create_tooltip_icon("Charts showing data from all the saved selections")
          ),
          p("Note a maximum of 4 geographies and 3 indicators can be shown."),
          bslib::navset_tab(
            # Line chart ------------------------------------------------------
            CreateOwnLineChartUI("create_own_line"),
            # Bar chart ------------------------------------------------------
            CreateOwnBarChartUI("create_own_bar")
          )
        )
      ),

      # =======================================================================
      # User guide
      # =======================================================================
      user_guide_panel(),

      # =======================================================================
      # Information pages
      # =======================================================================
      info_page_panel(),

      # =======================================================================
      # Accessibility
      # =======================================================================
      a11y_panel(),

      # =======================================================================
      # Support and feedback
      # =======================================================================
      bslib::nav_panel(
        value = "support_panel",
        shinyGovstyle::banner(
          "beta banner",
          "beta",
          paste0(
            "This page is in beta phase and we are still reviewing the content.
             We are aware the links in <b>Find more information on the data</b>
             section are currently incorrect. Please see the ",
            dfeshiny::external_link(
              href = parent_publication,
              link_text = "LAIT website"
            ),
            " for more information."
          )
        ),
        shiny::br(),
        title = shiny::HTML("Support and feedback<br>(Feedback form)"),
        dfeshiny::support_panel(
          team_email = "jake.tufts@education.gov.uk",
          repo_name = "https://github.com/dfe-analytical-services/local-authority-interactive-tool",
          form_url = "https://forms.office.com/e/gTNw1EBgsn"
        )
      ),

      # =======================================================================
      # Cookies info
      # =======================================================================
      bslib::nav_panel(
        value = "cookies_panel_ui",
        title = "Cookies",
        dfeshiny::cookies_panel_ui(google_analytics_key = google_analytics_key)
      )
    ),

    # Footer ==================================================================
    shinyGovstyle::footer(full = TRUE)
  )
}
