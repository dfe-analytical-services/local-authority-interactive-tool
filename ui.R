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
    tags$head(HTML(paste0("<title>", site_title, "</title>"))),
    tags$head(tags$link(rel = "shortcut icon", href = "dfefavicon.png")),
    tags$head(includeHTML(("google-analytics.html"))),
    shinytitle::use_shiny_title(),
    tags$html(lang = "en"),
    # Add meta description for search engines
    metathis::meta() |>
      metathis::meta_general(
        application_name = site_title,
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
      links = site_primary,
      dashboard_title = site_title
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
      name = site_title
    ),

    # Header ------------------------------------------------------------------
    dfeshiny::header(site_title),

    # Beta banner -------------------------------------------------------------
    shiny::tagList(
      shinyGovstyle::banner(
        ifelse(banner_update_msg == "", "beta-banner", "beta-banner-no-border"),
        "Beta",
        "This Dashboard is in beta phase and we are still reviewing performance and reliability."
      )
    ),
    # News banner --------------------------------------------------------------
    if (banner_update_msg != "") {
      shinyGovstyle::banner(
        inputId = "update-msg-banner",
        type = "News",
        label = banner_update_msg
      )
    },

    # Start of app ============================================================
    # Define the main layout with hidden navigation
    shinyGovstyle::gov_main_layout(
      bslib::navset_hidden(
        id = "pages",
        # Main dashboard content
        bslib::nav_panel(
          "dashboard",
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = c(2, 10), md = c(2, 10), lg = c(2, 10)),

            # Left navigation
            dfe_contents_links(
              links_list = c(
                "LA Level",
                "Regional Level",
                "Statistical Neighbour Level",
                "All LA Level",
                "Create Your Own",
                "User Guide",
                "Information Page"
              )
            ),

            # Hidden dashboard panels
            bslib::navset_hidden(
              id = "left_nav",
              # LA Level
              bslib::nav_panel(
                "la_level",
                PageHeaderUI("la_header"),
                appInputsUI("la_inputs"),
                LA_LevelTableUI("la_table"),
                LA_StatsTableUI("la_stats"),
                div(
                  class = "well",
                  style = "overflow-y: visible;",
                  bslib::navset_card_underline(
                    id = "la_charts",
                    LA_LineChartUI("la_line_chart"),
                    LA_BarChartUI("la_bar_chart")
                  )
                ),
                LA_LevelMetaUI("la_meta")
              ),
              # Regional Level
              bslib::nav_panel(
                "regional_level",
                PageHeaderUI("region_header"),
                appInputsUI("region_inputs"),
                RegionLevel_TableUI("region_tables"),
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
                LA_LevelMetaUI("region_meta")
              ),
              # Statistical Neighbour Level
              bslib::nav_panel(
                "statistical_neighbour_level",
                PageHeaderUI("stat_n_header"),
                appInputsUI("stat_n_inputs"),
                StatN_TablesUI("stat_n_tables"),
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
                LA_LevelMetaUI("stat_n_meta")
              ),
              # All LA Level
              bslib::nav_panel(
                "all_la_level",
                PageHeaderUI("all_la_header"),
                appInputsUI("all_la_inputs"),
                AllLA_TableUI("all_la_table"),
                LA_LevelMetaUI("all_la_meta")
              ),
              # Create Your Own
              bslib::nav_panel(
                "create_your_own",
                full_data_on_github_noti(),
                div(
                  class = "well",
                  style = "overflow-y: visible; padding: 1rem;",
                  bslib::layout_column_wrap(
                    Create_MainInputsUI("create_inputs")["Main choices"]
                  ),
                  bslib::layout_column_wrap(
                    Create_MainInputsUI("create_inputs")["LA grouping"],
                    Create_MainInputsUI("create_inputs")["Other grouping"],
                    YearRangeUI("year_range"),
                    Create_MainInputsUI("create_inputs")["Clear all current selections"]
                  )
                ),
                StagingTableUI("staging_table"),
                QueryTableUI("query_table"),
                CreateOwnTableUI("create_own_table"),
                div(
                  class = "well",
                  style = "overflow-y: visible;",
                  shiny::h3(
                    "Output Charts",
                    create_tooltip_icon("Charts showing data from all the saved selections")
                  ),
                  shiny::p("Note a maximum of 4 geographies and 3 indicators can be shown."),
                  bslib::navset_tab(
                    CreateOwnLineChartUI("create_own_line"),
                    CreateOwnBarChartUI("create_own_bar")
                  )
                )
              ),
              # User Guide
              bslib::nav_panel("user_guide", user_guide_panel()),
              # Info Page
              bslib::nav_panel("information_page", info_page_panel())
            )
          )
        ),
        # Footer pages
        bslib::nav_panel(
          value = "support",
          # Add in back link
          actionLink(
            class = "govuk-back-link",
            style = "margin-top: 0.2rem; margin-bottom: 1.2rem;",
            "support_to_dashboard",
            "Back to dashboard"
          ),
          title = shiny::HTML("Support and feedback<br>(Feedback form)"),
          dfeshiny::support_panel(
            team_email = "jake.tufts@education.gov.uk",
            repo_name = "https://github.com/dfe-analytical-services/local-authority-interactive-tool",
            form_url = "https://forms.office.com/e/gTNw1EBgsn"
          )
        ),
        bslib::nav_panel("accessibility_statement", a11y_panel()),
        bslib::nav_panel(
          value = "cookies_information",
          title = "Cookies",
          # Add backlink
          actionLink(
            class = "govuk-back-link",
            style = "margin-top: 0.2rem; margin-bottom: 1.2rem;",
            "cookies_to_dashboard",
            "Back to dashboard"
          ),
          dfeshiny::cookies_panel_ui(google_analytics_key = google_analytics_key)
        )
      ),

      # Footer
      dfe_footer(
        links_list = c("Support", "Accessibility Statement", "Cookies Information")
      )
    )
  )
}
