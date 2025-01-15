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
        bslib::nav_panel(
          "dashboard",
          div(
            class = "dashboard-container",
            # Left navigation
            div(
              class = "navigation-panel",
              dfe_contents_links(
                links_list = c(
                  "LA Level",
                  "Regional Level",
                  "Statistical Neighbour Level",
                  "All LA Level",
                  "Create Your Own",
                  "User Guide",
                  "Updates and Sources"
                )
              )
            ),
            # Main content area
            div(
              class = "main-content",
              bslib::navset_hidden(
                id = "left_nav",
                la_level_panel(),
                region_level_panel(),
                stat_n_level_panel(),
                all_la_level_panel(),
                create_your_own_panel(),
                bslib::nav_panel("user_guide", user_guide_panel()),
                bslib::nav_panel("updates_and_sources", info_page_panel())
              )
            )
          )
        ),
        support_panel(),
        bslib::nav_panel("accessibility_statement", accessibility_panel()),
        bslib::nav_panel(
          value = "cookies_information",
          title = "Cookies",
          actionLink(
            class = "govuk-back-link",
            "cookies_to_dashboard",
            "Back to dashboard"
          ),
          dfeshiny::cookies_panel_ui(google_analytics_key = google_analytics_key)
        )
      ),
      tags$div(
        style = "text-align: center; margin-top: 50px; margin-bottom: 15px;",
        tags$a(href = "#top", "Go to the top of page")
      )
    ),
    dfe_footer(
      links_list = c(
        "Support",
        "Accessibility Statement",
        "Cookies Information"
      )
    )
  )
}
