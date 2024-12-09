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

# Load ui panels
list.files("R/ui_panels/", full.names = TRUE) |>
  purrr::walk(source)

ui_dev <- function(input, output, session) {
  bslib::page_fillable(

    # Set application metadata ------------------------------------------------
    tags$head(HTML("<title>Local Authority Interactive Tool (LAIT)</title>")),
    tags$head(tags$link(rel = "shortcut icon", href = here::here("www/dfefavicon.png"))),
    tags$head(includeHTML(here::here("google-analytics.html"))),
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
    shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),
    # Remove any gaps between elements
    gap = 0,

    # Load javascript dependencies --------------------------------------------
    shinyWidgets::useShinydashboard(),
    shinyjs::useShinyjs(),
    tags$head(htmltools::includeScript(here::here("www/custom_js.js"))),
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
      logo = "www/images/DfE_logo_landscape.png",
      logo_width = 150,
      logo_height = 32
    ),

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

    # Nav panels --------------------------------------------------------------
    bslib::navset_pill_list(
      "",
      id = "navsetpillslist",
      widths = c(2, 10),
      well = FALSE,

      # =======================================================================
      # User guide
      # =======================================================================
      info_page_panel()
    )
  )
}


# Define the `server` function
server_dev <- function(input, output, session) {
  # Call the banner module server
  bannerModuleServer(
    "banner_module",
    banner_update_msg
  )

  # Indicator information table
  IndicatorInfoTableServer(
    "indicator_info_table",
    metrics_clean
  )

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

  # Useful links
  UsefulLinksServer(
    "useful_links",
    useful_links
  )
}

# Launch the Shiny app
shinyApp(ui_dev, server_dev)
