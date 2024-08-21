# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- function(input, output, session) {
  bslib::page_fillable(

    shinytitle::use_shiny_title(),

    ## Custom CSS =============================================================
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_styles.css"
      )
    ),


    # Start of app ============================================================
    # Tab header ==============================================================
    h1("Local Authority View"),

    # User Inputs =============================================================
    appInputsUI("la_level"),

    # LA Tables ===============================================================
    # Main table
    LA_LevelTableUI("la_table"),

    # Stats table
    LA_StatsTableUI("la_stats"),

    # LA Charts ===============================================================
    LA_ChartUI("la_chart"),

    # LA Metadata =============================================================
    LA_LevelMetaUI("la_meta")
  )
}
