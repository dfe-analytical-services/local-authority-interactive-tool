# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# Find out more about building Shiny applications: http://shiny.rstudio.com/
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  # User Inputs ===============================================================
  app_inputs <- appInputsServer("la_level")

  # LA level tables ===========================================================
  # Main table
  LA_LevelTableServer(
    "la_table",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Stats table
  LA_StatsTableServer(
    "la_stats",
    app_inputs,
    bds_metrics,
    stat_n_la
  )


  # LA level charts ===========================================================
  # Line chart
  LA_LineChartServer(
    "la_chart",
    app_inputs,
    bds_metrics,
    stat_n_la
  )

  # Bar chart
  LA_BarChartServer(
    "la_chart",
    app_inputs,
    bds_metrics,
    stat_n_la
  )


  # LA Metadata ===============================================================
  LA_LevelMetaServer(
    "la_meta",
    app_inputs$indicator,
    metrics_clean
  )
}
