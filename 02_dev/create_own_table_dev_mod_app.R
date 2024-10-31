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

# Main App UI
ui <- bslib::page_fillable(
  ## Other language dependencies ===============================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),
  tags$head(htmltools::includeScript("www/custom_js.js")),
  # Makes the remove button work
  reactable.extras::reactable_extras_dependency(),

  # Start of app ===============================================================

  # Main selections ============================================================
  h1("Create your own"),
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
      Create_MainInputsUI("create_inputs")["Add selection"]
    )
  ),
  h4("Selected outputs:"),
  verbatimTextOutput("selected_inputs")
)

# Main App Server
server <- function(input, output, session) {
  # Call the main inputs module
  create_inputs <- Create_MainInputsServer("create_inputs", bds_metrics)

  # Year range
  year_range <- YearRangeServer(
    "year_range",
    bds_metrics,
    create_inputs$indicator
  )

  # Geog Groupings
  geog_groups <- GroupingInputServer(
    "geog_groups",
    create_inputs,
    la_names_bds,
    region_names_bds,
    stat_n_geog,
    stat_n_la
  )

  # Filtering BDS for staging data
  staging_bds <- StagingBDSServer(
    "staging_bds",
    create_inputs,
    geog_groups,
    year_range,
    bds_metrics
  )

  # Build staging data
  staging_data <- StagingDataServer(
    "staging_data",
    create_inputs,
    staging_bds,
    region_names_bds,
    la_names_bds,
    stat_n_la
  )


  output$selected_inputs <- renderPrint({
    list(
      Geography = create_inputs$geog(),
      Topic = create_inputs$topic(),
      Indicator = create_inputs$indicator(),
      Topic_Indicator_pair = create_inputs$selected_indicators(),
      Year = year_range(),
      Geog_groups = geog_groups(),
      Staging_BDS = staging_bds(),
      Staging_Data = staging_data()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
