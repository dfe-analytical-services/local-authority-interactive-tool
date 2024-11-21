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
  # Full dataset notification banner
  full_data_on_github_noti(),
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
      Create_MainInputsUI("create_inputs")["Add selection"],
      Create_MainInputsUI("create_inputs")["Clear all current selections"]
    )
  ),
  StagingTableUI("staging_table"),
  QueryTableUI("query_table"),
  CreateOwnTableUI("create_own_table"),
  # Charts =====================================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Output Charts (Charts showing data from saved selections)"),
    p("Note a maximum of 4 geographies and 3 indicators can be shown."),

    # Line chart ---------------------------------------------------------------
    bslib::navset_tab(
      CreateOwnLineChartUI("create_own_line"),
      CreateOwnBarChartUI("create_own_bar")
    )
  )
)

# Main App Server
server <- function(input, output, session) {
  # Call the main inputs module
  create_inputs <- Create_MainInputsServer("create_inputs", bds_metrics)

  # Year range
  year_input <- YearRangeServer(
    "year_range",
    bds_metrics,
    create_inputs$indicator,
    create_inputs$clear_selections
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
    year_input,
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

  # Output staging table
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

  query_data <- QueryDataServer(
    "query_data",
    create_inputs,
    geog_groups,
    year_input,
    staging_data
  )

  query_table <- QueryTableServer(
    "query_table",
    query_data
  )

  CreateOwnTableServer(
    "create_own_table",
    query_table,
    bds_metrics
  )

  CreateOwnLineChartServer(
    "create_own_line",
    query_table,
    bds_metrics,
    covid_affected_indicators
  )

  CreateOwnBarChartServer(
    "create_own_bar",
    query_table,
    bds_metrics,
    covid_affected_indicators
  )
}

# Run the application
shinyApp(ui = ui, server = server)
