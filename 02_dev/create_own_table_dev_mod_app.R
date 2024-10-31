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
    Create_MainInputsUI("main_inputs"),
    bslib::layout_column_wrap(
      GroupingInputUI("geog_groups")["LA groups"],
      GroupingInputUI("geog_groups")["Other groups"],
      YearRangeUI("year_range_input"),
      # Button container with styling
      div(
        style = "height: 100%; display: flex; justify-content: center; align-items: flex-end;",
        shiny::actionButton("add_query", "Add selections", class = "gov-uk-button")
      )
    )
  ),
  h4("Selected outputs:"),
  verbatimTextOutput("selected_inputs")
)

# Main App Server
server <- function(input, output, session) {
  # Call the main inputs module
  main_inputs <- Create_MainInputsServer("main_inputs", bds_metrics)

  # Year range
  year_range <- YearRangeServer("year_range_input", bds_metrics, main_inputs$indicator)

  # Geog Groupings
  geog_groups <- GroupingInputServer(
    "geog_groups",
    main_inputs$geog,
    la_names_bds,
    region_names_bds,
    stat_n_geog,
    stat_n_la
  )

  # Stat neighbour association table
  stat_n_association <- StatN_AssociationServer("geog_groups", main_inputs$geog, la_names_bds, stat_n_la)

  output$selected_inputs <- renderPrint({
    list(
      Geography = main_inputs$geog(),
      Topic = main_inputs$topic(),
      Indicator = main_inputs$indicator(),
      Year = year_range(),
      Geog_groups = geog_groups(),
      stat_n_association = stat_n_association()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
