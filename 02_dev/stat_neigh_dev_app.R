# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)

# UI
ui_dev <- bslib::page_fillable(

  ## Custom CSS =============================================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),

  # Tab header ==============================================================
  h1("Regional Level"),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = "15rem", # Minimum width for each input box before wrapping
      shiny::selectInput(
        inputId = "la_input",
        label = "Change Authority:",
        choices = la_names_bds
      ),
      shiny::selectInput(
        inputId = "topic_input",
        label = "Topic:",
        choices = metric_topics
      ),
      shiny::selectInput(
        inputId = "indicator",
        label = NULL,
        choices = metric_names
      )
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Statistical Neighbours"),
      bslib::card_body(
        div(
          reactable::reactableOutput("stat_n_sns_table")
        ),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          reactable::reactableOutput("stat_n_comp_table")
        )
      )
    )
  ) # ,
  # div(
  #   class = "well",
  #   bslib::layout_column_wrap(
  #     bslib::card(
  #       bslib::card_header(),
  #       bslib::card_body(
  #         div(
  #           reactable::reactableOutput("stat_n_stats_table")
  #         )
  #       )
  #     ),
  #     bslib::card(
  #       bslib::card_header(),
  #       bslib::card_body(
  #         div(
  #           reactable::reactableOutput("stat_n_stats_la_table")
  #         )
  #       )
  #     )
  #   )
  # )
)


# Server
server_dev <- function(input, output, session) {
  # Input ----------------------------------
  # Using the server to power to the provider dropdown for increased speed
  shiny::observeEvent(input$topic_input, {
    # Get indicator choices for selected topic
    filtered_topic_bds <- bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input
      ) |>
      pull_uniques("Measure")

    updateSelectInput(
      session = session,
      inputId = "indicator",
      label = "Indicator:",
      choices = filtered_topic_bds
    )
  })


  # Region LA Level table ----------------------------------
  # Filter for selected topic and indicator
  # Define filtered_bds outside of observeEvent
  filtered_bds <- reactiveValues(data = NULL)

  observeEvent(input$indicator, {
    # Region LA Level table ----------------------------------
    # Filter for selected topic and indicator
    filtered_bds$data <- bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input,
        Measure == input$indicator,
        !is.na(Years)
      )
  })

  # Get decimal places for indicator selected
  indicator_dps <- reactive({
    filtered_bds$data |>
      get_indicator_dps()
  })

  # Get statistical neighbours
  stat_n_sns <- reactive({
    stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input) |>
      pull_uniques("LA Name_sn")
  })

  # Get region
  stat_n_region <- reactive({
    stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input) |>
      pull_uniques("GOReg") |>
      clean_ldn_region(filtered_bds$data)
  })

  # Long format Statistical Neighbour data
  stat_n_long <- reactive({
    # Calculate SN average
    stat_n_sn_avg <- filtered_bds$data |>
      dplyr::filter(`LA and Regions` %in% stat_n_sns()) |>
      dplyr::summarise(
        values_num = mean(values_num, na.rm = TRUE),
        .by = c("Years", "Years_num")
      ) |>
      dplyr::mutate(
        "LA Number" = "-",
        "LA and Regions" = "Statistical Neighbours",
        .before = "Years"
      )

    # Statistical Neighbours long data
    filtered_bds$data |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values) |>
      dplyr::bind_rows(stat_n_sn_avg) |>
      dplyr::mutate(
        `LA and Regions` = factor(
          `LA and Regions`,
          levels = c(
            input$la_input, stat_n_sns(), "Statistical Neighbours",
            stat_n_region(), "England"
          )
        )
      )
  })

  # Difference between last two years
  stat_n_diff <- reactive({
    stat_n_long() |>
      calculate_change_from_prev_yr()
  })

  # Most recent year
  current_year <- reactive({
    stat_n_long() |>
      dplyr::filter(Years_num == max(Years_num)) |>
      pull_uniques("Years")
  })

  # Build main Statistical Neighbour formatted table (used to create the others)
  stat_n_table <- shiny::reactive({
    # Join difference and pivot wider
    stat_n_long() |>
      dplyr::bind_rows(stat_n_diff()) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num,
      ) |>
      pretty_num_table(
        dp = indicator_dps(),
        exclude_columns = "LA Number"
      )
  })

  output$stat_n_sns_table <- reactable::renderReactable({
    stat_n_sns_table <- stat_n_table() |>
      dplyr::filter(`LA and Regions` %in% c(input$la_input, stat_n_sns())) |>
      dplyr::arrange(.data[[current_year()]], `LA and Regions`)

    dfe_reactable(
      stat_n_sns_table,
      columns = align_reactable_cols(
        stat_n_sns_table,
        num_exclude = "LA Number"
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, stat_n_sns_table, input$la_input)
      },
      pagination = FALSE
    )
  })

  # Join difference and pivot wider to recreate 2nd Statistical Neighbours table
  output$stat_n_comp_table <- reactable::renderReactable({
    stat_n_comp_table <- stat_n_table() |>
      dplyr::filter(`LA and Regions` %in% c(
        "Statistical Neighbours",
        stat_n_region(),
        "England"
      )) |>
      dplyr::arrange(`LA and Regions`)

    # Output table
    dfe_reactable(
      stat_n_comp_table,
      # Create the reactable with specific column alignments
      columns = align_reactable_cols(stat_n_comp_table, num_exclude = "LA Number"),
      pagination = FALSE
    )
  })
}

shiny::shinyApp(ui_dev, server_dev)
