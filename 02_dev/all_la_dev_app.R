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
  h1("All Local Authorities"),
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
      bslib::card_header(shiny::uiOutput("all_la_table_name")),
      bslib::card_body(
        div(
          reactable::reactableOutput("all_la_la_table")
        ),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          reactable::reactableOutput("all_la_region_table")
        )
      )
    )
  )
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

  # All LA formatted table
  all_la_table <- reactive({
    # All LAs long data
    all_la_long <- filtered_bds$data |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values)

    # Difference between last two years
    all_la_diff <- all_la_long |>
      calculate_change_from_prev_yr()

    # Get polarity of indicator
    indicator_polarity <- filtered_bds$data |>
      pull_uniques("Polarity")

    # Get latest rank, ties are set to min & NA vals to NA rank
    all_la_ranked <- filtered_bds$data |>
      filter_la_regions(la_names_bds, latest = TRUE) |>
      dplyr::mutate(
        Rank = dplyr::case_when(
          is.na(values_num) ~ NA,
          # Rank in descending order
          indicator_polarity == "High" ~ rank(-values_num, ties.method = "min", na.last = TRUE),
          # Rank in ascending order
          indicator_polarity == "Low" ~ rank(values_num, ties.method = "min", na.last = TRUE)
        )
      ) |>
      dplyr::select(`LA and Regions`, Rank)

    # Convert to wide format and join rank column
    all_la_long |>
      rbind(all_la_diff) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num,
      ) |>
      dplyr::left_join(all_la_ranked, by = "LA and Regions") |>
      pretty_num_table(
        dp = get_indicator_dps(filtered_bds$data),
        exclude_columns = c("LA Number", "Rank")
      )
  })

  # All LA Level LA table -----------------------------------------------------
  output$all_la_la_table <- reactable::renderReactable({
    # Filter for LAs and arrange by alphabetical order
    all_la_la_table <- all_la_table() |>
      dplyr::filter(`LA and Regions` %in% la_names_bds) |>
      dplyr::arrange(`LA and Regions`)

    # Output table
    dfe_reactable(
      all_la_la_table,
      # Create the reactable with specific column alignments
      columns = align_reactable_cols(all_la_la_table, num_exclude = "LA Number"),
      rowStyle = function(index) {
        highlight_selected_row(index, all_la_la_table, input$la_input)
      },
      pagination = FALSE
    )
  })

  # All LA Level Region table -------------------------------------------------
  output$all_la_region_table <- reactable::renderReactable({
    # Filter and prepare Region table
    all_la_region_table <- all_la_table() |>
      # Keep only Regions and England (remove London Inner/Outer with all NAs)
      dplyr::filter(
        `LA and Regions` %notin% la_names_bds,
        !(`LA and Regions` %in% c("London (Inner)", "London (Outer)") &
          # Sums number of non-NA cols (left of LA and Regions) and checks if = 0
          rowSums(!is.na(dplyr::select(all_la_table(), -c(`LA Number`, `LA and Regions`)))) == 0)
      ) |>
      # Replace Rank with a blank col
      dplyr::mutate(Rank = "") |>
      dplyr::arrange(`LA Number`)

    # Get region of LA
    all_la_region <- stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input) |>
      pull_uniques("GOReg") |>
      clean_ldn_region(filtered_bds$data)

    # Output table
    dfe_reactable(
      all_la_region_table,
      # Create the reactable with specific column alignments
      columns = align_reactable_cols(all_la_region_table, num_exclude = "LA Number"),
      rowStyle = function(index) {
        highlight_selected_row(index, all_la_region_table, all_la_region())
      },
      pagination = FALSE,
      class = "hidden-column-headers"
    )
  })

  output$all_la_table_name <- shiny::renderUI({
    filtered_bds$data |>
      pull_uniques("Chart_title")
  })
}

shiny::shinyApp(ui_dev, server_dev)
