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
    bslib::navset_card_tab(
      id = "all_la_table_tabs",
      bslib::nav_panel(
        "Tables",
        bslib::card_header(
          shiny::uiOutput("all_la_table_name"),
          style = "text-align: center;"
        ),
        reactable::reactableOutput("all_la_la_table"),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          reactable::reactableOutput("all_la_region_table")
        )
      ),
      bslib::nav_panel(
        "Download data",
        shinyGovstyle::radio_button_Input(
          inputId = "file_type",
          label = h2("Choose download file format"),
          hint_label = paste0(
            "This will download all data related to the providers and options selected.",
            " The XLSX format is designed for use in Microsoft Excel."
          ),
          choices = c("CSV (Up to 5.47 MB)", "XLSX (Up to 1.75 MB)"),
          selected = "CSV (Up to 5.47 MB)"
        ),
        shiny::downloadButton(
          "la_download",
          label = "Download LA table",
          class = "gov-uk-button",
          icon = NULL
        ),
        shiny::downloadButton(
          "region_download",
          label = "Download Region table",
          class = "gov-uk-button",
          icon = NULL
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
        if (!is.null(input$topic_input)) .data$Topic == input$topic_input else TRUE
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
        Measure == input$indicator
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
      )
  })

  # All LA Level LA table -----------------------------------------------------
  output$all_la_la_table <- reactable::renderReactable({
    # Filter for LAs and arrange by alphabetical order
    all_la_la_table <- all_la_table() |>
      filter_la_data_all_la(la_names_bds)

    # Output table
    dfe_reactable(
      all_la_la_table,
      # Create the reactable with specific column alignments
      columns = utils::modifyList(
        format_num_reactable_cols(
          all_la_la_table,
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number",
          categorical = "Rank"
        ),
        set_custom_default_col_widths()
      ),
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
      filter_region_data_all_la(la_names_bds)

    # Get region of LA
    all_la_region <- stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input) |>
      pull_uniques("GOReg") |>
      clean_ldn_region(filtered_bds$data)

    # Output table
    dfe_reactable(
      all_la_region_table,
      # Create the reactable with specific column alignments
      columns = utils::modifyList(
        format_num_reactable_cols(
          all_la_region_table,
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        set_custom_default_col_widths()
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, all_la_region_table, all_la_region)
      },
      pagination = FALSE,
      class = "hidden-column-headers"
    )
  })

  # Download tables
  # Store the table and export file in reactive values
  la_local <- reactiveValues(export_file = NULL, data = NULL, file_type = NULL, file_name = NULL)
  region_local <- reactiveValues(export_file = NULL, data = NULL, file_type = NULL, file_name = NULL)

  # Observe when input$file_type or all_la_table is updated and create relevant file
  observeEvent(c(input$file_type, all_la_table()), {
    # LA table
    # Setting parameters
    la_local$file_type <- input$file_type
    la_local$file_name <- c(input$la_input, input$indicator, "All-LA-LA-table")

    # Extracting data for download
    la_local$data <- all_la_table() |>
      filter_la_data_all_la(la_names_bds)

    # Creating download
    la_local$export_file <- generate_download_file(la_local$data, input$file_type)

    # Region table
    region_local$file_type <- input$file_type
    region_local$file_name <- c(input$la_input, input$indicator, "All-LA-Region-table")

    region_local$data <- all_la_table() |>
      filter_region_data_all_la(la_names_bds)

    region_local$export_file <- generate_download_file(region_local$data, input$file_type)
  })

  # Download handlers
  output$la_download <- create_download_handler(la_local)
  output$region_download <- create_download_handler(region_local)

  # Get chart title for All LA table name
  output$all_la_table_name <- shiny::renderUI({
    filtered_bds$data |>
      pull_uniques("Chart_title")
  })
}

shiny::shinyApp(ui_dev, server_dev)
