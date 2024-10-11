# nolint start: object_name
#
# General modules =============================================================
# These are nested within other modules

Build_AllLATableServer <- function(id, filtered_bds, la_names_bds) {
  moduleServer(id, function(input, output, session) {
    # All LA formatted table
    reactive({
      # All LAs long data
      all_la_long <- filtered_bds() |>
        dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values)

      # Difference between last two years
      all_la_diff <- all_la_long |>
        calculate_change_from_prev_yr()

      # Get polarity of indicator
      indicator_polarity <- filtered_bds() |>
        pull_uniques("Polarity")

      # Get latest rank, ties are set to min & NA vals to NA rank
      all_la_ranked <- filtered_bds() |>
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
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = c("LA Number", "Rank")
        )
    })
  })
}


# Get All LA table name
Get_AllLATableNameUI <- function(id) {
  ns <- NS(id)

  shiny::uiOutput(ns("table_name"))
}

Get_AllLATableNameServer <- function(id, filtered_bds) {
  moduleServer(id, function(input, output, session) {
    # Pull chart title
    output$table_name <- shiny::renderUI({
      filtered_bds() |>
        pull_uniques("Chart_title")
    })
  })
}


# Download data
Download_DataUI <- function(id, download_label) {
  ns <- NS(id)

  shiny::downloadButton(
    ns("download"),
    label = paste0("Download ", download_label),
    class = "gov-uk-button",
    icon = NULL
  )
}

Download_DataServer <- function(id, file_type_input, data_for_download, download_name) {
  moduleServer(id, function(input, output, session) {
    # Download tables
    # Store the table and export file in reactive values
    local <- reactiveValues(data = NULL, export_file = NULL)

    # Observe when input$file_type or all_la_table is updated and create relevant file
    observeEvent(c(file_type_input(), data_for_download()), {
      # LA table
      local$data <- data_for_download()

      generate_csv(local, file_type_input())
    })

    # Download handlers
    output$download <- create_download_handler(
      local,
      file_type_input,
      download_name
    )
  })
}



AllLA_TableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_tab(
      id = "all_la_table_tabs",
      bslib::nav_panel(
        "Tables",
        bslib::card_header(
          Get_AllLATableNameUI(ns("table_name")),
          style = "text-align: center;"
        ),
        bslib::card_header("Local Authorities"),
        reactable::reactableOutput(ns("la_table")),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          bslib::card_header("Regions"),
          reactable::reactableOutput(ns("region_table"))
        )
      ),
      bslib::nav_panel(
        "Download data",
        shinyGovstyle::radio_button_Input(
          inputId = ns("file_type"),
          label = h2("Choose download file format"),
          hint_label = paste0(
            "This will download all data related to the providers and options selected.",
            " The XLSX format is designed for use in Microsoft Excel."
          ),
          choices = c("CSV (Up to 5.47 MB)", "XLSX (Up to 1.75 MB)"),
          selected = "CSV (Up to 5.47 MB)"
        ),
        Download_DataUI(ns("la_download"), "LA Table"),
        Download_DataUI(ns("region_download"), "Region Table")
      )
    )
  )
}


AllLA_TableServer <- function(id, app_inputs, bds_metrics, la_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer(
      "filtered_bds",
      app_inputs,
      bds_metrics
    )

    # Get table name ----------------------------------------------------------
    Get_AllLATableNameServer(
      "table_name",
      filtered_bds
    )

    # Get all LA formatted table
    all_la_table <- Build_AllLATableServer(
      "all_la_table",
      filtered_bds,
      la_names_bds
    )


    # All LA Level LA table ---------------------------------------------------
    output$la_table <- reactable::renderReactable({
      # Filter for LAs and arrange by alphabetical order
      all_la_la_table <- all_la_table() |>
        filter_la_data_all_la(la_names_bds)

      # Output table
      dfe_reactable(
        all_la_la_table,
        # Create the reactable with specific column alignments
        columns = align_reactable_cols(all_la_la_table, num_exclude = "LA Number"),
        rowStyle = function(index) {
          highlight_selected_row(index, all_la_la_table, app_inputs$la())
        },
        pagination = FALSE
      )
    })

    # LA table download -------------------------------------------------------
    Download_DataServer(
      "la_download",
      reactive({
        input$file_type
      }),
      reactive({
        filter_la_data_all_la(all_la_table(), la_names_bds)
      }),
      paste0(app_inputs$indicator(), "-All-LAs-Local-Authorities")
    )

    # All LA Level Region table -----------------------------------------------
    output$region_table <- reactable::renderReactable({
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
        dplyr::rename(` ` = "Rank") |>
        dplyr::arrange(`LA Number`)

      # Get region of LA
      all_la_region <- stat_n_la |>
        dplyr::filter(`LA Name` == app_inputs$la()) |>
        pull_uniques("GOReg") |>
        clean_ldn_region(filtered_bds())

      # Output table
      dfe_reactable(
        all_la_region_table,
        # Create the reactable with specific column alignments
        columns = align_reactable_cols(all_la_region_table, num_exclude = "LA Number"),
        rowStyle = function(index) {
          highlight_selected_row(index, all_la_region_table, all_la_region)
        },
        pagination = FALSE
        # class = "hidden-column-headers"
      )
    })

    # Region table download ---------------------------------------------------
    Download_DataServer(
      "region_download",
      reactive({
        input$file_type
      }),
      reactive({
        filter_region_data_all_la(all_la_table(), la_names_bds)
      }),
      paste0(app_inputs$indicator(), "-All-LAs-Regions")
    )
  })
}
