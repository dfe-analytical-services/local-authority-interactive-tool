# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


ui <- bslib::page_fillable(
  ## Custom CSS =============================================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),
  # Include reactable.extras in your UI
  reactable.extras::reactable_extras_dependency(),

  # Tab header ==============================================================
  h1("Create your own"),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = "15rem",
      shiny::selectInput(
        inputId = "geog_input",
        label = "LAs and Regions:",
        choices = c(la_names_bds, region_names_bds),
        multiple = TRUE
      ),
      shiny::selectInput(
        inputId = "topic_input",
        label = "Topic:",
        choices = metric_topics
      ),
      shiny::selectInput(
        inputId = "indicator",
        label = "Indicator:",
        choices = metric_names
      ),
      shiny::checkboxInput("all_las", "Include All LAs", FALSE),
      shiny::checkboxInput("all_regions", "Include All Regions", FALSE),
      shiny::checkboxInput("region_las", "Include LAs in the same Region", FALSE),
      shiny::checkboxInput("la_stat_ns", "Include statistical neighbours", FALSE),
      shiny::actionButton("add_query", "Add Query", class = "gov-uk-button")
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Staging Table (View of Current Query)"),
    bslib::card(
      reactable::reactableOutput("staging_table")
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("List of queries"),
    bslib::card(
      reactable::reactableOutput("query_table")
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Final Output Table (View of Saved Queries)"),
    bslib::card(
      reactable::reactableOutput("output_table")
    )
  )
)


server <- function(input, output, session) {
  # Input ----------------------------------
  # Get and set indicator choices for selected topic
  shiny::observeEvent(input$topic_input, {
    filtered_topic_bds <- bds_metrics |>
      dplyr::filter(Topic %in% input$topic_input) |>
      pull_uniques("Measure")

    updateSelectInput(
      session = session,
      inputId = "indicator",
      label = "Indicator:",
      choices = filtered_topic_bds
    )
  })

  # Setting combination of user input choices (regarding geographies)
  user_geog_inputs <- reactive({
    # Selected LAs
    inputs <- input$geog_input

    # Append all LAs
    if (isTRUE(input$all_las)) {
      inputs <- c(inputs, la_names_bds)
    }

    # Append all Regions
    if (isTRUE(input$all_regions)) {
      inputs <- c(inputs, region_names_bds)
    }

    # Append Regions LAs
    if (isTRUE(input$region_las)) {
      # Get all LAs in the regions
      selected_la_regions <- get_la_region(stat_n_geog, input$geog_input)
      all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)

      inputs <- c(inputs, all_region_las)
    }

    # Append LA statistical neighbours
    if (isTRUE(input$la_stat_ns)) {
      # Get statistical neighbours
      selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, input$geog_input)

      inputs <- c(inputs, selected_la_stat_n)
    }

    # Return unique values to avoid duplication
    unique(inputs)
  })


  # Filter BDS based on user inputs (topic, indicator, geographies)
  filtered_bds <- reactive({
    req(input$topic_input, input$indicator)
    bds_metrics |>
      dplyr::filter(
        Topic %in% input$topic_input,
        Measure %in% input$indicator,
        `LA and Regions` %in% user_geog_inputs(),
        !is.na(Years)
      )
  })

  # Reactive value "query" used to store query data
  # Uses lists to store multiple inputs (geographies)
  query <- reactiveValues(
    data = data.frame(
      Topic = I(list()),
      Indicator = I(list()),
      `LA and Regions` = I(list()),
      `Click to remove query` = character(),
      check.names = FALSE
    )
  )

  # Build the staging table (select data and make wide)
  staging_table <- reactive({
    filtered_bds() |>
      dplyr::select(
        `LA Number`, `LA and Regions`, Topic,
        Measure, Years, Years_num, values_num, Values
      ) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
        names_from = Years,
        values_from = values_num,
      )
  })

  # Staging table output
  output$staging_table <- reactable::renderReactable({
    reactable::reactable(
      staging_table()
    )
  })

  # When "Add query" button clicked - add query to saved queries
  observeEvent(input$add_query, {
    # Get query information
    new_query <- data.frame(
      Topic = I(list(input$topic_input)),
      Indicator = I(list(input$indicator)),
      `LA and Regions` = I(list(
        get_geog_selection(input, la_names_bds, region_names_bds, stat_n_geog)
      )),
      `Click to remove query` = "Remove",
      check.names = FALSE
    )

    # Append query to existing query data
    query$data <- query$data |>
      # Remove row identifier as not needed yet (and not available initially)
      dplyr::select(-.internal_uuid) |>
      rbind(new_query)
  })


  # Query table output
  output$query_table <- reactable::renderReactable({
    req(nrow(query$data))
    # Set the new row identifier (a counter)
    query$data$.internal_uuid <- seq_len(nrow(query$data))

    reactable::reactable(
      query$data,
      columns = list(
        # JS used from `reactable.extras::button_extra()` to create btn in table
        # Uses the row identifier to know which row to remove
        `Click to remove query` = reactable::colDef(
          cell = reactable::JS(
            "function(cellInfo) {
            // Use the unique row identifier as the button ID
            const buttonId = 'remove_' + cellInfo.row['.internal_uuid'];
            return React.createElement(ButtonExtras, {
              id: buttonId,
              label: 'Remove',
              uuid: cellInfo.row['.internal_uuid'],
              column: cellInfo.column.id,
              class: 'govuk-button--warning',
              className: 'govuk-button--warning'
            }, cellInfo.index);
          }"
          )
        ),
        # Don't show row identifier
        .internal_uuid = reactable::colDef(show = FALSE)
      )
    )
  })

  # Remove query button
  observe({
    req(nrow(query$data))
    # Set the row identifier
    query$data$.internal_uuid <- seq_len(nrow(query$data))

    # Create btn observers for each row using the unique row identifier
    lapply(query$data$.internal_uuid, function(uuid) {
      # Create matching row identifier for each remove button
      remove_button_id <- paste0("remove_", uuid)

      # Observe the button click
      observeEvent(input[[remove_button_id]],
        {
          # Remove the corresponding row from query$data using the row identifier
          query$data <- query$data[query$data$.internal_uuid != uuid, , drop = FALSE]
        },
        ignoreInit = TRUE
      )
    })
  })

  # Final output table (based on saved queries)
  output$output_table <- reactable::renderReactable({
    req(query$data)

    # Check if there are any selected measures
    if (nrow(query$data) == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add queries.",
          check.names = FALSE
        )
      ))
    }

    # Main logic for query data processing
    selected_indicators <- unique(unlist(query$data$Indicator))
    no_selected_indicators <- length(selected_indicators)

    if (no_selected_indicators == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No years available for selected measures.",
          check.names = FALSE
        )
      ))
    }

    # Filter BDS for selected indicators
    raw_indicator_filtered_bds <- filter_bds_for_indicators(bds_metrics, selected_indicators)

    # Check if all years have consistent suffix
    query_str_years <- check_year_suffix_consistency(raw_indicator_filtered_bds)

    # Get min and max years
    year_bounds <- get_min_max_years(raw_indicator_filtered_bds)
    all_years <- create_years_df(year_bounds$min_year, year_bounds$max_year)

    final_query_data <- data.frame()

    # Loop through each row in the query table and process data
    for (i in seq_len(nrow(query$data))) {
      current_topic <- query$data$Topic[[i]]
      current_measure <- query$data$Indicator[[i]]
      current_geog <- query$data$`LA and Regions`[[i]]

      # Sorting geography filters
      # Adding all LAs in selected LA regions
      if (any(grepl("LAs in ", current_geog))) {
        current_la_regions <- current_geog |>
          stringr::str_subset("^LAs in ") |> # Subset strings that start with "LAs in "
          stringr::str_remove("^LAs in ") # Remove the "LAs in " prefix

        # Extract all LAs in the region and add to current geog
        current_region_las <- get_las_in_regions(stat_n_geog, current_la_regions)
        current_geog <- c(current_geog, current_region_las)
      }
      # Adding statistical neighbours of selected LAs
      if (any(grepl(" statistical neighbours", current_geog))) {
        current_la_sns <- current_geog |>
          stringr::str_subset(" statistical neighbours$") |>
          stringr::str_remove(" statistical neighbours$")

        # Extract all statistical neighbours
        selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, current_la_sns)
        current_geog <- c(current_geog, selected_la_stat_n)
      }
      # Append all LAs or Regions to the current geography if needed
      if ("All LAs" %in% current_geog) current_geog <- c(current_geog, la_names_bds)
      if ("All Regions" %in% current_geog) current_geog <- c(current_geog, region_names_bds)

      current_geog <- unique(current_geog)

      # Filter BDS for the current query
      raw_query_data <- bds_metrics |>
        dplyr::filter(
          Topic %in% current_topic,
          Measure %in% current_measure,
          `LA and Regions` %in% current_geog,
          !is.na(Years)
        ) |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Topic, Measure, Years, Years_num, values_num, Values
        )

      # Join the data with the full years to fill gaps
      full_query_data <- dplyr::full_join(raw_query_data, all_years, by = "Years_num") |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
          names_from = ifelse(query_str_years || no_selected_indicators == 1, "Years", "Years_num"),
          values_from = values_num
        )

      # Sort year columns with full names preserved
      sorted_year_cols <- sort_year_columns(full_query_data)

      clean_query_data <- full_query_data |>
        dplyr::filter(!dplyr::if_all(everything(), is.na)) |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Topic, Measure,
          dplyr::all_of(sorted_year_cols)
        )

      # Append the cleaned query data to final query data
      final_query_data <- dplyr::bind_rows(final_query_data, clean_query_data)
    }

    # Check if final_data has any rows before rendering
    if (nrow(final_query_data) == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No data available based on the selected queries.",
          check.names = FALSE
        )
      ))
    }

    # Output final table
    reactable::reactable(final_query_data)
  })
}

shinyApp(ui, server)
