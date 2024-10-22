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
        `LA and Regions` %in% user_geog_inputs()
      )
  })

  # Reactive value "query" used to store query data
  # Uses lists to store multiple inputs (geographies)
  query <- reactiveValues(
    data = data.frame(
      Topic = I(list()),
      Measure = I(list()),
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
      Measure = I(list(input$indicator)),
      `LA and Regions` = I(list(
        get_geog_selection(input, la_names_bds, region_names_bds)
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

    # Get max and min years to expand each measure (if needed)
    # to match indicators with more years data
    # Extracting selected measures
    selected_indicators <- unique(unlist(query$data$Measure))

    # Create a simple data frame with col for all years from min_year to max_year
    if (length(selected_indicators) > 0) {
      # Get max year and min year
      max_year <- bds_metrics |>
        dplyr::filter(Measure %in% selected_indicators) |>
        dplyr::pull(Years_num) |>
        max(na.rm = TRUE)

      min_year <- bds_metrics |>
        dplyr::filter(Measure %in% selected_indicators) |>
        dplyr::pull(Years_num) |>
        min(na.rm = TRUE)

      # Years col df
      all_years <- data.frame(Years_num = seq(min_year, max_year))
    } else {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No years available for selected measures.",
          check.names = FALSE
        )
      ))
    }

    # Setup the final_data which stores all the query data
    final_query_data <- data.frame()

    # Loop through each row in the query table and query results to final_table
    for (i in seq_len(nrow(query$data))) {
      # Get the current query values
      current_topic <- query$data$Topic[[i]]
      current_measure <- query$data$Measure[[i]]
      current_geog <- query$data$`LA and Regions`[[i]]

      # Set geography filters
      # Append all LAs
      if ("All LAs" %in% current_geog) {
        current_geog <- c(current_geog, la_names_bds)
      }
      # Append all Regions
      if ("All Regions" %in% current_geog) {
        current_geog <- c(current_geog, region_names_bds)
      }
      # Return unique geogs
      current_geog <- unique(current_geog)

      # Filter BDS for the current query
      raw_query_data <- bds_metrics |>
        dplyr::filter(
          Topic %in% current_topic,
          Measure %in% current_measure,
          `LA and Regions` %in% current_geog
        ) |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Topic,
          Measure, Years, Years_num, values_num, Values
        )

      # Create the cleaned query
      # Merge the temp_data with the all_years data to ensure query has
      # year cols to match other queries (for easy row join)
      clean_query_data <- dplyr::full_join(
        raw_query_data,
        all_years,
        by = c("Years_num")
      ) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
          names_from = Years_num,
          values_from = values_num
        ) |>
        # Remove any all NA rows
        # (created from join where indicator has missing year)
        dplyr::filter(!dplyr::if_all(everything(), is.na)) |>
        dplyr::select(
          `LA Number`,
          `LA and Regions`,
          Topic,
          Measure,
          tidyselect::num_range("", min_year:max_year)
        )

      # Combine the current query into the final query data frame
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
