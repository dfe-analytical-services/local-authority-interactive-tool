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
        inputId = "la_input",
        label = "Change Authority:",
        choices = la_names_bds,
        multiple = TRUE,
        selected = "Barking and Dagenham"
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
      checkboxInput("all_las", "Include All LAs", FALSE),
      checkboxInput("all_regions", "Include All Regions", FALSE),
      actionButton("add_query", "Add Query")
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Staging Table (Current Query)"),
    reactable::reactableOutput("react_staging")
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Query Table"),
    reactable::reactableOutput("react_query_table")
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Final Output Table (Saved Queries)"),
    reactable::reactableOutput("react_final_output")
  )
)


server <- function(input, output, session) {
  # Input ----------------------------------
  # Using the server to power to the provider dropdown for increased speed
  shiny::observeEvent(input$topic_input, {
    # Get indicator choices for selected topic
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

  user_geog_inputs <- reactive({
    inputs <- input$la_input

    # Append all LAs if the checkbox is checked
    if (isTRUE(input$all_las)) {
      inputs <- c(inputs, la_names_bds)
    }

    # Append all Regions if the checkbox is checked
    if (isTRUE(input$all_regions)) {
      inputs <- c(inputs, region_names_bds)
    }

    # Return unique values to avoid duplication
    unique(inputs)
  })


  # Filtered data for staging table based on inputs
  filtered_bds <- reactive({
    req(input$topic_input, input$indicator)
    bds_metrics |>
      dplyr::filter(
        Topic %in% input$topic_input,
        Measure %in% input$indicator,
        `LA and Regions` %in% user_geog_inputs()
      )
  })

  # Reactive values to store data
  query_table <- reactiveValues(
    data = data.frame(
      Topic = I(list()),
      Measure = I(list()),
      `LA and Regions` = I(list()),
      Remove = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )

  # Building the staging table
  create_own_bds <- reactive({
    staging_table <- filtered_bds() |>
      dplyr::select(
        `LA Number`, `LA and Regions`, Topic,
        Measure, Years, Years_num, values_num, Values
      ) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
        names_from = Years,
        values_from = values_num,
      )
    staging_table
  })

  # Staging table based on current inputs
  output$react_staging <- reactable::renderReactable({
    reactable::reactable(
      create_own_bds()
    )
  })

  # Add query button action
  observeEvent(input$add_query, {
    new_row <- data.frame(
      # Store as a list to allow multiple topics
      Topic = I(list(input$topic_input)),
      Measure = I(list(input$indicator)),
      `LA and Regions` = I(list(
        if (input$all_las) {
          "All LAs"
        } else if (input$all_regions) {
          "All Regions"
        } else {
          input$la_input
        }
      )),
      Remove = "Remove",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    query_table$data <- rbind(query_table$data |> dplyr::select(-.internal_uuid), new_row)
  })


  # Render Reactable with unique button IDs using UUID
  output$react_query_table <- reactable::renderReactable({
    req(nrow(query_table$data)) # Ensure there is data to render
    query_table$data$.internal_uuid <- seq_len(nrow(query_table$data))

    reactable::reactable(
      query_table$data,
      columns = list(
        Remove = reactable::colDef(
          cell = reactable::JS(
            "function(cellInfo) {
            // Use the unique row identifier as the button ID
            const buttonId = 'remove_' + cellInfo.row['.internal_uuid'];
            return React.createElement(ButtonExtras, {
              id: buttonId,
              label: 'Remove',
              uuid: cellInfo.row['.internal_uuid'],
              column: cellInfo.column.id,
              class: 'button-extra',
              className: 'button-extra'
            }, cellInfo.index);
          }"
          )
        ),
        .internal_uuid = reactable::colDef(show = F)
      )
    )
  })

  # Reactive observer for remove buttons
  observe({
    req(nrow(query_table$data)) # Ensure there's data
    query_table$data$.internal_uuid <- seq_len(nrow(query_table$data))

    # Create observers for each row using the unique ID
    lapply(query_table$data$.internal_uuid, function(uuid) {
      # Create a unique input ID for each remove button
      remove_button_id <- paste0("remove_", uuid)

      # Observe the button click
      observeEvent(input[[remove_button_id]],
        {
          # Remove the corresponding row from query_table$data using the UUID
          query_table$data <- query_table$data[query_table$data$.internal_uuid != uuid, , drop = FALSE]
        },
        ignoreInit = TRUE
      )
    })
  })





  # Final output table based on saved queries
  output$react_final_output <- reactable::renderReactable({
    req(query_table$data)

    # Check if there are any selected measures
    if (nrow(query_table$data) == 0) {
      return(reactable::reactable(data.frame(Message = "Please add queries.")))
    }

    # Extracting filters while ensuring that lists are flattened
    selected_indicators <- unique(unlist(query_table$data$Measure))

    # Get max year and min year for reference only if there are selected indicators
    if (length(selected_indicators) > 0) {
      max_year <- bds_metrics |>
        dplyr::filter(Measure %in% selected_indicators) |>
        dplyr::pull(Years_num) |>
        max(na.rm = TRUE)

      min_year <- bds_metrics |>
        dplyr::filter(Measure %in% selected_indicators) |>
        dplyr::pull(Years_num) |>
        min(na.rm = TRUE)

      # Create a complete data frame for all years from min_year to max_year
      all_years <- data.frame(Years_num = seq(min_year, max_year))
    } else {
      return(reactable::reactable(data.frame(Message = "No data available for selected measures.")))
    }

    final_data <- data.frame() # Initialize the final_data variable

    # Loop through each row in the query table
    for (i in seq_len(nrow(query_table$data))) {
      # Get the current row values
      current_topic <- query_table$data$Topic[[i]]
      current_measure <- query_table$data$Measure[[i]]
      current_geog <- query_table$data$`LA and Regions`[[i]]

      # Create a temporary filtered data set for the current row
      temp_data <- bds_metrics |>
        dplyr::filter(
          Topic %in% current_topic,
          Measure %in% current_measure,
          `LA and Regions` %in% current_geog
        ) |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Topic,
          Measure, Years, Years_num, values_num, Values
        )

      # Create a complete data frame for the current temp_data
      # Merge the temp_data with the complete years data to fill in NAs
      complete_temp_data <- dplyr::full_join(
        temp_data,
        all_years,
        by = c("Years_num")
      ) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
          names_from = Years_num,
          values_from = values_num
        ) |>
        # Remove any NA rows (created from join)
        dplyr::filter(!dplyr::if_all(everything(), is.na))

      # Combine the filtered data into the final data frame
      final_data <- dplyr::bind_rows(final_data, complete_temp_data)
    }

    # Check if final_data has any rows before rendering
    if (nrow(final_data) == 0) {
      return(reactable::reactable(
        data.frame(Message = "No data available based on the selected queries.")
      ))
    }

    # Output final table
    reactable::reactable(final_data)
  })
}

shinyApp(ui, server)
