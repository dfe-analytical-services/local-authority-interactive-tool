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
    style = "overflow-y: visible; padding: 1rem;",

    # Using layout_column_wrap for responsive design
    bslib::layout_column_wrap(
      # Geographic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = "geog_input",
          label = "LAs, Regions, and England:",
          choices = c(la_names_bds, region_names_bds, "England"),
          multiple = TRUE,
          options = list(
            "placeholder" = "Select a LA, Region or England"
          )
        )
      ),
      # Topic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = "topic_input",
          label = "Topic:",
          choices = metric_topics
        )
      ),
      # Indicator input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = "indicator",
          label = "Indicator:",
          choices = metric_names,
          multiple = TRUE,
          options = list(
            "placeholder" = "Select an indicator"
          )
        )
      )
    ),
    bslib::layout_column_wrap(
      # Checkbox inputs for LAs, Regions, etc
      shiny::radioButtons(
        inputId = "la_groups",
        label = "LA Groupings (choose one)",
        choices = list(
          "None" = "no_groups",
          "Include All LAs" = "all_las",
          "Include LAs in the same Region" = "region_las",
          "Include statistical neighbours" = "la_stat_ns"
        ),
        selected = NULL,
        inline = FALSE
      ),
      div(
        style = "width: fit-content;",
        shiny::p("Other groupings"),
        shiny::checkboxInput("all_regions", "Include All Regions", FALSE),
        shiny::checkboxInput("inc_england", "Include England", FALSE),
        shinyWidgets::pickerInput(
          inputId = "year_range",
          label = "Select year range:",
          choices = NULL,
          multiple = TRUE
        ),
      )
    ),
    shiny::br(),
    # Action button
    shiny::actionButton("add_query", "Add table", class = "gov-uk-button")
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Staging Table (View of current selections)"),
    bslib::card(
      reactable::reactableOutput("staging_table")
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Summary of Selections"),
    bslib::card(
      reactable::reactableOutput("query_table")
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Output Table (View of all saved selections)"),
    bslib::card(
      reactable::reactableOutput("output_table")
    )
  )
)


server <- function(input, output, session) {
  # Input ----------------------------------
  # Reactive to store all selected indicators along with their topics
  selected_indicators <- reactiveVal({
    data.frame(
      Topic = character(),
      Measure = character()
    )
  })

  # Reactive expression to compute years_choices based on selected indicator
  years_choices <- reactive({
    # Filter the bds_metrics dataset based on the selected indicator
    years_dict <- bds_metrics |>
      dplyr::filter(Measure %in% input$indicator, !is.na(Years)) |>
      dplyr::distinct(Years, Years_num)

    consistent_year_suffix <- years_dict |>
      check_year_suffix_consistency()

    no_indicators <- length(input$indicator)

    if (no_indicators > 1 && !consistent_year_suffix) {
      years_dict$Years_num |>
        sort()
    } else {
      years_dict$Years |>
        sort()
    }
  })

  # Observe changes in the selected indicator and update pickerInput
  observeEvent(input$indicator, {
    # Update the pickerInput choices with the filtered years
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "year_range",
      choices = years_choices(),
      options = shinyWidgets::pickerOptions(
        maxOptions = 2,
        maxOptionsText = "Deselect a year",
        multipleSeparator = " to ",
        noneSelectedText = "All years available"
      )
    )
  })

  # Initialize the picker input with "Select an indicator" when the app starts
  observe({
    if (is.null(input$indicator) || length(input$indicator) == 0) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "year_range",
        choices = "Please select an indicator.",
        options = shinyWidgets::pickerOptions(
          noneSelectedText = "Select an indicator"
        )
      )
    }
  })

  # Filter indicator choices based on the selected topic
  shiny::observeEvent(input$topic_input, {
    filtered_topic_bds <- bds_metrics |>
      dplyr::filter(Topic %in% input$topic_input) |>
      dplyr::select(Topic, Measure) # Select both Topic and Measure

    # Get the currently selected indicators
    current_selection <- selected_indicators()

    # Combine currently selected indicators with the new filtered ones
    combined_choices <- unique(rbind(current_selection, filtered_topic_bds))

    # Update the choices with new topic whilst retaining the
    # already selected measures
    shiny::updateSelectizeInput(
      session = session,
      inputId = "indicator",
      choices = combined_choices$Measure,
      selected = current_selection$Measure
    )
  })

  # Update the selected values reactive for selected indicators
  # This keeps selection consistent across topics
  shiny::observeEvent(input$indicator,
    {
      # Get the current topic-filtered measures
      current_filtered <- bds_metrics |>
        dplyr::filter(
          Topic %in% input$topic_input,
          Measure %in% input$indicator
        ) |>
        dplyr::distinct(Topic, Measure)

      # Get previously selected indicators
      previous_selection <- selected_indicators()

      # Remove any indicators that have been deselected
      deselected_measures <- setdiff(previous_selection$Measure, input$indicator)
      updated_selection <- previous_selection |>
        dplyr::filter(!Measure %in% deselected_measures)

      # Combine the current filtered selection with the previous selection
      combined_selection <- unique(rbind(updated_selection, current_filtered))

      # Update the reactive value for all selected indicators
      selected_indicators(combined_selection)
    },
    ignoreNULL = FALSE
  )

  # Reactive to handle geography user input choices
  geog_inputs <- reactive({
    # Value from LA & Region input
    inputs <- input$geog_input

    # Add geography groupings
    # All LAs
    if (isTRUE(input$la_groups == "all_las")) {
      inputs <- unique(c(inputs, la_names_bds))
    }

    # All Regions
    if (isTRUE(input$all_regions)) {
      inputs <- unique(c(inputs, region_names_bds))
    }

    # Include England
    if (isTRUE(input$inc_england)) {
      inputs <- unique(c(inputs, "England"))
    }

    # All LAs from selected LA region
    if (isTRUE(input$la_groups == "region_las")) {
      selected_la_regions <- get_la_region(stat_n_geog, input$geog_input)
      all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)

      inputs <- unique(c(inputs, all_region_las))
    }

    # LA statistical neighbours
    if (isTRUE(input$la_groups == "la_stat_ns")) {
      selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, input$geog_input)

      inputs <- c(inputs, selected_la_stat_n)
    }

    # Return unique geographies
    unique(inputs)
  })

  # Assign LA statistical neighbours their selected LA association
  stat_n_association <- reactive({
    association_table <- data.frame(
      `LA and Regions` = character(),
      `sn_group` = character(),
      check.names = FALSE
    )

    # Create mini df of sns and selected LAs
    if (isTRUE(input$la_groups == "la_stat_ns")) {
      # Get LAs
      input_las <- intersect(input$geog_input, la_names_bds)
      # Build association df
      stat_n_groups <- lapply(input_las, function(la) {
        data.frame(
          `LA and Regions` = c(la, get_la_stat_neighbrs(stat_n_la, la)),
          `sn_group` = la,
          check.names = FALSE
        )
      })

      # Combine all statistical neighbour associations into a single data frame
      if (length(input_las) > 0) {
        association_table <- do.call(rbind, stat_n_groups)
      }
    }

    # Return the association data
    association_table
  })


  # Filter BDS for topic and indicator duos in selected values reactive
  # and geographies
  filtered_bds <- reactive({
    req(input$topic_input, input$indicator)
    req(nrow(selected_indicators()) > 0)

    # Filter the bds_metrics by topic, measure, and geography
    bds_filtered <- bds_metrics |>
      dplyr::semi_join(
        selected_indicators(),
        by = c(
          "Topic" = "Topic",
          "Measure" = "Measure"
        )
      ) |>
      dplyr::filter(
        `LA and Regions` %in% geog_inputs(),
        !is.na(Years)
      )

    # Check if all years have consistent suffix
    consistent_str_years <- check_year_suffix_consistency(bds_filtered)

    # If more than one indicator and not consistent cols use the cleaned cols
    if (length(input$indicator) > 1 && !consistent_str_years) {
      bds_filtered <- bds_filtered |>
        dplyr::mutate(
          Years = Years_num
        )
    }

    # Filter years
    if (length(input$year_range) == 1) {
      bds_filtered <- bds_filtered |>
        dplyr::filter(
          Years == input$year_range[1]
        )
    } else if (length(input$year_range) == 2) {
      bds_filtered <- bds_filtered |>
        dplyr::filter(
          Years >= input$year_range[1],
          Years <= input$year_range[2]
        )
    }

    # Return the filtered data with the associated LA column
    bds_filtered
  })


  # Reactive value "query" used to store query data
  # Uses lists to store multiple inputs (geographies)
  query <- reactiveValues(
    data = data.frame(
      Topic = I(list()),
      Indicator = I(list()),
      `LA and Regions` = I(list()),
      `Year range` = I(list()),
      `Click to remove query` = character(),
      `.internal_uuid` = numeric(),
      check.names = FALSE
    ),
    output = data.frame(
      `LA Number` = character(),
      `LA and Regions` = character(),
      Region = character(),
      Topic = character(),
      Measure = character(),
      check.names = FALSE
    )
  )

  # Build the staging table (select data and make wide)
  staging_table <- reactive({
    wide_table <- filtered_bds() |>
      dplyr::select(
        `LA Number`, `LA and Regions`, Topic,
        Measure, Years, Years_num, values_num, Values
      ) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
        names_from = Years,
        values_from = values_num,
      ) |>
      # Join region col
      dplyr::left_join(
        stat_n_geog |>
          dplyr::select(`LA num`, GOReg),
        by = c("LA Number" = "LA num")
      ) |>
      # Set regions and England as themselves for Region
      dplyr::mutate(GOReg = dplyr::case_when(
        `LA and Regions` %in% c("England", region_names_bds) ~ `LA and Regions`,
        TRUE ~ GOReg
      ))

    # Order columns (sort years)
    wide_table_ordered <- wide_table |>
      dplyr::select(
        `LA Number`, `LA and Regions`,
        "Region" = "GOReg",
        Topic, Measure,
        dplyr::all_of(sort_year_columns(wide_table))
      )


    # If sns included, add sns LA association column
    # Multi-join as want to include an association for every row (even duplicates)
    if (isTRUE(input$la_groups == "la_stat_ns")) {
      wide_table_ordered <- wide_table_ordered |>
        dplyr::left_join(
          stat_n_association(),
          by = "LA and Regions",
          relationship = "many-to-many"
        ) |>
        dplyr::relocate(sn_group, .after = "Measure") |>
        dplyr::rename("Statistical Neighbour Group" = "sn_group")
    }

    wide_table_ordered
  })

  # Staging table output
  output$staging_table <- reactable::renderReactable({
    # Display messages if there are incorrect selections
    if (is.null(geog_inputs()) && is.null(input$geog_input)) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add selections (above).",
          check.names = FALSE
        )
      ))
    } else if (is.null(input$indicator)) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add an indicator selection (above).",
          check.names = FALSE
        )
      ))
    } else if (is.null(geog_inputs())) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add a geography selection (above).",
          check.names = FALSE
        )
      ))
    }

    # Output table
    reactable::reactable(
      staging_table()
    )
  })

  # When "Add table" button clicked - add query to saved queries
  observeEvent(input$add_query, {
    # Check if anything selected
    if (length(geog_inputs()) > 0 && nrow(selected_indicators()) > 0) {
      # Get a unique identifier for this new query (UUID)
      new_uuid <- max(c(0, query$data$.internal_uuid), na.rm = TRUE) + 1

      # Get the full range of available years from the reactive expression
      available_years <- range(years_choices())

      # Define the year range display logic
      year_range_display <- dplyr::case_when(
        length(input$year_range) == 0 ~ paste0(
          "All years (", available_years[1], " to ", available_years[2], ")"
        ),
        length(input$year_range) == 2 ~ paste(
          input$year_range[1], "to", input$year_range[2]
        ),
        TRUE ~ input$year_range[1]
      )

      # Get query information
      # Format with commas and line breaks
      new_query <- data.frame(
        .internal_uuid = new_uuid, # Assign the new UUID to the query
        Topic = I(list(selected_indicators()$Topic)),
        Indicator = paste(selected_indicators()$Measure, collapse = ",<br>"),
        `LA and Regions` = paste(
          get_geog_selection(input, la_names_bds, region_names_bds, stat_n_geog),
          collapse = ",<br>"
        ),
        `Year range` = year_range_display,
        `Click to remove query` = "Remove",
        check.names = FALSE
      )

      # Append query to existing query data
      query$data <- query$data |>
        rbind(new_query)

      # Append the corresponding staging table data with the same .internal_uuid
      staging_data <- staging_table()
      staging_data$.internal_uuid <- new_uuid # Assign the same UUID to staging data

      # If more than one indicator and not consistent cols use the cleaned cols
      # Check if all years have consistent suffix
      consistent_staging_final_yrs <- data.frame(
        Years = c(
          colnames(query$output)[grepl("^\\d{4}", colnames(query$output))],
          colnames(staging_data)[grepl("^\\d{4}", colnames(staging_data))]
        )
      ) |>
        check_year_suffix_consistency()

      staging_indicators <- query$data |>
        pull_uniques("Indicator")
      final_table_indicators <- query$output |>
        pull_uniques("Measure")
      total_unique_indicators <- unique(c(staging_indicators, final_table_indicators)) |>
        length()

      if (total_unique_indicators > 1 && !consistent_staging_final_yrs && nrow(query$output) > 0) {
        query$output <- query$output |>
          rename_columns_with_year() |>
          dplyr::bind_rows(rename_columns_with_year(staging_data))
      } else {
        query$output <- query$output |>
          dplyr::bind_rows(staging_data)
      }
    }
  })

  # Query table output
  output$query_table <- reactable::renderReactable({
    req(nrow(query$data))

    # Check if there are any selected queries
    if (nrow(query$data) == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No saved selections.",
          check.names = FALSE
        )
      ))
    }

    reactable::reactable(
      query$data,
      columns = list(
        Indicator = html_colDef(),
        `LA and Regions` = html_colDef(),
        `Click to remove query` = reactable::colDef(
          cell = reactable::JS(
            "function(cellInfo) {
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
        # Customize the other columns (e.g., Topic)
        Topic = reactable::colDef(
          cell = function(value) {
            unique_values <- unique(unlist(value))
            if (length(unique_values) > 0) {
              return(paste(unique_values, collapse = ",<br>"))
            } else {
              return("")
            }
          },
          html = TRUE
        ),
        .internal_uuid = reactable::colDef(show = FALSE)
      )
    )
  })

  # Remove query button
  observe({
    req(nrow(query$data))
    # Create button observers for each row using the unique row identifier
    lapply(query$data$.internal_uuid, function(uuid) {
      # Create matching row identifier for each remove button
      remove_button_id <- paste0("remove_", uuid)

      # Observe the button click
      observeEvent(input[[remove_button_id]],
        {
          # Remove the corresponding row from query$data using the row identifier
          query$data <- query$data[query$data$.internal_uuid != uuid, , drop = FALSE]

          # Also remove the corresponding rows from query$output
          query$output <- query$output[query$output$.internal_uuid != uuid, , drop = FALSE]
        },
        ignoreInit = TRUE
      )
    })
  })

  # Final output table (based on saved queries)
  output$output_table <- reactable::renderReactable({
    req(nrow(query$data))

    # Check if there are any selected queries
    if (nrow(query$data) == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No saved selections.",
          check.names = FALSE
        )
      ))
    }

    output_indicators <- query$output |>
      pull_uniques("Measure")

    share_year_suffix <- bds_metrics |>
      dplyr::filter(
        Measure %in% output_indicators,
        !is.na(Years)
      ) |>
      check_year_suffix_consistency()



    if (length(output_indicators) == 1 || share_year_suffix) {
      years_dict <- bds_metrics |>
        dplyr::filter(
          Measure %in% output_indicators,
          !is.na(Years)
        ) |>
        dplyr::distinct(Years, Years_num)

      # Replace the matching column names in query$output using Years_num -> Years
      new_col_names <- colnames(query$output) |>
        (\(cols) ifelse(cols %in% years_dict$Years_num,
          years_dict$Years[match(cols, years_dict$Years_num)],
          cols
        ))()

      # Apply the new column names to query$output
      colnames(query$output) <- new_col_names

      # Drop columns not in the new_col_names years
      valid_year_cols <- years_dict$Years
      query$output <- query$output[, colnames(query$output) %in% valid_year_cols | !grepl("^\\d{4}", colnames(query$output))]
    }

    query_table_ordered_cols <- query$output |>
      dplyr::select(
        `LA Number`, `LA and Regions`,
        Region, Topic, Measure,
        tidyselect::any_of("Statistical Neighbour Group"),
        dplyr::all_of(sort_year_columns(query$output))
      )


    # Display the final query table data
    reactable::reactable(query_table_ordered_cols)
  })
}

shinyApp(ui, server)
