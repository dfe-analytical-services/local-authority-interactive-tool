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
        shiny::selectInput(
          inputId = "geog_input",
          label = "LAs, Regions, and England:",
          choices = c(la_names_bds, region_names_bds, "England"),
          multiple = TRUE
        )
      ),
      # Topic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectInput(
          inputId = "topic_input",
          label = "Topic:",
          choices = metric_topics
        )
      ),
      # Indicator input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectInput(
          inputId = "indicator",
          label = "Indicator:",
          choices = metric_names,
          multiple = TRUE
        )
      )
    ),
    bslib::layout_column_wrap(
      # Checkbox inputs for LAs, Regions, etc
      shiny::radioButtons(
        inputId = "la_groups",
        label = "LA Groupings",
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
        shiny::p("Other groupings"),
        shiny::checkboxInput("all_regions", "Include All Regions", FALSE)
      )
    ),
    shiny::br(),
    # Action button
    shiny::actionButton("add_query", "Add Query", class = "gov-uk-button")
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
  # Reactive to store all selected indicators along with their topics
  selected_indicators <- reactiveVal({
    data.frame(
      Topic = character(),
      Measure = character()
    )
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
    updateSelectInput(
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
      if (length(stat_n_groups) > 0) {
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
      `Click to remove query` = character(),
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
      dplyr::left_join(
        stat_n_geog |>
          dplyr::select(`LA num`, GOReg),
        by = c("LA Number" = "LA num")
      ) |>
      # Set regions and England as themselves for Region
      dplyr::mutate(GOReg = dplyr::case_when(
        `LA and Regions` %in% c("England", region_names_bds) ~ `LA and Regions`,
        TRUE ~ GOReg
      )) |>
      dplyr::relocate(GOReg, .after = `LA and Regions`) |>
      dplyr::rename("Region" = "GOReg")

    # If sns included, add sns LA association column
    # Multi-join as want to include an association for every row (even duplicates)
    if (isTRUE(input$la_groups == "la_stat_ns")) {
      wide_table <- wide_table |>
        dplyr::left_join(
          stat_n_association(),
          by = "LA and Regions",
          relationship = "many-to-many"
        ) |>
        dplyr::relocate(sn_group, .after = "Measure") |>
        dplyr::rename("Statistical Neighbour group" = "sn_group")
    }

    wide_table
  })

  # Staging table output
  output$staging_table <- reactable::renderReactable({
    # Display messages if there are incorrect selections
    if (is.null(input$indicator) && is.null(input$geog_input)) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add data selections (above).",
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
    } else if (is.null(input$geog_input)) {
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

  # When "Add query" button clicked - add query to saved queries
  observeEvent(input$add_query, {
    # Check if anything selected
    if (length(geog_inputs()) > 0 && nrow(selected_indicators()) > 0) {
      # Get query information
      new_query <- data.frame(
        Topic = I(list(selected_indicators()$Topic)),
        Indicator = I(list(selected_indicators()$Measure)),
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
    }
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
        # Returning unique topics
        Topic = reactable::colDef(
          cell = function(value) {
            unique_values <- unique(unlist(value))
            if (length(unique_values) > 0) {
              return(paste(unique_values, collapse = ", "))
            } else {
              return("")
            }
          }
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

    ### Main logic for query data processing
    # Get indicators (& number of)
    selected_indicators <- unique(unlist(query$data$Indicator))
    no_selected_indicators <- length(selected_indicators)

    # Message for no indicators
    if (no_selected_indicators == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No years available for selected measures.",
          check.names = FALSE
        )
      ))
    }

    # Logic to calculate what to do with years cols
    # Check if consistent suffix (no cleaning needed)
    # Create an all years df (min to max years of dataset)
    # for expanding data so rbind works
    # Filter BDS for selected indicators
    raw_indicator_filtered_bds <- filter_bds_for_indicators(bds_metrics, selected_indicators)

    # Check if all years have consistent suffix
    query_str_years <- check_year_suffix_consistency(raw_indicator_filtered_bds)

    # Get min and max years
    year_bounds <- get_min_max_years(raw_indicator_filtered_bds)
    all_years <- create_years_df(year_bounds$min_year, year_bounds$max_year)

    # Initialise output table
    final_query_data <- data.frame()

    # Loop through each row in the query table and process data
    for (i in seq_len(nrow(query$data))) {
      # Extract current query topics, indicators and geogs
      current_topic <- query$data$Topic[[i]]
      current_measure <- query$data$Indicator[[i]]
      current_geog <- query$data$`LA and Regions`[[i]]

      # Create a data frame for the current topic-indicator pairings
      current_topic_indicator <- data.frame(
        Topic = current_topic,
        Indicator = current_measure
      )

      # Sorting geography filters - from query (shortened names)
      # Adding all LAs in selected LA regions
      if (any(grepl("LAs in ", current_geog))) {
        current_la_regions <- current_geog |>
          stringr::str_subset("^LAs in ") |>
          stringr::str_remove("^LAs in ")

        # Extract all LAs in the regions and add to current geog
        current_region_las <- get_las_in_regions(stat_n_geog, current_la_regions)

        # Add all LAs in Region (also includes clean selected LA...
        # i.e., no "LAs in..."
        current_geog <- c(current_geog, current_region_las)
      }

      # Stat neigh inclusion param
      include_stat_n <- FALSE

      # Adding statistical neighbours of selected LAs
      if (any(grepl(" statistical neighbours", current_geog))) {
        include_stat_n <- TRUE
        current_la_sns <- current_geog |>
          stringr::str_subset(" statistical neighbours$") |>
          stringr::str_remove(" statistical neighbours$")

        # Extract all statistical neighbours
        selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, current_la_sns)

        # Add stat neigh LAs and cleaned LA (minus the "statistical...")
        current_geog <- c(current_geog, current_la_sns, selected_la_stat_n)

        # Adding sn LA associations
        current_association <- data.frame(
          `LA and Regions` = character(),
          `sn_group` = character(),
          check.names = FALSE
        )

        # Create mini df of sns and selected LA
        current_stat_n_groups <- lapply(current_la_sns, function(la) {
          data.frame(
            `LA and Regions` = c(la, get_la_stat_neighbrs(stat_n_la, la)),
            `sn_group` = la,
            check.names = FALSE
          )
        })

        # Combine all statistical neighbour associations into a single data frame
        if (length(current_stat_n_groups) > 0) {
          current_association <- do.call(rbind, current_stat_n_groups)
        }
      }

      # Append all LAs or Regions to the current geography if needed
      if ("All LAs" %in% current_geog) current_geog <- c(current_geog, la_names_bds)
      if ("All Regions" %in% current_geog) current_geog <- c(current_geog, region_names_bds)

      # Collect current geog filters
      current_geog <- unique(current_geog)

      ### Main current query filtering
      # Filter BDS for the current query
      raw_query_data <- bds_metrics |>
        # Semi join to filter for indicator & topic (avoids duplicates)
        dplyr::semi_join(
          current_topic_indicator,
          by = c(
            "Topic" = "Topic",
            "Measure" = "Indicator"
          )
        ) |>
        dplyr::filter(
          `LA and Regions` %in% current_geog,
          !is.na(Years)
        ) |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Topic, Measure,
          Years, Years_num, values_num, Values
        )

      # Join the data with the full years to fill gaps in years
      full_query_data <- dplyr::full_join(
        raw_query_data,
        all_years,
        by = "Years_num"
      ) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
          names_from = ifelse(query_str_years || no_selected_indicators == 1,
            "Years",
            "Years_num"
          ),
          values_from = values_num
        )

      # Sort year columns with full names preserved
      sorted_year_cols <- sort_year_columns(full_query_data)

      # Clean data for new col order and any NAs from missing year joins
      clean_query_data <- full_query_data |>
        dplyr::filter(!dplyr::if_all(everything(), is.na)) |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Topic, Measure,
          dplyr::all_of(sorted_year_cols)
        )

      # If sns included, add sns LA association column
      if (include_stat_n) {
        clean_query_data <- clean_query_data |>
          dplyr::left_join(
            current_association,
            by = "LA and Regions",
            relationship = "many-to-many"
          ) |>
          dplyr::relocate(sn_group, .after = "Measure") |>
          dplyr::rename("Statistical Neighbour group" = "sn_group")
      }

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
