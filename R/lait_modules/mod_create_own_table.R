Create_MainInputsUI <- function(id) {
  ns <- NS(id)

  tagList(
    "Main choices" = bslib::layout_column_wrap(
      # Geographic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = ns("geog_input"),
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
          inputId = ns("topic_input"),
          label = "Topic:",
          choices = metric_topics
        )
      ),
      # Indicator input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = ns("indicator"),
          label = "Indicator:",
          choices = metric_names,
          multiple = TRUE,
          options = list(
            "placeholder" = "Select an indicator"
          )
        )
      )
    ),
    # LA groupings
    "LA grouping" = shiny::radioButtons(
      inputId = ns("la_group"),
      label = "LA Groupings (choose one):",
      choices = list(
        "None" = "no_groups",
        "Include All LAs" = "all_las",
        "Include LAs in the same Region" = "region_las",
        "Include statistical neighbours" = "la_stat_ns"
      ),
      selected = NULL,
      inline = FALSE
    ),
    # Other groupings
    "Other grouping" = div(
      shiny::p("Other groupings:"),
      shiny::checkboxInput(ns("inc_regions"), "Include All Regions", FALSE),
      shiny::checkboxInput(ns("inc_england"), "Include England", FALSE)
    ),
    # Add selection (query) button
    "Add selection" = div(
      style = "height: 100%; display: flex; justify-content: center; align-items: flex-end;",
      shiny::actionButton("add_query", "Add selections", class = "gov-uk-button")
    )
  )
}

Create_MainInputsServer <- function(id, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Geography and Indicator inputs ---------------------------------------------
    # Reactive to store all selected indicators along with their topics
    selected_indicators <- reactiveVal({
      data.frame(
        Topic = character(),
        Measure = character()
      )
    })

    # Filter indicator choices based on the selected topic
    shiny::observeEvent(input$topic_input, {
      # Available indicators (based on topic chosen)
      filtered_topic_bds <- bds_metrics |>
        dplyr::filter(Topic %in% input$topic_input) |>
        dplyr::select(Topic, Measure)

      # Get the currently selected topic-indicator pairs
      current_selection <- selected_indicators()

      # Combine currently selected topic-indicator pairs with the new filtered ones
      combined_choices <- unique(rbind(current_selection, filtered_topic_bds))

      # Update the choices with new topic whilst retaining the
      # already selected indicators
      shiny::updateSelectizeInput(
        session = session,
        inputId = "indicator",
        choices = combined_choices$Measure,
        selected = current_selection$Measure
      )
    })

    # Update the selected_indicators reactive for newly selected topic-indicator pairs
    # This keeps selection consistent across topics
    shiny::observeEvent(input$indicator,
      {
        # Get the new topic-indicator pairs
        current_filtered <- bds_metrics |>
          dplyr::filter(
            Topic %in% input$topic_input,
            Measure %in% input$indicator
          ) |>
          dplyr::distinct(Topic, Measure)

        # Get previously selected indicators
        previous_selection <- selected_indicators()

        # Remove any topic-indicator pairs that have been deselected
        deselected_measures <- setdiff(previous_selection$Measure, input$indicator)
        updated_selection <- previous_selection |>
          dplyr::filter(!Measure %in% deselected_measures)

        # Combine the new topic-indicator pairs with the previous selections
        combined_selection <- unique(rbind(updated_selection, current_filtered))

        # Update the reactive value for all topic-indicator pairs
        selected_indicators(combined_selection)
      },
      ignoreNULL = FALSE
    )

    # Return create your own table main inputs
    create_inputs <- list(
      geog = reactive({
        input$geog_input
      }),
      topic = reactive({
        selected_indicators()$Topic
      }),
      indicator = reactive({
        selected_indicators()$Measure
      }),
      selected_indicators = reactive({
        selected_indicators()
      }),
      la_group = reactive({
        input$la_group
      }),
      inc_regions = reactive({
        input$inc_regions
      }),
      inc_england = reactive({
        input$inc_england
      }) # ,
      # add_query = reactive({
      #   input$add_query
      # }),
    )

    return(create_inputs)
  })
}



# Year range input module
YearRangeUI <- function(id) {
  ns <- NS(id)

  shinyWidgets::pickerInput(
    ns("year_range"),
    "Select Year Range",
    choices = NULL,
    multiple = TRUE
  )
}

YearRangeServer <- function(id, bds_metrics, indicator_input) {
  moduleServer(id, function(input, output, session) {
    # Compute years choices available based on selected indicator
    years_choices <- reactive({
      years_dict <- bds_metrics |>
        dplyr::filter(Measure %in% indicator_input(), !is.na(Years)) |>
        dplyr::distinct(Years, Years_num)

      # Boolean to check for matching years' suffixes
      consistent_year_suffix <- years_dict |>
        check_year_suffix_consistency()

      # Display string years if matching suffix (numeric/clean if not)
      if (consistent_year_suffix) {
        sort(years_dict$Years)
      } else {
        sort(years_dict$Years_num)
      }
    })

    # Update the year range choices based on the selected indicator
    observeEvent(indicator_input(), {
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

    # When no indicators selected year range displays "Select an indicator"
    observe({
      if (is.null(indicator_input()) || length(indicator_input()) == 0) {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "year_range",
          choices = "Please select an indicator first",
          options = shinyWidgets::pickerOptions(
            noneSelectedText = "Select an indicator to see year range"
          )
        )
      }
    })

    return(year_range = reactive(input$year_range))
  })
}


GroupingInputServer <- function(id, create_inputs, la_names_bds, region_names_bds, stat_n_geog, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Collating user selections ==================================================
    # Geography inputs -----------------------------------------------------------
    geog_inputs <- reactive({
      # Value from LA & Region input
      inputs <- create_inputs$geog()

      # Add geography groupings (if selected)
      # All LAs
      if (isTRUE(create_inputs$la_group() == "all_las")) {
        inputs <- unique(c(inputs, la_names_bds))
      }

      # All Regions
      if (isTRUE(create_inputs$inc_regions())) {
        inputs <- unique(c(inputs, region_names_bds))
      }

      # Include England
      if (isTRUE(create_inputs$inc_england())) {
        inputs <- unique(c(inputs, "England"))
      }

      # All LAs from selected LA region
      if (isTRUE(create_inputs$la_group() == "region_las")) {
        selected_la_regions <- get_la_region(stat_n_geog, create_inputs$geog())
        all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)

        inputs <- unique(c(inputs, all_region_las))
      }

      # LA statistical neighbours
      if (isTRUE(create_inputs$la_group() == "la_stat_ns")) {
        selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, create_inputs$geog())

        inputs <- c(inputs, selected_la_stat_n)
      }

      # Return unique geographies
      unique(inputs)
    })

    geog_inputs
  })
}



# Statistical neighbour input ------------------------------------------------
# Assign LA statistical neighbours their selected LA association
StatN_AssociationServer <- function(id, create_inputs, la_names_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    stat_n_association <- reactive({
      # If SN selected create the SN association df
      req(create_inputs$la_group() == "la_stat_ns")

      # Create mini association df of SN LAs and their parent SN LA
      association_table <- data.frame(
        `LA and Regions` = character(),
        `sn_parent` = character(),
        check.names = FALSE
      )

      # Get LAs from geogs selected
      input_las <- intersect(create_inputs$geog(), la_names_bds)

      if (length(input_las) > 0) {
        # Build association df (include LA itself too)
        stat_n_groups <- lapply(input_las, function(la) {
          data.frame(
            `LA and Regions` = c(la, get_la_stat_neighbrs(stat_n_la, la)),
            `sn_parent` = la,
            check.names = FALSE
          )
        })

        # Combine all statistical neighbour associations into a single data frame
        association_table <- do.call(rbind, stat_n_groups)
      }

      # Return the association data
      association_table
    })
  })
}



StagingBDSServer <- function(id, create_inputs, geog_groups, year_range, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Staging table ==============================================================

    # Filter BDS for topic-indicator pairs in the selected_values reactive
    topic_indicator_bds <- reactive({
      req(nrow(create_inputs$selected_indicators()) > 0)
      bds_metrics |>
        dplyr::semi_join(
          create_inputs$selected_indicators(),
          by = c(
            "Topic" = "Topic",
            "Measure" = "Measure"
          )
        )
    })

    # Filter BDS for geographies and year range
    staging_bds <- reactive({
      req(geog_groups(), topic_indicator_bds())
      # Filter the bds_metrics geography
      filtered_bds <- topic_indicator_bds() |>
        dplyr::filter(
          `LA and Regions` %in% geog_groups(),
          !is.na(Years)
        )

      # Cleaning Years
      # Check if all years have consistent suffix
      consistent_str_years <- check_year_suffix_consistency(filtered_bds)

      # If not consistent suffix use the cleaned year cols (numeric years)
      if (!consistent_str_years) {
        filtered_bds <- filtered_bds |>
          dplyr::mutate(
            Years = Years_num
          )
      }

      # Apply the year range filter
      # If only one year selected then show just that year
      if (length(year_range()) == 1) {
        filtered_bds <- filtered_bds |>
          dplyr::filter(
            Years == year_range()[1]
          )
      } else if (length(year_range()) == 2) {
        filtered_bds <- filtered_bds |>
          dplyr::filter(
            Years >= year_range()[1],
            Years <= year_range()[2]
          )
      }

      # Return the user selection filtered data for staging table
      filtered_bds
    })

    staging_bds
  })
}



StagingDataServer <- function(
    id, create_inputs, staging_bds, region_names_bds, la_names_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Stat neighbour association table
    stat_n_association <- StatN_AssociationServer(
      "stat_n_association",
      create_inputs,
      la_names_bds,
      stat_n_la
    )

    # Build the staging table
    staging_table <- reactive({
      # Selected relevant cols
      # Coerce to wide format
      # Join region col (set regions and England as themselves for Region)
      wide_table <- staging_bds() |>
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
        dplyr::mutate(GOReg = dplyr::case_when(
          `LA and Regions` %in% c("England", region_names_bds) ~ `LA and Regions`,
          TRUE ~ GOReg
        ))

      # Order columns (and sort year cols order)
      wide_table_ordered <- wide_table |>
        dplyr::select(
          `LA Number`, `LA and Regions`,
          "Region" = "GOReg",
          Topic, Measure,
          dplyr::all_of(sort_year_columns(wide_table))
        )


      # If SNs included, add SN LA association column
      # Multi-join as want to include an association for every row (even duplicates)
      if (isTRUE(create_inputs$la_group() == "la_stat_ns")) {
        wide_table_ordered <- wide_table_ordered |>
          dplyr::left_join(
            stat_n_association(),
            by = "LA and Regions",
            relationship = "many-to-many"
          ) |>
          dplyr::relocate(sn_parent, .after = "Measure") |>
          dplyr::rename("Statistical Neighbour Group" = "sn_parent")
      }

      # Staging table formatted and ready for output
      wide_table_ordered
    })

    staging_table
  })
}



StagingTableUI <- function(id) {
  ns <- NS(id)

  # Staging table ==============================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Staging Table (View of current selections)"),
    bslib::card(
      reactable::reactableOutput(ns("staging_table"))
    )
  )
}

StagingTableServer <- function(id,
                               create_inputs,
                               staging_bds,
                               region_names_bds,
                               la_names_bds,
                               stat_n_la,
                               geog_groups,
                               year_range,
                               bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Staging table output -------------------------------------------------------
    output$staging_table <- reactable::renderReactable({
      # Display messages if there are incorrect selections
      if (length(create_inputs$indicator()) == 0 && is.null(create_inputs$geog())) {
        return(reactable::reactable(
          data.frame(
            `Message from tool` = "Please add selections (above).",
            check.names = FALSE
          )
        ))
      } else if (length(create_inputs$indicator()) == 0) {
        return(reactable::reactable(
          data.frame(
            `Message from tool` = "Please add an indicator selection (above).",
            check.names = FALSE
          )
        ))
      } else if (is.null(create_inputs$geog())) {
        return(reactable::reactable(
          data.frame(
            `Message from tool` = "Please add a geography selection (above).",
            check.names = FALSE
          )
        ))
      }

      # Filtering BDS for staging data
      staging_bds <- StagingBDSServer(
        "staging_bds",
        create_inputs,
        geog_groups,
        year_range,
        bds_metrics
      )

      # Build staging data
      staging_data <- StagingDataServer(
        "staging_data",
        create_inputs,
        staging_bds,
        region_names_bds,
        la_names_bds,
        stat_n_la
      )

      # Output table - formatting numbers, long text and page settings
      dfe_reactable(
        staging_data(),
        columns = utils::modifyList(
          format_num_reactable_cols(
            staging_data(),
            get_indicator_dps(staging_bds()),
            num_exclude = c("LA Number", "Measure")
          ),
          list(
            set_custom_default_col_widths(
              Measure = set_min_col_width(90)
            ),
            # Truncates long cell values and displays hover with full value
            Measure = reactable::colDef(
              html = TRUE,
              cell = function(value, index, name) {
                render.reactable.cell.with.tippy(text = value, tooltip = value)
              }
            )
          )
        ),
        defaultPageSize = 3,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(3, 5, 10, 25),
        compact = TRUE
      )
    })
  })
}
