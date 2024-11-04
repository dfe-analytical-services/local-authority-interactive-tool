# nolint start: object_name
#
# Create Own main user inputs ==================================================
# Can choose Geography, Topic, Indicator,
# LA group, England, Regions, Year range and the "Add selection" button
# Create Own main user inputs UI -----------------------------------------------
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
      shiny::actionButton(ns("add_query"), "Add selections", class = "gov-uk-button")
    )
  )
}

# Create Own Inputs Server -----------------------------------------------------
Create_MainInputsServer <- function(id, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Reactive to store all selected topic-indicator pairs
    # Used to filter BDS correctly (due to duplication of indicator names
    # across topics)
    selected_indicators <- reactiveVal({
      data.frame(
        Topic = character(),
        Measure = character()
      )
    })

    # Filter indicator choices based on the selected topic
    # But keep already selected indicators from other topics
    shiny::observeEvent(input$topic_input, {
      # Available indicators (based on topic chosen)
      filtered_topic_bds <- bds_metrics |>
        dplyr::filter(Topic %in% input$topic_input) |>
        dplyr::select(Topic, Measure)

      # Get the already selected topic-indicator pairs
      current_selection <- selected_indicators()

      # Combine already selected topic-indicator pairs with new topic indicators
      # Allows indicators to stay selected despite not being part of the new topic
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

    # Return create your own main inputs
    create_inputs <- list(
      geog = reactive(input$geog_input),
      topic = reactive(selected_indicators()$Topic),
      indicator = reactive(selected_indicators()$Measure),
      selected_indicators = reactive(selected_indicators()),
      la_group = reactive(input$la_group),
      inc_regions = reactive(input$inc_regions),
      inc_england = reactive(input$inc_england),
      add_query = reactive(input$add_query)
    )

    # Return inputs
    create_inputs
  })
}


# Year range input UI ----------------------------------------------------------
YearRangeUI <- function(id) {
  ns <- NS(id)

  shinyWidgets::pickerInput(
    ns("year_range"),
    "Select Year Range",
    choices = NULL,
    multiple = TRUE
  )
}

# Year range input server ------------------------------------------------------
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

    # Collect selected year range and available year choices
    # (choices are used in query table to set year range info)
    year_input <- list(
      range = reactive(input$year_range),
      choices = years_choices
    )

    # Return year inputs
    year_input
  })
}


# Geography grouping -----------------------------------------------------------
# Combines the user geography input with any additional geography groupings
GroupingInputServer <- function(id,
                                create_inputs,
                                la_names_bds,
                                region_names_bds,
                                stat_n_geog,
                                stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Combine the geography selections
    geog_inputs <- reactive({
      # Value from main geography input
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

    # Return full geography input
    geog_inputs
  })
}


# Statistical neighbour association --------------------------------------------
# Assign statistical neighbours their parent LA association
StatN_AssociationServer <- function(id, create_inputs, la_names_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    stat_n_association <- reactive({
      # Only if SN grouping selected compute rest of module
      req(create_inputs$la_group() == "la_stat_ns")

      # Create mini association df of SNs and their parent LA
      association_table <- data.frame(
        `LA and Regions` = character(),
        `sn_parent` = character(),
        check.names = FALSE
      )

      # Get parent LAs from geogs selected (all LAs in main geog input)
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

      # Return the association df
      association_table
    })
  })
}

# nolint end
