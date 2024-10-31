Create_MainInputsUI <- function(id) {
  ns <- NS(id)

  bslib::layout_column_wrap(
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
    create_main_inputs <- list(
      geog = reactive({
        input$geog_input
      }),
      topic = reactive({
        selected_indicators()$Topic
      }),
      indicator = reactive({
        selected_indicators()$Measure
      })
    )

    return(create_main_inputs)
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


GroupingInputUI <- function(id) {
  ns <- NS(id)

  tagList(
    "LA groups" = shiny::radioButtons(
      inputId = ns("la_groups"),
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
    "Other groups" = list(
      shiny::p("Other groupings:"),
      shiny::checkboxInput(ns("all_regions"), "Include All Regions", FALSE),
      shiny::checkboxInput(ns("inc_england"), "Include England", FALSE)
    )
  )
}

GroupingInputServer <- function(id, geog_input, la_names_bds, region_names_bds, stat_n_geog, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Collating user selections ==================================================
    # Geography inputs -----------------------------------------------------------
    geog_inputs <- reactive({
      # Value from LA & Region input
      inputs <- geog_input()

      # Add geography groupings (if selected)
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
        selected_la_regions <- get_la_region(stat_n_geog, geog_input())
        all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)

        inputs <- unique(c(inputs, all_region_las))
      }

      # LA statistical neighbours
      if (isTRUE(input$la_groups == "la_stat_ns")) {
        selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, geog_input())

        inputs <- c(inputs, selected_la_stat_n)
      }

      # Return unique geographies
      unique(inputs)
    })

    geog_inputs
  })
}
