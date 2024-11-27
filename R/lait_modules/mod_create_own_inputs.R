# nolint start: object_name
#
# Create Own main user inputs ==================================================
# Can choose Geography, Topic, Indicator,
# LA group, England, Regions, Year range and the "Add selection" button
# Create Own main user inputs UI -----------------------------------------------
#
#' Create Main User Inputs UI
#'
#' This function creates a user interface for selecting geographical areas,
#' topics, and indicators for the "Create Your Own" feature in the Shiny app.
#' It includes inputs for LA groupings and options to include all regions
#' or England, along with an action button to add selections.
#'
#' @param id A unique identifier for the Shiny module.
#' @return A list of UI elements for user input.
#'
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
            "placeholder" = "Start typing or scroll to find a LA, Region or England...",
            plugins = list("remove_button")
          )
        )
      ),
      # Topic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = ns("topic_input"),
          label = "Topic:",
          choices = c("All Topics", metric_topics),
          selected = "All Topics",
          options = list(
            placeholder = "No topic selected, showing all indicators.",
            plugins = list("clear_button")
          )
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
            "placeholder" = "Start typing or scroll to find an indicator...",
            plugins = list("remove_button")
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
    # Clear all current selections
    "Clear all current selections" = div(
      style = "height: 100%; display: flex; justify-content: center; align-items: flex-end;",
      shinyGovstyle::button_Input(
        inputId = ns("clear_all"),
        label = "Clear all current selections",
        type = "warning"
      )
    ),
    # Add selection (query) button
    "Add selection" = div(
      style = "height: 100%; display: flex; justify-content: center; align-items: flex-end;",
      shinyGovstyle::button_Input(
        inputId = ns("add_query"),
        label = "Add selections",
        type = "start"
      )
    )
  )
}

# Create Own Inputs Server -----------------------------------------------------
#
#' Create Main User Inputs Server
#'
#' This function handles the server logic for the user inputs in the
#' "Create Your Own" feature of the Shiny app. It allows users to select
#' topics and indicators, managing the selections to ensure consistency
#' across different topics. It also provides reactive outputs for the
#' selected inputs.
#'
#' @param id A unique identifier for the Shiny module.
#' @param bds_metrics A data frame containing the available metrics
#'                   used for filtering indicators based on selected topics.
#' @return A list of reactive values containing user inputs.
#'
Create_MainInputsServer <- function(id, topic_indicator_full) {
  moduleServer(id, function(input, output, session) {
    # Reactive to store all selected topic-indicator pairs
    # Used to filter BDS correctly (due to duplication of indicator names
    # across topics)
    selected_indicators <- reactiveVal(NULL)

    # Filter indicator choices based on the selected topic
    # But keep already selected indicators from other topics
    shiny::observeEvent(input$topic_input,
      {
        req(input$topic_input)
        # Available indicators (based on topic chosen)
        topic_indicators <- topic_indicator_full |>
          filter_by_topic("Topic", input$topic_input) |>
          pull_uniques("Measure")

        # Get the already selected topic-indicator pairs
        current_selection <- selected_indicators()

        # Combine already selected topic-indicator pairs with new topic indicators
        # Allows indicators to stay selected despite not being part of the new topic
        # Ensure only valid indicators are retained
        combined_choices <- unique(c(current_selection, topic_indicators))

        # Update the choices with new topic whilst retaining the
        # already selected indicators
        shiny::updateSelectizeInput(
          session = session,
          inputId = "indicator",
          choices = combined_choices,
          selected = current_selection
        )
      },
      priority = 1
    )

    # Update the selected_indicators reactive for newly selected topic-indicator pairs
    # This keeps selection consistent across topics
    shiny::observeEvent(input$indicator,
      {
        # Get the new topic-indicator pairs
        current_filtered <- topic_indicator_full |>
          dplyr::filter(
            Measure %in% input$indicator
          ) |>
          pull_uniques("Measure")

        # Get previously selected indicators
        previous_selection <- selected_indicators()

        # Remove any topic-indicator pairs that have been deselected
        updated_selection <- setdiff(input$indicator, previous_selection)

        # Combine the new topic-indicator pairs with the previous selections
        combined_selection <- unique(c(updated_selection, current_filtered))

        # Update the reactive value for all topic-indicator pairs
        selected_indicators(combined_selection)
      },
      ignoreNULL = FALSE,
      priority = 2
    )

    # Clear all current selections
    observeEvent(input$clear_all, {
      # Reset inputs to their initial state
      updateSelectizeInput(session, "geog_input", selected = NA)
      updateSelectizeInput(session, "indicator", selected = NA)
      updateRadioButtons(session, "la_group", selected = "no_groups")
      updateCheckboxInput(session, "inc_regions", value = FALSE)
      updateCheckboxInput(session, "inc_england", value = FALSE)

      # Emit a reset signal for year_range
      session$sendCustomMessage("clear_year_range", TRUE)
    })

    # Return create your own main inputs
    create_inputs <- list(
      geog = reactive(input$geog_input),
      indicator = reactive(selected_indicators()),
      la_group = reactive(input$la_group),
      inc_regions = reactive(input$inc_regions),
      inc_england = reactive(input$inc_england),
      clear_selections = reactive(input$clear_all),
      add_query = reactive(input$add_query)
    )

    # Return inputs
    create_inputs
  })
}


# Year range input UI ----------------------------------------------------------
#
#' Year Range Input UI
#'
#' This function creates a user interface component for selecting a range
#' of years. It utilizes a picker input to allow users to select one
#' or more years from the available options.
#'
#' @param id A unique identifier for the Shiny module.
#' @return A shinyWidgets picker input for year range selection.
#'
YearRangeUI <- function(id) {
  ns <- NS(id)

  shinyWidgets::pickerInput(
    ns("year_range"),
    "Select Year Range",
    choices = all_year_types,
    choicesOpt = list(
      content = rep("Loading...", length(all_year_types))
    ),
    options = shinyWidgets::pickerOptions(
      noneSelectedText = "Loading...",
      maxOptions = 2,
      maxOptionsText = "Still loading...",
      size = 1
    ),
    multiple = TRUE
  )
}

# Year range input server ------------------------------------------------------
#
#' Year Range Input Server
#'
#' This function handles the server-side logic for the year range input.
#' It dynamically generates the choices available for years based on
#' the selected indicator. The year range can be updated accordingly
#' and provides feedback when no indicators are selected.
#'
#' @param id A unique identifier for the Shiny module.
#' @param bds_metrics A data frame containing metrics used to determine
#'                    available years based on selected indicators.
#' @param indicator_input A reactive expression that returns the current
#'                        selection of indicators.
#' @return A list containing reactive values for selected year range
#'         and available year choices.
#'
YearRangeServer <- function(id, bds_metrics, indicator_input, clear_selections) {
  moduleServer(id, function(input, output, session) {
    # Compute years choices available based on selected indicator
    years_choices <- reactive({
      years_dict <- bds_metrics |>
        dplyr::filter(Measure %in% indicator_input()) |>
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
      # Get the valid choices based on the selected indicator
      valid_choices <- years_choices()

      # Retain only the valid selected years from the current input
      valid_selection <- intersect(input$year_range, valid_choices)

      # Update the picker input with the new choices and valid selections
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "year_range",
        choices = valid_choices,
        selected = valid_selection,
        options = shinyWidgets::pickerOptions(
          maxOptions = 2,
          maxOptionsText = "Deselect a year",
          multipleSeparator = " to ",
          noneSelectedText = "All years available",
          size = "auto"
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
            noneSelectedText = "Select an indicator to see year range",
            maxOptions = 2,
            maxOptionsText = "Select and indicator",
            size = "auto"
          )
        )
      }
    })

    # Reset year range when clear all current selections button clicked
    observeEvent(clear_selections(), {
      shinyWidgets::updatePickerInput(session, "year_range", selected = NULL)
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
#
#' Geography Grouping Server
#'
#' This function combines user-selected geography inputs with any additional
#' geography groupings based on the chosen options. It allows for the
#' inclusion of all local authorities, regions, or statistical neighbors
#' based on user input.
#'
#' @param id A unique identifier for the Shiny module.
#' @param create_inputs A list of reactive inputs created by the main input
#'                      module, including user selections for geography,
#'                      indicators, and groupings.
#' @param la_names_bds A vector of local authority names used for
#'                      selecting all LAs.
#' @param region_names_bds A vector of region names used for selecting
#'                         all regions.
#' @param stat_n_geog A data frame containing geographical information for
#'                     local authorities and their regions.
#' @param stat_n_la A data frame containing statistical neighbor information
#'                   for local authorities.
#' @return A reactive value containing the combined geography inputs based
#'         on user selections and additional groupings.
#'
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
#
#' Statistical Neighbour Association Server
#'
#' This function establishes associations between statistical neighbours
#' (SNs) and their parent local authority (LA) based on user selections.
#' It computes the association only if the statistical neighbour grouping
#' is selected.
#'
#' @param id A unique identifier for the Shiny module.
#' @param create_inputs A list of reactive inputs created by the main input
#'                      module, which includes user selections for local
#'                      authorities and grouping options.
#' @param la_names_bds A vector of local authority names used to identify
#'                      which LAs are selected.
#' @param stat_n_la A data frame containing statistical neighbour information
#'                   for local authorities.
#' @return A reactive value containing a data frame that lists local
#'         authorities and their corresponding statistical neighbours, with
#'         the parent LA indicated for each SN.
#'
StatN_AssociationServer <- function(id,
                                    create_inputs,
                                    la_names_bds,
                                    stat_n_la) {
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
