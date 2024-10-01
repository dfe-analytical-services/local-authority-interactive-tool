# nolint start: object_name
#
# General modules =============================================================
# These are nested within other modules
#
#' LA Region Server Module
#'
#' A module that retrieves the region corresponding to the selected LA.
#'
#' @param id The namespace ID for the module.
#' @param la_input A reactive expression returning the selected LA name.
#' @param stat_n_geog A data frame containing LA names and corresponding
#' regions.
#'
#' @return A reactive expression that outputs the region associated with the
#' selected LA.
#'
#' @examples
#' LA_RegionServer("region", input$la, stat_n_geog)
#'
LA_RegionServer <- function(id, la_input, stat_n_geog) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Filter geogs by selected LA and pull Region
      stat_n_geog |>
        dplyr::filter(`LA Name` == la_input()) |>
        dplyr::pull(GOReg)
    })
  })
}


#' Clean LA Region Server Module
#'
#' A module that cleans and returns the region associated with the selected LA.
#'
#' @param id The namespace ID for the module.
#' @param app_inputs A list of reactive inputs, including the selected LA.
#' @param stat_n_geog A data frame containing LA names and corresponding regions.
#' @param bds_metrics A data frame containing metric data for filtering.
#'
#' @return A reactive expression that returns a cleaned region for the selected
#' LA, accounting for missing indicators at Inner/Outer London levels.
#'
#' @examples
#' Clean_RegionServer("clean_region", app_inputs, stat_n_geog, bds_metrics)
#'
Clean_RegionServer <- function(id, app_inputs, stat_n_geog, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get LA region
    region_la <- LA_RegionServer("region_la", app_inputs$la, stat_n_geog)

    reactive({
      # Cleans Regions for which London to use
      # Some indicators are not provided at (Inner)/ (Outer) level
      clean_ldn_region(region_la(), filtered_bds())
    })
  })
}


#' Region LA Long Data Server Module
#'
#' A module that filters and formats the dataset for the selected LA and
#' region in long format.
#'
#' @param id The namespace ID for the module.
#' @param stat_n_geog A data frame containing LA and regional data.
#' @param region_la A reactive expression returning the selected LA's region.
#' @param filtered_bds A reactive expression returning the filtered dataset.
#'
#' @return A reactive expression that returns the long-format dataset
#' for the selected LA and regional LAs, including numeric year values.
#'
#' @examples
#' RegionLA_LongDataServer(
#'   "region_la_long_data", stat_n_geog, region_la,
#'   filtered_bds
#' )
#'
RegionLA_LongDataServer <- function(id, stat_n_geog, region_la, filtered_bds) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Get other LAs in the region
      region_la_la <- stat_n_geog |>
        dplyr::filter(GOReg == region_la()) |>
        pull_uniques("LA Name")

      # Then filter for selected LA and regional LAs
      region_la_filtered_bds <- filtered_bds() |>
        dplyr::filter(
          `LA and Regions` %in% region_la_la
        )

      # Select needed columns, factorise geogs and create numeric yr
      region_la_long <- region_la_filtered_bds |>
        dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num) |>
        dplyr::mutate(
          `LA and Regions` = factor(`LA and Regions`)
        )

      region_la_long
    })
  })
}


#' Current Year Server Module
#'
#' A module that retrieves the most recent year from the dataset based on
#' the maximum numeric year.
#'
#' @param id The namespace ID for the module.
#' @param region_la_long A reactive expression returning the long-format dataset.
#'
#' @return A reactive expression that returns the most recent year as a string
#' from the dataset.
#'
#' @examples
#' Current_YearServer("current_year", region_la_long)
#'
Current_YearServer <- function(id, region_la_long) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Extract string year for max numeric year
      region_la_long() |>
        dplyr::filter(Years_num == max(Years_num)) |>
        pull_uniques("Years")
    })
  })
}


# Region LA Table =============================================================
#' Region LA Data Server
#'
#' Server module to process and transform data for Region LA table. It filters,
#' formats, and calculates differences in values across years for selected
#' Local Authorities.
#'
#' @param id The namespace ID for the server module.
#' @param app_inputs Reactive inputs for the app, including selected LA.
#' @param bds_metrics Dataset of BDS metrics for filtering.
#' @param stat_n_geog Dataset containing geographic information for LAs.
#'
#' @return A reactive object containing the formatted Region LA table data.
#'
#' @examples
#' RegionLA_DataServer("region_la_data", app_inputs, bds_metrics, stat_n_geog)
#'
RegionLA_DataServer <- function(id, app_inputs, bds_metrics, stat_n_geog) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get LA region
    region_la <- LA_RegionServer("region_la", app_inputs$la, stat_n_geog)

    # Long format Region LA data
    region_la_long <- RegionLA_LongDataServer(
      "region_la_long",
      stat_n_geog,
      region_la,
      filtered_bds
    )

    # Current year
    current_year <- Current_YearServer("current_year", region_la_long)

    # Difference between last two years
    region_la_diff <- reactive({
      region_la_long() |>
        calculate_change_from_prev_yr()
    })

    # Build Region LA table
    shiny::reactive({
      # Join difference and pivot wider
      region_la_long() |>
        dplyr::bind_rows(region_la_diff()) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num
        )
    })
  })
}


#' Region LA Table UI
#'
#' Creates a UI component for displaying a table of Local Authorities within
#' a region using `reactable`.
#'
#' @param id The namespace ID for the UI module.
#'
#' @return A UI element containing a `reactable` output for displaying the
#' Region LA table.
#'
#' @examples
#' RegionLA_TableUI("region_la_table")
#'
RegionLA_TableUI <- function(id) {
  ns <- NS(id)

  div(
    style = "overflow-y: visible;",
    bslib::card(
      # bslib::card_header(""),
      bslib::card_body(
        reactable::reactableOutput(ns("region_la_table"))
      )
    )
  )
}


#' Region LA Table Server
#'
#' Server module for processing and rendering the Region LA table. It filters
#' data for selected topics and indicators, calculates current year values,
#' and formats the data for display in a reactable table.
#'
#' @param id The namespace ID for the server module.
#' @param app_inputs Reactive inputs from the application, including the
#' selected Local Authority.
#' @param bds_metrics Dataset containing BDS metrics for filtering data.
#' @param stat_n_geog Dataset with geographic information for Local Authorities.
#'
#' @return A reactive table output ready for rendering in the UI.
#'
#' @examples
#' RegionLA_TableServer("region_la_table", app_inputs, bds_metrics, stat_n_geog)
#'
RegionLA_TableServer <- function(id, app_inputs, bds_metrics, stat_n_geog) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get LA region
    region_la <- LA_RegionServer("region_la", app_inputs$la, stat_n_geog)

    # Long format Region LA data
    region_la_long <- RegionLA_LongDataServer(
      "region_la_long",
      stat_n_geog,
      region_la,
      filtered_bds
    )

    # Get Region LA table
    region_la_table_raw <- RegionLA_DataServer(
      "region_la_table_raw",
      app_inputs,
      bds_metrics,
      stat_n_geog
    )

    # Current year
    current_year <- Current_YearServer("current_year", region_la_long)

    # Pretty and order table ready for rendering
    region_la_table <- reactive({
      region_la_table_raw() |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = "LA Number"
        ) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`)
    })

    # Table output
    output$region_la_table <- reactable::renderReactable({
      dfe_reactable(
        region_la_table(),
        columns = align_reactable_cols(
          region_la_table(),
          num_exclude = "LA Number"
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, region_la_table(), app_inputs$la())
        }
      )
    })
  })
}


# Region Table ================================================================
#' Region Long Data Server
#'
#' Server module to process and format long data for regions and England.
#' It filters the data based on selected regions and transforms it into
#' a long format suitable for further analysis and visualization.
#'
#' @param id The namespace ID for the server module.
#' @param filtered_bds A reactive expression providing filtered BDS data.
#' @param region_names_bds A vector of region names for filtering.
#'
#' @return A reactive object containing the long-format data for the regions.
#'
#' @examples
#' Region_LongDataServer("region_long_data", filtered_bds, region_names_bds)
#'
Region_LongDataServer <- function(id, filtered_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Filter for all regions and England
      region_filtered_bds <- filtered_bds() |>
        dplyr::filter(
          `LA and Regions` %in% c(region_names_bds, "England")
        )

      # Region levels long
      region_long <- region_filtered_bds |>
        dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num) |>
        dplyr::mutate(
          `LA and Regions` = factor(`LA and Regions`)
        )

      region_long
    })
  })
}


#' Region Data Server
#'
#' Server module that processes data for regions, filtering by the
#' selected topic and indicator. It generates a long format dataset
#' for regions and calculates the difference between the last two years.
#'
#' @param id The namespace ID for the server module.
#' @param app_inputs A reactive input list containing application inputs.
#' @param bds_metrics A dataset containing metrics for filtering.
#' @param region_names_bds A vector of region names for filtering.
#'
#' @return A reactive data frame containing the region table with values
#' for the last two years, structured for further analysis and display.
#'
#' @examples
#' Region_DataServer("region_data", app_inputs, bds_metrics, region_names_bds)
#'
Region_DataServer <- function(id, app_inputs, bds_metrics, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format Region LA data
    region_long <- Region_LongDataServer(
      "region_long",
      filtered_bds,
      region_names_bds
    )

    # Build Region table
    shiny::reactive({
      # Difference between last two years
      region_diff <- region_long() |>
        calculate_change_from_prev_yr()

      # Join difference and pivot wider to recreate Region table
      region_table <- region_long() |>
        dplyr::bind_rows(region_diff) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num
        )

      region_table
    })
  })
}


#' Region Table UI
#'
#' UI module to create a card that displays a table for region data.
#' The card includes a styled border for visual separation and ensures
#' that the table output is scrollable as needed.
#'
#' @param id The namespace ID for the UI module.
#'
#' @return A UI element containing the card with the region table output.
#'
#' @examples
#' Region_TableUI("region_table_ui")
#'
Region_TableUI <- function(id) {
  ns <- NS(id)

  div(
    # Add black border between the tables
    style = "overflow-y: visible;border-top: 2px solid black; padding-top: 2.5rem;",
    bslib::card(
      # bslib::card_header(""),
      bslib::card_body(
        reactable::reactableOutput(ns("region_table"))
      )
    )
  )
}


#' Region Table Server
#'
#' Server module that processes and renders the region table, filtering
#' data based on selected topics and indicators. It prepares a long format
#' dataset, calculates the current year, and arranges the table for display,
#' ensuring the England row is placed at the bottom.
#'
#' @param id The namespace ID for the server module.
#' @param app_inputs A reactive input list containing application inputs.
#' @param bds_metrics A dataset containing metrics for filtering.
#' @param stat_n_geog A dataset for geographic statistics.
#' @param region_names_bds A vector of region names for filtering.
#'
#' @return A rendered reactable displaying the region data, formatted and
#' ordered based on the current year.
#'
#' @examples
#' Region_TableServer(
#'   "region_table", app_inputs, bds_metrics,
#'   stat_n_geog, region_names_bds
#' )
#'
Region_TableServer <- function(id,
                               app_inputs,
                               bds_metrics,
                               stat_n_geog,
                               region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer(
      "filtered_bds",
      app_inputs,
      bds_metrics
    )

    # Long format Region LA data
    region_long <- Region_LongDataServer(
      "region_long",
      filtered_bds,
      region_names_bds
    )

    # Current year
    current_year <- Current_YearServer("current_year", region_long)

    # Region table
    region_table_raw <- Region_DataServer(
      "region_table_raw",
      app_inputs, bds_metrics, region_names_bds
    )

    # Pretty and order table ready for rendering
    region_table <- reactive({
      region_table_raw() |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = "LA Number"
        ) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`) |>
        # Places England row at the bottom of the table
        dplyr::mutate(is_england = ifelse(
          grepl("^England", `LA and Regions`), 1, 0
        )) |>
        dplyr::arrange(is_england, .by_group = FALSE) |>
        dplyr::select(-is_england)
    })

    # Get clean Regions
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Table output
    output$region_table <- reactable::renderReactable({
      dfe_reactable(
        region_table(),
        columns = align_reactable_cols(
          region_table(),
          num_exclude = "LA Number"
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, region_table(), region_clean())
        },
        pagination = FALSE
      )
    })
  })
}


# Region Stats table ==========================================================
#' Region Stats Table UI
#'
#' UI module that creates a card layout for displaying the region statistics
#' table. It includes styling for overflow and a top border to separate the
#' table visually.
#'
#' @param id The namespace ID for the UI module.
#'
#' @return A UI element containing a reactable output for the region stats table.
#'
#' @examples
#' Region_StatsTableUI("region_stats_table")
#'
Region_StatsTableUI <- function(id) {
  ns <- NS(id)

  div(
    # Add black border between the tables
    style = "overflow-y: visible;border-top: 2px solid black; padding-top: 2.5rem;",
    bslib::card(
      # bslib::card_header(""),
      bslib::card_body(
        reactable::reactableOutput(ns("region_stats_table"))
      )
    )
  )
}


#' Region Stats Table Server
#'
#' Server module that processes and manages the data for the region stats
#' table. It filters the data based on selected inputs and prepares the
#' statistics for rendering.
#'
#' @param id The namespace ID for the server module.
#' @param app_inputs Reactive inputs from the main app, including selected
#'                  local authority (LA).
#' @param bds_metrics Data metrics used for filtering and processing.
#' @param stat_n_geog Data frame containing geographic information for
#'                     regions.
#' @param region_names_bds Names of regions for filtering the dataset.
#'
#' @return A reactive output for rendering the region stats table.
#'
#' @examples
#' Region_StatsTableServer(
#'   "region_stats_table", app_inputs, bds_metrics,
#'   stat_n_geog, region_names_bds
#' )
#'
Region_StatsTableServer <- function(id,
                                    app_inputs,
                                    bds_metrics,
                                    stat_n_geog,
                                    region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get Region LA table
    region_la_table_raw <- RegionLA_DataServer(
      "region_la_raw",
      app_inputs,
      bds_metrics,
      stat_n_geog
    )

    # Region table
    region_table_raw <- Region_DataServer(
      "region_table_raw",
      app_inputs, bds_metrics, region_names_bds
    )

    # Get clean Regions
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Build stats table
    region_stats_table <- reactive({
      # Get LA numbers
      # Selected LA
      region_la_la_num <- region_la_table_raw() |>
        filter_la_regions(app_inputs$la(), pull_col = "LA Number")

      # Region and England
      region_la_num <- region_table_raw() |>
        filter_la_regions(c(region_clean(), "England"), pull_col = "LA Number")

      # Get change in previous year
      # Selected LA
      region_la_change_prev <- region_la_table_raw() |>
        filter_la_regions(app_inputs$la(),
          latest = FALSE,
          pull_col = "Change from previous year"
        )

      # Region and England
      region_change_prev <- region_table_raw() |>
        filter_la_regions(c(region_clean(), "England"),
          latest = FALSE,
          pull_col = "Change from previous year"
        )

      # Creating the stats table cols
      region_stats_la_num <- c(region_la_la_num, region_la_num)
      region_stats_name <- c(app_inputs$la(), region_clean(), "England")
      region_stats_change <- c(region_la_change_prev, region_change_prev)

      # Creating the trend descriptions
      region_trend <- as.numeric(region_stats_change)

      # Build stats table
      build_region_stats_table(
        region_stats_la_num,
        region_stats_name,
        region_trend,
        region_stats_change,
        filtered_bds()
      )
    })

    # Table output
    output$region_stats_table <- reactable::renderReactable({
      dfe_reactable(
        region_stats_table(),
        columns = modifyList(
          align_reactable_cols(
            region_stats_table(),
            num_exclude = "LA Number",
            categorical = c("Trend", "Quartile Banding")
          ),
          # Trend icon arrows
          list(
            Trend = reactable::colDef(
              cell = trend_icon_renderer
            )
          )
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, region_stats_table(), app_inputs$la())
        }
      )
    })
  })
}

# nolint end
