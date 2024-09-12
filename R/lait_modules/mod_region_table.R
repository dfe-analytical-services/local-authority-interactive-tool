# Get the LA region
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


# Get clean LA Region
Clean_RegionServer <- function(id, region_la, filtered_bds) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Cleans Regions for which London to use
      # Some indicators are not provided at (Inner)/ (Outer) level
      clean_ldn_region(region_la(), filtered_bds())
    })
  })
}

# Get clean LA Region
# region_clean <- Clean_RegionServer("region_clean", region_la, filtered_bds)


# Long format Region LA data
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
        dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
        dplyr::mutate(
          `LA and Regions` = factor(`LA and Regions`),
          Years_num = as.numeric(substr(Years, start = 1, stop = 4))
        )

      region_la_long
    })
  })
}


# Most recent year
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

# LA Level table ==============================================================
#' Shiny Module UI for Displaying the LA Level Table
#'
#' @param id A unique ID that identifies the UI element
#' @return A div object that contains the UI elements for the module
RegionLA_TableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Regional Authorities"),
      bslib::card_body(
        reactable::reactableOutput(ns("region_la_table"))
      )
    )
  )
}

# Region LA Level table -------------------------------------------------------
RegionLA_TableServer <- function(id, app_inputs, bds_metrics, stat_n_geog) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get LA region
    region_la <- LA_RegionServer("region_la", app_inputs$la, stat_n_geog)

    # Long format Region LA data
    region_la_long <- RegionLA_LongDataServer("region_la_long", stat_n_geog, region_la, filtered_bds)

    # Current year
    current_year <- Current_YearServer("current_year", region_la_long)


    # Difference between last two years
    region_la_diff <- reactive({
      region_la_long() |>
        calculate_change_from_prev_yr()
    })

    # Build Region LA table
    region_la_table <- shiny::reactive({
      # Join difference and pivot wider
      region_la_long() |>
        dplyr::bind_rows(region_la_diff()) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num
        ) |>
        pretty_num_table(dp = 1) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`)
    })

    output$region_la_table <- reactable::renderReactable({
      dfe_reactable(
        region_la_table(),
        rowStyle = function(index) {
          highlight_selected_row(index, region_la_table(), app_inputs$la())
        }
      )
    })
  })
}
