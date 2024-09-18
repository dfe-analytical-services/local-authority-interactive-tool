# nolint start: object_name
#
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

# Get national term
Region_NationalServer <- function(id, national_names_bds, filtered_bds) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Filter for national terms that are do not have all NA values
      filtered_bds() |>
        dplyr::filter(`LA and Regions` %in% national_names_bds & !is.na(values_num)) |>
        pull_uniques("LA and Regions")
    })
  })
}


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

# Region LA table UI ==========================================================
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

# Region LA table data --------------------------------------------------------
RegionLA_DataServer <- function(id, app_inputs, bds_metrics, stat_n_geog) {
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
    shiny::reactive({
      # Join difference and pivot wider
      region_la_long() |>
        dplyr::bind_rows(region_la_diff()) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num
        ) |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = "LA Number"
        ) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`)
    })
  })
}

# Region LA table Server ------------------------------------------------------
RegionLA_TableServer <- function(id, app_inputs, bds_metrics, stat_n_geog) {
  moduleServer(id, function(input, output, session) {
    # Get Region LA table
    region_la_table <- RegionLA_DataServer("region_la_table", app_inputs, bds_metrics, stat_n_geog)

    # Table output
    output$region_la_table <- reactable::renderReactable({
      dfe_reactable(
        region_la_table(),
        columns = align_reactable_cols(region_la_table(),
          exclude = "LA Number"
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, region_la_table(), app_inputs$la())
        }
      )
    })
  })
}



# Get long data format for Regions
Region_LongDataServer <- function(id, filtered_bds, region_names_bds, region_national) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Filter for all regions and England
      region_filtered_bds <- filtered_bds() |>
        dplyr::filter(
          `LA and Regions` %in% c(region_names_bds, region_national())
        )

      # Region levels long
      region_long <- region_filtered_bds |>
        dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
        dplyr::mutate(
          `LA and Regions` = factor(`LA and Regions`),
          Years_num = as.numeric(substr(Years, start = 1, stop = 4))
        )

      region_long
    })
  })
}


# Region table UI =============================================================
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

# Region table data -----------------------------------------------------------
Region_DataServer <- function(id, app_inputs, bds_metrics, national_names_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get relevant National
    region_national <- Region_NationalServer("region_national", national_names_bds, filtered_bds)

    # Long format Region LA data
    region_long <- Region_LongDataServer("region_long", filtered_bds, region_names_bds, region_national)

    # Current year
    current_year <- Current_YearServer("current_year", region_long)

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
        ) |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = "LA Number"
        ) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`) |>
        # Places England row at the bottom of the table
        dplyr::mutate(is_england = ifelse(grepl("^England", `LA and Regions`), 1, 0)) |>
        dplyr::arrange(is_england, .by_group = FALSE) |>
        dplyr::select(-is_england)

      region_table
    })
  })
}

# Region table Server ---------------------------------------------------------
Region_TableServer <- function(id, app_inputs, bds_metrics, stat_n_geog, national_names_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Region table
    region_table <- Region_DataServer(
      "region_table",
      app_inputs, bds_metrics, national_names_bds, region_names_bds
    )

    # Get clean Regions
    region_clean <- Clean_RegionServer("region_clean", app_inputs, stat_n_geog, bds_metrics)

    # Table output
    output$region_table <- reactable::renderReactable({
      dfe_reactable(
        region_table(),
        columns = align_reactable_cols(region_table(),
          exclude = "LA Number"
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, region_table(), region_clean())
        }
      )
    })
  })
}


# Region Stats table UI =======================================================
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

# Region Stats table Server ---------------------------------------------------
Region_StatsTableServer <- function(
    id, app_inputs, bds_metrics, stat_n_geog, national_names_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get Region LA table
    region_la_table <- RegionLA_DataServer("region_la", app_inputs, bds_metrics, stat_n_geog)

    # Get Region table
    region_table <- Region_DataServer(
      "region_table",
      app_inputs, bds_metrics, national_names_bds, region_names_bds
    )

    # Get clean Regions
    region_clean <- Clean_RegionServer("region_clean", app_inputs, stat_n_geog, bds_metrics)

    # Get relevant National
    region_national <- Region_NationalServer("region_national", national_names_bds, filtered_bds)

    # Build stats table
    region_stats_table <- reactive({
      # Get LA numbers
      # Selected LA
      region_la_la_num <- region_la_table() |>
        filter_la_regions(app_inputs$la(), pull_col = "LA Number")

      # Region and England
      region_la_num <- region_table() |>
        filter_la_regions(c(region_clean(), region_national()), pull_col = "LA Number")

      # Get change in previous year
      # Selected LA
      region_la_change_prev <- region_la_table() |>
        filter_la_regions(app_inputs$la(),
          latest = FALSE,
          pull_col = "Change from previous year"
        )

      # Region and England
      region_change_prev <- region_table() |>
        filter_la_regions(c(region_clean(), region_national()),
          latest = FALSE,
          pull_col = "Change from previous year"
        )

      # Creating the stats table cols
      region_stats_la_num <- c(region_la_la_num, region_la_num)
      region_stats_name <- c(app_inputs$la(), region_clean(), region_national())
      region_stats_change <- c(region_la_change_prev, region_change_prev)

      # Creating the trend descriptions
      region_trend <- dplyr::case_when(
        is.na(region_stats_change) ~ NA_character_,
        region_stats_change > 0 ~ "Increase",
        region_stats_change < 0 ~ "Decrease",
        TRUE ~ "No trend"
      )

      # Build stats table
      data.frame(
        "LA Number" = region_stats_la_num,
        "LA and Regions" = region_stats_name,
        "Trend" = region_trend,
        "Change from previous year" = region_stats_change,
        check.names = FALSE
      )
    })

    # Table output
    output$region_stats_table <- reactable::renderReactable({
      dfe_reactable(
        region_stats_table(),
        columns = align_reactable_cols(region_stats_table(),
          exclude = "LA Number"
        )
      )
    })
  })
}

# nolint end
