# # shareable modules:
# indicator_dps - done
#
# stat_n_sns
#
# stat_n_region
#
# stat_n_long
#
# stat_n_diff
#
# current_year
#
# stat_n_table



Get_LAStatNsServer <- function(id, la_input, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Get selected LA statistical neighbours
      stat_n_la |>
        dplyr::filter(`LA Name` == la_input()) |>
        pull_uniques("LA Name_sn")
    })
  })
}


Get_LACleanRegionServer <- function(id, la_input, filtered_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Get selected LA Region (London cleaned)
      stat_n_la |>
        dplyr::filter(`LA Name` == la_input()) |>
        pull_uniques("GOReg") |>
        clean_ldn_region(filtered_bds())
    })
  })
}


# Long format Statistical Neighbour data
StatN_LongServer <- function(id, la_input, filtered_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Get LA statistical neighbours
    stat_n_sns <- Get_LAStatNsServer(
      "stat_n_sns",
      la_input,
      stat_n_la
    )

    # Get LA Region (clean)
    stat_n_region <- Get_LACleanRegionServer(
      "stat_n_region",
      la_input,
      filtered_bds,
      stat_n_la
    )

    reactive({
      # Calculate SN average
      stat_n_sn_avg <- filtered_bds() |>
        dplyr::filter(`LA and Regions` %in% stat_n_sns()) |>
        dplyr::summarise(
          values_num = mean(values_num, na.rm = TRUE),
          .by = c("Years", "Years_num")
        ) |>
        dplyr::mutate(
          "LA Number" = "-",
          "LA and Regions" = "Statistical Neighbours",
          .before = "Years"
        )

      # Statistical Neighbours long data
      filtered_bds() |>
        dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values) |>
        dplyr::bind_rows(stat_n_sn_avg) |>
        dplyr::mutate(
          `LA and Regions` = factor(
            `LA and Regions`,
            levels = c(
              la_input(), stat_n_sns(), "Statistical Neighbours",
              stat_n_region(), "England"
            )
          )
        )
    })
  })
}


StatN_DiffServer <- function(id, la_input, filtered_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Get Statistical Neighbour long format
    stat_n_long <- StatN_LongServer(
      "stat_n_long",
      la_input,
      filtered_bds,
      stat_n_la
    )

    reactive({
      # Difference between last two years
      stat_n_long() |>
        calculate_change_from_prev_yr()
    })
  })
}


StatN_DataServer <- function(id, la_input, filtered_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Get Statistical Neighbour long format
    stat_n_long <- StatN_LongServer(
      "stat_n_long",
      la_input,
      filtered_bds,
      stat_n_la
    )

    # Get Statistical Neighbour long format
    stat_n_diff <- StatN_DiffServer(
      "stat_n_diff",
      la_input,
      filtered_bds,
      stat_n_la
    )

    # Build main Statistical Neighbour formatted table (used to create the others)
    shiny::reactive({
      # Join difference and pivot wider
      stat_n_long() |>
        dplyr::bind_rows(stat_n_diff()) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num,
        ) |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = "LA Number"
        )
    })
  })
}



StatN_LASNsTableUI <- function(id) {
  ns <- NS(id)

  bslib::card_body(
    div(
      reactable::reactableOutput(ns("output_table"))
    )
  )
}


StatN_LASNsTableServer <- function(id,
                                   app_inputs,
                                   bds_metrics,
                                   stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer(
      "filtered_bds",
      app_inputs,
      bds_metrics
    )

    # Get statistical neighbour main formatted stats table
    stat_n_table <- StatN_DataServer(
      "StatN_DataServer",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Get LA statistical neighbours
    stat_n_sns <- Get_LAStatNsServer(
      "stat_n_sns",
      app_inputs$la,
      stat_n_la
    )

    # Current year
    current_year <- Current_YearServer("current_year", stat_n_table, "wide")

    # Table output
    output$output_table <- reactable::renderReactable({
      # Filter to LA and SNs
      stat_n_sns_table <- stat_n_table() |>
        dplyr::filter(`LA and Regions` %in% c(app_inputs$la(), stat_n_sns())) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`)

      # Create table with correct formatting
      dfe_reactable(
        stat_n_sns_table,
        columns = align_reactable_cols(
          stat_n_sns_table,
          num_exclude = "LA Number"
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, stat_n_sns_table, app_inputs$la())
        },
        pagination = FALSE
      )
    })
  })
}



StatN_GeogCompTableUI <- function(id) {
  ns <- NS(id)

  bslib::card_body(
    div(
      # Add black border between the tables
      style = "overflow-y: visible;border-top: 2px solid black; padding-top: 2.5rem;",
      reactable::reactableOutput(ns("output_table"))
    )
  )
}


StatN_GeogCompTableServer <- function(id,
                                      app_inputs,
                                      bds_metrics,
                                      stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer(
      "filtered_bds",
      app_inputs,
      bds_metrics
    )

    # Get statistical neighbour main formatted stats table
    stat_n_table <- StatN_DataServer(
      "StatN_DataServer",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Get LA Region (clean)
    stat_n_region <- Get_LACleanRegionServer(
      "stat_n_region",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Table output
    output$output_table <- reactable::renderReactable({
      # Keep LA geographic comparison areas
      stat_n_comp_table <- stat_n_table() |>
        dplyr::filter(`LA and Regions` %in% c(
          "Statistical Neighbours",
          stat_n_region(),
          "England"
        )) |>
        dplyr::arrange(`LA and Regions`)

      # Output table
      dfe_reactable(
        stat_n_comp_table,
        # Create the reactable with specific column alignments
        columns = align_reactable_cols(stat_n_comp_table, num_exclude = "LA Number"),
        pagination = FALSE
      )
    })
  })
}


StatN_StatsTableUI <- function(id) {
  ns <- NS(id)

  div(
    bslib::card(
      # bslib::card_header(""),
      bslib::card_body(
        reactable::reactableOutput(ns("output_table"))
      )
    )
  )
}


StatN_StatsTableServer <- function(id,
                                   app_inputs,
                                   bds_metrics,
                                   stat_n_la,
                                   la_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer(
      "filtered_bds",
      app_inputs,
      bds_metrics
    )

    # Get LA Region (clean)
    stat_n_region <- Get_LACleanRegionServer(
      "stat_n_region",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Get Statistical Neighbour long format
    stat_n_diff <- StatN_DiffServer(
      "stat_n_diff",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Build stats table
    stat_n_stats_table <- reactive({
      # Areas needed for the stats table
      stat_n_stats_geog <- c(app_inputs$la(), stat_n_region(), "England")

      # Extract change from prev year
      stat_n_change_prev <- stat_n_diff() |>
        filter_la_regions(stat_n_stats_geog,
          pull_col = "values_num"
        )

      # Get polarity of indicator
      stat_n_indicator_polarity <- filtered_bds() |>
        pull_uniques("Polarity")

      # Set the trend value
      stat_n_trend <- as.numeric(stat_n_change_prev)

      # Get latest rank, ties are set to min & NA vals to NA rank
      stat_n_rank <- filtered_bds() |>
        filter_la_regions(la_names_bds, latest = TRUE) |>
        dplyr::mutate(
          rank = dplyr::case_when(
            is.na(values_num) ~ NA,
            # Rank in descending order
            stat_n_indicator_polarity == "High" ~ rank(-values_num, ties.method = "min", na.last = TRUE),
            # Rank in ascending order
            stat_n_indicator_polarity == "Low" ~ rank(values_num, ties.method = "min", na.last = TRUE)
          )
        ) |>
        filter_la_regions(app_inputs$la(), pull_col = "rank")


      # Calculate quartile bands for indicator
      stat_n_quartile_bands <- filtered_bds() |>
        filter_la_regions(la_names_bds, latest = TRUE, pull_col = "values_num") |>
        quantile(na.rm = TRUE)

      # Extracting LA latest value
      stat_n_indicator_val <- filtered_bds() |>
        filter_la_regions(app_inputs$la(), latest = TRUE, pull_col = "values_num")

      # Calculating which quartile this value sits in
      stat_n_quartile <- calculate_quartile_band(
        stat_n_indicator_val,
        stat_n_quartile_bands,
        stat_n_indicator_polarity
      )

      # SN stats table
      data.frame(
        "LA Number" = stat_n_diff() |>
          filter_la_regions(stat_n_stats_geog, pull_col = "LA Number"),
        "LA and Regions" = stat_n_stats_geog,
        "Trend" = stat_n_trend,
        "Change from previous year" = stat_n_change_prev,
        "National Rank" = c(stat_n_rank, NA, NA),
        "Quartile Banding" = c(stat_n_quartile, NA, NA),
        "Polarity" = stat_n_indicator_polarity,
        check.names = FALSE
      )
    })

    # Main stats table
    output$output_table <- reactable::renderReactable({
      stat_n_stats_output <- stat_n_stats_table() |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          include_columns = c("Change from previous year")
        )

      dfe_reactable(
        stat_n_stats_output |>
          dplyr::select(-Polarity),
        columns = modifyList(
          # Create the reactable with specific column alignments
          align_reactable_cols(
            stat_n_stats_output,
            num_exclude = "LA Number",
            categorical = c("Trend", "Quartile Banding")
          ),
          # Define specific formatting for the Trend and Quartile Banding columns
          list(
            Trend = reactable::colDef(
              cell = trend_icon_renderer
            ),
            `National Rank` = reactable::colDef(na = ""),
            `Quartile Banding` = reactable::colDef(
              style = quartile_banding_col_def(stat_n_stats_output),
              na = ""
            )
          )
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, stat_n_stats_output, app_inputs$la())
        }
      )
    })
  })
}
