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

  div(
    # Add black border between the tables
    style = "overflow-y: visible;border-top: 2px solid black; padding-top: 2.5rem;",
    bslib::card(
      # bslib::card_header(""),
      bslib::card_body(
        reactable::reactableOutput(ns("output_table"))
      )
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
