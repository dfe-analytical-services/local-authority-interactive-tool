# nolint start: object_name
#
# General modules =============================================================
# These are nested within other modules
#
#' Shiny Server Function for Filtering BDS Metrics
#'
#' @description
#' This only filters for Topic and Indicatro so that it can be reused across
#' pages where location filters my differ.
#'
#' @param id A unique ID that identifies the server function
#' @param app_inputs A list of inputs from the Shiny app
#' @param bds_metrics A data frame of BDS metrics
#' @return A reactive expression that contains the filtered BDS metrics
BDS_FilteredServer <- function(id, app_inputs, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    # Define filtered_bds outside of observeEvent
    filtered_bds <- reactiveValues(data = NULL)

    # Must ensure filtering only done when Indicator is changed
    # Otherwise it will filter immediately on Topic change
    observeEvent(app_inputs$indicator(), {
      filtered_bds$data <- bds_metrics |>
        dplyr::filter(
          Topic == app_inputs$topic(),
          Measure == app_inputs$indicator(),
          !is.na(Years)
        )
    })

    reactive({
      filtered_bds$data
    })
  })
}


#' Shiny Server Function for Creating Long Format LA Data
#'
#' @param id A unique ID that identifies the server function
#' @param app_inputs A list of inputs from the Shiny app
#' @param bds_metrics A data frame of BDS metrics
#' @param stat_n_la A data frame of statistical neighbours for each LA
#' @return A reactive expression that contains the long format LA data
LA_LongDataServer <- function(id, app_inputs, bds_metrics, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format LA data
    la_long <- reactive({
      # Filter stat neighbour for selected LA
      filtered_sn <- stat_n_la |>
        dplyr::filter(`LA Name` == app_inputs$la())

      # Statistical Neighbours
      la_sns <- filtered_sn |>
        pull_uniques("LA Name_sn")

      # LA region
      la_region <- filtered_sn |>
        pull_uniques("GOReg")

      # Determine London region to use
      la_region_ldn_clean <- clean_ldn_region(
        la_region,
        filtered_bds()
      )

      # Then filter for selected LA, region, stat neighbours and national
      la_filtered_bds <- filtered_bds() |>
        dplyr::filter(
          `LA and Regions` %in% c(
            app_inputs$la(), la_region_ldn_clean,
            la_sns, "England"
          )
        )

      # SN average
      sn_avg <- la_filtered_bds |>
        dplyr::filter(`LA and Regions` %in% la_sns) |>
        dplyr::summarise(
          values_num = mean(values_num, na.rm = TRUE),
          .by = c("Years", "Years_num")
        ) |>
        dplyr::mutate(
          "LA Number" = "-",
          "LA and Regions" = "Statistical Neighbours",
          .before = "Years"
        )

      # LA levels long
      la_long <- la_filtered_bds |>
        dplyr::filter(`LA and Regions` %notin% c(la_sns)) |>
        dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num) |>
        dplyr::bind_rows(sn_avg) |>
        dplyr::mutate(
          `LA and Regions` = factor(
            `LA and Regions`,
            levels = c(
              app_inputs$la(), la_region_ldn_clean,
              "Statistical Neighbours", "England"
            )
          )
        )
    })

    la_long
  })
}


# LA Level table ==============================================================
#' Shiny Module UI for Displaying the LA Level Table
#'
#' @param id A unique ID that identifies the UI element
#' @return A div object that contains the UI elements for the module
LA_LevelTableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Local Authority, Region and England"),
      bslib::card_body(
        reactable::reactableOutput(ns("la_table"))
      )
    )
  )
}


#' Shiny Server Function for Computing and Rendering the LA Level Table
#'
#' @param id A unique ID that identifies the server function
#' @param app_inputs A list of inputs from the Shiny app
#' @param bds_metrics A data frame of BDS metrics
#' @param stat_n_la A data frame of statistical neighbours for each LA
#' @return A list of outputs for the UI, including a data table of the LA levels
LA_LevelTableServer <- function(id, app_inputs, bds_metrics, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Main LA Level table ----------------------------------
    # Long format LA data
    la_long <- LA_LongDataServer(
      "la_table_data", app_inputs,
      bds_metrics, stat_n_la
    )

    # Difference between last two years
    la_diff <- reactive({
      la_long() |>
        calculate_change_from_prev_yr()
    })

    # Build Main LA Level table
    # Join difference and pivot wider to recreate LAIT table
    la_table <- reactive({
      la_long() |>
        dplyr::bind_rows(la_diff()) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num
        ) |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = "LA Number"
        ) |>
        dplyr::arrange(`LA and Regions`)
    })

    output$la_table <- reactable::renderReactable({
      dfe_reactable(
        la_table(),
        columns = align_reactable_cols(la_table(), num_exclude = "LA Number"),
        rowStyle = function(index) {
          highlight_selected_row(index, la_table(), app_inputs$la())
        }
      )
    })
  })
}


# LA Stats table module =======================================================
#' Shiny Module UI for Displaying the LA Stats Table
#'
#' @param id A unique ID that identifies the UI element
#' @return A div object that contains the UI elements for the module
LA_StatsTableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = NULL,
      style = htmltools::css(
        grid_template_columns = "3fr 2fr",
        max_width = "100%"
      ),
      bslib::card(
        bslib::card_body(
          reactable::reactableOutput(ns("la_stats"))
        )
      ),
      bslib::card(
        bslib::card_header("Quartile bands"),
        bslib::card_body(
          reactable::reactableOutput(ns("la_quartiles"))
        )
      )
    )
  )
}


#' Shiny Server Function for Computing and Rendering the LA Stats Table
#'
#' @param id A unique ID that identifies the server function
#' @param app_inputs A list of inputs from the Shiny app
#' @param bds_metrics A data frame of BDS metrics
#' @param stat_n_la A data frame of statistical neighbours for each LA
#' @return A list of outputs for the UI, including a data table of the LA stats
LA_StatsTableServer <- function(id, app_inputs, bds_metrics, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format LA data
    la_long <- LA_LongDataServer(
      "la_table_data", app_inputs,
      bds_metrics, stat_n_la
    )

    # Difference between last two years
    la_diff <- reactive({
      la_long() |>
        calculate_change_from_prev_yr()
    })

    # Build Stats LA Level table ----------------------------------
    la_stats_table <- shiny::reactive({
      # Extract change from prev year (from LA table)
      la_change_prev <- la_diff() |>
        filter_la_regions(app_inputs$la(), pull_col = "values_num")

      # Get polarity of indicator
      la_indicator_polarity <- filtered_bds() |>
        pull_uniques("Polarity")

      # Set the trend value
      la_trend <- as.numeric(la_change_prev)

      # Get latest rank, ties are set to min & NA vals to NA rank
      la_rank <- filtered_bds() |>
        filter_la_regions(la_names_bds, latest = TRUE) |>
        calculate_rank(la_indicator_polarity) |>
        filter_la_regions(app_inputs$la(), pull_col = "rank")

      # Calculate quartile bands for indicator
      la_quartile_bands <- filtered_bds() |>
        filter_la_regions(la_names_bds,
          latest = TRUE,
          pull_col = "values_num"
        ) |>
        quantile(na.rm = TRUE)

      # Extracting LA latest value
      la_indicator_val <- filtered_bds() |>
        filter_la_regions(app_inputs$la(),
          latest = TRUE,
          pull_col = "values_num"
        )

      # Calculating which quartile this value sits in
      la_quartile <- calculate_quartile_band(
        la_indicator_val,
        la_quartile_bands,
        la_indicator_polarity
      )

      # Build stats LA Level table
      la_stats_table <- build_la_stats_table(
        la_diff(),
        app_inputs$la(),
        la_trend,
        la_change_prev,
        la_rank,
        la_quartile,
        la_quartile_bands,
        la_indicator_polarity
      ) |>
        pretty_num_table(
          dp = get_indicator_dps(filtered_bds()),
          exclude_columns = c("LA Number", "Trend", "Latest National Rank")
        )

      la_stats_table
    })

    # LA Stats table
    output$la_stats <- reactable::renderReactable({
      dfe_reactable(
        la_stats_table() |>
          dplyr::select(!dplyr::ends_with("including"), -Polarity),
        columns = modifyList(
          # Create the reactable with specific column alignments
          align_reactable_cols(
            la_stats_table() |>
              dplyr::select(-Polarity),
            num_exclude = "LA Number",
            categorical = c("Trend", "Quartile Banding")
          ),
          # Style Quartile Banding column with colour
          list(
            `Quartile Banding` = reactable::colDef(
              style = quartile_banding_col_def(la_stats_table())
            ),
            Trend = reactable::colDef(
              cell = trend_icon_renderer
            )
          )
        )
      )
    })

    # Quartile banding table
    output$la_quartiles <- reactable::renderReactable({
      dfe_reactable(
        la_stats_table() |>
          dplyr::select(dplyr::ends_with("including"), -Polarity),
        columns = align_reactable_cols(
          la_stats_table() |>
            dplyr::select(dplyr::ends_with("including"), -Polarity)
        )
      )
    })
  })
}

# nolint end
