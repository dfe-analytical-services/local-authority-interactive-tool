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
      # Don't change the currently selected indicator if no indicator is selected
      if (is.null(app_inputs$indicator()) || app_inputs$indicator() == "") {
        return()
      }

      # Filter for selected indicator
      filtered_data <- bds_metrics |>
        dplyr::filter(
          Measure == app_inputs$indicator()
        )

      # Check for duplicates
      duplicates <- filtered_data |>
        dplyr::group_by(Measure, `LA and Regions`, Years) |>
        dplyr::summarise(YearCount = dplyr::n(), .groups = "drop") |>
        dplyr::filter(YearCount > 1)

      # Issue warning if duplicates exist
      if (nrow(duplicates) > 0) {
        warning(
          "There is duplicate data for the following indicator:\n",
          paste0("Indicator: ", app_inputs$indicator())
        )
      }

      # Update reactive values with the filtered data
      filtered_bds$data <- filtered_data
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
          values_num = dplyr::na_if(mean(values_num, na.rm = TRUE), NaN),
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
    bslib::navset_card_tab(
      id = "la_lvl_table_tabs",
      bslib::nav_panel(
        "Table",
        with_gov_spinner(
          reactable::reactableOutput(ns("la_table"))
        )
      ),
      bslib::nav_panel(
        "Download data",
        shiny::uiOutput(ns("download_file_txt")),
        Download_DataUI(ns("la_download"), "LA Table")
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
        dplyr::arrange(`LA and Regions`)
    })


    # LA table download -------------------------------------------------------
    # File download text - calculates file size
    ns <- NS(id)
    output$download_file_txt <- shiny::renderUI({
      file_type_input_btn(ns("file_type"), la_table())
    })

    # Download dataset
    Download_DataServer(
      "la_download",
      reactive(input$file_type),
      reactive(la_table()),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Local-Authority-View"))
    )

    # Reactable table output
    output$la_table <- reactable::renderReactable({
      dfe_reactable(
        la_table(),
        columns = utils::modifyList(
          format_num_reactable_cols(
            la_table(),
            get_indicator_dps(filtered_bds()),
            num_exclude = "LA Number"
          ),
          set_custom_default_col_widths()
        ),
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
    bslib::card(
      bslib::layout_column_wrap(
        width = NULL,
        style = htmltools::css(
          grid_template_columns = "3fr 2fr",
          max_width = "100%"
        ),
        div(
          bslib::card_header("Summary"),
          with_gov_spinner(
            reactable::reactableOutput(ns("la_stats")),
            size = 0.4
          )
        ),
        div(
          bslib::card_header("Quartile bands"),
          with_gov_spinner(
            reactable::reactableOutput(ns("la_quartiles")),
            size = 0.4
          )
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
LA_StatsTableServer <- function(id,
                                app_inputs,
                                bds_metrics,
                                stat_n_la,
                                no_qb_indicators) {
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
      # Helper to avoid zero-length issues
      safe_scalar <- function(x, na_value) {
        if (length(x) == 0 || all(is.na(x))) {
          return(na_value)
        }
        x
      }

      # Extract change from prev year (from LA table)
      la_change_prev <- safe_scalar(
        la_diff() |>
          filter_la_regions(app_inputs$la(), pull_col = "values_num"),
        NA_real_
      )

      # Get polarity of indicator
      la_indicator_polarity <- safe_scalar(
        filtered_bds() |> pull_uniques("Polarity"),
        NA_character_
      )

      # Set the trend value
      la_trend <- safe_scalar(as.numeric(la_change_prev), NA_real_)

      # Get latest rank, ties are set to min & NA vals to NA rank
      la_rank <- safe_scalar(
        filtered_bds() |>
          filter_la_regions(la_names_bds, latest = TRUE) |>
          calculate_rank(la_indicator_polarity) |>
          filter_la_regions(app_inputs$la(), pull_col = "rank"),
        NA_integer_
      )

      # Calculate quartile bands for indicator
      la_quartile_bands <- safe_scalar(
        filtered_bds() |>
          filter_la_regions(la_names_bds,
            latest = TRUE,
            pull_col = "values_num"
          ) |>
          quantile(na.rm = TRUE),
        rep(NA_real_, 5) # quantile returns vector of length 5
      )

      # Extracting LA latest value
      la_indicator_val <- safe_scalar(
        filtered_bds() |>
          filter_la_regions(app_inputs$la(),
            latest = TRUE,
            pull_col = "values_num"
          ),
        NA_real_
      )

      # Boolean as to whether to include Quartile Banding
      no_show_qb <- app_inputs$indicator() %in% no_qb_indicators

      # Calculating which quartile this value sits in
      la_quartile <- safe_scalar(
        calculate_quartile_band(
          la_indicator_val,
          la_quartile_bands,
          la_indicator_polarity,
          no_show_qb
        ),
        NA_character_
      )

      # Build stats LA Level table (now protected from empty values)
      build_la_stats_table(
        la_diff(),
        safe_scalar(app_inputs$la(), NA_character_),
        la_trend,
        la_change_prev,
        la_rank,
        la_quartile,
        la_quartile_bands,
        safe_scalar(get_indicator_dps(filtered_bds()), NA_integer_),
        la_indicator_polarity,
        no_show_qb
      )
    })


    # LA Stats table
    output$la_stats <- reactable::renderReactable({
      # Drop Quartile Bands
      stats_table <- la_stats_table() |>
        dplyr::select(-dplyr::any_of(c("A", "B", "C", "D", "No Quartiles")))

      dfe_reactable(
        stats_table,
        columns = modifyList(
          # Create the reactable with specific column alignments
          format_num_reactable_cols(
            la_stats_table() |>
              dplyr::select(!dplyr::ends_with("including")),
            get_indicator_dps(filtered_bds()),
            num_exclude = "LA Number",
            categorical = c("Trend", "Quartile Banding", "Latest National Rank")
          ),
          # Style Quartile Banding column with colour
          list(
            set_custom_default_col_widths(),
            Trend = reactable::colDef(
              header = add_tooltip_to_reactcol(
                "Trend",
                "Based on change from previous year",
                placement = "top"
              ),
              cell = trend_icon_renderer,
              style = function(value) {
                get_trend_colour(value, la_stats_table()$Polarity[1])
              }
            ),
            `Quartile Banding` = reactable::colDef(
              style = function(value, index) {
                quartile_banding_col_def(la_stats_table()[index, ])
              }
            ),
            `Latest National Rank` = reactable::colDef(
              header = add_tooltip_to_reactcol(
                "Latest National Rank",
                "Rank 1 is always best/top",
                placement = "right"
              )
            ),
            Polarity = reactable::colDef(show = FALSE)
          )
        )
      )
    })

    # Quartile banding table
    output$la_quartiles <- reactable::renderReactable({
      # Get quartile bands only
      qb_table <- la_stats_table() |>
        dplyr::select(dplyr::any_of(c("A", "B", "C", "D", "No Quartiles")))

      dfe_reactable(
        qb_table,
        # Format as categorical
        columns = lapply(names(qb_table), function(col) {
          format_reactable_cat_col()
        }) |>
          setNames(names(qb_table))
      )
    })
  })
}

# nolint end
