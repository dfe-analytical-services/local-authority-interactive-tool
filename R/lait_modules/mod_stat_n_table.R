# nolint start: object_name
#
# General modules =============================================================
# These are nested within other modules
#
#' Get Local Authority Statistical Neighbours Server Module
#'
#' This module retrieves the statistical neighbours for a selected Local
#' Authority (LA) based on the input from the user.
#'
#' @param id A character string representing the module's namespace ID.
#' @param la_input A reactive expression returning the selected Local Authority
#' name, which is used to filter the statistical neighbours.
#' @param stat_n_la A data frame containing statistical neighbours information
#' for all Local Authorities. The data must include columns for "LA Name" and
#' "LA Name_sn" (the statistical neighbours).
#'
#' @return A reactive expression that provides the list of statistical
#' neighbours (as "LA Name_sn") for the selected Local Authority.
#'
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


#' Get Local Authority Cleaned Region Server Module
#'
#' This module retrieves the region for a selected Local Authority (LA) and
#' applies cleaning to the region data (e.g., adjusting for London regions).
#'
#' @param id A character string representing the module's namespace ID.
#' @param la_input A reactive expression returning the selected Local Authority
#' name, used to filter the region data.
#' @param filtered_bds A reactive expression returning additional boundary
#' data needed for cleaning the region information.
#' @param stat_n_la A data frame containing the region and statistical
#' neighbours information for all Local Authorities, which includes "LA Name"
#' and "GOReg" (Government Office Region).
#'
#' @return A reactive expression that provides the cleaned region for the
#' selected Local Authority.
#'
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


#' Long Format Statistical Neighbour Data Server Module
#'
#' This module generates long-format data for a selected Local Authority (LA),
#' including the statistical neighbours and region information, and computes
#' the statistical neighbour average.
#'
#' @param id A character string representing the module's namespace ID.
#' @param la_input A reactive expression returning the selected Local Authority
#' name, used to filter the data.
#' @param filtered_bds A reactive expression returning the boundary data,
#' which includes values for different Local Authorities and their neighbours.
#' @param stat_n_la A data frame containing statistical neighbours and region
#' information for all Local Authorities.
#'
#' @return A reactive expression providing long-format data, which includes
#' the selected LA, statistical neighbours, the average for statistical
#' neighbours, and region information.
#'
#' @details The module makes use of two helper server modules:
#' \code{\link{Get_LAStatNsServer}} to obtain the LA's statistical neighbours
#' and \code{\link{Get_LACleanRegionServer}} to retrieve the LA's region with
#' cleaning applied. The output is suitable for visualisation in a time series
#' format or for further analysis.
#'
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
      req(filtered_bds(), la_input(), stat_n_sns(), stat_n_region())
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
        dplyr::filter(`LA and Regions` %in% c(la_input(), stat_n_sns(), stat_n_region(), "England")) |>
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


#' Statistical Neighbour Yearly Difference Server Module
#'
#' This module calculates the year-over-year difference in values for a selected
#' Local Authority (LA) and its statistical neighbours.
#'
#' @param id A character string representing the module's namespace ID.
#' @param la_input A reactive expression returning the selected Local Authority
#' name, used to filter the data.
#' @param filtered_bds A reactive expression returning the boundary data,
#' which includes values for different Local Authorities and their neighbours.
#' @param stat_n_la A data frame containing statistical neighbours and region
#' information for all Local Authorities.
#'
#' @return A reactive expression providing a data frame with the difference
#' in values between the last two years for the selected LA, its statistical
#' neighbours, and the region.
#'
#' @details This module relies on \code{\link{StatN_LongServer}} to obtain
#' long-format data for the LA and its statistical neighbours. The module
#' calculates the change in values between the last two years for analysis or
#' display purposes.
#'
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


#' Statistical Neighbour Data Table Server Module
#'
#' This module builds a formatted table for a selected Local Authority (LA)
#' and its statistical neighbours, including yearly values and differences.
#'
#' @param id A character string representing the module's namespace ID.
#' @param la_input A reactive expression returning the selected Local Authority
#' name, used to filter the data.
#' @param filtered_bds A reactive expression returning the boundary data,
#' which includes values for different Local Authorities and their neighbours.
#' @param stat_n_la A data frame containing statistical neighbours and region
#' information for all Local Authorities.
#'
#' @return A reactive expression providing a data frame formatted as a wide
#' table, where rows represent Local Authorities and columns represent yearly
#' values, including the difference between years.
#'
#' @details This module integrates data from two other server modules:
#' \code{\link{StatN_LongServer}} for the long-format data of the LA and its
#' statistical neighbours, and \code{\link{StatN_DiffServer}} to calculate the
#' difference in values between years. The final output is a pretty-printed
#' table suitable for presentation or further manipulation.
#'
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
      req(stat_n_long(), stat_n_diff())
      # Join difference and pivot wider
      stat_n_long() |>
        dplyr::bind_rows(stat_n_diff()) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions"),
          names_from = Years,
          values_from = values_num
        )
    })
  })
}

# LA Statistical Neighbours table =============================================
#' Statistical Neighbour Table UI Module
#'
#' This UI module generates a table to display the selected Local Authority (LA)
#' and its statistical neighbours.
#'
#' @param id A character string representing the module's namespace ID.
#'
#' @return A UI component containing a reactable output for displaying the
#' statistical neighbour table.
#'
StatN_LASNsTableUI <- function(id) {
  ns <- NS(id)

  reactable::reactableOutput(ns("output_table"))
}
StatN_TablesUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_tab(
      id = "stat_n_tables_tabs",
      bslib::nav_panel(
        "Tables",
        bslib::card(
          # Statistical Neighbour LA SNs Table --------------------------------
          bslib::card_header("Statistical Neighbours"),
          reactable::reactableOutput(ns("statn_table")),
          # Statistical Neighbour LA Geog Compare Table -----------------------
          div(
            # Add black border between the tables
            style = "overflow-y: visible;border-top: 2px solid black; padding-top: 2.5rem;",
            bslib::card_header("Other Geographies"),
            reactable::reactableOutput(ns("geog_table"))
          )
        ),
        br(),
        # Statistical Neighbour Statistics Table ------------------------------
        StatN_StatsTableUI("stat_n_stats_mod")
      ),
      bslib::nav_panel(
        "Download",
        file_type_input_btn(ns("file_type")),
        Download_DataUI(ns("statn_download"), "Statistical Neighbour Table"),
        Download_DataUI(ns("geog_download"), "Other Geographies Table")
      )
    )
  )
}


#' Statistical Neighbour Table Server Module
#'
#' This module generates a formatted reactable table that displays statistical
#' data for the selected Local Authority (LA) and its statistical neighbours,
#' along with the current year.
#'
#' @param id A character string representing the module's namespace ID.
#' @param app_inputs A list of reactive inputs, including the selected LA and
#' other app-level filters.
#' @param bds_metrics A reactive expression providing the metrics data
#' (boundaries) for filtering and analysis.
#' @param stat_n_la A data frame containing statistical neighbours and region
#' information for all Local Authorities.
#'
#' @return This module does not return a value. It renders a reactable output
#' displaying the LA and its statistical neighbours in a formatted table.
#'
#' @details The module integrates the output of \code{\link{StatN_DataServer}}
#' for statistical neighbour data, \code{\link{Get_LAStatNsServer}} for LA
#' neighbours, and \code{\link{Current_YearServer}} for retrieving the current
#' year. The final reactable table is formatted using a custom dfe_reactable
#' function, and rows are highlighted based on the selected LA.
#'
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

    # Filter for selected LA and its SNs - ready for table output
    stat_n_sns_table <- reactive({
      stat_n_table() |>
        dplyr::filter(`LA and Regions` %in% c(app_inputs$la(), stat_n_sns())) |>
        dplyr::arrange(.data[[current_year()]], `LA and Regions`)
    })

    # Download ----------------------------------------------------------------
    Download_DataServer(
      "statn_download",
      reactive(input$file_type),
      reactive(stat_n_sns_table()),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "SN-Stat-Neighbour-Level"))
    )

    # Table output ------------------------------------------------------------
    output$statn_table <- reactable::renderReactable({
      # Create table with correct formatting
      dfe_reactable(
        stat_n_sns_table(),
        columns = utils::modifyList(
          format_num_reactable_cols(
            stat_n_sns_table(),
            get_indicator_dps(filtered_bds()),
            num_exclude = "LA Number"
          ),
          set_custom_default_col_widths()
        ),
        rowStyle = function(index) {
          highlight_selected_row(index, stat_n_sns_table(), app_inputs$la())
        },
        pagination = FALSE
      )
    })
  })
}


# LA Statistical Neighbour Geographic comparison table ========================
#' Statistical Neighbour Geographic Comparison Table UI Module
#'
#' This UI module generates a table for comparing the selected Local Authority
#' (LA) with its statistical neighbours, region, and England.
#'
#' @param id A character string representing the module's namespace ID.
#'
#' @return A UI component containing a reactable output for displaying the
#' geographic comparison table. The table includes a black border and spacing
#' between sections for better visual distinction.
#'
StatN_GeogCompTableUI <- function(id) {
  ns <- NS(id)

  div(
    # Add black border between the tables
    style = "overflow-y: visible;border-top: 2px solid black; padding-top: 2.5rem;",
    reactable::reactableOutput(ns("output_table"))
  )
}


#' Statistical Neighbour Geographic Comparison Table Server Module
#'
#' This module generates a reactable table comparing the selected Local
#' Authority (LA) with its statistical neighbours, region, and England.
#'
#' @param id A character string representing the module's namespace ID.
#' @param app_inputs A list of reactive inputs, including the selected LA and
#' other app-level filters.
#' @param bds_metrics A reactive expression providing the metrics data
#' (boundaries) for filtering and analysis.
#' @param stat_n_la A data frame containing statistical neighbours and region
#' information for all Local Authorities.
#'
#' @return This module does not return a value. It renders a reactable output
#' displaying the geographic comparison between the LA, its statistical
#' neighbours, the region, and England.
#'
#' @details The module uses \code{\link{StatN_DataServer}} to fetch the formatted
#' statistical data and \code{\link{Get_LACleanRegionServer}} to retrieve the
#' LA's cleaned region. The table compares values across geographic areas,
#' and column alignments are handled using a custom \code{format_num_reactable_cols}
#' function.
#'
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

    # Keep LA geographic comparison areas
    stat_n_geog_table <- reactive({
      stat_n_table() |>
        dplyr::filter(`LA and Regions` %in% c(
          "Statistical Neighbours",
          stat_n_region(),
          "England"
        )) |>
        dplyr::arrange(`LA and Regions`)
    })

    # Download ----------------------------------------------------------------
    Download_DataServer(
      "geog_download",
      reactive(input$file_type),
      reactive(stat_n_geog_table()),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Geog-Stat-Neighbour-Level"))
    )

    # Table output ------------------------------------------------------------
    output$geog_table <- reactable::renderReactable({
      # Output table
      dfe_reactable(
        stat_n_geog_table(),
        # Create the reactable with specific column alignments
        columns = utils::modifyList(
          format_num_reactable_cols(
            stat_n_geog_table(),
            get_indicator_dps(filtered_bds()),
            num_exclude = "LA Number"
          ),
          set_custom_default_col_widths()
        ),
        pagination = FALSE
      )
    })
  })
}


# LA Statistical Neighbours Statistics table ==================================
#' Statistical Neighbour Statistics Table UI Module
#'
#' This UI module generates a table to display statistical data for the
#' selected Local Authority (LA) and its geographic comparison areas.
#'
#' @param id A character string representing the module's namespace ID.
#'
#' @return A UI component containing a reactable output for displaying the
#' statistical data in a formatted table.
#'
#' @importFrom reactable reactableOutput
#' @importFrom bslib card card_body
#' @export
#'
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


#' Statistical Neighbour Statistics Table Server Module
#'
#' This module generates a table with detailed statistical metrics for the
#' selected Local Authority (LA), including trend, national rank, and quartile
#' banding.
#'
#' @param id A character string representing the module's namespace ID.
#' @param app_inputs A list of reactive inputs, including the selected LA and
#' other app-level filters.
#' @param bds_metrics A reactive expression providing the metrics data
#' (boundaries) for filtering and analysis.
#' @param stat_n_la A data frame containing statistical neighbours and region
#' information for all Local Authorities.
#' @param la_names_bds A character vector of LA names used for ranking and
#' comparison in the data.
#'
#' @return This module does not return a value. It renders a reactable output
#' displaying the statistical data table with columns for trend, change from
#' previous year, national rank, and quartile banding.
#'
#' @details This module uses \code{\link{StatN_DiffServer}} to calculate
#' the yearly change, and ranks the selected LA against other regions.
#' It also calculates quartile banding based on the latest values for
#' the selected indicator.
#'
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
      stat_n_stats_output <- stat_n_stats_table()

      dfe_reactable(
        stat_n_stats_output |>
          dplyr::select(-Polarity),
        columns = modifyList(
          # Create the reactable with specific column alignments
          format_num_reactable_cols(
            stat_n_stats_output |>
              dplyr::select(-Polarity),
            get_indicator_dps(filtered_bds()),
            num_exclude = "LA Number",
            categorical = c("Trend", "Quartile Banding", "National Rank")
          ),
          # Define specific formatting for the Trend and Quartile Banding columns
          list(
            set_custom_default_col_widths(),
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

# nolint end
