# nolint start: object_name
#
#' Local Authority Chart UI Module
#'
#' This module creates the UI elements for displaying Local Authority
#' (LA) charts.
#' It provides a container with navigation tabs to toggle between a line
#' chart and a bar chart.
#'
#' @param id A unique identifier for the module instance.
#'
#' @return A `div` containing the UI elements for the Local Authority charts.
#'
#' @details
#' This UI module creates a well-styled container that includes a
#' tabbed interface.
#' The tabs allow users to switch between a line chart and a bar chart,
#' both rendered using the `ggiraph::girafeOutput` for interactive plotting.
#'
#' The UI components are wrapped in `bslib::navset_card_underline`,
#' which provides the tabbed navigation.
#' Each tab contains a `bslib::card` with a `bslib::card_body` that
#' holds the chart output.
#' The charts are named as `line_chart` and `bar_chart`,
#' and are dynamically rendered based on the inputs and server logic.
#'
LA_ChartUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "la_charts",
      bslib::nav_panel(
        title = "Line chart",
        bslib::card(
          bslib::card_body(
            ggiraph::girafeOutput(ns("line_chart"))
          ),
          full_screen = TRUE
        ),
      ),
      bslib::nav_panel(
        title = "Bar chart",
        bslib::card(
          bslib::card_body(
            ggiraph::girafeOutput(ns("bar_chart"))
          ),
          full_screen = TRUE
        )
      )
    )
  )
}


#' Local Authority Line Chart Server Module
#'
#' This module generates and renders an interactive line chart for
#' Local Authorities
#' using the ggiraph package, based on the selected inputs and data.
#'
#' @param id A unique identifier for the module instance.
#' @param app_inputs A reactive object containing the application inputs
#' (e.g., selected topic, indicator).
#' @param bds_metrics A data frame containing the metrics data for
#' various Local Authorities.
#' @param stat_n_la A data frame containing statistical data for the
#' Local Authorities.
#'
#' @return None (This function is used for its side effects).
#'
#' @details
#' This server module creates a reactive expression for generating the
#' line chart based on the filtered data.
#'
#' The line chart is constructed using `ggplot2` and made interactive
#' with `ggiraph`.
#' Custom tooltips, hover effects, and interactive elements are added for
#' enhanced user experience.
#'
#' The final chart is rendered using `ggiraph::renderGirafe` and displayed
#' in the `line_chart` UI output.
#' The chart is designed to be fully responsive and interactive,
#' allowing users to explore the data visually.
#'
LA_LineChartServer <- function(id, app_inputs, bds_metrics, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format LA data
    la_long <- LA_LongDataServer(
      "la_table_data", app_inputs,
      bds_metrics, stat_n_la
    )

    # LA Level line chart plot ----------------------------------
    la_line_chart <- reactive({
      # Plot
      la_line_chart <- la_long() |>
        ggplot2::ggplot() +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            color = `LA and Regions`,
            shape = `LA and Regions`,
            data_id = `LA and Regions`
          ),
          na.rm = TRUE
        ) +
        ggiraph::geom_line_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            color = `LA and Regions`,
            data_id = `LA and Regions`
          ),
          na.rm = TRUE
        ) +
        format_axes(la_long()) +
        set_plot_colours(la_long()) +
        set_plot_labs(filtered_bds(), app_inputs$indicator()) +
        custom_theme()


      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(la_long()),
        tooltip_vlines,
        la_long()
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (la_line_chart + vertical_hover),
        width_svg = 8,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        )
      )
    })

    output$line_chart <- ggiraph::renderGirafe({
      la_line_chart()
    })
  })
}


#' Local Authority Bar Chart Server Module
#'
#' This module generates and renders an interactive bar chart for
#' Local Authorities
#' using the ggiraph package, based on the selected inputs and data.
#'
#' @param id A unique identifier for the module instance.
#' @param app_inputs A reactive object containing the application inputs
#' (e.g., selected topic, indicator).
#' @param bds_metrics A data frame containing the metrics data for various
#' Local Authorities.
#' @param stat_n_la A data frame containing statistical data for the
#' Local Authorities.
#'
#' @return None (This function is used for its side effects).
#'
#' @details
#' This server module creates a reactive expression for generating the
#' bar chart based on the filtered data.
#'
#' The bar chart is constructed using `ggplot2` and made interactive
#' with `ggiraph`.
#' Custom tooltips, hover effects, and interactive elements are added
#' for enhanced user experience.
#'
#' The final chart is rendered using `ggiraph::renderGirafe` and
#' displayed in the `bar_chart` UI output.
#' The chart is designed to be fully responsive and interactive,
#' allowing users to explore the data visually.
#'
LA_BarChartServer <- function(id, app_inputs, bds_metrics, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format LA data
    la_long <- LA_LongDataServer(
      "la_table_data", app_inputs,
      bds_metrics, stat_n_la
    )

    # LA Level bar plot ----------------------------------
    la_bar_chart <- reactive({
      # Build plot
      la_bar_chart <- la_long() |>
        ggplot2::ggplot() +
        ggiraph::geom_col_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            fill = `LA and Regions`,
            tooltip = glue::glue_data(
              la_long() |>
                pretty_num_table(include_columns = "values_num", dp = 1),
              "Year: {Years}\n{`LA and Regions`}: {values_num}"
            ),
            data_id = `LA and Regions`
          ),
          position = "dodge",
          width = 0.6,
          na.rm = TRUE,
          colour = "black"
        ) +
        format_axes(la_long()) +
        set_plot_colours(la_long(), "fill") +
        set_plot_labs(filtered_bds(), app_inputs$indicator()) +
        custom_theme()

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = la_bar_chart,
        width_svg = 8,
        options = generic_ggiraph_options()
      )
    })


    output$bar_chart <- ggiraph::renderGirafe({
      la_bar_chart()
    })
  })
}

# nolint end
