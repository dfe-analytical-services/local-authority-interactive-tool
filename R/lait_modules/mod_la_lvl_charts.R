# nolint start: object_name
#
#' Line Chart UI Module
#'
#' Creates a user interface component for displaying a line chart with
#' download options. This UI module is designed to be used within a Shiny
#' application and provides a structured layout for presenting a line chart
#' alongside relevant download buttons.
#'
#' @param id A unique identifier for the module. This is used for namespacing
#'   the UI elements within the Shiny app.
#'
#' @return A `shiny::tagList` containing a navigation panel with a line chart
#'   display, download options, and a hidden static plot for copy-to-clipboard
#'   functionality.
#'
#' @details
#' The UI includes:
#' - A navigation panel titled "Line chart".
#' - A flexbox layout that contains the line chart and download options,
#'   styled for a cohesive appearance.
#' - A hidden plot used for copying the chart to the clipboard, ensuring
#'   users can easily export the chart without additional steps.
#'
#' @examples
#' # Example usage in UI
#' LA_LineChartUI("line_chart_ui")
#'
LA_LineChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart",
    div(
      style = "display: flex; justify-content: space-between; align-items: center; background: white;",
      # Line chart
      create_chart_card_ui(
        ns("line_chart"),
        paste(
          "Line chart displaying the data in the first table on the LA page.",
          "This includes the selected Local Authority, its Region, Statistical",
          "Neighbour average and England."
        )
      ),
      # Download options
      create_download_options_ui(
        ns("download_btn"),
        ns("copybtn")
      )
    ),
    # Hidden static plot for copy-to-clipboard
    create_hidden_clipboard_plot(ns("copy_plot"))
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
LA_LineChartServer <- function(id,
                               app_inputs,
                               bds_metrics,
                               stat_n_la,
                               covid_affected_data) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format LA data
    la_long <- LA_LongDataServer(
      "la_table_data", app_inputs,
      bds_metrics, stat_n_la
    )

    # Build main static plot
    line_chart <- reactive({
      # Generate the covid plot data if add_covid_plot is TRUE
      covid_plot <- calculate_covid_plot(
        la_long(),
        covid_affected_data,
        app_inputs$indicator(),
        "line"
      )

      # Build plot
      la_long() |>
        # Set geog orders so selected LA is on top of plot
        reorder_la_regions(reverse = TRUE) |>
        ggplot2::ggplot() +
        ggiraph::geom_line_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            color = `LA and Regions`,
            data_id = `LA and Regions`
          ),
          na.rm = TRUE,
          linewidth = 1
        ) +
        # Only show point data where line won't appear (NAs)
        ggplot2::geom_point(
          data = subset(
            create_show_point(la_long(), covid_affected_data, app_inputs$indicator()),
            show_point
          ),
          ggplot2::aes(x = Years_num, y = values_num, color = `LA and Regions`),
          shape = 15,
          size = 1,
          na.rm = TRUE
        ) +
        # Add COVID plot if indicator affected
        add_covid_elements(covid_plot) +
        format_axes(la_long()) +
        set_plot_colours(la_long(), "colour", app_inputs$la()) +
        set_plot_labs(filtered_bds()) +
        custom_theme() +
        # Revert order of the legend so goes from right to left
        ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
    })

    # Build interactive line chart
    interactive_line_chart <- reactive({
      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(la_long()),
        tooltip_vlines,
        la_long(),
        get_indicator_dps(filtered_bds())
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (line_chart() + vertical_hover),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })

    # Line chart download ------------------------------------------------------
    # Initialise server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Line")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = line_chart(), "html" = interactive_line_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "LA-Level-Line-Chart"))
    )

    # Plot used for copy to clipboard (hidden)
    output$copy_plot <- shiny::renderPlot(
      {
        line_chart()
      },
      res = 200,
      width = 24 * 96,
      height = 12 * 96
    )

    # LA Level line chart plot ------------------------------------------------
    output$line_chart <- ggiraph::renderGirafe({
      interactive_line_chart()
    })
  })
}


#' Bar Chart UI Module
#'
#' Creates a user interface component for displaying a bar chart with
#' download options. This UI module is intended for use within a Shiny
#' application and provides a structured layout for presenting a bar chart
#' alongside relevant download buttons.
#'
#' @param id A unique identifier for the module. This is used for namespacing
#'   the UI elements within the Shiny app.
#'
#' @return A `shiny::tagList` containing a navigation panel with a bar chart
#'   display, download options, and a hidden static plot for copy-to-clipboard
#'   functionality.
#'
#' @details
#' The UI includes:
#' - A navigation panel titled "Bar chart".
#' - A flexbox layout that contains the bar chart and download options,
#'   styled for a cohesive appearance.
#' - A hidden plot used for copying the chart to the clipboard, allowing
#'   users to easily export the chart without additional steps.
#'
#' @examples
#' # Example usage in UI
#' LA_BarChartUI("bar_chart_ui")
#'
LA_BarChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Bar chart",
    div(
      style = "display: flex; justify-content: space-between; align-items: center; background: white;",
      # Bar chart
      create_chart_card_ui(
        ns("bar_chart"),
        paste(
          "Bar chart displaying the data in the first table on the LA page.",
          "This includes the selected Local Authority, its Region, Statistical",
          "Neighbour average and England."
        )
      ),
      # Download options
      create_download_options_ui(
        ns("download_btn"),
        ns("copybtn")
      )
    ),
    # Hidden static plot for copy-to-clipboard
    create_hidden_clipboard_plot(ns("copy_plot"))
  )
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
LA_BarChartServer <- function(id,
                              app_inputs,
                              bds_metrics,
                              stat_n_la,
                              covid_affected_data) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format LA data
    la_long <- LA_LongDataServer(
      "la_table_data", app_inputs,
      bds_metrics, stat_n_la
    )

    # Build main static plot
    bar_chart <- reactive({
      # Generate the covid plot data if add_covid_plot is TRUE
      covid_plot <- calculate_covid_plot(
        la_long(),
        covid_affected_data,
        app_inputs$indicator(),
        "bar"
      )

      # Build plot
      la_long() |>
        ggplot2::ggplot() +
        ggiraph::geom_col_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            fill = `LA and Regions`,
            tooltip = tooltip_bar(
              la_long(),
              get_indicator_dps(filtered_bds()),
              app_inputs$la()
            ),
            data_id = `LA and Regions`
          ),
          position = "dodge",
          width = 0.6,
          na.rm = TRUE,
          colour = "black"
        ) +
        # Add COVID plot if indicator affected
        add_covid_elements(covid_plot) +
        format_axes(la_long()) +
        set_plot_colours(la_long(), "fill", app_inputs$la()) +
        set_plot_labs(filtered_bds()) +
        custom_theme()
    })

    # Plotting interactive graph
    interactive_bar_chart <- reactive({
      ggiraph::girafe(
        ggobj = bar_chart(),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })

    # Bar chart download ------------------------------------------------------
    # Initialise server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Bar")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = bar_chart(), "html" = interactive_bar_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "LA-Level-Bar-Chart"))
    )

    # Plot used for copy to clipboard (hidden)
    output$copy_plot <- shiny::renderPlot(
      {
        bar_chart()
      },
      res = 200,
      width = 24 * 96,
      height = 12 * 96
    )

    # LA Level bar chart plot -------------------------------------------------
    output$bar_chart <- ggiraph::renderGirafe({
      interactive_bar_chart()
    })
  })
}

# nolint end
