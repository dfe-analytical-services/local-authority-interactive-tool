# nolint start: object_name
#
# General modules =============================================================
# Building data for plotting
#
#' Long Plot Server Module for Regions
#'
#' This module handles the server-side logic for creating long format data
#' for regional local authorities. It filters data based on selected topics
#' and indicators, and removes London regions with all NA values, along with
#' England from the final output.
#'
#' @param id A unique identifier for the module instance.
#' @param app_inputs A list of input parameters from the application.
#' @param bds_metrics A data frame containing metrics for filtering.
#' @param region_names_bds A data frame of regional names for mapping.
#'
#' @return A reactive expression containing filtered long format data for
#' regions, excluding specified regions and handling NA values.
#'
#' @examples
#' Region_LongPlotServer(
#'   "region_plot", app_inputs, metrics_data,
#'   region_names
#' )
#'
Region_LongPlotServer <- function(id, app_inputs, bds_metrics, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Long format Region LA data
    region_long <- Region_LongDataServer("region_long", filtered_bds, region_names_bds)

    # Filter region_long data for any (Ldn) regions with all NA values, and England
    reactive({
      region_long() |>
        dplyr::group_by(`LA and Regions`) |>
        dplyr::filter(
          !(grepl("^London \\(", `LA and Regions`) & dplyr::n() == sum(is.na(values_num))),
          `LA and Regions` %notin% "England"
        ) |>
        dplyr::ungroup()
    })
  })
}


# Multi-choice chart input module =============================================
#' Chart Input UI Module
#'
#' Creates a user interface component for selecting regions to compare in
#' a chart. Users can select up to three regions from a provided list.
#'
#' @param id A unique identifier for the module instance.
#'
#' @return A `div` containing a selectize input for region selection.
#'
#' @examples
#' Chart_InputUI("chart_input_module")
#'
Chart_InputUI <- function(id) {
  ns <- NS(id)

  div(
    shiny::selectizeInput(
      inputId = ns("chart_input"),
      label = "Select region to compare (max 3)",
      choices = region_names_bds,
      multiple = TRUE,
      options = list(
        maxItems = 3,
        plugins = list("remove_button"),
        dropdownParent = "body"
      )
    )
  )
}


#' Chart Input Server Module
#'
#' Handles server-side logic for the Chart Input module. It filters the
#' available regions based on user input and updates the selection
#' dynamically when the default region changes. Ensures that the default
#' region is not selectable.
#'
#' @param id A unique identifier for the module instance.
#' @param app_inputs A list of input parameters from the application.
#' @param region_long_plot A reactive expression providing long format
#' region data.
#' @param region_clean A reactive expression for the default region to
#' exclude from selection.
#'
#' @return A reactive expression containing the valid selected regions
#' for the chart, excluding the default region.
#'
#' @examples
#' Chart_InputServer(
#'   "chart_input_module", app_inputs, region_long_data,
#'   region_default
#' )
#'
Chart_InputServer <- function(id, app_inputs, region_long_plot, region_clean) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression to generate multi_chart_data
    multi_chart_data <- reactive({
      region_long_plot() |>
        dplyr::filter(
          `LA and Regions` != region_clean()
        ) |>
        pull_uniques("LA and Regions")
    })

    # Update chart input selection when region_clean changes
    shiny::observeEvent(region_clean(), {
      shiny::updateSelectizeInput(
        session = session,
        inputId = "chart_input",
        choices = multi_chart_data(),
        selected = setdiff(input$chart_input, region_clean())
      )
    })

    # Return valid selected chart input
    reactive({
      # Remove region_clean() (default region from selected LA) from
      # chart selected Regions
      setdiff(input$chart_input, region_clean())
    })
  })
}


# Region chart module =========================================================
#' Region Focus Line Chart UI Module
#'
#' Creates a user interface component for displaying a focus line chart
#' of regions. The chart is embedded in a navigational panel and styled
#' as a card for better presentation.
#'
#' @param id A unique identifier for the module instance.
#'
#' @return A `nav_panel` containing a card with a line chart output.
#'
#' @examples
#' Region_FocusLine_chartUI("focus_line_chart_module")
#'
Region_FocusLine_chartUI <- function(id) {
  ns <- NS(id)

  # Define the UI panel for the focus line chart
  bslib::nav_panel(
    title = "Line chart - Focus",
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      # Focus line chart
      create_chart_card_ui(ns("output_chart")),
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


#' Region Focus Line Chart Server Module
#'
#' Handles server-side logic for the Region Focus Line Chart module. It
#' retrieves and processes data for the chart, including filtering and
#' formatting. The chart is interactive and allows for a dynamic display
#' of region data over time.
#'
#' @param id A unique identifier for the module instance.
#' @param app_inputs A list of input parameters from the application.
#' @param bds_metrics A dataset containing metrics for plotting.
#' @param stat_n_geog A geographic identifier for statistics.
#' @param region_names_bds A dataset containing names of regions.
#'
#' @return None. Outputs an interactive line chart for the selected regions.
#'
#' @examples
#' Region_FocusLine_chartServer(
#'   "focus_line_chart_module", app_inputs,
#'   bds_metrics, stat_n_geog, region_names_bds
#' )
#'
Region_FocusLine_chartServer <- function(id,
                                         app_inputs,
                                         bds_metrics,
                                         stat_n_geog,
                                         region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Get data for the region's long format plot
    region_long_plot <- Region_LongPlotServer(
      "region_long_plot",
      app_inputs,
      bds_metrics,
      region_names_bds
    )

    # Clean region names based on selected inputs
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Filter data for the selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Retrieve the current year from the region plot data
    current_year <- Current_YearServer("current_year", region_long_plot)

    # Prepare the chart data, setting the selected region to appear at the front
    chart_data <- reactive({
      region_long_plot() |>
        reorder_la_regions(region_clean(), after = Inf)
    })

    # Build the static version of the focus line chart
    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        chart_data() |>
          ggplot2::ggplot() +
          ggiraph::geom_line_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`,
              size = `LA and Regions`,
              data_id = `LA and Regions`
            ),
            na.rm = TRUE
          ) +
          format_axes(chart_data()) +
          set_plot_colours(
            chart_data(),
            colour_type = "focus",
            focus_group = region_clean()
          ) +
          set_plot_labs(filtered_bds()) +
          ggrepel::geom_label_repel(
            data = subset(chart_data(), Years == current_year()),
            aes(
              x = Years_num,
              y = values_num,
              label = `LA and Regions`
            ),
            color = "black",
            segment.colour = NA,
            label.size = NA,
            max.overlaps = Inf,
            nudge_x = 2,
            direction = "y",
            vjust = .5,
            hjust = 1,
            show.legend = FALSE,
            na.rm = TRUE
          ) +
          custom_theme() +
          coord_cartesian(clip = "off") +
          theme(plot.margin = margin(5.5, 66, 5.5, 5.5)) +
          guides(colour = "none", size = "none")
      }
    })

    # Create the interactive version of the focus line chart
    interactive_chart <- reactive({
      output_chart <- if (all(is.na(chart_data()$values_num))) {
        static_chart()
      } else {
        # Creating vertical geoms to make vertical hover tooltip
        vertical_hover <- lapply(
          get_years(chart_data()),
          tooltip_vlines,
          chart_data(),
          get_indicator_dps(filtered_bds())
        )

        # Combine static chart and vertical hover into one ggplot object
        full_plot <- static_chart() + vertical_hover
      }

      # Now pass the full ggplot object to ggiraph::girafe
      ggiraph::girafe(
        ggobj = output_chart,
        width_svg = 12,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })

    # Chart download -----------------------------------------------------------
    # Initialise server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Focus Line")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Regional-Level-Focus-Line-Chart"))
    )

    # Plot used for copy to clipboard (hidden)
    output$copy_plot <- shiny::renderPlot(
      {
        static_chart()
      },
      res = 200,
      width = 24 * 96,
      height = 12 * 96
    )

    # Render the interactive line chart in the UI
    output$output_chart <- ggiraph::renderGirafe({
      interactive_chart()
    })
  })
}


# Region multi-choice line chart module =======================================
#' Region Multi-Choice Line Chart UI Module
#'
#' Creates a user interface component for displaying a line chart based
#' on user-selected regions. The UI includes a filter sidebar for selecting
#' multiple regions to compare.
#'
#' @param id A unique identifier for the module instance.
#'
#' @return A `nav_panel` containing a card with a sidebar and line chart output.
#'
#' @examples
#' Region_Multi_chartUI("multi_chart_module")
#'
Region_Multi_chartUI <- function(id) {
  ns <- NS(id)

  # Create a navigation panel for the multi-line chart
  bslib::nav_panel(
    title = "Line chart - user selection",
    # Main UI layout with chart and modal trigger button
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      # Card for the chart and filter inputs
      bslib::card(
        id = "region_multi_line",
        bslib::card_body(
          # Sidebar for filter options and chart display
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              title = "Filter options",
              position = "left",
              width = "30%",
              open = list(desktop = "open", mobile = "always-above"),
              # UI for chart input filters
              Chart_InputUI(ns("chart_input"))
            ),
            # Chart display area
            ggiraph::girafeOutput(ns("output_chart"))
          )
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
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


#' Region Multi-Choice Line Chart Server Module
#'
#' Handles server-side logic for the Region Multi-Choice Line Chart module.
#' It retrieves and processes data for the chart, filtering based on user
#' selections and building an interactive line chart that displays selected
#' regions over time.
#'
#' @param id A unique identifier for the module instance.
#' @param app_inputs A list of input parameters from the application.
#' @param bds_metrics A dataset containing metrics for plotting.
#' @param stat_n_geog A geographic identifier for statistics.
#' @param region_names_bds A dataset containing names of regions.
#'
#' @return None. Outputs an interactive multi-choice line chart for selected regions.
#'
#' @examples
#' Region_Multi_chartServer(
#'   "multi_chart_module", app_inputs,
#'   bds_metrics, stat_n_geog, region_names_bds
#' )
#'
Region_Multi_chartServer <- function(id,
                                     app_inputs,
                                     bds_metrics,
                                     stat_n_geog,
                                     region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Obtain data for plotting by region
    region_long_plot <- Region_LongPlotServer(
      "region_long_plot",
      app_inputs,
      bds_metrics,
      region_names_bds
    )

    # Get cleaned region names for use in filtering and plotting
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Filter BDS metrics based on selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Retrieve the current year for analysis
    current_year <- Current_YearServer("current_year", region_long_plot)

    # Get user-selected choices for the chart
    chart_input <- Chart_InputServer(
      "chart_input",
      app_inputs,
      region_long_plot,
      region_clean
    )

    # Create reactive chart data based on selected regions and user choices
    chart_data <- reactive({
      region_long_plot() |>
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% region_clean())
        ) |>
        # Reorder regions to layer lines based on selection
        reorder_la_regions(
          rev(c(region_clean(), chart_input()))
        )
    })

    # Build a static multi-choice chart
    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        chart_data() |>
          ggplot2::ggplot() +
          # Add interactive points for user engagement
          ggiraph::geom_point_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`,
              data_id = `LA and Regions`
            ),
            na.rm = TRUE
          ) +
          # Add interactive lines for visual representation
          ggiraph::geom_line_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`,
              data_id = `LA and Regions`
            ),
            na.rm = TRUE
          ) +
          format_axes(chart_data()) +
          manual_colour_mapping(
            unique(c(region_clean(), chart_input())),
            type = "line"
          ) +
          set_plot_labs(filtered_bds()) +
          custom_theme() +
          # Reverse legend order for better readability
          ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
      }
    })

    # Create the interactive version of the multi-choice line chart
    interactive_chart <- reactive({
      output_chart <- if (all(is.na(chart_data()$values_num))) {
        static_chart()
      } else {
        # Creating vertical geoms to make vertical hover tooltip
        vertical_hover <- lapply(
          get_years(chart_data()),
          tooltip_vlines,
          chart_data(),
          get_indicator_dps(filtered_bds())
        )

        # Combine static chart and vertical hover into one ggplot object
        full_plot <- static_chart() + vertical_hover
      }

      # Now pass the full ggplot object to ggiraph::girafe
      ggiraph::girafe(
        ggobj = output_chart,
        width_svg = 12,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })

    # Chart download -----------------------------------------------------------
    # Initialise server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Multi Line")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Regional-Level-Multi-Line-Chart"))
    )

    # Plot used for copy to clipboard (hidden)
    output$copy_plot <- shiny::renderPlot(
      {
        static_chart()
      },
      res = 200,
      width = 24 * 96,
      height = 12 * 96
    )

    # Render the interactive plot output
    output$output_chart <- ggiraph::renderGirafe({
      interactive_chart()
    })
  })
}

# nolint end
