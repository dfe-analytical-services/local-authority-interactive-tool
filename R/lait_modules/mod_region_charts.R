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
Region_FocusLineChartUI <- function(id) {
  ns <- NS(id)

  # Define the UI panel for the focus line chart
  bslib::nav_panel(
    title = "Line chart - Focus",
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      id = id,
      # Focus line chart
      create_chart_card_ui(
        ns("output_chart"),
        paste0(
          "Line chart displaying the data in the Regions table above.",
          "This is a focus chart, which displays the selected local authority's",
          "Region in blue and all other regions in grey."
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
Region_FocusLineChartServer <- function(id,
                                        app_inputs,
                                        bds_metrics,
                                        stat_n_geog,
                                        region_names_bds,
                                        covid_affected_data) {
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
        # Generate the covid plot data if add_covid_plot is TRUE
        covid_plot <- calculate_covid_plot(
          chart_data(),
          covid_affected_data,
          app_inputs$indicator(),
          "line"
        )

        # Build plot
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
          # Only show point data where line won't appear (NAs)
          ggplot2::geom_point(
            data = subset(
              create_show_point(chart_data(), covid_affected_data, app_inputs$indicator()),
              show_point
            ),
            ggplot2::aes(
              x = Years_num, y = values_num,
              color = `LA and Regions`,
              size = `LA and Regions`
            ),
            shape = 15,
            na.rm = TRUE
          ) +
          add_covid_elements(covid_plot) +
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
          get_indicator_dps(filtered_bds()),
          region_clean()
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


#' Region Focus Bar Chart UI Module
#'
#' Creates a user interface (UI) for displaying a bar chart focused on regions.
#' The chart is interactive and includes download options. It also provides
#' a hidden plot for copying the chart to the clipboard.
#'
#' @param id The unique module ID for this UI element. It is used to namespace
#'   input and output elements for this specific instance of the module.
#'
#' @return A `bslib::nav_panel` containing:
#'   - A bar chart with interactive features.
#'   - Download options for the chart.
#'   - A hidden static plot for copying the chart to the clipboard.
#'
#' @details This function uses `bslib::nav_panel()` to display a panel containing:
#'   - A bar chart created using `create_chart_card_ui()`, which includes a
#'     downloadable chart and interactive elements.
#'   - A `create_download_options_ui()` UI for download and copy functionalities.
#'   - A hidden static plot for copying the chart to the clipboard.
#'
#' @examples
#' \dontrun{
#' Region_FocusBarChartUI("focus_bar_chart")
#' }
Region_FocusBarChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Bar chart - Focus",
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      # Focus bar chart
      create_chart_card_ui(
        ns("output_chart"),
        paste(
          "Bar chart displaying the data in the Regions table above.",
          "This is a focus chart, which displays the selected local authority's",
          "Region in blue and all other regions in grey."
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


#' Region Focus Bar Chart Server Module
#'
#' A server-side module that generates and manages the logic for rendering a
#' region-focused bar chart. The chart is interactive, and it includes download
#' options for saving the chart as either SVG or HTML. The module handles data
#' filtering, processing, and plotting based on user inputs.
#'
#' @param id The unique module ID used to namespace input and output elements
#'   for this specific instance of the module.
#' @param app_inputs A list of inputs from the Shiny app used for filtering and
#'   plotting the data.
#' @param bds_metrics A dataset containing the relevant metrics for the chart.
#' @param stat_n_geog A dataset used for cleaning and processing region data.
#' @param region_names_bds A dataset containing the region names.
#' @param covid_affected_data A dataset that contains information about the
#'   impact of COVID-19, used for generating the COVID-related chart elements.
#'
#' @return This function does not return a value directly. It renders an interactive
#'   bar chart using `ggiraph::girafe()` and provides a download feature via
#'   `Download_DataServer`. It also provides a static plot for clipboard copying.
#'
#' @details The module performs the following steps:
#'   - Retrieves and filters the data based on selected inputs using various
#'     server-side modules (e.g., `Region_LongPlotServer`, `Clean_RegionServer`).
#'   - Prepares and processes the data to ensure the selected region appears first
#'     on the bar chart.
#'   - Uses `ggplot2` and `ggiraph` to generate an interactive bar chart, with
#'     optional COVID-related data if relevant.
#'   - Provides download options for saving the chart in different formats.
#'   - Generates a hidden static chart for copying the plot to the clipboard.
#'
#' @examples
#' \dontrun{
#' Region_FocusBarChartServer(
#'   "focus_bar_chart", app_inputs, bds_metrics, stat_n_geog, region_names_bds,
#'   covid_affected_data
#' )
#' }
Region_FocusBarChartServer <- function(id,
                                       app_inputs,
                                       bds_metrics,
                                       stat_n_geog,
                                       region_names_bds,
                                       covid_affected_data) {
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

    # Prepare the chart data, setting the selected region to appear as first bar
    chart_data <- reactive({
      region_long_plot() |>
        reorder_la_regions(region_clean())
    })

    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Generate the covid plot data if add_covid_plot is TRUE
        covid_plot <- calculate_covid_plot(
          chart_data(),
          covid_affected_data,
          app_inputs$indicator(),
          "bar"
        )

        # Build plot
        chart_data() |>
          ggplot2::ggplot() +
          ggiraph::geom_col_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              fill = `LA and Regions`,
              tooltip = tooltip_bar(
                chart_data(),
                get_indicator_dps(filtered_bds()),
                region_clean(),
                "#12436D"
              ),
              data_id = `LA and Regions`
            ),
            position = "dodge",
            width = 0.6,
            na.rm = TRUE,
            colour = "black"
          ) +
          add_covid_elements(covid_plot) +
          format_axes(chart_data()) +
          set_plot_colours(chart_data(), "focus-fill", region_clean()) +
          set_plot_labs(filtered_bds()) +
          custom_theme() +
          guides(fill = "none")
      }
    })

    interactive_chart <- reactive({
      # Now pass the full ggplot object to ggiraph::girafe
      ggiraph::girafe(
        ggobj = static_chart(),
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
    DownloadChartBtnServer("download_btn", id, "Focus Bar")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Regional-Level-Focus-Bar-Chart"))
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

    # Return the interactive plot
    output$output_chart <- ggiraph::renderGirafe({
      interactive_chart()
    })
  })
}


#' Region Multi-Chart Input UI Module
#'
#' Creates a user interface (UI) for selecting regions to compare across multiple
#' charts (line and bar). The UI includes two `selectizeInput` elements that allow
#' users to select up to three regions for comparison.
#'
#' @param id The unique module ID for this UI element. It is used to namespace
#'   input and output elements for this specific instance of the module.
#'
#' @return A `tagList` containing two `selectizeInput` elements:
#'   - A `selectizeInput` for selecting up to three regions for the line chart.
#'   - A `selectizeInput` for selecting up to three regions for the bar chart.
#'
#' @details This function creates two `selectizeInput` elements:
#'   - One for selecting regions to be compared in a line chart.
#'   - One for selecting regions to be compared in a bar chart.
#' The maximum number of selected regions is limited to three for each chart.
#'
#' @examples
#' \dontrun{
#' Region_MultiChartInputUI("multi_chart_input")
#' }
Region_MultiChartInputUI <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::selectizeInput(
      inputId = ns("chart_line_input"),
      label = "Select Regions to compare (max 3)",
      choices = NULL,
      multiple = TRUE,
      options = list(
        maxItems = 3,
        plugins = list("remove_button"),
        dropdownParent = "body",
        placeholder = "Start typing or scroll to add..."
      )
    ),
    shiny::selectizeInput(
      inputId = ns("chart_bar_input"),
      label = "Select Regions to compare (max 3)",
      choices = NULL,
      multiple = TRUE,
      options = list(
        maxItems = 3,
        plugins = list("remove_button"),
        dropdownParent = "body",
        placeholder = "Start typing or scroll to add..."
      )
    )
  )
}


#' Region Multi-Chart Input Server Module
#'
#' A server-side module that handles the selection and synchronisation of regions
#' for comparing multiple charts (line and bar). It ensures that the selected regions
#' for both charts are valid and synchronised. The module also retains the user’s
#' previous selections and updates the UI accordingly.
#'
#' @param id The unique module ID used to namespace input and output elements
#'   for this specific instance of the module.
#' @param app_inputs A list of inputs from the Shiny app used for filtering and
#'   plotting the data.
#' @param stat_n_geog A dataset used for cleaning and processing region data.
#' @param bds_metrics A dataset containing the relevant metrics for the chart.
#' @param region_names_bds A dataset containing the region names.
#' @param shared_values A reactive object used to store and share selections
#'   between the line and bar chart inputs.
#'
#' @return This function does not return a value directly. It renders the user
#'   interface elements and ensures that the inputs for the line and bar charts
#'   are synchronised, valid, and updated based on user selection.
#'
#' @details The module performs the following tasks:
#'   - Ensures that only valid region selections (those not equal to the selected
#'     local authority) are allowed for the line and bar charts.
#'   - Retains previous selections for both charts and updates the inputs with
#'     valid options when the user changes the main LA or indicator.
#'   - Synchronises the line and bar chart inputs so that they both reflect the
#'     same selected regions, with a maximum of three regions selected for each chart.
#'   - Returns the selected regions for both the line and bar charts as reactive values
#'     for use in other parts of the app.
#'
#' @examples
#' \dontrun{
#' Region_MultiChartInputServer(
#'   "multi_chart_input", app_inputs, stat_n_geog, bds_metrics, region_names_bds, shared_values
#' )
#' }
Region_MultiChartInputServer <- function(id,
                                         app_inputs,
                                         stat_n_geog,
                                         bds_metrics,
                                         region_names_bds,
                                         shared_values) {
  moduleServer(id, function(input, output, session) {
    # Helper function to retain only the valid selections that are in the available choices
    retain_valid_selections <- function(current_choices,
                                        previous_selections) {
      intersect(previous_selections, current_choices)
    }

    # Clean region names based on selected inputs
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Reactive expression to get the valid Regions (not the selected LA's Region)
    valid_selections <- reactive({
      region_names_bds |>
        setdiff(region_clean())
    })

    # Observe when the main LA input changes to update both chart inputs (line and bar)
    observeEvent(list(app_inputs$la(), app_inputs$indicator()), {
      # Get previous selections for both line and bar inputs from shared values
      prev_line_selections <- shared_values$chart_line_input
      prev_bar_selections <- shared_values$chart_bar_input

      # Retain only valid selections from the previous inputs
      valid_line_selections <- retain_valid_selections(valid_selections(), prev_line_selections)
      valid_bar_selections <- retain_valid_selections(valid_selections(), prev_bar_selections)

      # Update the line chart selectize input with valid selections
      updateSelectizeInput(
        session = session,
        inputId = "chart_line_input",
        choices = valid_selections(),
        selected = valid_line_selections
      )

      # Update the bar chart selectize input with valid selections
      updateSelectizeInput(
        session = session,
        inputId = "chart_bar_input",
        choices = valid_selections(),
        selected = valid_bar_selections
      )
    })

    # Line chart input --------------------------------------------------------
    observeEvent(input$chart_line_input,
      {
        if (!setequal(input$chart_line_input, shared_values$chart_line_input)) {
          # Update line chart shared val with user input
          shared_values$chart_line_input <- input$chart_line_input
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Keep the bar selected synchronized with shared values
    observeEvent(shared_values$chart_line_input,
      {
        later::later(function() {
          isolate({
            if (!setequal(input$chart_bar_input, shared_values$chart_line_input)) {
              updateSelectizeInput(
                session = session,
                inputId = "chart_bar_input",
                selected = if (is.null(shared_values$chart_line_input)) {
                  character(0)
                } else {
                  shared_values$chart_line_input
                }
              )
            }
          })
        }, delay = 0.5)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Bar chart input ---------------------------------------------------------
    observeEvent(input$chart_bar_input,
      {
        if (!setequal(input$chart_bar_input, shared_values$chart_bar_input)) {
          # Update bar chart shared val with user input
          shared_values$chart_bar_input <- input$chart_bar_input
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Keep the line selected synchronized with shared values
    observeEvent(shared_values$chart_bar_input,
      {
        later::later(function() {
          isolate({
            if (!setequal(input$chart_line_input, shared_values$chart_bar_input)) {
              updateSelectizeInput(
                session = session,
                inputId = "chart_line_input",
                selected = if (is.null(shared_values$chart_bar_input)) {
                  character(0)
                } else {
                  shared_values$chart_bar_input
                }
              )
            }
          })
        }, delay = 0.5)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Return the selected inputs as reactive values for use elsewhere in the app
    list(
      line_input = reactive(shared_values$chart_line_input),
      bar_input = reactive(shared_values$chart_bar_input)
    )
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
Region_MultiLineChartUI <- function(id) {
  ns <- NS(id)

  # Create a navigation panel for the multi-line chart
  bslib::nav_panel(
    title = "Line chart - User selection",
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
              Region_MultiChartInputUI(
                ns("chart_line_input") # Line chart input only
              )[[1]]
            ),
            # Chart display area
            shiny::div(
              with_gov_spinner(ggiraph::girafeOutput(ns("output_chart"))),
              role = "img",
              `aria-label` = paste(
                "Line chart displaying the data in the Regions table above.",
                "The default chart shows just data for the selected local authority's region.",
                "Users can add up to 3 other Regions and England to this chart."
              )
            )
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
Region_MultiLineChartServer <- function(id,
                                        app_inputs,
                                        bds_metrics,
                                        stat_n_geog,
                                        region_names_bds,
                                        shared_values,
                                        covid_affected_data) {
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
    chart_input <- Region_MultiChartInputServer(
      "chart_line_input",
      app_inputs,
      stat_n_geog,
      bds_metrics,
      region_names_bds,
      shared_values
    )$line_input

    # Create reactive chart data based on selected regions and user choices
    chart_data <- reactive({
      region_long_plot() |>
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% region_clean())
        ) |>
        # Reorder regions to layer lines based on selection
        reorder_la_regions(
          rev(unique(c(region_clean(), chart_input())))
        )
    })

    # Build a static multi-choice chart
    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Generate the covid plot data if add_covid_plot is TRUE
        covid_plot <- calculate_covid_plot(
          chart_data(),
          covid_affected_data,
          app_inputs$indicator(),
          "line"
        )

        # Built plot
        chart_data() |>
          ggplot2::ggplot() +
          # Add interactive lines for visual representation
          ggiraph::geom_line_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`,
              data_id = `LA and Regions`
            ),
            na.rm = TRUE,
            linewidth = 1.5
          ) +
          # Only show point data where line won't appear (NAs)
          ggplot2::geom_point(
            data = subset(
              create_show_point(chart_data(), covid_affected_data, app_inputs$indicator()),
              show_point
            ),
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`
            ),
            shape = 15,
            na.rm = TRUE,
            size = 1.5
          ) +
          add_covid_elements(covid_plot) +
          format_axes(chart_data()) +
          set_plot_colours(
            data.frame(
              `LA and Regions` = c(region_clean(), chart_input()),
              check.names = FALSE
            ),
            "colour",
            region_clean()
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
          chart_data() |>
            reorder_la_regions(unique(c(region_clean(), chart_input()))),
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


#' Region Multi Bar Chart UI Module
#'
#' Creates the user interface (UI) for displaying a multi-region bar chart. The UI
#' includes a sidebar layout with filter options, a section for rendering the bar
#' chart, and options for downloading or copying the chart. Users can select multiple
#' regions to compare in the bar chart.
#'
#' @param id The unique module ID for this UI element. It is used to namespace
#'   input and output elements for this specific instance of the module.
#'
#' @return A `bslib::nav_panel` containing the following elements:
#'   - A sidebar with filtering options for selecting regions to compare in the bar chart.
#'   - A chart output section to display the interactive bar chart.
#'   - Download and copy options for the chart.
#'
#' @details This function constructs the UI for the multi-region bar chart, which
#'   includes:
#'   - A sidebar for filtering and selecting up to three regions to be displayed on
#'     the bar chart.
#'   - An interactive chart area using `ggiraph` for interactivity (e.g., hover effects).
#'   - Download and copy options for saving or copying the chart.
#'
#' @examples
#' \dontrun{
#' Region_MultiBarChartUI("multi_bar_chart")
#' }
Region_MultiBarChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Bar chart - User selection",
    div(
      style = "display: flex;
             justify-content: space-between;
             align-items: center;
             background: white;",
      bslib::card(
        id = "region_multi_line",
        title = "Line chart - User selection",
        bslib::card_body(
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              title = "Filter options",
              position = "left",
              width = "30%",
              open = list(desktop = "open", mobile = "always-above"),
              Region_MultiChartInputUI(
                ns("chart_bar_input") # Line chart input only
              )[[2]]
            ),
            shiny::div(
              with_gov_spinner(ggiraph::girafeOutput(ns("output_chart"))),
              role = "img",
              `aria-label` = paste(
                "Bar chart displaying the data in the Regions table above.",
                "The default chart shows just data for the selected local authority's region.",
                "Users can add up to 3 other Regions and England to this chart."
              )
            )
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


#' Region Multi Bar Chart Server Module
#'
#' A server-side module that handles the rendering and interaction of the multi-region
#' bar chart. This module filters and processes the selected regions, builds the plot,
#' and enables functionality for downloading and copying the chart.
#'
#' @param id The unique module ID used to namespace input and output elements
#'   for this specific instance of the module.
#' @param app_inputs A list of inputs from the Shiny app used for filtering and
#'   processing data for the chart.
#' @param bds_metrics A dataset containing the relevant metrics for the chart.
#' @param stat_n_geog A dataset for cleaning and processing region data.
#' @param region_names_bds A dataset containing the names of regions.
#' @param shared_values A reactive object used to store and share selections
#'   between the regions for the multi-region chart.
#' @param covid_affected_data A dataset containing COVID-related data for adjusting
#'   the bar chart with COVID-related visual elements.
#'
#' @return This function does not return a value directly. It processes the input data
#'   and renders the multi-region bar chart, making it available as an interactive plot.
#'   Additionally, it supports downloading the plot in various formats and copying it
#'   to the clipboard.
#'
#' @details The module performs the following tasks:
#'   - Filters the data to include only the selected regions, ensuring the chart displays
#'     valid data for comparison.
#'   - Constructs a static bar chart using `ggplot2` and adds interactivity with `ggiraph`.
#'   - Adds a COVID-19 adjustment layer to the plot if applicable.
#'   - Provides download functionality for the chart in `SVG` and `HTML` formats.
#'   - Provides a copy-to-clipboard functionality for users to copy the plot image.
#'   - Returns the rendered interactive plot for use in the UI.
#'
#' @examples
#' \dontrun{
#' Region_MultiBarChartServer(
#'   "multi_bar_chart", app_inputs, bds_metrics, stat_n_geog, region_names_bds,
#'   shared_values, covid_affected_data
#' )
#' }
Region_MultiBarChartServer <- function(id,
                                       app_inputs,
                                       bds_metrics,
                                       stat_n_geog,
                                       region_names_bds,
                                       shared_values,
                                       covid_affected_data) {
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

    # Get user-selected choices for the chart
    chart_input <- Region_MultiChartInputServer(
      "chart_bar_input",
      app_inputs,
      stat_n_geog,
      bds_metrics,
      region_names_bds,
      shared_values
    )$bar_input

    # Regional multi-choice bar plot -------------------------------------------
    # Filter data for selections
    multi_chart_data <- reactive({
      region_long_plot() |>
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% region_clean())
        ) |>
        # Set area orders so selection is first bar
        reorder_la_regions(region_clean())
    })

    # Build static plot
    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(multi_chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Generate the covid plot data if add_covid_plot is TRUE
        covid_plot <- calculate_covid_plot(
          multi_chart_data(),
          covid_affected_data,
          app_inputs$indicator(),
          "bar"
        )

        # Build plot
        multi_chart_data() |>
          ggplot2::ggplot() +
          ggiraph::geom_col_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              fill = `LA and Regions`,
              tooltip = tooltip_bar(
                multi_chart_data(),
                get_indicator_dps(filtered_bds()),
                region_clean()
              ),
              data_id = `LA and Regions`
            ),
            position = "dodge",
            width = 0.6,
            na.rm = TRUE,
            colour = "black"
          ) +
          add_covid_elements(covid_plot) +
          format_axes(multi_chart_data()) +
          set_plot_colours(multi_chart_data(), "fill", region_clean()) +
          set_plot_labs(filtered_bds()) +
          custom_theme()
      }
    })

    interactive_chart <- reactive({
      # Now pass the full ggplot object to ggiraph::girafe
      ggiraph::girafe(
        ggobj = static_chart(),
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
    DownloadChartBtnServer("download_btn", id, "Multi Bar")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Regional-Level-Multi-Bar-Chart"))
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

    # Return the interactive plot
    output$output_chart <- ggiraph::renderGirafe({
      interactive_chart()
    })
  })
}

# nolint end
