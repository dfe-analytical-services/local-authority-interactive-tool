# nolint start: object_name
#
# Statistical Neighbour focus plots ===========================================
#' UI for the Focus Line Chart of Statistical Neighbours
#'
#' This function creates the UI for the Focus Line Chart, which displays
#' an interactive line chart comparing the selected Local Authority (LA)
#' with its statistical neighbours over time.
#'
#' @param id A string representing the namespace for the module, used
#'   to create a unique identifier for UI elements.
#'
#' @return A UI element consisting of a `bslib::nav_panel()` containing
#'   a full-screen card with an interactive line chart output.
#'
StatN_FocusLineChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart - Focus",
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      bslib::card(
        bslib::card_body(
          ggiraph::girafeOutput(ns("output_chart"))
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
      ),
      div(
        shiny::tagAppendAttributes(
          Download_DataUI(ns("svg_download"), "Download SVG"),
          style = "max-width: none;"
        ),
        Download_DataUI(ns("html_download"), "Download HTML"),
        shiny::tagAppendAttributes(
          actionButton(
            ns("copybtn"),
            "Copy Chart to Clipboard",
            icon = icon("copy"),
            class = "gov-uk-button"
          ),
          style = "max-width: none;"
        ),
        style = "display: flex; flex-direction: column; align-self: flex-start; margin-left: 15px;"
      )
    ),
    div(
      shiny::plotOutput(ns("copy_plot")),
      style = "content-visibility: hidden;"
    )
  )
}


#' Server logic for the Focus Line Chart of Statistical Neighbours
#'
#' This function contains the server logic for generating and rendering
#' the Focus Line Chart. It filters data based on user inputs,
#' prepares the data for plotting, and renders an interactive line chart
#' using `ggiraph`.
#'
#' @param id A string representing the namespace for the module, used
#'   to create a unique identifier for server logic.
#' @param app_inputs A reactive list of application inputs needed to
#'   filter data for the line chart.
#' @param bds_metrics A reactive data frame containing metrics for the
#'   analysis.
#' @param stat_n_la A reactive data frame containing Local Authority
#'   statistical neighbours data.
#'
#' @return A `ggiraph::renderGirafe()` object that renders an
#'   interactive line chart of the selected LA and its statistical
#'   neighbours.
#'
StatN_FocusLineChartServer <- function(id,
                                       app_inputs,
                                       bds_metrics,
                                       stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get Statistical Neighbour long format
    stat_n_long <- StatN_LongServer(
      "stat_n_long",
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
    current_year <- Current_YearServer("current_year", stat_n_long)

    # Chart data
    # Filter SN long for LAs and SNs
    # Set selected LA to last level so appears at front of plot
    focus_chart_data <- reactive({
      stat_n_long() |>
        dplyr::filter(`LA and Regions` %in% c(app_inputs$la(), stat_n_sns())) |>
        reorder_la_regions(app_inputs$la(), after = Inf)
    })

    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(focus_chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Build plot
        focus_chart_data() |>
          ggplot2::ggplot() +
          ggiraph::geom_line_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`,
              size = `LA and Regions`,
              data_id = `LA and Regions`,
            ),
            na.rm = TRUE
          ) +
          format_axes(focus_chart_data()) +
          set_plot_colours(focus_chart_data(), colour_type = "focus", focus_group = app_inputs$la()) +
          set_plot_labs(filtered_bds()) +
          ggrepel::geom_label_repel(
            data = subset(focus_chart_data(), Years == current_year()),
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
            hjust = 1,
            show.legend = FALSE,
            na.rm = TRUE
          ) +
          custom_theme() +
          coord_cartesian(clip = "off") +
          theme(plot.margin = margin(5.5, 66, 5.5, 5.5)) +
          guides(color = "none", size = "none")
      }
    })

    interactive_chart <- reactive({
      if (all(is.na(focus_chart_data()$values_num))) {
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
      } else {
        # Creating vertical geoms to make vertical hover tooltip
        vertical_hover <- lapply(
          get_years(focus_chart_data()),
          tooltip_vlines,
          focus_chart_data(),
          get_indicator_dps(filtered_bds())
        )

        # Combine static chart and vertical hover into one ggplot object
        full_plot <- static_chart() + vertical_hover

        # Now pass the full ggplot object to ggiraph::girafe
        ggiraph::girafe(
          ggobj = full_plot,
          width_svg = 12,
          options = generic_ggiraph_options(
            opts_hover(
              css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
            )
          ),
          fonts = list(sans = "Arial")
        )
      }
    })

    # Set up the download handlers for the chart -------------------------------
    Download_DataServer(
      "svg_download",
      reactive("SVG"),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Stat-Neighbour-Focus-Line-Chart"))
    )

    Download_DataServer(
      "html_download",
      # Determine HTML file type based on shiny test mode
      # HTML doesn't work due to github shiny not having pandoc so need to use svg for tests
      reactive({
        # Check if shiny.testmode is enabled
        if (is.null(getOption("shiny.testmode"))) {
          "HTML" # Use HTML in normal mode
        } else {
          "SVG" # Use SVG in test mode
        }
      }),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Stat-Neighbour-Focus-Line-Chart"))
    )

    output$copy_plot <- shiny::renderPlot(
      {
        static_chart()
      },
      res = 200,
      width = 24 * 96,
      height = 12 * 96
    )

    output$output_chart <- ggiraph::renderGirafe({
      interactive_chart()
    })
  })
}


#' UI for the Focus Bar Chart of Statistical Neighbours
#'
#' This function creates the user interface (UI) for the Focus Bar Chart,
#' which displays an interactive bar chart comparing the selected Local
#' Authority (LA) with its statistical neighbours across different years.
#'
#' The chart is displayed in a `bslib::nav_panel()` with a card layout,
#' allowing for a full-screen view of the interactive chart output.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique UI elements that do not conflict
#'   with other modules in the Shiny application.
#'
#' @return A UI element consisting of a `bslib::nav_panel()` containing
#'   a full-screen card with an output for the interactive bar chart.
#'
StatN_FocusBarChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Bar chart - Focus",
    bslib::card(
      bslib::card_body(
        ggiraph::girafeOutput(ns("output_chart"))
      ),
      full_screen = TRUE
    )
  )
}


#' Server logic for the Focus Bar Chart of Statistical Neighbours
#'
#' This function contains the server logic for generating and rendering
#' the Focus Bar Chart. It processes data based on user inputs, prepares
#' the data for plotting, and renders an interactive bar chart using
#' `ggiraph`.
#'
#' The bar chart compares the selected Local Authority with its statistical
#' neighbours, displaying the values for each year in a clear and
#' interactive format.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique server logic that does not conflict
#'   with other modules in the Shiny application.
#' @param app_inputs A reactive list of application inputs used to filter
#'   data for the bar chart, including the selected Local Authority.
#' @param bds_metrics A reactive data frame containing metrics needed for
#'   analysis and visualization in the chart.
#' @param stat_n_la A reactive data frame containing Local Authority
#'   statistical neighbours data used for filtering and comparison.
#'
#' @return A `ggiraph::renderGirafe()` object that renders an
#'   interactive bar chart of the selected Local Authority and its
#'   statistical neighbours, allowing users to hover over bars for
#'   additional information.
#'
StatN_FocusBarChartServer <- function(id,
                                      app_inputs,
                                      bds_metrics,
                                      stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get Statistical Neighbour long format
    stat_n_long <- StatN_LongServer(
      "stat_n_long",
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

    # Statistical Neighbour focus bar plot ------------------------------------
    output$output_chart <- ggiraph::renderGirafe({
      focus_bar_data <- stat_n_long() |>
        dplyr::filter(`LA and Regions` %in% c(app_inputs$la(), stat_n_sns())) |>
        reorder_la_regions(app_inputs$la())

      # Check to see if any data - if not display error plot
      if (all(is.na(focus_bar_data$values_num))) {
        ggiraph::girafe(
          ggobj = display_no_data_plot(),
          width_svg = 8.5,
          options = generic_ggiraph_options(),
          fonts = list(sans = "Arial")
        )
      } else {
        stat_n_focus_bar_chart <- focus_bar_data |>
          ggplot2::ggplot() +
          ggiraph::geom_col_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              fill = `LA and Regions`,
              tooltip = glue::glue_data(
                focus_bar_data |>
                  pretty_num_table(
                    include_columns = "values_num",
                    dp = get_indicator_dps(filtered_bds())
                  ),
                "Year: {Years}\n{`LA and Regions`}: {values_num}"
              ),
              data_id = `LA and Regions`
            ),
            position = "dodge",
            width = 0.6,
            na.rm = TRUE,
            colour = "black"
          ) +
          format_axes(focus_bar_data) +
          set_plot_colours(focus_bar_data, "focus-fill", app_inputs$la()) +
          set_plot_labs(filtered_bds()) +
          custom_theme() +
          guides(fill = "none")

        # Plotting interactive graph
        ggiraph::girafe(
          ggobj = stat_n_focus_bar_chart,
          width_svg = 8.5,
          options = generic_ggiraph_options(),
          fonts = list(sans = "Arial")
        )
      }
    })
  })
}


# Statistical Neighbour multi-choice plots ====================================
#' UI for Chart Input Selection
#'
#' This function creates the user interface (UI) for selecting regions
#' to compare in the line and bar charts. It provides two
#' `selectizeInput` elements, each allowing the user to select up to
#' three regions for comparison.
#'
#' The selected regions will be used to filter the data displayed in
#' the respective charts, enabling users to focus on specific areas
#' of interest.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique UI elements that do not conflict
#'   with other modules in the Shiny application.
#'
#' @return A UI element containing a `tagList` of two `selectizeInput`
#'   elements for chart input selection, one for the line chart and one
#'   for the bar chart.
#'
StatN_Chart_InputUI <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::selectizeInput(
      inputId = ns("chart_line_input"),
      label = "Select statistical neighbour to compare (max 3)",
      choices = NULL,
      multiple = TRUE,
      options = list(
        maxItems = 3,
        plugins = list("remove_button"),
        dropdownParent = "body"
      )
    ),
    shiny::selectizeInput(
      inputId = ns("chart_bar_input"),
      label = "Select statistical neighbour to compare (max 3)",
      choices = NULL,
      multiple = TRUE,
      options = list(
        maxItems = 3,
        plugins = list("remove_button"),
        dropdownParent = "body"
      )
    )
  )
}


#' Server logic for Chart Input Selection
#'
#' This function contains the server logic for managing the selection
#' of regions to compare in both the line and bar charts. It synchronizes
#' the selected inputs and ensures that they remain valid and consistent
#' across both inputs.
#'
#' The function allows for dynamic updates of the input choices based on
#' the selected Local Authority (LA) and maintains shared values
#' across different chart inputs, ensuring the user experience is smooth
#' and intuitive.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique server logic that does not conflict
#'   with other modules in the Shiny application.
#' @param la_input A reactive expression containing the selected
#'   Local Authority input. This input is used to filter available
#'   choices for the charts.
#' @param stat_n_long A reactive data frame containing statistical
#'   neighbour data in long format, used for filtering choices.
#' @param shared_values A reactive list for storing shared input values
#'   between the line and bar chart selections, allowing for easy
#'   synchronization of selections.
#'
#' @return A list containing reactive expressions for the selected line
#'   and bar chart inputs. This allows other parts of the application to
#'   access the current selections made by the user.
#'
StatN_Chart_InputServer <- function(id, la_input, stat_n_long, shared_values) {
  moduleServer(id, function(input, output, session) {
    # Helper function to retain only the valid selections that are in the available choices
    retain_valid_selections <- function(current_choices, previous_selections) {
      intersect(previous_selections, current_choices)
    }

    # Reactive expression to get the valid areas (LAs and Regions) excluding the currently selected LA
    valid_selections <- reactive({
      stat_n_long() |>
        dplyr::filter(`LA and Regions` != la_input()) |> # Exclude the selected LA from choices
        pull_uniques("LA and Regions") |> # Get the unique values of LA and Regions
        as.character()
    })

    # Observe when the main LA input changes to update both chart inputs (line and bar)
    observeEvent(la_input(), {
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


#' UI for Multi-Line Chart
#'
#' This function creates the user interface (UI) for displaying a
#' multi-line chart based on user selections. It includes a sidebar
#' for filtering options, allowing users to select regions for
#' comparison in the line chart.
#'
#' The UI consists of a navigation panel containing a card with
#' layout elements for filtering options and the chart output area.
#' This module integrates the chart input UI and the interactive
#' output for the multi-line chart.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique UI elements that do not
#'   conflict with other modules in the Shiny application.
#'
#' @return A UI element representing the multi-line chart with
#'   filtering options for user selection.
#'
StatN_MultiLineChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart - user selection",
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      bslib::card(
        id = "stat_n_multi_line",
        bslib::card_body(
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              title = "Filter options",
              position = "left",
              width = "30%",
              open = list(desktop = "open", mobile = "always-above"),
              StatN_Chart_InputUI(
                ns("chart_line_input") # Line chart input only
              )[[1]]
            ),
            ggiraph::girafeOutput(ns("output_chart"))
          )
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
      ),
      div(
        # Download button to trigger chart download modal
        shiny::tagAppendAttributes(
          DownloadChartBtnUI(ns("download_btn")),
          style = "max-width: none; margin-left: 0;"
        ),
        br(),
        shiny::tagAppendAttributes(
          actionButton(
            ns("copybtn"),
            "Copy Chart to Clipboard",
            icon = icon("copy"),
            class = "gov-uk-button"
          ),
          style = "max-width: none;"
        ),
        style = "display: flex; flex-direction: column; align-self: flex-start; margin-left: 15px;"
      )
    ),
    div(
      shiny::plotOutput(ns("copy_plot")),
      style = "content-visibility: hidden;"
    )
  )
}


#' Server logic for Multi-Line Chart
#'
#' This function contains the server logic for generating a multi-line
#' chart based on user-selected regions. It filters the data according
#' to the selections made in the UI and renders the interactive line
#' chart using `ggiraph`.
#'
#' The function integrates filtering based on the selected local
#' authority (LA) and updates the chart output dynamically as the
#' user makes selections. It ensures that the chart reflects the
#' most current data based on the user's inputs.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique server logic that does not
#'   conflict with other modules in the Shiny application.
#' @param app_inputs A reactive list of application inputs, including
#'   the selected LA, used for filtering the chart data.
#' @param bds_metrics A reactive list of BDS metrics for filtering
#'   purposes in the data.
#' @param stat_n_la A reactive data frame containing statistical
#'   neighbour data for the selected LA.
#' @param shared_values A reactive list for storing shared input values
#'   between different chart selections, facilitating synchronization.
#'
#' @return NULL This function does not return any values; it generates
#'   the multi-line chart and updates the UI based on user selections.
#'
StatN_MultiLineChartServer <- function(id,
                                       app_inputs,
                                       bds_metrics,
                                       stat_n_la,
                                       shared_values) {
  moduleServer(id, function(input, output, session) {
    # Initialize server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Line")

    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get Statistical Neighbour long format
    stat_n_long <- StatN_LongServer(
      "stat_n_long",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Pulling specific choices available for selected LA & indicator
    chart_input <- StatN_Chart_InputServer(
      "chart_line_input",
      app_inputs$la,
      stat_n_long,
      shared_values
    )$line_input

    # Pulling specific choices available for selected LA & indicator
    chart_input <- StatN_Chart_InputServer(
      "chart_line_input",
      app_inputs$la,
      stat_n_long,
      shared_values
    )$line_input

    # Build chart data
    chart_data <- reactive({
      # Stores all valid regions in data
      valid_regions <- stat_n_long()$`LA and Regions`

      # Filter Statistical Neighbour data for these areas
      stat_n_long() |>
        # Filter for random areas - simulate user choosing up to 6 areas
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% app_inputs$la())
        ) |>
        # Set area orders so selection order starts on top of plot
        reorder_la_regions(
          rev(intersect(c(app_inputs$la(), chart_input()), valid_regions)),
          after = Inf
        )
    })

    # Statistical Neighbour Level SN multi-choice line plot -----------------------
    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Plot - selected areas
        chart_data() |>
          ggplot2::ggplot() +
          ggiraph::geom_point_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              color = `LA and Regions`,
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
          format_axes(chart_data()) +
          manual_colour_mapping(
            c(app_inputs$la(), chart_input()),
            type = "line"
          ) +
          set_plot_labs(filtered_bds()) +
          custom_theme() +
          # Revert order of the legend so goes from right to left
          ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
      }
    })

    interactive_chart <- reactive({
      if (all(is.na(chart_data()$values_num))) {
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

        # Now pass the full ggplot object to ggiraph::girafe
        ggiraph::girafe(
          ggobj = full_plot,
          width_svg = 12,
          options = generic_ggiraph_options(
            opts_hover(
              css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
            )
          ),
          fonts = list(sans = "Arial")
        )
      }
    })

    # Set up the download handlers for the chart -------------------------------
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = static_chart(), "html" = interactive_chart())),
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Regional-Level-Multi-Line-Chart"))
    )

    # Plot used for copy to clipboard
    output$copy_plot <- shiny::renderPlot(
      {
        static_chart()
      },
      res = 200,
      width = 24 * 96,
      height = 12 * 96
    )

    # Interactive plot output
    output$output_chart <- ggiraph::renderGirafe({
      interactive_chart()
    })
  })
}


#' UI for Multi-Bar Chart
#'
#' This function creates the user interface (UI) for displaying a
#' multi-bar chart based on user selections. It includes a sidebar
#' for filtering options, allowing users to select regions for
#' comparison in the bar chart.
#'
#' The UI consists of a navigation panel containing a card with
#' layout elements for filtering options and the chart output area.
#' This module integrates the chart input UI and the interactive
#' output for the multi-bar chart.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique UI elements that do not
#'   conflict with other modules in the Shiny application.
#'
#' @return A UI element representing the multi-bar chart with
#'   filtering options for user selection.
#'
StatN_MultiBarChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Bar chart - user selection",
    bslib::card(
      id = "stat_n_multi_line",
      bslib::card_body(
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Filter options",
            position = "left",
            width = "30%",
            open = list(desktop = "open", mobile = "always-above"),
            StatN_Chart_InputUI(
              ns("chart_bar_input") # Line chart input only
            )[[2]]
          ),
          ggiraph::girafeOutput(ns("output_chart"))
        )
      ),
      full_screen = TRUE
    )
  )
}


#' Server logic for Multi-Bar Chart
#'
#' This function contains the server logic for generating a multi-bar
#' chart based on user-selected regions. It filters the data according
#' to the selections made in the UI and renders the interactive bar
#' chart using `ggiraph`.
#'
#' The function integrates filtering based on the selected local
#' authority (LA) and updates the chart output dynamically as the
#' user makes selections. It ensures that the chart reflects the
#' most current data based on the user's inputs.
#'
#' @param id A string representing the namespace for the module. This
#'   identifier is used to create unique server logic that does not
#'   conflict with other modules in the Shiny application.
#' @param app_inputs A reactive list of application inputs, including
#'   the selected LA, used for filtering the chart data.
#' @param bds_metrics A reactive list of BDS metrics for filtering
#'   purposes in the data.
#' @param stat_n_la A reactive data frame containing statistical
#'   neighbour data for the selected LA.
#' @param shared_values A reactive list for storing shared input values
#'   between different chart selections, facilitating synchronization.
#'
#' @return NULL This function does not return any values; it generates
#'   the multi-bar chart and updates the UI based on user selections.
#'
StatN_MultiBarChartServer <- function(id,
                                      app_inputs,
                                      bds_metrics,
                                      stat_n_la,
                                      shared_values) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get Statistical Neighbour long format
    stat_n_long <- StatN_LongServer(
      "stat_n_long",
      app_inputs$la,
      filtered_bds,
      stat_n_la
    )

    # Pulling specific choices available for selected LA & indicator
    chart_input <- StatN_Chart_InputServer(
      "chart_bar_input",
      app_inputs$la,
      stat_n_long,
      shared_values
    )$bar_input

    # Statistical Neighbour multi-choice bar plot -------------------------------
    output$output_chart <- ggiraph::renderGirafe({
      # Stores all valid regions in data
      valid_regions <- stat_n_long()$`LA and Regions`

      stat_n_bar_multi_data <- stat_n_long() |>
        # Filter for random areas - simulate user choosing up to 6 areas
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% app_inputs$la())
        ) |>
        # Set area orders so selection order starts on top of plot
        reorder_la_regions(
          intersect(c(app_inputs$la(), chart_input()), valid_regions)
        )

      # Check to see if any data - if not display error plot
      if (all(is.na(stat_n_bar_multi_data$values_num))) {
        ggiraph::girafe(
          ggobj = display_no_data_plot(),
          width_svg = 8.5,
          options = generic_ggiraph_options(),
          fonts = list(sans = "Arial")
        )
      } else {
        stat_n_multi_bar_chart <- stat_n_bar_multi_data |>
          ggplot2::ggplot() +
          ggiraph::geom_col_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              fill = `LA and Regions`,
              tooltip = glue::glue_data(
                stat_n_bar_multi_data |>
                  pretty_num_table(include_columns = "values_num", dp = get_indicator_dps(filtered_bds())),
                "Year: {Years}\n{`LA and Regions`}: {values_num}"
              ),
              data_id = `LA and Regions`
            ),
            position = "dodge",
            width = 0.6,
            na.rm = TRUE,
            colour = "black"
          ) +
          format_axes(stat_n_bar_multi_data) +
          manual_colour_mapping(
            c(app_inputs$la(), chart_input()),
            type = "bar"
          ) +
          set_plot_labs(filtered_bds()) +
          custom_theme()

        # Plotting interactive graph
        ggiraph::girafe(
          ggobj = stat_n_multi_bar_chart,
          width_svg = 8.5,
          options = generic_ggiraph_options(),
          fonts = list(sans = "Arial")
        )
      }
    })
  })
}

# nolint end
