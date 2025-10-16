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
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      # Focus line chart
      create_chart_card_ui(
        ns("output_chart"),
        paste(
          "Line chart displaying the data in the Statistical Neighbours table above.",
          "This is a focus chart, which displays the selected local authority",
          "in blue and all other statistical neighbour local authorities in grey."
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
                                       stat_n_la,
                                       covid_affected_data) {
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
        reorder_la_regions(app_inputs$la(), after = Inf) |>
        # Creating options for graph labels
        dplyr::mutate(
          label_color = ifelse(`LA and Regions` == app_inputs$la(), get_focus_front_colour(), get_gov_secondary_text_colour()),
          label_fontface = ifelse(`LA and Regions` == app_inputs$la(), "bold", "plain")
        )
    })

    static_chart <- reactive({
      year_count <- dplyr::n_distinct(focus_chart_data()$Years_num)
      # Check to see if any data - if not display error plot
      if (all(is.na(focus_chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Generate the covid plot data if add_covid_plot is TRUE
        covid_plot <- calculate_covid_plot(
          focus_chart_data(),
          covid_affected_data,
          app_inputs$indicator(),
          "line"
        )
        if (year_count == 1) {
          focus_chart_data() |>
            ggplot() +
            ggplot2::geom_point(
              ggplot2::aes(
                x = Years_num,
                y = values_num,
                color = `LA and Regions`,
              ),
              size = 3,
              shape = 21,
              stroke = 1,
              fill = "white"
            ) +
            ggplot2::labs(subtitle = "Only one year of data available") +
            format_axes(focus_chart_data()) +
            set_plot_labs(filtered_bds())
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
            # Only show point data where line won't appear (NAs)
            ggplot2::geom_point(
              data = subset(
                create_show_point(focus_chart_data(), covid_affected_data, app_inputs$indicator()),
                show_point
              ),
              ggplot2::aes(
                x = Years_num,
                y = values_num,
                color = `LA and Regions`,
                size = `LA and Regions`
              ),
              shape = 15,
              na.rm = TRUE
            ) +
            add_covid_elements(covid_plot) +
            format_axes(focus_chart_data()) +
            set_plot_colours(focus_chart_data(), colour_type = "focus", focus_group = app_inputs$la()) +
            set_plot_labs(filtered_bds()) +
            ggrepel::geom_label_repel(
              data = subset(focus_chart_data(), Years == current_year()),
              aes(
                x = Years_num,
                y = values_num,
                label = `LA and Regions`,
                fontface = label_fontface
              ),
              colour = subset(focus_chart_data(), Years == current_year())$label_color,
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
            theme(plot.margin = margin(5.5, 66, 5.5, 5.5)) +
            guides(color = "none", size = "none")
        }
      }
    })

    interactive_chart <- reactive({
      output_chart <- if (all(is.na(focus_chart_data()$values_num))) {
        static_chart()
      } else {
        # Creating vertical geoms to make vertical hover tooltip
        vertical_hover <- lapply(
          get_years(focus_chart_data()),
          tooltip_vlines,
          focus_chart_data(),
          get_indicator_dps(filtered_bds()),
          app_inputs$la()
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
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Stat-Neighbour-Focus-Line-Chart"))
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
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      # Focus line chart
      create_chart_card_ui(
        ns("output_chart"),
        paste(
          "Bar chart displaying the data in the Statistical Neighbours table above.",
          "This is a focus chart, which displays the selected local authority",
          "in blue and all other statistical neighbour local authorities in grey."
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
#'   analysis and visualisation in the chart.
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
                                      stat_n_la,
                                      covid_affected_data) {
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

    # Statistical Neighbour focus bar plot -------------------------------------
    # Filter SN long for LAs and SNs
    # Set selected LA to last level so appears at front of plot
    focus_chart_data <- reactive({
      stat_n_long() |>
        dplyr::filter(`LA and Regions` %in% c(app_inputs$la(), stat_n_sns())) |>
        reorder_la_regions(app_inputs$la())
    })

    static_chart <- reactive({
      # Check to see if any data - if not display error plot
      if (all(is.na(focus_chart_data()$values_num))) {
        display_no_data_plot()
      } else {
        # Generate the covid plot data if add_covid_plot is TRUE
        covid_plot <- calculate_covid_plot(
          focus_chart_data(),
          covid_affected_data,
          app_inputs$indicator(),
          "bar"
        )

        # Build plot
        focus_chart_data() |>
          ggplot2::ggplot() +
          ggiraph::geom_col_interactive(
            ggplot2::aes(
              x = Years_num,
              y = values_num,
              fill = `LA and Regions`,
              tooltip = tooltip_bar(
                focus_chart_data(),
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
          add_covid_elements(covid_plot) +
          format_axes(focus_chart_data()) +
          set_plot_colours(focus_chart_data(), "focus-fill", app_inputs$la()) +
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
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Stat-Neighbour-Focus-Bar-Chart"))
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
        dropdownParent = "body",
        placeholder = "Start typing or scroll to add..."
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
        dropdownParent = "body",
        placeholder = "Start typing or scroll to add..."
      )
    )
  )
}


#' Server logic for Chart Input Selection
#'
#' This function contains the server logic for managing the selection
#' of regions to compare in both the line and bar charts. It synchronises
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
StatN_Chart_InputServer <- function(id, app_inputs, stat_n_long, shared_values) {
  moduleServer(id, function(input, output, session) {
    # Helper function to retain only the valid selections that are in the available choices
    retain_valid_selections <- function(current_choices, previous_selections) {
      intersect(previous_selections, current_choices)
    }

    # Reactive expression to get the valid areas (LAs and Regions) excluding the currently selected LA
    valid_selections <- reactive({
      stat_n_long() |>
        dplyr::filter(`LA and Regions` != app_inputs$la()) |> # Exclude the selected LA from choices
        pull_uniques("LA and Regions") |> # Get the unique values of LA and Regions
        as.character()
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
    title = "Line chart - User selection",
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
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
            shiny::div(
              with_gov_spinner(ggiraph::girafeOutput(ns("output_chart"))),
              role = "img",
              `aria-label` = paste(
                "Line chart displaying the data in the Statistical Neighbour table above.",
                "The default chart shows just data for the selected local authority.",
                "Users can add up to 3 other statistical neighbour local",
                "authorities, the selected local authority's region or England to this chart."
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
#'   between different chart selections, facilitating synchronisation.
#'
#' @return NULL This function does not return any values; it generates
#'   the multi-line chart and updates the UI based on user selections.
#'
StatN_MultiLineChartServer <- function(id,
                                       app_inputs,
                                       bds_metrics,
                                       stat_n_la,
                                       shared_values,
                                       covid_affected_data) {
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
      "chart_line_input",
      app_inputs,
      stat_n_long,
      shared_values
    )$line_input

    # Build chart data
    chart_data <- reactive({
      # Stores all valid regions in data
      valid_regions <- stat_n_long()$`LA and Regions`

      # Filter Statistical Neighbour data for these areas
      stat_n_long() |>
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
      year_count <- dplyr::n_distinct(chart_data()$Years_num)
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
        if (year_count == 1) {
          chart_data() |>
            ggplot() +
            ggplot2::geom_point(
              ggplot2::aes(
                x = Years_num,
                y = values_num,
                color = `LA and Regions`,
              ),
              size = 3,
              shape = 21,
              stroke = 1,
              fill = "white"
            ) +
            ggplot2::labs(subtitle = "Only one year of data available") +
            format_axes(chart_data()) +
            set_plot_labs(filtered_bds())
        } else {
          # Plot - selected areas
          chart_data() |>
            ggplot2::ggplot() +
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
              ggplot2::aes(x = Years_num, y = values_num, color = `LA and Regions`),
              shape = 15,
              na.rm = TRUE,
              size = 1.5
            ) +
            add_covid_elements(covid_plot) +
            format_axes(chart_data()) +
            set_plot_colours(
              data.frame(
                `LA and Regions` = c(app_inputs$la(), chart_input()),
                check.names = FALSE
              ),
              "colour",
              app_inputs$la()
            ) +
            set_plot_labs(filtered_bds()) +
            custom_theme() +
            # Revert order of the legend so goes from right to left
            ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
        }
      }
    })

    interactive_chart <- reactive({
      output_chart <- if (all(is.na(chart_data()$values_num))) {
        static_chart()
      } else {
        # Creating vertical geoms to make vertical hover tooltip
        vertical_hover <- lapply(
          get_years(chart_data()),
          tooltip_vlines,
          chart_data() |>
            reorder_la_regions(
              intersect(c(app_inputs$la(), chart_input()), stat_n_long()$`LA and Regions`)
            ),
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
    title = "Bar chart - User selection",
    div(
      style = "display: flex;
             justify-content: space-between;
             align-items: center;
             background: white;",
      bslib::card(
        id = "stat_n_multi_line",
        title = "Line chart - User selection",
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
            shiny::div(
              with_gov_spinner(ggiraph::girafeOutput(ns("output_chart"))),
              role = "img",
              `aria-label` = paste(
                "Bar chart displaying the data in the Statistical Neighbour table above.",
                "The default chart shows just data for the selected local authority.",
                "Users can add up to 3 other statistical neighbour local",
                "authorities, the selected local authority's region or England to this chart."
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
#'   between different chart selections, facilitating synchronisation.
#'
#' @return NULL This function does not return any values; it generates
#'   the multi-bar chart and updates the UI based on user selections.
#'
StatN_MultiBarChartServer <- function(id,
                                      app_inputs,
                                      bds_metrics,
                                      stat_n_la,
                                      shared_values,
                                      covid_affected_data) {
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
      app_inputs,
      stat_n_long,
      shared_values
    )$bar_input

    # Statistical Neighbour multi-choice bar plot ------------------------------
    multi_chart_data <- reactive({
      # Stores all valid regions in data
      valid_regions <- stat_n_long()$`LA and Regions`

      # Filter for user added selections and currently selected LA
      stat_n_long() |>
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% app_inputs$la())
        ) |>
        # Set area orders so selection order starts on top of plot
        reorder_la_regions(
          intersect(c(app_inputs$la(), chart_input()), valid_regions)
        )
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
                get_indicator_dps(filtered_bds())
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
          set_plot_colours(multi_chart_data(), "fill", app_inputs$la()) +
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
      reactive(c(app_inputs$la(), app_inputs$indicator(), "Stat-Neighbour-Multi-Bar-Chart"))
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
