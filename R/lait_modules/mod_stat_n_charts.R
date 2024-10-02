StatN_FocusLineChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart - Focus",
    bslib::card(
      bslib::card_body(
        ggiraph::girafeOutput(ns("output_chart"))
      ),
      full_screen = TRUE
    )
  )
}



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

    output$output_chart <- ggiraph::renderGirafe({
      # Filter SN long for LAs and SNs
      # Set selected LA to last level so appears at front of plot
      focus_line_data <- stat_n_long() |>
        dplyr::filter(`LA and Regions` %in% c(app_inputs$la(), stat_n_sns())) |>
        reorder_la_regions(app_inputs$la(), after = Inf)

      # Build plot
      focus_line_chart <- focus_line_data |>
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
        format_axes(focus_line_data) +
        set_plot_colours(focus_line_data, colour_type = "focus", focus_group = app_inputs$la()) +
        set_plot_labs(filtered_bds()) +
        ggrepel::geom_label_repel(
          data = subset(focus_line_data, Years == current_year()),
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


      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(focus_line_data),
        tooltip_vlines,
        focus_line_data,
        get_indicator_dps(filtered_bds())
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (focus_line_chart + vertical_hover),
        width_svg = 12,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })
  })
}



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
        pull_uniques("LA and Regions") # Get the unique values of LA and Regions
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

    # Observe if bar input becomes empty and clear both shared inputs if it does
    observe({
      if (is.null(input$chart_bar_input) || length(input$chart_bar_input) == 0) {
        shared_values$chart_bar_input <- NULL
        shared_values$chart_line_input <- NULL
      }
    })

    # Observe if line input becomes empty and clear both shared inputs if it does
    observe({
      if (is.null(input$chart_line_input) || length(input$chart_line_input) == 0) {
        shared_values$chart_bar_input <- NULL
        shared_values$chart_line_input <- NULL
      }
    })

    # Keep the line input synchronized with shared values
    observe({
      updateSelectizeInput(
        session = session,
        inputId = "chart_line_input",
        selected = if (is.null(shared_values$chart_line_input)) character(0) else shared_values$chart_line_input
      )
    })

    # Keep the bar input synchronized with shared values
    observe({
      updateSelectizeInput(
        session = session,
        inputId = "chart_bar_input",
        selected = if (is.null(shared_values$chart_bar_input)) character(0) else shared_values$chart_bar_input
      )
    })

    # Update shared values when the bar chart input changes
    observeEvent(input$chart_bar_input, {
      shared_values$chart_bar_input <- if (length(input$chart_bar_input) == 0) NULL else input$chart_bar_input
    })

    # Update shared values when the line chart input changes
    observeEvent(input$chart_line_input, {
      shared_values$chart_line_input <- if (length(input$chart_line_input) == 0) NULL else input$chart_line_input
    })

    # Synchronize the bar input with the line input whenever the bar input changes
    observeEvent(shared_values$chart_bar_input, {
      isolate({
        shared_values$chart_line_input <- shared_values$chart_bar_input

        # Update the line input to reflect the changes in the bar input
        updateSelectizeInput(
          session = session,
          inputId = "chart_line_input",
          selected = if (is.null(shared_values$chart_bar_input)) character(0) else shared_values$chart_bar_input
        )
      })
    })

    # Synchronize the line input with the bar input whenever the line input changes
    observeEvent(shared_values$chart_line_input, {
      isolate({
        shared_values$chart_bar_input <- shared_values$chart_line_input

        # Update the bar input to reflect the changes in the line input
        updateSelectizeInput(
          session = session,
          inputId = "chart_bar_input",
          selected = if (is.null(shared_values$chart_line_input)) character(0) else shared_values$chart_line_input
        )
      })
    })

    # Return the selected inputs as reactive values for use elsewhere in the app
    return(
      list(
        line_input = reactive(shared_values$chart_line_input),
        bar_input = reactive(shared_values$chart_bar_input)
      )
    )
  })
}












StatN_MultiLineChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart - user selection",
    bslib::card(
      id = "stat_n_multi_line",
      bslib::card_body(
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Filter options",
            position = "left",
            StatN_Chart_InputUI(
              ns("chart_line_input") # Line chart input only
            )[[1]]
          ),
          ggiraph::girafeOutput(ns("output_chart"))
        )
      ),
      full_screen = TRUE
    )
  )
}




StatN_MultiLineChartServer <- function(id,
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
      "chart_line_input",
      app_inputs$la,
      stat_n_long,
      shared_values
    )$line_input

    # Statistical Neighbour Level SN multi-choice line plot -----------------------
    output$output_chart <- ggiraph::renderGirafe({
      # Stores all valid regions in data
      valid_regions <- stat_n_long()$`LA and Regions`

      # Filter Statistical Neighbour data for these areas
      stat_n_line_chart_data <- stat_n_long() |>
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

      # Plot - selected areas
      multi_line_chart <- stat_n_line_chart_data |>
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
        format_axes(stat_n_line_chart_data) +
        manual_colour_mapping(
          c(app_inputs$la(), chart_input()),
          type = "line"
        ) +
        set_plot_labs(filtered_bds()) +
        custom_theme() +
        # Revert order of the legend so goes from right to left
        ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))


      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(stat_n_line_chart_data),
        tooltip_vlines,
        stat_n_line_chart_data,
        get_indicator_dps(filtered_bds())
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (multi_line_chart + vertical_hover),
        width_svg = 8,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })
  })
}




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
        width_svg = 8,
        options = generic_ggiraph_options(),
        fonts = list(sans = "Arial")
      )
    })
  })
}
