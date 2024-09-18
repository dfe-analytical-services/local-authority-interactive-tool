# nolint start: object_name
#
# Build data for plotting
Region_LongPlotServer <- function(id, app_inputs, bds_metrics, national_names_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Get relevant National
    region_national <- Region_NationalServer("region_national", national_names_bds, filtered_bds)

    # Long format Region LA data
    region_long <- Region_LongDataServer("region_long", filtered_bds, region_names_bds, region_national)

    # Filter region_long data for any (Ldn) regions with all NA values, and England
    reactive({
      region_long() |>
        dplyr::group_by(`LA and Regions`) |>
        dplyr::filter(
          !(grepl("^London \\(", `LA and Regions`) & dplyr::n() == sum(is.na(values_num))),
          `LA and Regions` %notin% national_names_bds
        ) |>
        dplyr::ungroup()
    })
  })
}



Chart_InputUI <- function(id) {
  ns <- NS(id)

  div(
    shiny::selectizeInput(
      inputId = ns("chart_input"),
      label = "Select region to compare (max 3)",
      choices = region_names_bds,
      multiple = TRUE,
      options = list(maxItems = 3)
    )
  )
}


# Restrict the user input choices to only eligible Regions
# Not England, Ldn () where missing, and default Region
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



# Region chart modules ========================================================
# Region Focus line chart UI ==================================================
Region_FocusLine_chartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart - Focus",
    bslib::card(
      bslib::card_body(
        ggiraph::girafeOutput(ns("region_focus_line_chart"))
      ),
      full_screen = TRUE
    )
  )
}

# Region Focus line chart Server ----------------------------------------------
Region_FocusLine_chartServer <- function(id, app_inputs, bds_metrics, stat_n_geog, national_names_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Get Region plotting data
    region_long_plot <- Region_LongPlotServer(
      "region_long_plot",
      app_inputs,
      bds_metrics,
      national_names_bds,
      region_names_bds
    )

    # Get clean Region names
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Current year
    current_year <- Current_YearServer("current_year", region_long_plot)

    # Build focus line plot
    region_focus_line_chart <- reactive({
      # Set selected region to last level so appears at front of plot
      region_focus_line_data <- region_long_plot() |>
        reorder_la_regions(region_clean(), after = Inf)

      # Built focus plot
      region_line_chart <- region_focus_line_data |>
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
        format_axes(region_focus_line_data) +
        set_plot_colours(region_focus_line_data,
          colour_type = "focus",
          focus_group = region_clean()
        ) +
        set_plot_labs(filtered_bds()) +
        ggrepel::geom_label_repel(
          data = subset(region_focus_line_data, Years == current_year()),
          aes(
            x = Years_num,
            y = values_num,
            label = `LA and Regions`
          ),
          color = "black",
          segment.colour = NA,
          label.size = NA,
          nudge_x = 2,
          direction = "y",
          vjust = .5,
          hjust = 1,
          show.legend = FALSE
        ) +
        custom_theme() +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(5.5, 66, 5.5, 5.5)) +
        guides(colour = "none", size = "none")

      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(region_focus_line_data),
        tooltip_vlines,
        region_focus_line_data,
        get_indicator_dps(filtered_bds())
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (region_line_chart + vertical_hover),
        width_svg = 12,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })

    # Chart output
    output$region_focus_line_chart <- ggiraph::renderGirafe({
      region_focus_line_chart()
    })
  })
}



# Region multi-choice line chart UI ===========================================
Region_Multi_chartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart - user selection",
    bslib::card(
      id = "region_multi_line",
      bslib::card_body(
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Filter options",
            position = "left",
            Chart_InputUI(ns("chart_input"))
          ),
          ggiraph::girafeOutput(ns("region_multi_line_chart"))
        )
      ),
      full_screen = TRUE
    )
  )
}

# Region multi-choice line chart Server ---------------------------------------
Region_Multi_chartServer <- function(id, app_inputs, bds_metrics, stat_n_geog, national_names_bds, region_names_bds) {
  moduleServer(id, function(input, output, session) {
    # Get Region plotting data
    region_long_plot <- Region_LongPlotServer(
      "region_long_plot",
      app_inputs,
      bds_metrics,
      national_names_bds,
      region_names_bds
    )

    # Get clean Region names
    region_clean <- Clean_RegionServer(
      "region_clean",
      app_inputs,
      stat_n_geog,
      bds_metrics
    )

    # Filter for selected topic and indicator
    filtered_bds <- BDS_FilteredServer("filtered_bds", app_inputs, bds_metrics)

    # Current year
    current_year <- Current_YearServer("current_year", region_long_plot)

    # Pulling specific choices available for selected app & organisational level
    chart_input <- Chart_InputServer(
      "chart_input",
      app_inputs,
      region_long_plot,
      region_clean
    )

    # Built multi-choice plot
    region_multi_line_chart <- reactive({
      # Filtering plotting data for selected LA region and others user choices
      region_multi_choice_data <- region_long_plot() |>
        dplyr::filter(
          (`LA and Regions` %in% chart_input()) |
            (`LA and Regions` %in% region_clean())
        ) |>
        # Reordering so lines are layered by selection choice, ensuring no duplicates
        reorder_la_regions(
          rev(c(region_clean(), chart_input()))
        )

      # Reactive expression to handle plot building
      region_multi_line <- region_multi_choice_data |>
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
        format_axes(region_multi_choice_data) +
        manual_colour_mapping(
          unique(c(region_clean(), chart_input())),
          type = "line"
        ) +
        set_plot_labs(filtered_bds()) +
        custom_theme() +
        # Revert order of the legend so goes from right to left
        ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))

      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(region_multi_choice_data),
        tooltip_vlines,
        region_multi_choice_data,
        get_indicator_dps(filtered_bds())
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = region_multi_line + vertical_hover,
        width_svg = 8,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    })

    # Render the reactive plot output separately
    output$region_multi_line_chart <- ggiraph::renderGirafe({
      region_multi_line_chart()
    })
  })
}

# nolint end
