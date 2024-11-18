# nolint start: object_name
#
# Create Own Charts ============================================================
# Create Own Chart Data --------------------------------------------------------
#
#' Create Own Chart Data Server
#'
#' This function manages the server logic for creating the chart data
#' based on saved selections from the output table. It computes the number
#' of indicators and geographies and prepares the data in a suitable format
#' for plotting.
#'
#' @param id A unique identifier for the Shiny module.
#' @param create_own_table A reactive function that returns the data
#'                          from the saved selections.
#' @param query A reactive object containing saved queries and their data.
#' @return A list containing the number of selected indicators, the number
#'         of selected geographies, and the prepared data for chart plotting.
#'
CreateOwnChartDataServer <- function(id, create_own_table, query) {
  moduleServer(id, function(input, output, session) {
    # Compute number of indicators & geographies - used to determine whether data
    # is displayed or computed
    # Allowing a max of 3 and 4 respectively
    number_of_indicators <- reactive({
      length(pull_uniques(create_own_table(), "Measure"))
    })

    number_of_geogs <- reactive({
      length(pull_uniques(create_own_table(), "LA and Regions"))
    })

    # Create chart data --------------------------------------------------------
    chart_plotting_data <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_table()),
        number_of_indicators() <= 3,
        number_of_geogs() <= 4
      )

      # Pull order that geogs and indicators are added
      # This is used to set the levels of the factor (so display in order in chart)
      geog_chart_order <- query$data |>
        get_query_table_values(`LA and Regions`)

      indicator_chart_order <- query$data |>
        get_query_table_values(Indicator)

      # Coerce final output table to long data (for plotting)
      # Recreate Years_num & values_num, also factor `LA and Regions` & Measure
      create_own_table() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("20"),
          names_to = "Years",
          values_to = "Values"
        ) |>
        dplyr::mutate(
          Years_num = as.numeric(substr(Years, start = 1, stop = 4)),
          values_num = Values,
          `LA and Regions` = factor(`LA and Regions`, levels = geog_chart_order),
          Measure = factor(Measure, levels = indicator_chart_order)
        ) |>
        # Replace NAs caused by combining datasets with actual value
        dplyr::group_by(`LA and Regions`, Measure, Years_num) |>
        tidyr::fill(c("Values", "values_num"), .direction = "downup", ) |>
        dplyr::ungroup() |>
        # Remove duplicates
        dplyr::distinct(`LA and Regions`, Topic, Measure, Years_num, values_num, .keep_all = TRUE)
    })

    # Output number of selected indicators & geogs (for selection error messages)
    # and data in format for plotting
    chart_info <- list(
      no_indicators = number_of_indicators,
      no_geogs = number_of_geogs,
      data = chart_plotting_data
    )

    # Return information for plotting
    chart_info
  })
}


# Create Own Line Chart UI -----------------------------------------------------
#
#' Create Own Line Chart UI
#'
#' This function generates the user interface for the "Create Own Line Chart"
#' feature within a Shiny application. It includes a section for displaying
#' the line chart, download options, and a button to copy the chart to the
#' clipboard.
#'
#' @param id A unique identifier for the Shiny module.
#' @return A UI component that contains the line chart and associated controls.
#'
CreateOwnLineChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Line chart",
    # Line chart plot with download buttons
    div(
      style = "display: flex;
               justify-content: space-between;
               align-items: center;
               background: white;",
      # Line chart
      bslib::card(
        bslib::card_body(
          ggiraph::girafeOutput(ns("line_chart"))
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
      ),
      # Download options
      div(
        # Download button to trigger chart download modal
        shiny::tagAppendAttributes(
          DownloadChartBtnUI(ns("download_btn")),
          style = "max-width: none; margin-left: 0;  align-self: auto;"
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
        style = "display: flex; flex-direction: column; align-self: flex-start; margin: 15px;"
      )
    ),
    # Hidden static plot for copy-to-clipboard
    div(
      shiny::plotOutput(ns("copy_plot")),
      style = "content-visibility: hidden;"
    )
  )
}

# Create Own Line Chart Server -------------------------------------------------
#
#' Create Own Line Chart Server
#'
#' This function handles the server logic for generating a line chart based
#' on saved selections from the output table. It manages data preparation,
#' rendering of both static and interactive line charts, and
#' download functionality.
#'
#' @param id A unique identifier for the Shiny module.
#' @param query A reactive object containing saved queries and their data.
#' @param bds_metrics A data frame containing metrics for the BDS.
#' @return None; this function is used to create and manage reactive elements
#'         within the Shiny application.
#'
CreateOwnLineChartServer <- function(id, query, bds_metrics, covid_affected_indicators) {
  moduleServer(id, function(input, output, session) {
    # Load Create Own Table data
    create_own_data <- CreateOwnDataServer(
      "create_own_table",
      query,
      bds_metrics
    )

    # Load Create Own BDS
    create_own_bds <- CreateOwnBDSServer(
      "create_own_bds",
      create_own_data,
      bds_metrics
    )

    # Get Create Own Chart data
    chart_info <- CreateOwnChartDataServer(
      "chart_info",
      create_own_data,
      query
    )

    # Make chart ---------------------------------------------------------------
    # Build static main plot
    line_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_data()),
        chart_info$no_indicators() <= 3,
        chart_info$no_geogs() <= 4
      )
      # Check if measure affected by COVID
      covid_affected <- create_own_bds() |>
        pull_uniques("Measure") %in% covid_affected_indicators

      # Generate the covid plot data if add_covid_plot is TRUE
      covid_plot <- calculate_covid_plot(chart_info$data(), covid_affected, "line")

      # Plot data - colour represents Geographies & linetype represents Indicator
      chart_info$data() |>
        ggplot2::ggplot() +
        ggiraph::geom_line_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            color = `LA and Regions`,
            linetype = Measure,
            data_id = `LA and Regions`
          ),
          na.rm = TRUE,
          linewidth = 1
        ) +
        # Only show point data where line won't appear (NAs)
        ggplot2::geom_point(
          data = subset(
            create_show_point(chart_info$data(), covid_affected),
            show_point
          ),
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            color = `LA and Regions`
          ),
          shape = 15,
          na.rm = TRUE
        ) +
        add_covid_elements(covid_plot) +
        format_axes(chart_info$data()) +
        set_plot_colours(chart_info$data()) +
        set_plot_labs(create_own_bds()) +
        custom_theme(title_margin = chart_info$no_indicators() - 1) +
        # Setting legend title at top
        ggplot2::theme(
          legend.title = ggplot2::element_text(),
          legend.title.position = "top",
          legend.spacing.x = unit(5, "lines")
        ) +
        # Creating nice looking legend content
        ggplot2::guides(
          color = ggplot2::guide_legend(
            order = 1,
            ncol = 1,
            title = "Geographies (colour):",
            override.aes = list(size = 3, shape = 15, linetype = NULL)
          ),
          linetype = ggplot2::guide_legend(
            order = 2,
            ncol = 1,
            title = "Indicators (line-type):"
          )
        )
    })

    # Build interactive line chart
    interactive_line_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_data()),
        chart_info$no_indicators() <= 3,
        chart_info$no_geogs() <= 4
      )
      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(chart_info$data()),
        tooltip_vlines,
        chart_info$data(),
        get_indicator_dps(create_own_bds()),
        include_measure = TRUE
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

    # Line chart plot output ---------------------------------------------------
    output$line_chart <- ggiraph::renderGirafe({
      # Error messages for incorrect/ missing selections
      if ("Message from tool" %in% colnames(create_own_data())) {
        ggiraph::girafe(
          ggobj = display_no_data_plot("No plot as not enough selections made"),
          width_svg = 8.5
        )
      } else if (
        chart_info$no_geogs() > 4
      ) {
        ggiraph::girafe(
          ggobj = display_no_data_plot(label = "No plot as too many Geographies selected"),
          width_svg = 8.5
        )
      } else if (
        chart_info$no_indicators() > 3
      ) {
        ggiraph::girafe(
          ggobj = display_no_data_plot(label = "No plot as too many Indicators selected"),
          width_svg = 8.5
        )

        # Plot line chart
      } else {
        interactive_line_chart()
      }
    })

    # Line chart download ------------------------------------------------------
    # Initialise server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Line")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = line_chart(), "html" = interactive_line_chart())),
      reactive(c("LAIT-create-your-own-line-chart"))
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
  })
}


# Create Own Bar Chart UI ------------------------------------------------------
#
#' Create Own Bar Chart UI
#'
#' This function generates the user interface for the "Create Own Bar Chart"
#' feature within a Shiny application. It includes a section for displaying
#' the bar chart, download options, and a button to copy the chart to the
#' clipboard.
#'
#' @param id A unique identifier for the Shiny module.
#' @return A UI component that contains the bar chart and associated controls.
#'
CreateOwnBarChartUI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Bar chart",
    # Line chart plot with download buttons
    div(
      style = "display: flex;
                   justify-content: space-between;
                   align-items: center;
                   background: white;",
      # Bar chart
      bslib::card(
        bslib::card_body(
          ggiraph::girafeOutput(ns("bar_chart"))
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
      ),
      # Download options
      div(
        # Download button to trigger chart download modal
        shiny::tagAppendAttributes(
          DownloadChartBtnUI(ns("download_btn")),
          style = "max-width: none; margin-left: 0; align-self: auto;"
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
        style = "display: flex; flex-direction: column; align-self: flex-start; margin: 15px;"
      )
    ),
    # Hidden static plot for copy-to-clipboard
    div(
      shiny::plotOutput(ns("copy_plot")),
      style = "content-visibility: hidden;"
    )
  )
}

# Create Own Bar Chart Server --------------------------------------------------
#
#' Create Own Bar Chart Server
#'
#' This function handles the server logic for generating a bar chart based
#' on saved selections from the output table. It manages data preparation,
#' rendering of both static and interactive bar charts, and
#' download functionality.
#'
#' @param id A unique identifier for the Shiny module.
#' @param query A reactive object containing saved queries and their data.
#' @param bds_metrics A data frame containing metrics for the BDS.
#' @return None; this function is used to create and manage reactive elements
#'         within the Shiny application.
#'
CreateOwnBarChartServer <- function(id, query, bds_metrics, covid_affected_indicators) {
  moduleServer(id, function(input, output, session) {
    # Load Create Own Table data
    create_own_data <- CreateOwnDataServer(
      "create_own_table",
      query,
      bds_metrics
    )

    # Load Create Own BDS
    create_own_bds <- CreateOwnBDSServer(
      "create_own_bds",
      create_own_data,
      bds_metrics
    )

    # Get Create Own Chart data
    chart_info <- CreateOwnChartDataServer(
      "chart_info",
      create_own_data,
      query
    )

    # Make chart ---------------------------------------------------------------
    # Build main bar static plot
    bar_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_data()),
        chart_info$no_indicators() <= 3,
        chart_info$no_geogs() <= 4
      )

      # Giving facet_wrap charts the correct chart names
      # Get chart names for each indicator
      chart_names <- create_own_bds() |>
        dplyr::distinct(Measure, Chart_title)

      # Wrap the chart names (dependent on number of indicators -
      # more narrows width available)
      chart_names_wrapped <- chart_names |>
        dplyr::mutate(Chart_title = stringr::str_wrap(
          Chart_title,
          width = 60 - length(chart_names$Measure) * 10
        ))

      # Create a named vector for custom titles for each indicator
      custom_titles <- setNames(
        chart_names_wrapped$Chart_title,
        chart_names_wrapped$Measure
      )

      # Set x axis limits if one bar so its not super wide
      n_chart_rows <- nrow(chart_info$data()) == 1
      thin_bar_xlim <- if (n_chart_rows) {
        thin_bar(chart_info$data(), Years_num)
      } else {
        NULL
      }
      # Check if measure affected by COVID
      covid_affected <- create_own_bds() |>
        pull_uniques("Measure") %in% covid_affected_indicators

      # Generate the covid plot data if add_covid_plot is TRUE
      covid_plot <- calculate_covid_plot(chart_info$data(), covid_affected, "bar")

      # Plot chart - split by indicators, colours represent Geographies
      chart_info$data() |>
        ggplot2::ggplot() +
        ggiraph::geom_col_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            fill = `LA and Regions`,
            tooltip = tooltip_bar(
              chart_info$data(),
              get_indicator_dps(create_own_bds()),
              include_measure = TRUE
            )
          ),
          position = "dodge",
          width = ifelse(n_chart_rows, 0.1, 0.6),
          na.rm = TRUE,
          color = "black"
        ) +
        {
          if (!is.null(covid_plot)) {
            ggplot2::geom_text(
              data = covid_plot,
              ggplot2::aes(
                x = label_x,
                y = Inf,
                label = "Some indicators have\nmissing data due to COVID"
              ),
              vjust = 0.5,
              color = "black",
              size = 3,
              fontface = "italic",
              inherit.aes = FALSE
            )
          }
        } +
        format_axes(chart_info$data()) +
        set_plot_colours(chart_info$data(), "fill") +
        set_plot_labs(create_own_bds()) +
        custom_theme() +
        ggplot2::theme(
          legend.title.position = "top",
          # Set heigh & size of mini chart titles
          strip.text = ggplot2::element_text(
            size = 11,
            margin = ggplot2::margin(b = 30)
          ),
          # Gives space between the charts so x-axis labels don't overlap
          plot.margin = ggplot2::margin(t = 10, r = 30, b = 10, l = 10),
          panel.spacing.x = unit(15, "mm")
        ) +
        guides(
          fill = ggplot2::guide_legend(ncol = 2, title = "Geographies:")
        ) +
        ggplot2::labs(title = "Bar charts showing selected indicators") +
        # Split chart by indicator
        ggplot2::facet_wrap(
          ~Measure,
          labeller = labeller(Measure = as_labeller(custom_titles)),
        ) +
        # Setting x limits for one value bar charts (to keep narrow)
        ggplot2::coord_cartesian(
          xlim = thin_bar_xlim,
          clip = "off"
        )
    })

    # Build interactive line chart
    interactive_bar_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_data()),
        chart_info$no_indicators() <= 3,
        chart_info$no_geogs() <= 4
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (bar_chart()),
        width_svg = 8.5,
        options = generic_ggiraph_options(),
        fonts = list(sans = "Arial")
      )
    })

    # Bar chart plot output ----------------------------------------------------
    output$bar_chart <- ggiraph::renderGirafe({
      # Error messages for missing or too many selections
      if ("Message from tool" %in% colnames(create_own_data())) {
        ggiraph::girafe(
          ggobj = display_no_data_plot("No plot as not enough selections made"),
          width_svg = 8.5
        )
      } else if (
        chart_info$no_geogs() > 4
      ) {
        ggiraph::girafe(
          ggobj = display_no_data_plot(label = "No plot as too many Geographies selected"),
          width_svg = 8.5
        )
      } else if (
        chart_info$no_indicators() > 3
      ) {
        ggiraph::girafe(
          ggobj = display_no_data_plot(label = "No plot as too many Indicators selected"),
          width_svg = 8.5
        )

        # Plot chart
      } else {
        interactive_bar_chart()
      }
    })

    # Bar chart download -------------------------------------------------------
    # Initialise server logic for download button and modal
    DownloadChartBtnServer("download_btn", id, "Bar")

    # Set up the download handlers for the chart
    Download_DataServer(
      "chart_download",
      reactive(input$file_type),
      reactive(list("svg" = bar_chart(), "html" = interactive_bar_chart())),
      reactive(c("LAIT-create-your-own-bar-chart"))
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
  })
}

# nolint end
