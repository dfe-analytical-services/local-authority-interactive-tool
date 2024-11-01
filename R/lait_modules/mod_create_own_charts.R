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

    # Create chart data ----------------------------------------------------------
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
        dplyr::distinct() |>
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
        )
    })

    chart_info <- list(
      no_indicators = number_of_indicators,
      no_geogs = number_of_geogs,
      data = chart_plotting_data
    )

    chart_info
  })
}



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
      bslib::card(
        bslib::card_body(
          ggiraph::girafeOutput(ns("line_chart"))
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
      ),
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

CreateOwnLineChartServer <- function(id, query, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    create_own_table <- CreateOwnDataServer(
      "create_own_table",
      query,
      bds_metrics
    )

    create_own_bds <- CreateOwnBDSServer(
      "create_own_bds",
      create_own_table,
      bds_metrics
    )

    chart_info <- CreateOwnChartDataServer(
      "chart_info",
      create_own_table,
      query
    )

    # Line chart -----------------------------------------------------------------
    # Build static main plot
    line_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_table()),
        chart_info$no_indicators() <= 3,
        chart_info$no_geogs() <= 4
      )

      # Count year cols - used to determine if to show geom_point
      # (If only one year then no line will show so point needed)
      num_year_cols <- chart_info$data() |>
        dplyr::distinct(Years) |>
        nrow()

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
          na.rm = TRUE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            color = `LA and Regions`
          ),
          na.rm = TRUE,
          size = ifelse(num_year_cols == 1, 3, 0),
          shape = 16
        ) +
        format_axes(chart_info$data()) +
        set_plot_colours(chart_info$data()) +
        set_plot_labs(create_own_bds()) +
        custom_theme() +
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
        "Message from tool" %notin% colnames(create_own_table()),
        chart_info$no_indicators() <= 3,
        chart_info$no_geogs() <= 4
      )
      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(chart_info$data()),
        tooltip_vlines,
        chart_info$data(),
        get_indicator_dps(create_own_bds()),
        TRUE
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

    # Line chart plot output -----------------------------------------------------
    output$line_chart <- ggiraph::renderGirafe({
      # Error messages for missing selections
      if ("Message from tool" %in% colnames(create_own_table())) {
        ggiraph::girafe(
          ggobj = display_no_data_plot("No plot as not enough selections made"),
          width_svg = 8.5
        )
        # Error messages for too many selections
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

    # Line chart download --------------------------------------------------------
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



CreateOwnBarChartUI <- function(id) {
  ns <- NS(id)

  # Bar chart --------------------------------------------------------------
  bslib::nav_panel(
    title = "Bar chart",
    # Line chart plot with download buttons
    div(
      style = "display: flex;
                   justify-content: space-between;
                   align-items: center;
                   background: white;",
      bslib::card(
        bslib::card_body(
          ggiraph::girafeOutput(ns("bar_chart"))
        ),
        full_screen = TRUE,
        style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
      ),
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

CreateOwnBarChartServer <- function(id, query, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    create_own_table <- CreateOwnDataServer(
      "create_own_table",
      query,
      bds_metrics
    )

    create_own_bds <- CreateOwnBDSServer(
      "create_own_bds",
      create_own_table,
      bds_metrics
    )

    chart_info <- CreateOwnChartDataServer(
      "chart_info",
      create_own_table,
      query
    )

    # Bar chart -----------------------------------------------------------------
    # Build main bar static plot
    bar_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_table()),
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




      # Plot chart - split by indicators, colours represent Geographies
      chart_info$data() |>
        ggplot2::ggplot() +
        ggiraph::geom_col_interactive(
          ggplot2::aes(
            x = Years_num,
            y = values_num,
            fill = `LA and Regions`,
            tooltip = glue::glue_data(
              chart_info$data() |>
                pretty_num_table(
                  include_columns = "values_num",
                  dp = get_indicator_dps(create_own_bds())
                ),
              "Measure: {Measure}\nYear: {Years}\n\n{`LA and Regions`}: {values_num}"
            )
          ),
          position = position_dodge(width = 0.6),
          width = 0.6,
          na.rm = TRUE,
          color = "black"
        ) +
        format_axes(chart_info$data()) +
        set_plot_colours(chart_info$data(), "fill") +
        set_plot_labs(create_own_bds()) +
        custom_theme() +
        ggplot2::theme(
          legend.title = ggplot2::element_text(),
          legend.title.position = "top"
        ) +
        guides(
          fill = ggplot2::guide_legend(ncol = 2, title = "Geographies:")
        ) +
        ggplot2::labs(title = "Bar charts showing selected indicators") +
        ggplot2::facet_wrap(
          ~Measure,
          labeller = labeller(Measure = as_labeller(custom_titles)),
        ) +
        # Gives space between the charts so x-axis labels don't overlap
        theme(
          panel.spacing.x = unit(15, "mm"),
          plot.margin = ggplot2::margin(r = 30)
        )
    })

    # Build interactive line chart
    interactive_bar_chart <- reactive({
      req(
        "Message from tool" %notin% colnames(create_own_table()),
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

    # Bar chart plot output ------------------------------------------------------
    output$bar_chart <- ggiraph::renderGirafe({
      # Error messages for missing or too many selections
      if ("Message from tool" %in% colnames(create_own_table())) {
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

    # Bar chart download ---------------------------------------------------------
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
