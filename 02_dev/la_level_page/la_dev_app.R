# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


# UI
ui_dev <- bslib::page_fillable(

  ## Custom CSS =============================================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),

  # Tab header ==============================================================
  h1("Local Authority View"),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = "15rem", # Minimum width for each input box before wrapping
      shiny::selectInput(
        inputId = "la_input",
        label = "LA:",
        choices = la_names_bds
      ),
      shiny::selectInput(
        inputId = "topic_input",
        label = "Topic:",
        choices = metric_topics
      ),
      shiny::selectInput(
        inputId = "indicator",
        label = NULL,
        choices = metric_names
      )
    ),
    # Conditional State-funded school banner
    shiny::uiOutput("state_funded_banner")
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Local Authority, Region and England"),
      bslib::card_body(
        shinycssloaders::withSpinner(
          reactable::reactableOutput("la_table"),
          type = 6,
          color = "#1d70b8"
        )
      )
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_body(
        shinycssloaders::withSpinner(
          reactable::reactableOutput("la_stats_table"),
          type = 6,
          color = "#1d70b8",
          size = 0.5,
          proxy.height = "100px"
        )
      )
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "la_charts",
      bslib::nav_panel(
        title = "Line chart",
        bslib::card(
          bslib::card_body(
            shinycssloaders::withSpinner(
              ggiraph::girafeOutput("la_line_chart"),
              type = 6,
              color = "#1d70b8"
            )
          ),
          full_screen = TRUE
        ),
      ),
      bslib::nav_panel(
        title = "Bar chart",
        bslib::card(
          id = "la_bar_body",
          bslib::card_body(
            shinycssloaders::withSpinner(
              ggiraph::girafeOutput("la_bar_chart"),
              type = 6,
              color = "#1d70b8"
            )
          ),
          full_screen = TRUE
        )
      )
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_body(
        h3("Description:"),
        shinycssloaders::withSpinner(
          textOutput("description"),
          type = 6,
          color = "#1d70b8"
        ),
        h3("Methodology:"),
        shinycssloaders::withSpinner(
          uiOutput("methodology"),
          type = 6,
          color = "#1d70b8"
        ),
        div(
          # Creates a flex container where the items are centered vertically
          style = "display: flex; align-items: baseline;",
          h3("Last Updated:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          shinycssloaders::withSpinner(
            textOutput("last_update"),
            type = 6,
            color = "#1d70b8"
          )
        ),
        div(
          style = "display: flex; align-items: baseline;",
          h3("Next Updated:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          shinycssloaders::withSpinner(
            uiOutput("next_update"),
            type = 6,
            color = "#1d70b8"
          )
        ),
        div(
          style = "display: flex; align-items: baseline;",
          h3("Source:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          shinycssloaders::withSpinner(
            uiOutput("source"),
            type = 6,
            color = "#1d70b8"
          )
        )
      )
    )
  )
)


# Server
server_dev <- function(input, output, session) {
  # Input ----------------------------------
  # Using the server to power to the provider dropdown for increased speed
  shiny::observeEvent(input$topic_input, {
    # Get indicator choices for selected topic
    filtered_topic_bds <- bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input
      ) |>
      pull_uniques("Measure")

    updateSelectInput(
      session = session,
      inputId = "indicator",
      label = "Indicator:",
      choices = filtered_topic_bds
    )
  })


  # Main LA Level table ----------------------------------
  # Filter for selected topic and indicator
  # Define filtered_bds outside of observeEvent
  filtered_bds <- reactiveValues(data = NULL)

  observeEvent(input$indicator, {
    # Main LA Level table ----------------------------------
    # Filter for selected topic and indicator
    filtered_bds$data <- bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input,
        Measure == input$indicator
      )
  })

  # Get decimal places for indicator selected
  indicator_dps <- reactive({
    filtered_bds$data |>
      get_indicator_dps()
  })

  # Long format LA data
  la_long <- reactive({
    # Filter stat neighbour for selected LA
    filtered_sn <- stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input)

    # Statistical Neighbours
    la_sns <- filtered_sn |>
      pull_uniques("LA Name_sn")

    # LA region
    la_region <- filtered_sn |>
      pull_uniques("GOReg")

    # Determine London region to use
    la_region_ldn_clean <- clean_ldn_region(
      la_region,
      filtered_bds$data
    )

    # Then filter for selected LA, region, stat neighbours and relevant national
    la_filtered_bds <- filtered_bds$data |>
      dplyr::filter(
        `LA and Regions` %in% c(input$la_input, la_region_ldn_clean, la_sns, "England")
      )

    # SN average
    sn_avg <- la_filtered_bds |>
      dplyr::filter(`LA and Regions` %in% la_sns) |>
      dplyr::summarise(
        values_num = dplyr::na_if(mean(values_num, na.rm = TRUE), NaN),
        .by = c("Years", "Years_num")
      ) |>
      dplyr::mutate(
        "LA Number" = "-",
        "LA and Regions" = "Statistical Neighbours",
        .before = "Years"
      )

    # LA levels long
    la_filtered_bds |>
      dplyr::filter(`LA and Regions` %notin% c(la_sns)) |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num) |>
      dplyr::bind_rows(sn_avg) |>
      dplyr::mutate(
        `LA and Regions` = factor(
          `LA and Regions`,
          levels = c(
            input$la_input, la_region_ldn_clean,
            "Statistical Neighbours", "England"
          )
        )
      )
  })

  # Difference between last two years
  la_diff <- reactive({
    la_long() |>
      dplyr::group_by(`LA and Regions`) |>
      dplyr::arrange(`LA and Regions`, desc(Years)) |>
      dplyr::mutate(
        values_num = dplyr::lag(values_num) - values_num,
        Years = "Change from previous year"
      ) |>
      dplyr::filter(dplyr::row_number() == 2)
  })

  # Build Main LA Level table
  la_table <- shiny::reactive({
    # Join difference and pivot wider to recreate LAIT table
    la_long() |>
      dplyr::bind_rows(la_diff()) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num
      ) |>
      dplyr::arrange(`LA and Regions`)
  })


  # Stet funded school banner (appears for certain indicators)
  output$state_funded_banner <- renderUI({
    # Get whether state-funded idnicator
    state_funded <- filtered_bds$data |>
      pull_uniques("state_funded_flag") |>
      (\(x) !is.na(x))()

    # Render banner if state-funded
    if (state_funded) {
      tagList(
        br(),
        shinyGovstyle::noti_banner(
          inputId = "notId",
          title_txt = "Note",
          body_txt = "Data includes only State-funded Schools."
        )
      )
    }
  })

  output$la_table <- reactable::renderReactable({
    dfe_reactable(
      la_table(),
      columns = utils::modifyList(
        format_num_reactable_cols(
          la_table(),
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        set_custom_default_col_widths()
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, la_table(), input$la_input)
      }
    )
  })


  # Stats LA Level table ----------------------------------
  la_stats_table <- shiny::reactive({
    # Extract change from prev year (from LA table)
    la_change_prev <- la_diff() |>
      filter_la_regions(input$la_input, pull_col = "values_num")

    # Set the trend value
    la_trend <- as.numeric(la_change_prev)

    # Get polarity of indicator
    la_indicator_polarity <- filtered_bds$data |>
      pull_uniques("Polarity")

    # Get latest rank, ties are set to min & NA vals to NA rank
    la_rank <- filtered_bds$data |>
      filter_la_regions(la_names_bds, latest = TRUE) |>
      calculate_rank(la_indicator_polarity) |>
      filter_la_regions(input$la_input, pull_col = "rank")

    # Calculate quartile bands for indicator
    la_quartile_bands <- filtered_bds$data |>
      filter_la_regions(la_names_bds, latest = TRUE, pull_col = "values_num") |>
      quantile(na.rm = TRUE)

    # Extracting LA latest value
    la_indicator_val <- filtered_bds$data |>
      filter_la_regions(input$la_input, latest = TRUE, pull_col = "values_num")

    # Calculating which quartile this value sits in
    la_quartile <- calculate_quartile_band(
      la_indicator_val,
      la_quartile_bands,
      la_indicator_polarity
    )

    # Build stats LA Level table
    la_stats_table <- build_la_stats_table(
      la_diff(),
      input$la_input,
      la_trend,
      la_change_prev,
      la_rank,
      la_quartile,
      la_quartile_bands,
      get_indicator_dps(filtered_bds$data),
      la_indicator_polarity
    )

    la_stats_table
  })

  output$la_stats_table <- reactable::renderReactable({
    dfe_reactable(
      la_stats_table(),
      columns = modifyList(
        # Create the reactable with specific column alignments
        format_num_reactable_cols(
          la_stats_table(),
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number",
          categorical = c(
            "Trend", "Quartile Banding", "Latest National Rank",
            "A", "B",
            "C", "D"
          )
        ),
        # Style Quartile Banding column with colour
        list(
          set_custom_default_col_widths(),
          Trend = reactable::colDef(
            header = add_tooltip_to_reactcol(
              "Trend",
              "Based on change from previous year"
            ),
            cell = trend_icon_renderer,
            style = function(value) {
              get_trend_colour(value, la_stats_table()$Polarity[1])
            }
          ),
          `Quartile Banding` = reactable::colDef(
            style = function(value, index) {
              quartile_banding_col_def(la_stats_table()[index, ])
            }
          ),
          `Latest National Rank` = reactable::colDef(
            header = add_tooltip_to_reactcol(
              "Latest National Rank",
              paste0(
                "Rank 1 corresponds to the best value based on the ",
                "indicator's direction."
              )
            )
          ),
          Polarity = reactable::colDef(show = FALSE)
        )
      )
    )
  })


  # LA Level line chart plot ----------------------------------
  la_line_chart <- reactive({
    # Check if measure affected by COVID
    covid_affected <- input$indicator %in% covid_affected_indicators

    # Generate the covid plot data if add_covid_plot is TRUE
    covid_plot <- calculate_covid_plot(la_long(), covid_affected, "line")

    # Build plot
    la_line_chart <- la_long() |>
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
        data = subset(create_show_point(la_long(), covid_affected), show_point),
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          color = `LA and Regions`
        ),
        shape = 15,
        size = 1,
        na.rm = TRUE
      ) +
      # Add COVID plot if indicator affected
      add_covid_elements(covid_plot) +
      format_axes(la_long()) +
      set_plot_colours(la_long(), focus_group = input$la_input) +
      set_plot_labs(filtered_bds$data) +
      custom_theme() +
      # Revert order of the legend so goes from right to left
      ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))

    # Creating vertical geoms to make vertical hover tooltip
    vertical_hover <- lapply(
      get_years(la_long()),
      tooltip_vlines,
      la_long(),
      indicator_dps(),
      input$la_input
    )

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = (la_line_chart + vertical_hover),
      width_svg = 8.5,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      ),
      fonts = list(sans = "Arial")
    )
  })

  output$la_line_chart <- ggiraph::renderGirafe({
    la_line_chart()
  })


  # LA Level bar plot ----------------------------------
  la_bar_chart <- reactive({
    # Check if measure affected by COVID
    covid_affected <- input$indicator %in% covid_affected_indicators

    # Generate the covid plot data if add_covid_plot is TRUE
    covid_plot <- calculate_covid_plot(la_long(), covid_affected, "bar")

    # Build plot
    la_bar_chart <- la_long() |>
      ggplot2::ggplot() +
      ggiraph::geom_col_interactive(
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          fill = `LA and Regions`,
          tooltip = tooltip_bar(la_long(), indicator_dps(), input$la_input),
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
      set_plot_colours(la_long(), "fill", input$la_input) +
      set_plot_labs(filtered_bds$data) +
      custom_theme()

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = la_bar_chart,
      width_svg = 8.5,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      ),
      fonts = list(sans = "Arial")
    )
  })


  output$la_bar_chart <- ggiraph::renderGirafe({
    la_bar_chart()
  })


  # LA Metadata ----------------------------------

  # Description
  output$description <- renderText({
    metrics_clean |>
      get_metadata(input$indicator, "Description")
  })

  # Methodology
  output$methodology <- renderUI({
    metrics_clean |>
      get_metadata(input$indicator, "Methodology")
  })

  # Last updated
  output$last_update <- renderText({
    metrics_clean |>
      get_metadata(input$indicator, "Last Update")
  })

  # Next updated
  output$next_update <- renderUI({
    metrics_clean |>
      get_metadata(input$indicator, "Next Update")
  })

  # Source (hyperlink)
  output$source <- renderUI({
    hyperlink <- metrics_clean |>
      get_metadata(input$indicator, "Hyperlink(s)")
    label <- input$indicator
    dfeshiny::external_link(
      href = hyperlink,
      link_text = label
    )
  })
}

# App
shinyApp(ui_dev, server_dev)
