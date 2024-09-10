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
  h1("Regional Level"),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = "15rem", # Minimum width for each input box before wrapping
      shiny::selectInput(
        inputId = "la_input",
        label = "Change Authority:",
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
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::card(
      bslib::card_header("Regional Authorities"),
      bslib::card_body(
        reactable::reactableOutput("region_la_table"),
        reactable::reactableOutput("region_table")
      )
    )
  )
  # div(
  #   class = "well",
  #   style = "overflow-y: visible;",
  #   bslib::card(
  #     bslib::card_body(
  #       reactable::reactableOutput("la_stats_table")
  #     )
  #   )
  # ),
  # div(
  #   class = "well",
  #   style = "overflow-y: visible;",
  #   bslib::navset_card_underline(
  #     id = "la_charts",
  #     bslib::nav_panel(
  #       title = "Line chart",
  #       bslib::card(
  #         bslib::card_body(
  #           ggiraph::girafeOutput("la_line_chart")
  #         ),
  #         full_screen = TRUE
  #       ),
  #     ),
  #     bslib::nav_panel(
  #       title = "Bar chart",
  #       bslib::card(
  #         id = "la_bar_body",
  #         bslib::card_body(
  #           ggiraph::girafeOutput("la_bar_chart")
  #         ),
  #         full_screen = TRUE
  #       )
  #     )
  #   )
  # ),
  # div(
  #   class = "well",
  #   style = "overflow-y: visible;",
  #   bslib::card(
  #     bslib::card_body(
  #       h3("Description:"),
  #       textOutput("description"),
  #       h3("Methodology:"),
  #       uiOutput("methodology"),
  #       div(
  #         # Creates a flex container where the items are centered vertically
  #         style = "display: flex; align-items: baseline;",
  #         h3("Last Updated:",
  #            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
  #         ),
  #         textOutput("last_update")
  #       ),
  #       div(
  #         style = "display: flex; align-items: baseline;",
  #         h3("Next Updated:",
  #            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
  #         ),
  #         uiOutput("next_update")
  #       ),
  #       div(
  #         style = "display: flex; align-items: baseline;",
  #         h3("Source:",
  #            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
  #         ),
  #         uiOutput("source")
  #       )
  #     )
  #   )
  # )
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


  # Region LA Level table ----------------------------------
  # Filter for selected topic and indicator
  # Define filtered_bds outside of observeEvent
  filtered_bds <- reactiveValues(data = NULL)

  observeEvent(input$indicator, {
    # Region LA Level table ----------------------------------
    # Filter for selected topic and indicator
    filtered_bds$data <- bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input,
        Measure == input$indicator
      )
  })

  # Long format Region LA data
  region_la_long <- reactive({
    # Get the LA region
    region_la <- stat_n_geog |>
      dplyr::filter(`LA Name` == input$la_input) |>
      dplyr::pull(GOReg)

    # Get other LAs in the region
    region_la_la <- stat_n_geog |>
      dplyr::filter(GOReg == region_la) |>
      pull_uniques("LA Name")

    # Then filter for selected LA and regional LAs
    region_la_filtered_bds <- filtered_bds$data |>
      dplyr::filter(
        `LA and Regions` %in% c(input$la_input, region_la_la)
      )

    # Region LA levels long
    region_la_long <- region_la_filtered_bds |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
      dplyr::mutate(
        `LA and Regions` = factor(`LA and Regions`),
        Years_num = as.numeric(substr(Years, start = 1, stop = 4))
      )
  })

  # Difference between last two years
  region_la_diff <- reactive({
    region_la_long() |>
      calculate_change_from_prev_yr()
  })

  # Most recent year
  current_year <- reactive({
    region_la_long() |>
      dplyr::filter(Years_num == max(Years_num)) |>
      pull_uniques("Years")
  })

  # Build Region LA table
  region_la_table <- shiny::reactive({
    # Join difference and pivot wider to recreate LAIT table
    region_la_long() |>
      dplyr::bind_rows(region_la_diff()) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num
      ) |>
      pretty_num_table(dp = 1) |>
      dplyr::arrange(.data[[current_year()]], `LA and Regions`)
  })

  output$region_la_table <- reactable::renderReactable({
    dfe_reactable(region_la_table())
  })


  # Regional Level Regions table ----------------------------------------------
  region_table <- shiny::reactive({
    # Get national term
    region_national <- filtered_bds$data |>
      dplyr::filter(`LA and Regions` %in% national_names_bds & !is.na(values_num)) |>
      pull_uniques("LA and Regions")

    # Filter for all regions and England
    region_filtered_bds <- filtered_bds$data |>
      dplyr::filter(
        `LA and Regions` %in% c(region_names_bds, region_national)
      )

    # Region levels long
    region_long <- region_filtered_bds |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
      dplyr::mutate(
        `LA and Regions` = factor(`LA and Regions`),
        Years_num = as.numeric(substr(Years, start = 1, stop = 4))
      )

    # Difference between last two years
    region_diff <- region_long |>
      calculate_change_from_prev_yr()

    # Join difference and pivot wider to recreate Region table
    region_table <- region_long |>
      dplyr::bind_rows(region_diff) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num
      ) |>
      pretty_num_table(dp = 1) |>
      dplyr::arrange(.data[[current_year()]], `LA and Regions`) |>
      # Places England row at the bottom of the table
      dplyr::mutate(is_england = ifelse(grepl("England \\(", `LA and Regions`), 1, 0)) |>
      dplyr::arrange(is_england, .by_group = FALSE) |>
      dplyr::select(-is_england)

    region_table
  })

  output$region_table <- reactable::renderReactable({
    dfe_reactable(region_table())
  })


  # # LA Level line chart plot ----------------------------------
  # la_line_chart <- reactive({
  #   # Build plot
  #   la_line_chart <- la_long() |>
  #     ggplot2::ggplot() +
  #     ggiraph::geom_point_interactive(
  #       ggplot2::aes(
  #         x = Years_num,
  #         y = values_num,
  #         color = `LA and Regions`,
  #         shape = `LA and Regions`,
  #         data_id = `LA and Regions`
  #       ),
  #       na.rm = TRUE
  #     ) +
  #     ggiraph::geom_line_interactive(
  #       ggplot2::aes(
  #         x = Years_num,
  #         y = values_num,
  #         color = `LA and Regions`,
  #         data_id = `LA and Regions`
  #       ),
  #       na.rm = TRUE
  #     ) +
  #     format_axes(la_long()) +
  #     set_plot_colours(la_long()) +
  #     set_plot_labs(filtered_bds$data, input$indicator) +
  #     custom_theme()
  #
  #
  #   # Creating vertical geoms to make vertical hover tooltip
  #   vertical_hover <- lapply(
  #     get_years(la_long()),
  #     tooltip_vlines,
  #     la_long()
  #   )
  #
  #   # Plotting interactive graph
  #   ggiraph::girafe(
  #     ggobj = (la_line_chart + vertical_hover),
  #     width_svg = 8,
  #     options = generic_ggiraph_options(
  #       opts_hover(
  #         css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
  #       )
  #     )
  #   )
  # })
  #
  # output$la_line_chart <- ggiraph::renderGirafe({
  #   la_line_chart()
  # })
  #
  #
  # # LA Level bar plot ----------------------------------
  # la_bar_chart <- reactive({
  #   # Build plot
  #   la_bar_chart <- la_long() |>
  #     ggplot2::ggplot() +
  #     ggiraph::geom_col_interactive(
  #       ggplot2::aes(
  #         x = Years_num,
  #         y = values_num,
  #         fill = `LA and Regions`,
  #         tooltip = glue::glue_data(
  #           la_long() |>
  #             pretty_num_table(include_columns = "values_num", dp = 1),
  #           "Year: {Years}\n{`LA and Regions`}: {values_num}"
  #         ),
  #         data_id = `LA and Regions`
  #       ),
  #       position = "dodge",
  #       width = 0.6,
  #       na.rm = TRUE,
  #       colour = "black"
  #     ) +
  #     format_axes(la_long()) +
  #     set_plot_colours(la_long(), "fill") +
  #     set_plot_labs(filtered_bds$data, input$indicator) +
  #     custom_theme()
  #
  #   # Plotting interactive graph
  #   ggiraph::girafe(
  #     ggobj = la_bar_chart,
  #     width_svg = 8,
  #     options = generic_ggiraph_options()
  #   )
  # })
  #
  #
  # output$la_bar_chart <- ggiraph::renderGirafe({
  #   la_bar_chart()
  # })
  #
  #
  # # LA Metadata ----------------------------------
  #
  # # Description
  # output$description <- renderText({
  #   metrics_clean |>
  #     get_metadata(input$indicator, "Description")
  # })
  #
  # # Methodology
  # output$methodology <- renderUI({
  #   metrics_clean |>
  #     get_metadata(input$indicator, "Methodology")
  # })
  #
  # # Last updated
  # output$last_update <- renderText({
  #   metrics_clean |>
  #     get_metadata(input$indicator, "Last Update")
  # })
  #
  # # Next updated
  # output$next_update <- renderUI({
  #   metrics_clean |>
  #     get_metadata(input$indicator, "Next Update")
  # })
  #
  # # Source (hyperlink)
  # output$source <- renderUI({
  #   hyperlink <- metrics_clean |>
  #     get_metadata(input$indicator, "Hyperlink(s)")
  #   label <- input$indicator
  #   tags$a(href = hyperlink, class = "btn btn-default", label)
  # })
}

# App
shinyApp(ui_dev, server_dev)
