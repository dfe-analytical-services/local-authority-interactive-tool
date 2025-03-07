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
      shiny::selectizeInput(
        inputId = "la_input",
        label = "LA:",
        choices = la_names_bds
      ),
      shiny::selectizeInput(
        inputId = "topic_input",
        label = "Topic:",
        choices = c("All topics", metric_topics),
        multiple = TRUE,
        options = list(
          maxItems = 1,
          placeholder = "No topic selected, showing all indicators.",
          plugins = list("clear_button"),
          dropdownParent = "body"
        )
      ),
      shiny::selectizeInput(
        inputId = "indicator",
        label = "Indicator:",
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
        div(
          reactable::reactableOutput("region_la_table")
        ),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          reactable::reactableOutput("region_table")
        ),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          reactable::reactableOutput("region_stats_table")
        )
      )
    )
  ),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::navset_card_underline(
      id = "region_charts",
      bslib::nav_panel(
        title = "Focus line chart",
        bslib::card(
          bslib::card_body(
            ggiraph::girafeOutput("region_focus_line_chart")
          ),
          full_screen = TRUE
        ),
      ),
      bslib::nav_panel(
        title = "Line chart - User selection",
        bslib::card(
          id = "region_multi_line",
          bslib::card_body(
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                title = "Filter options",
                position = "left",
                width = "30%",
                open = list(desktop = "open", mobile = "always-above"),
                shiny::selectizeInput(
                  inputId = "chart_line_input",
                  label = "Select region to compare (max 3)",
                  choices = region_names_bds,
                  multiple = TRUE,
                  options = list(
                    maxItems = 3,
                    plugins = list("remove_button"),
                    dropdownParent = "body"
                  )
                )
              ),
              ggiraph::girafeOutput("region_multi_line_chart")
            )
          ),
          full_screen = TRUE
        )
      ),
      bslib::nav_panel(
        title = "Focus bar chart",
        bslib::card(
          bslib::card_body(
            ggiraph::girafeOutput("region_focus_bar_chart")
          ),
          full_screen = TRUE
        ),
      ),
      bslib::nav_panel(
        title = "Bar chart - User selection",
        bslib::card(
          id = "region_multi_bar",
          bslib::card_body(
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                title = "Filter options",
                position = "left",
                width = "30%",
                open = list(desktop = "open", mobile = "always-above"),
                shiny::selectizeInput(
                  inputId = "chart_bar_input",
                  label = "Select region to compare (max 3)",
                  choices = region_names_bds,
                  multiple = TRUE,
                  options = list(maxItems = 3)
                )
              ),
              ggiraph::girafeOutput("region_multi_bar_chart")
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
        textOutput("description"),
        h3("Methodology:"),
        uiOutput("methodology"),
        div(
          # Creates a flex container where the items are centered vertically
          style = "display: flex; align-items: baseline;",
          h3("Last Updated:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          textOutput("last_update")
        ),
        div(
          style = "display: flex; align-items: baseline;",
          h3("Next Updated:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          uiOutput("next_update")
        ),
        div(
          style = "display: flex; align-items: baseline;",
          h3("Source:",
            style = "margin-right: 1rem; margin-bottom: 0.3rem;"
          ),
          uiOutput("source")
        )
      )
    )
  )
)

# Server
server_dev <- function(input, output, session) {
  # Input ----------------------------------
  # Using the server to power to the provider dropdown for increased speed
  shiny::observeEvent(input$topic_input,
    {
      # Save the currently selected indicator
      current_indicator <- input$indicator

      # Get indicator choices for selected topic
      filtered_topic_bds <- bds_metrics |>
        dplyr::filter(
          # If topic_input is not NULL or "All topics", filter by selected topics
          # Include all rows if no topic is selected or "All topics" is selected
          if (is.null(input$topic_input) | "All topics" %in% input$topic_input) {
            TRUE
          } else {
            .data$Topic %in% input$topic_input
          }
        ) |>
        pull_uniques("Measure")

      # Ensure the current indicator stays selected if it's in the new list of available indicators
      # Default to the first available indicator if the current one is no longer valid
      selected_indicator <- if (current_indicator %in% filtered_topic_bds) {
        current_indicator
      } else {
        filtered_topic_bds[1]
      }

      shiny::updateSelectizeInput(
        session = session,
        inputId = "indicator",
        label = "Indicator:",
        choices = filtered_topic_bds,
        selected = selected_indicator
      )
    },
    ignoreNULL = FALSE
  )

  # Region LA Level table ----------------------------------
  # Define filtered_bds outside of observeEvent
  filtered_bds <- reactiveValues(data = NULL)

  observeEvent(input$indicator, {
    # Don't change the currently selected indicator if no indicator is selected
    if (is.null(input$indicator) || input$indicator == "") {
      return()
    }

    # Region LA Level table ----------------------------------
    # Filter for selected indicator
    filtered_bds$data <- bds_metrics |>
      dplyr::filter(
        Measure == input$indicator
      )
  })

  # Get decimal places for indicator selected
  indicator_dps <- reactive({
    filtered_bds$data |>
      get_indicator_dps()
  })

  # Get the LA region
  region_la <- reactive({
    stat_n_geog |>
      dplyr::filter(`LA Name` == input$la_input) |>
      dplyr::pull(GOReg)
  })

  # Determines which London to use
  # Some indicators are not provided at (Inner)/ (Outer) level
  region_la_ldn_clean <- reactive({
    clean_ldn_region(region_la(), filtered_bds$data)
  })

  # Long format Region LA data
  region_la_long <- reactive({
    # Get other LAs in the region
    region_la_la <- stat_n_geog |>
      dplyr::filter(GOReg == region_la()) |>
      pull_uniques("LA Name")

    # Then filter for selected LA and regional LAs
    region_la_filtered_bds <- filtered_bds$data |>
      dplyr::filter(
        `LA and Regions` %in% c(input$la_input, region_la_la)
      )

    # Region LA levels long
    region_la_long <- region_la_filtered_bds |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num) |>
      dplyr::mutate(
        `LA and Regions` = factor(`LA and Regions`)
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
      dplyr::arrange(.data[[current_year()]], `LA and Regions`)
  })

  output$region_la_table <- reactable::renderReactable({
    dfe_reactable(
      region_la_table(),
      columns = utils::modifyList(
        format_num_reactable_cols(
          region_la_table(),
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        set_custom_default_col_widths()
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, region_la_table(), input$la_input)
      }
    )
  })

  # Region long format data ---------------------------------------------------
  # Get long data format for Regions
  region_long <- reactive({
    # Filter for all regions and England
    region_filtered_bds <- filtered_bds$data |>
      dplyr::filter(
        `LA and Regions` %in% c(region_names_bds, "England")
      )

    # Region levels long
    region_long <- region_filtered_bds |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num) |>
      dplyr::mutate(
        `LA and Regions` = factor(`LA and Regions`)
      )

    region_long
  })

  # Regional Level Regions table ----------------------------------------------
  region_table <- shiny::reactive({
    # Difference between last two years
    region_diff <- region_long() |>
      calculate_change_from_prev_yr()

    # Join difference and pivot wider to recreate Region table
    region_table <- region_long() |>
      dplyr::bind_rows(region_diff) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num
      ) |>
      dplyr::arrange(.data[[current_year()]], `LA and Regions`) |>
      # Places England row at the bottom of the table
      dplyr::mutate(is_england = ifelse(grepl("^England", `LA and Regions`), 1, 0)) |>
      dplyr::arrange(is_england, .by_group = FALSE) |>
      dplyr::select(-is_england)

    region_table
  })

  output$region_table <- reactable::renderReactable({
    dfe_reactable(
      region_table(),
      columns = utils::modifyList(
        format_num_reactable_cols(
          region_table(),
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        set_custom_default_col_widths()
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, region_table(), region_la_ldn_clean())
      },
      pagination = FALSE
    )
  })


  # Regional Level Stats table --------------------------------------------------
  region_stats_table <- reactive({
    # Get LA numbers
    # Selected LA
    region_la_la_num <- region_la_table() |>
      filter_la_regions(input$la_input, pull_col = "LA Number")

    # Region and England
    region_la_num <- region_table() |>
      filter_la_regions(c(region_la_ldn_clean(), "England"), pull_col = "LA Number")

    # Get change in previous year
    # Difference between last two years
    region_diff <- region_long() |>
      calculate_change_from_prev_yr()

    # Selected LA
    region_la_change_prev <- region_la_diff() |>
      filter_la_regions(
        input$la_input,
        pull_col = "values_num"
      )

    # Region and England
    region_change_prev <- region_diff |>
      filter_la_regions(
        c(region_la_ldn_clean(), "England"),
        pull_col = "values_num"
      )

    # Creating the stats table cols
    region_stats_la_num <- c(region_la_la_num, region_la_num)
    region_stats_name <- c(input$la_input, region_la_ldn_clean(), "England")
    region_stats_change <- c(region_la_change_prev, region_change_prev)

    # Creating the trend descriptions
    region_trend <- as.numeric(region_stats_change)

    # Build stats table
    build_region_stats_table(
      region_stats_la_num,
      region_stats_name,
      region_trend,
      region_stats_change,
      filtered_bds$data
    )
  })

  output$region_stats_table <- reactable::renderReactable({
    dfe_reactable(
      region_stats_table(),
      columns = modifyList(
        # Create the reactable with specific column alignments
        format_num_reactable_cols(
          region_stats_table(),
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        # Define specific formatting for the Trend and Quartile Banding columns
        list(
          set_custom_default_col_widths(),
          Trend = reactable::colDef(
            cell = trend_icon_renderer,
            style = function(value) {
              get_trend_colour(value, region_stats_table()$Polarity[1])
            }
          ),
          Polarity = reactable::colDef(show = FALSE)
        )
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, region_stats_table(), input$la_input)
      }
    )
  })

  # Region charts -------------------------------------------------------------

  # Build data for plotting
  # Filter region_long data for any (Ldn) regions with all NA values, and England
  region_long_plot <- reactive({
    region_long() |>
      dplyr::group_by(`LA and Regions`) |>
      # Remove any London () regions that are all NA
      dplyr::filter(
        !(grepl("^London \\(", `LA and Regions`) & dplyr::n() == sum(is.na(values_num))),
        `LA and Regions` %notin% "England"
      )
  })

  # Restrict the user input choices to only elgible Regions
  # Not England, Ldn () where missing, and default Region
  shiny::observeEvent(input$la_input, {
    # Get indicator choices for selected topic
    multi_chart_data <- region_long_plot() |>
      dplyr::filter(
        `LA and Regions` != region_la_ldn_clean()
      ) |>
      pull_uniques("LA and Regions")

    updateSelectInput(
      session = session,
      inputId = "chart_line_input",
      choices = multi_chart_data
    )

    updateSelectInput(
      session = session,
      inputId = "chart_bar_input",
      choices = multi_chart_data
    )
  })

  # Region Focus line chart plot ----------------------------------------------
  region_focus_line_chart <- reactive({
    # Set selected region to last level so appears at front of plot
    region_focus_line_data <- region_long_plot() |>
      dplyr::ungroup() |>
      reorder_la_regions(region_la_ldn_clean(), after = Inf) |>
      # Creating options for graph labels
      dplyr::mutate(
        label_color = ifelse(`LA and Regions` == region_la_ldn_clean(),
          get_focus_front_colour(),
          get_gov_secondary_text_colour()
        ),
        label_fontface = ifelse(`LA and Regions` == region_la_ldn_clean(), "bold", "plain")
      )

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
        focus_group = region_la_ldn_clean()
      ) +
      set_plot_labs(filtered_bds$data) +
      ggrepel::geom_label_repel(
        data = subset(region_focus_line_data, Years == current_year()),
        aes(
          x = Years_num,
          y = values_num,
          label = `LA and Regions`,
          fontface = label_fontface
        ),
        colour = subset(region_focus_line_data, Years == current_year())$label_color,
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

    # Creating vertical geoms to make vertical hover tooltip
    vertical_hover <- lapply(
      get_years(region_focus_line_data),
      tooltip_vlines,
      region_focus_line_data,
      indicator_dps(),
      region_la_ldn_clean()
    )

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = (region_line_chart + vertical_hover),
      width_svg = 12,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      )
    )
  })

  output$region_focus_line_chart <- ggiraph::renderGirafe({
    region_focus_line_chart()
  })

  # Region multi-choice line chart plot ---------------------------------------
  region_multi_line_chart <- reactive({
    # Filtering plotting data for selected LA region and others user choices
    region_multi_choice_data <- region_long_plot() |>
      dplyr::ungroup() |>
      dplyr::filter(
        (`LA and Regions` %in% input$chart_line_input) |
          (`LA and Regions` %in% region_la_ldn_clean())
      ) |>
      # Reordering so lines are layered by selection choice
      reorder_la_regions(
        rev(c(region_la_ldn_clean(), input$chart_line_input))
      )

    # Build plot based on user choice of regions
    region_multi_line_chart <- region_multi_choice_data |>
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
      set_plot_colours(
        data.frame(
          `LA and Regions` = c(region_la_ldn_clean(), input$chart_line_input),
          check.names = FALSE
        ),
        "colour",
        region_la_ldn_clean()
      ) +
      set_plot_labs(filtered_bds$data) +
      custom_theme() +
      # Revert order of the legend so goes from right to left
      ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))


    # Creating vertical geoms to make vertical hover tooltip
    vertical_hover <- lapply(
      get_years(region_multi_choice_data),
      tooltip_vlines,
      region_multi_choice_data |>
        reorder_la_regions(c(region_la_ldn_clean(), input$chart_line_input)),
      indicator_dps()
    )

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = (region_multi_line_chart + vertical_hover),
      width_svg = 8.5,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      )
    )
  })

  output$region_multi_line_chart <- ggiraph::renderGirafe({
    region_multi_line_chart()
  })


  # Region focus bar plot -----------------------------------------------------
  region_focus_bar_chart <- reactive({
    # Reorder so that focus bar is first
    region_focus_bar_data <- region_long_plot() |>
      reorder_la_regions(region_la_ldn_clean())

    focus_bar_chart <- region_focus_bar_data |>
      ggplot2::ggplot() +
      ggiraph::geom_col_interactive(
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          fill = `LA and Regions`,
          tooltip = tooltip_bar(
            region_focus_bar_data,
            indicator_dps(),
            region_la_ldn_clean()
          ),
          data_id = `LA and Regions`
        ),
        position = "dodge",
        width = 0.6,
        na.rm = TRUE,
        colour = "black"
      ) +
      format_axes(region_focus_bar_data) +
      set_plot_colours(region_focus_bar_data, "focus-fill", region_la_ldn_clean()) +
      set_plot_labs(filtered_bds$data) +
      custom_theme() +
      guides(fill = "none")

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = focus_bar_chart,
      width_svg = 8.5,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      ),
      fonts = list(sans = "Arial")
    )
  })

  output$region_focus_bar_chart <- ggiraph::renderGirafe({
    region_focus_bar_chart()
  })

  # Region multi-choice bar plot ----------------------------------------------
  region_multi_bar_chart <- reactive({
    # Filtering plotting data for selected LA region and others user choices
    region_multi_choice_data <- region_long_plot() |>
      dplyr::filter(
        (`LA and Regions` %in% input$chart_bar_input) |
          (`LA and Regions` %in% region_la_ldn_clean())
      ) |>
      # Reordering so bars are ordered by selection choice
      reorder_la_regions(c(region_la_ldn_clean(), input$chart_bar_input))

    multi_bar_chart <- region_multi_choice_data |>
      ggplot2::ggplot() +
      ggiraph::geom_col_interactive(
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          fill = `LA and Regions`,
          tooltip = tooltip_bar(
            region_multi_choice_data,
            indicator_dps()
          ),
          data_id = `LA and Regions`
        ),
        position = "dodge",
        width = 0.6,
        na.rm = TRUE,
        colour = "black"
      ) +
      format_axes(region_multi_choice_data) +
      set_plot_colours(region_multi_choice_data, "fill", region_la_ldn_clean()) +
      set_plot_labs(filtered_bds$data) +
      custom_theme()

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = multi_bar_chart,
      width_svg = 8.5,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      ),
      fonts = list(sans = "Arial")
    )
  })

  output$region_multi_bar_chart <- ggiraph::renderGirafe({
    region_multi_bar_chart()
  })


  # LA Metadata ---------------------------------------------------------------
  # Reactive values to store previous data
  previous_metadata <- reactiveValues(
    description = NULL,
    methodology = NULL,
    last_update = NULL,
    next_update = NULL,
    source = NULL
  )

  # Outputs using the helper function
  output$description <- renderText({
    update_and_fetch_metadata(
      input$indicator,
      "Description",
      previous_metadata,
      "description"
    )
  })

  output$methodology <- renderUI({
    update_and_fetch_metadata(
      input$indicator,
      "Methodology",
      previous_metadata,
      "methodology"
    )
  })

  output$last_update <- renderText({
    update_and_fetch_metadata(
      input$indicator,
      "Last Update",
      previous_metadata,
      "last_update"
    )
  })

  output$next_update <- renderUI({
    update_and_fetch_metadata(
      input$indicator,
      "Next Update",
      previous_metadata,
      "next_update"
    )
  })

  output$source <- renderUI({
    hyperlink <- update_and_fetch_metadata(
      input$indicator,
      "Hyperlink(s)",
      previous_metadata,
      "source"
    )
    dfeshiny::external_link(href = hyperlink, link_text = input$indicator)
  })
}

# App
shinyApp(ui_dev, server_dev)
