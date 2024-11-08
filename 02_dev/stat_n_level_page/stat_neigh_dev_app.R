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
  h1("Statistical Neighbour Level"),
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
      bslib::card_header("Statistical Neighbours"),
      bslib::card_body(
        div(
          reactable::reactableOutput("stat_n_sns_table")
        ),
        div(
          style = "border-top: 2px solid black; padding-top: 2.5rem;", # Add black border between the tables
          reactable::reactableOutput("stat_n_comp_table")
        )
      )
    )
  ),
  div(
    class = "well",
    bslib::card(
      bslib::card_header(),
      bslib::card_body(
        div(
          reactable::reactableOutput("stat_n_stats_table")
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
            ggiraph::girafeOutput("stat_n_focus_line_chart")
          ),
          full_screen = TRUE
        ),
      ),
      bslib::nav_panel(
        title = "Line chart - user selection",
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
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 3,
                    plugins = list("remove_button"),
                    dropdownParent = "body"
                  )
                )
              ),
              ggiraph::girafeOutput("stat_n_multi_line_chart")
            )
          ),
          full_screen = TRUE
        )
      ),
      bslib::nav_panel(
        title = "Focus bar chart",
        bslib::card(
          bslib::card_body(
            ggiraph::girafeOutput("stat_n_focus_bar_chart")
          ),
          full_screen = TRUE
        ),
      ),
      bslib::nav_panel(
        title = "Bar chart - user selection",
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
                  choices = NULL,
                  multiple = TRUE,
                  options = list(maxItems = 3)
                )
              ),
              ggiraph::girafeOutput("stat_n_multi_bar_chart")
            )
          ),
          full_screen = TRUE
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

  # Get decimal places for indicator selected
  indicator_dps <- reactive({
    filtered_bds$data |>
      get_indicator_dps()
  })

  # Get statistical neighbours
  stat_n_sns <- reactive({
    stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input) |>
      pull_uniques("LA Name_sn")
  })

  # Get region
  stat_n_region <- reactive({
    stat_n_la |>
      dplyr::filter(`LA Name` == input$la_input) |>
      pull_uniques("GOReg") |>
      clean_ldn_region(filtered_bds$data)
  })

  # Long format Statistical Neighbour data
  stat_n_long <- reactive({
    # Calculate SN average
    stat_n_sn_avg <- filtered_bds$data |>
      dplyr::filter(`LA and Regions` %in% stat_n_sns()) |>
      dplyr::summarise(
        values_num = dplyr::na_if(mean(values_num, na.rm = TRUE), NaN),
        .by = c("Years", "Years_num")
      ) |>
      dplyr::mutate(
        "LA Number" = "-",
        "LA and Regions" = "Statistical Neighbours",
        .before = "Years"
      )

    # Statistical Neighbours long data
    filtered_bds$data |>
      dplyr::filter(`LA and Regions` %in% c(input$la_input, stat_n_sns(), stat_n_region(), "England")) |>
      dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values) |>
      dplyr::bind_rows(stat_n_sn_avg) |>
      dplyr::mutate(
        `LA and Regions` = factor(
          `LA and Regions`,
          levels = c(
            input$la_input, stat_n_sns(), "Statistical Neighbours",
            stat_n_region(), "England"
          )
        )
      )
  })

  # Difference between last two years
  stat_n_diff <- reactive({
    stat_n_long() |>
      calculate_change_from_prev_yr()
  })

  # Most recent year
  current_year <- reactive({
    stat_n_long() |>
      dplyr::filter(Years_num == max(Years_num)) |>
      pull_uniques("Years")
  })

  # Build main Statistical Neighbour formatted table (used to create the others)
  stat_n_table <- shiny::reactive({
    # Join difference and pivot wider
    stat_n_long() |>
      dplyr::bind_rows(stat_n_diff()) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions"),
        names_from = Years,
        values_from = values_num,
      )
  })

  output$stat_n_sns_table <- reactable::renderReactable({
    stat_n_sns_table <- stat_n_table() |>
      dplyr::filter(`LA and Regions` %in% c(input$la_input, stat_n_sns())) |>
      dplyr::arrange(.data[[current_year()]], `LA and Regions`)

    dfe_reactable(
      stat_n_sns_table,
      columns = utils::modifyList(
        format_num_reactable_cols(
          stat_n_sns_table,
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        set_custom_default_col_widths()
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, stat_n_sns_table, input$la_input)
      },
      pagination = FALSE
    )
  })

  # Join difference and pivot wider to recreate 2nd Statistical Neighbours table
  output$stat_n_comp_table <- reactable::renderReactable({
    stat_n_comp_table <- stat_n_table() |>
      dplyr::filter(`LA and Regions` %in% c(
        "Statistical Neighbours",
        stat_n_region(),
        "England"
      )) |>
      dplyr::arrange(`LA and Regions`)

    # Output table
    dfe_reactable(
      stat_n_comp_table,
      # Create the reactable with specific column alignments
      columns = utils::modifyList(
        format_num_reactable_cols(
          stat_n_comp_table,
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number"
        ),
        set_custom_default_col_widths()
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, stat_n_comp_table)
      },
      pagination = FALSE
    )
  })


  # Statistical Neighbour Level stats table -------------------------------------
  stat_n_stats_table <- reactive({
    stat_n_stats_geog <- c(input$la_input, stat_n_region(), "England")

    # Extract change from prev year
    stat_n_change_prev <- stat_n_diff() |>
      filter_la_regions(stat_n_stats_geog,
        pull_col = "values_num"
      )

    # Get polarity of indicator
    stat_n_indicator_polarity <- filtered_bds$data |>
      pull_uniques("Polarity")

    # Set the trend value
    stat_n_trend <- as.numeric(stat_n_change_prev)

    # Get latest rank, ties are set to min & NA vals to NA rank
    stat_n_rank <- filtered_bds$data |>
      filter_la_regions(la_names_bds, latest = TRUE) |>
      calculate_rank(stat_n_indicator_polarity) |>
      filter_la_regions(input$la_input, pull_col = "rank")


    # Calculate quartile bands for indicator
    stat_n_quartile_bands <- filtered_bds$data |>
      filter_la_regions(la_names_bds, latest = TRUE, pull_col = "values_num") |>
      quantile(na.rm = TRUE)

    # Extracting LA latest value
    stat_n_indicator_val <- filtered_bds$data |>
      filter_la_regions(input$la_input, latest = TRUE, pull_col = "values_num")

    # Calculating which quartile this value sits in
    stat_n_quartile <- calculate_quartile_band(
      stat_n_indicator_val,
      stat_n_quartile_bands,
      stat_n_indicator_polarity
    )

    # SN stats table
    build_sn_stats_table(
      stat_n_diff(),
      stat_n_stats_geog,
      stat_n_trend,
      stat_n_change_prev,
      stat_n_rank,
      stat_n_quartile,
      stat_n_indicator_polarity
    )
  })

  # Main stats table
  output$stat_n_stats_table <- reactable::renderReactable({
    stat_n_stats_output <- stat_n_stats_table()

    dfe_reactable(
      stat_n_stats_output,
      columns = modifyList(
        # Create the reactable with specific column alignments
        format_num_reactable_cols(
          stat_n_stats_output,
          get_indicator_dps(filtered_bds$data),
          num_exclude = "LA Number",
          categorical = c("Trend", "Quartile Banding", "National Rank")
        ),
        # Define specific formatting for the Trend and Quartile Banding columns
        list(
          set_custom_default_col_widths(),
          Trend = reactable::colDef(
            cell = trend_icon_renderer,
            style = function(value) {
              get_trend_colour(value, stat_n_stats_output$Polarity[1])
            }
          ),
          `Quartile Banding` = reactable::colDef(
            style = function(value, index) {
              color <- get_quartile_band_cell_colour(
                stat_n_stats_output[index, "Polarity"],
                stat_n_stats_output[index, "Quartile Banding"]
              )
              list(background = color)
            }
          ),
          Polarity = reactable::colDef(show = FALSE)
        )
      ),
      rowStyle = function(index) {
        highlight_selected_row(index, stat_n_stats_output, input$la_input)
      }
    )
  })


  # Statistical Neighbour charts ----------------------------------------------

  # Shared inputs -------------------------------------------------------------
  # Restrict the user input choices to only eligble Regions
  # Helper function to filter and retain valid selections
  retain_valid_selections <- function(current_choices, previous_selections) {
    # Keep only selections that are part of the new choices
    intersect(previous_selections, current_choices)
  }

  # Update both chart inputs when la_input changes
  observeEvent(input$la_input, {
    # Get available regions excluding the selected LA
    multi_chart_data <- stat_n_long() |>
      dplyr::filter(`LA and Regions` != input$la_input) |>
      pull_uniques("LA and Regions")

    # Store previous selections for chart_line_input and chart_bar_input
    prev_line_selections <- input$chart_line_input
    prev_bar_selections <- input$chart_bar_input

    # Retain only valid selections from previous inputs
    valid_line_selections <- retain_valid_selections(multi_chart_data, prev_line_selections)
    valid_bar_selections <- retain_valid_selections(multi_chart_data, prev_bar_selections)

    # Update chart_line_input while retaining valid previous selections
    updateSelectInput(
      session = session,
      inputId = "chart_line_input",
      choices = multi_chart_data,
      selected = valid_line_selections # Restore previous valid selections
    )

    # Update chart_bar_input while retaining valid previous selections
    updateSelectInput(
      session = session,
      inputId = "chart_bar_input",
      choices = multi_chart_data,
      selected = valid_bar_selections # Restore previous valid selections
    )
  })

  # Stores the shared inputs between line and bar charts
  shared_chart_inputs <- shiny::reactiveValues(
    line_chart = NULL,
    bar_chart = NULL
  )

  # Keep line and bar inputs synchronised without resetting selections
  observeEvent(input$chart_line_input,
    {
      # Capture the current reactive values
      shared_chart_inputs$line_chart <- input$chart_line_input
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  # Update the bar chart with new line chart value
  observeEvent(shared_chart_inputs$line_chart,
    {
      later::later(function() {
        isolate({
          if (!setequal(shared_chart_inputs$line_chart, input$chart_bar_input)) {
            updateSelectInput(
              session = session,
              inputId = "chart_bar_input",
              selected = if (is.null(shared_chart_inputs$line_chart)) {
                character(0)
              } else {
                shared_chart_inputs$line_chart
              }
            )
          }
        })
      }, delay = 0.1)
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  # Update shared bar input
  observeEvent(input$chart_bar_input,
    {
      # Capture the current reactive values
      shared_chart_inputs$chart_bar_input <- input$chart_bar_input
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  # Update line chart with bar chart input
  observeEvent(shared_chart_inputs$bar_chart,
    {
      later::later(function() {
        isolate({
          if (!setequal(shared_chart_inputs$chart_bar_input, input$chart_line_input)) {
            updateSelectInput(
              session = session,
              inputId = "chart_line_input",
              selected = if (is.null(shared_chart_inputs$chart_bar_input)) {
                character(0)
              } else {
                shared_chart_inputs$chart_bar_input
              }
            )
          }
        })
      }, delay = 0.1)
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )


  # Statistical Neighbour Level SN focus plot -----------------------------------
  output$stat_n_focus_line_chart <- ggiraph::renderGirafe({
    # Set selected LA to last level so appears at front of plot
    focus_line_data <- stat_n_long() |>
      dplyr::filter(`LA and Regions` %in% c(input$la_input, stat_n_sns())) |>
      reorder_la_regions(input$la_input, after = Inf)

    if (all(is.na(focus_line_data$values_num))) {
      ggiraph::girafe(
        ggobj = display_no_data_plot(),
        width_svg = 8.5,
        options = generic_ggiraph_options(),
        fonts = list(sans = "Arial")
      )
    } else {
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
        set_plot_colours(focus_line_data, colour_type = "focus", focus_group = input$la_input) +
        set_plot_labs(filtered_bds$data) +
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
        indicator_dps(),
        input$la_input,
        "#12436D"
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
    }
  })


  # Statistical Neighbour Level SN multi-choice line plot -----------------------
  output$stat_n_multi_line_chart <- ggiraph::renderGirafe({
    # Stores all valid regions in data
    valid_regions <- stat_n_long()$`LA and Regions`

    # Filter Statistical Neighbour data for these areas
    stat_n_line_chart_data <- stat_n_long() |>
      # Filter for random areas - simulate user choosing up to 6 areas
      dplyr::filter(
        (`LA and Regions` %in% input$chart_line_input) |
          (`LA and Regions` %in% input$la_input)
      ) |>
      # Set area orders so selection order starts on top of plot
      reorder_la_regions(
        rev(intersect(c(input$la_input, input$chart_line_input), valid_regions)),
        after = Inf
      )

    if (all(is.na(stat_n_line_chart_data$values_num))) {
      ggiraph::girafe(
        ggobj = display_no_data_plot(),
        width_svg = 8.5,
        options = generic_ggiraph_options(),
        fonts = list(sans = "Arial")
      )
    } else {
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
        set_plot_colours(
          data.frame(
            `LA and Regions` = c(input$la_input, input$chart_line_input),
            check.names = FALSE
          ),
          "colour",
          input$la_input
        ) +
        set_plot_labs(filtered_bds$data) +
        custom_theme() +
        # Revert order of the legend so goes from right to left
        ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))


      # Creating vertical geoms to make vertical hover tooltip
      vertical_hover <- lapply(
        get_years(stat_n_line_chart_data),
        tooltip_vlines,
        stat_n_line_chart_data,
        indicator_dps(),
        input$la_input
      )

      # Plotting interactive graph
      ggiraph::girafe(
        ggobj = (multi_line_chart + vertical_hover),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    }
  })


  # Statistical Neighbour focus bar plot ----------------------------------------
  output$stat_n_focus_bar_chart <- ggiraph::renderGirafe({
    focus_bar_data <- stat_n_long() |>
      dplyr::filter(`LA and Regions` %in% c(input$la_input, stat_n_sns())) |>
      reorder_la_regions(input$la_input)

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
            tooltip = tooltip_bar(
              focus_bar_data,
              indicator_dps(),
              input$la_input,
              "#12436D"
            ),
            data_id = `LA and Regions`
          ),
          position = "dodge",
          width = 0.6,
          na.rm = TRUE,
          colour = "black"
        ) +
        format_axes(focus_bar_data) +
        set_plot_colours(focus_bar_data, "focus-fill", input$la_input) +
        set_plot_labs(filtered_bds$data) +
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


  # Statistical Neighbour multi-choice bar plot -------------------------------
  output$stat_n_multi_bar_chart <- ggiraph::renderGirafe({
    # Stores all valid regions in data
    valid_regions <- stat_n_long()$`LA and Regions`

    stat_n_bar_multi_data <- stat_n_long() |>
      # Filter for random areas - simulate user choosing up to 6 areas
      dplyr::filter(
        (`LA and Regions` %in% input$chart_bar_input) |
          (`LA and Regions` %in% input$la_input)
      ) |>
      # Set area orders so selection order starts on top of plot
      reorder_la_regions(
        intersect(c(input$la_input, input$chart_bar_input), valid_regions)
      )

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
            tooltip = tooltip_bar(
              stat_n_bar_multi_data,
              indicator_dps(),
              input$la_input
            ),
            data_id = `LA and Regions`
          ),
          position = "dodge",
          width = 0.6,
          na.rm = TRUE,
          colour = "black"
        ) +
        format_axes(stat_n_bar_multi_data) +
        set_plot_colours(stat_n_bar_multi_data, "fill", input$la_input) +
        set_plot_labs(filtered_bds$data) +
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
}

shiny::shinyApp(ui_dev, server_dev)
