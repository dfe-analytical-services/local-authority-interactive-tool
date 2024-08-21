library(shiny)

# Load in current app feature being worked on
source(here::here("02_dev/app_features_workshop.R"))

ui <- bslib::page_fillable(
  div(
    ## Custom CSS =============================================================
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),

    # Tab header ==============================================================
    h1("Local Authority View"),

    div(
      class = "well",
      style = "min-height: 100%; height: 100%; overflow-y: visible;",
      bslib::layout_column_wrap(
        width = "15rem", # Minimum width for each input box before wrapping
        shiny::selectInput(inputId = "la_input",
                           label = "LA:",
                           choices = la_names_bds),
        shiny::selectInput(inputId = "topic_input",
                              label = "Topic:",
                              choices = metric_topics),
        shiny::selectInput(inputId = "indicator_input",
                              label = NULL,
                              choices = metric_names)
      ),
      bslib::card(
        bslib::card_body(
          reactable::reactableOutput("la_table_stats")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Then for the respective National indicator (all schools or State funded)
  filtered_topic_bds <- reactive({
    bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input
      ) |>
      pull_uniques("Measure")
  })

  # Drop downs ==============================================================
  # Using the server to power to the provider dropdown for increased speed
  shiny::observeEvent(input$topic_input, {
    updateSelectizeInput(
      session = session,
      inputId = "indicator_input",
      label = "Indicator:",
      choices = filtered_topic_bds(),
      server = FALSE
    )
  })

  # Then for the respective National (all schools or State funded)
  filtered_bds <- reactive({
    bds_metrics |>
      dplyr::filter(
        Topic == input$topic_input,
        Measure == input$indicator_input
      )
  })


  la_table_stats <- shiny::reactive({

    # Extract change from prev year (from LA table)
    la_change_prev <- filtered_bds() |>
      filter_la_regions(input$la_input) |>
      dplyr::arrange(`LA and Regions`, desc(Years)) |>
      dplyr::mutate(values_num = dplyr::lag(values_num) - values_num,
                    Years = "Change from previous year") |>
      dplyr::filter(dplyr::row_number() == 2) |>
      dplyr::pull(values_num)

    # Set the trend value
    la_trend <- dplyr::case_when(
      is.na(la_change_prev) ~ NA_character_,
      la_change_prev > 0 ~ "Increase",
      la_change_prev < 0 ~ "Decrease",
      TRUE ~ "No trend"
    )

    # Get latest rank, ties are set to min & NA vals to NA rank
    la_rank <- filtered_bds() |>
      filter_la_regions(la_names_bds, latest = T) |>
      dplyr::mutate(
        rank = dplyr::case_when(
          is.na(values_num) ~ NA,
          TRUE ~ rank(values_num, ties.method = "min", na.last = TRUE)
        )
      ) |>
      filter_la_regions(input$la_input, pull_col = "rank")

    # Calculate quartile bands for indicator
    la_quartile_bands <- filtered_bds() |>
      filter_la_regions(la_names_bds, latest = T, pull_col = "values_num") |>
      quantile(na.rm = TRUE)

    # Extracting LA latest value
    la_indicator_val <- filtered_bds() |>
      filter_la_regions(input$la_input, latest = T, pull_col = "values_num")

    # Calculating which quartile this value sits in
    la_quartile <- dplyr::case_when(
      is.na(la_indicator_val) ~ NA_character_,
      (la_indicator_val >= la_quartile_bands[["0%"]]) &
        (la_indicator_val <= la_quartile_bands[["25%"]]) ~ "A",
      (la_indicator_val > la_quartile_bands[["25%"]]) &
        (la_indicator_val <= la_quartile_bands[["50%"]]) ~ "B",
      (la_indicator_val > la_quartile_bands[["50%"]]) &
        (la_indicator_val <= la_quartile_bands[["75%"]]) ~ "C",
      (la_indicator_val > la_quartile_bands[["75%"]]) &
        (la_indicator_val <= la_quartile_bands[["100%"]]) ~ "D",
      TRUE ~ "Error"
    )

    df_la_stats <- data.frame(
      "LA Number" = 203,
      "LA and Regions" = input$la_input,
      "Trend" = la_trend,
      "Change from previous year" = la_change_prev,
      "Latest National Rank" = la_rank,
      "Quartile Banding" = la_quartile,
      "(A) Up to and including" = la_quartile_bands[["25%"]],
      "(B) Up to and including" = la_quartile_bands[["50%"]],
      "(C) Up to and including" = la_quartile_bands[["75%"]],
      "(D) Up to and including" = la_quartile_bands[["100%"]],
      check.names = FALSE
    )

    df_la_stats
  })

  output$la_table_stats <- reactable::renderReactable({
    dfe_reactable(la_table_stats())
  })

}

shinyApp(ui, server)
