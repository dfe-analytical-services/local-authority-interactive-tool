# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


string_list <- function(values) {
  paste0(
    "{", paste0(names(values), " : ", unlist(values), collapse = ", "), "}"
  )
}


ui <- bslib::page_fillable(
  ## Custom CSS =============================================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),
  # Include reactable.extras in your UI
  reactable.extras::reactable_extras_dependency(),

  # Tab header ==============================================================
  h1("Create your own"),
  div(
    class = "well",
    style = "overflow-y: visible;",
    bslib::layout_column_wrap(
      width = "15rem", # Minimum width for each input box before wrapping
      shiny::selectInput(
        inputId = "la_input",
        label = "Change Authority:",
        choices = la_names_bds,
        multiple = TRUE,
        selected = "Barking and Dagenham"
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
    reactable::reactableOutput("react"),
  ),
  hr(),
  textOutput("date_text"),
  textOutput("button_text"),
  textOutput("check_text"),
  textOutput("dropdown_text"),
  textOutput("text")
)

server <- function(input, output, session) {
  # Input ----------------------------------
  # Using the server to power to the provider dropdown for increased speed
  shiny::observeEvent(input$topic_input, {
    # Get indicator choices for selected topic
    filtered_topic_bds <- bds_metrics |>
      dplyr::filter(
        Topic %in% input$topic_input
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
        Topic %in% input$topic_input,
        Measure %in% input$indicator,
        !is.na(Years)
      )
  })

  # Building the staging table
  create_own_bds <- reactive({
    staging_table <- filtered_bds$data |>
      dplyr::filter(`LA and Regions` %in% input$la_input) |>
      dplyr::select(
        `LA Number`, `LA and Regions`, Topic,
        Measure, Years, Years_num, values_num, Values
      ) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
        names_from = Years,
        values_from = values_num,
      )

    staging_table |>
      dplyr::mutate(
        Include = rep(TRUE, nrow(staging_table)),
        `Region LAs` = rep(FALSE, nrow(staging_table)),
        `Remove Row` = rep("Remove", nrow(staging_table))
      ) |>
      dplyr::relocate(Include, .before = dplyr::everything())
  })

  output$react <- reactable::renderReactable({
    reactable::reactable(
      create_own_bds(),
      columns = list(
        Include = reactable::colDef(
          cell = reactable.extras::checkbox_extra("include_check", class = "checkbox-extra"),
          align = "left"
        ),
        `Region LAs` = reactable::colDef(
          cell = reactable.extras::checkbox_extra("region_la_check", class = "checkbox-extra"),
          align = "left"
        ),
        `Remove Row` = reactable::colDef(
          cell = reactable.extras::button_extra("button", class = "button-extra")
        )
      )
    )
  })

  output$check_text <- renderText({
    req(input$include_check)
    values <- input$include_check
    paste0(
      "Check: ",
      string_list(values)
    )
  })
  output$check_text <- renderText({
    req(input$region_la_check)
    values <- input$region_la_check
    paste0(
      "Check: ",
      string_list(values)
    )
  })
  output$button_text <- renderText({
    req(input$button)
    values <- input$button
    paste0(
      "Button: ",
      string_list(values)
    )
  })
}

shinyApp(ui, server)
