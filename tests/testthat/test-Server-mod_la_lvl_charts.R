# LA_LineChartServer() --------------------------------------------------------
testthat::test_that("LA_LineChartServer creates a ggiraph chart with the correct title", {
  mute_cat(source(here::here("global.R")))

  # Mocking app_inputs and other required data
  app_inputs <- shiny::reactiveValues(
    la = reactive({
      "Cumbria"
    }),
    topic = reactive({
      "Looked After Children"
    }),
    indicator = reactive({
      "LAC Key Stage 4 - AVG Attainment 8 score"
    })
  )

  # Running the test server
  shiny::testServer(
    LA_LineChartServer,
    args = list(app_inputs, bds_metrics, stat_n_la, covid_affected_indicators),
    {
      # Trigger reactivity to simulate the app environment
      session$flushReact()

      # Retrieve the output plot
      output_plot <- interactive_line_chart()

      # Check that the output is a ggiraph object
      testthat::expect_true(inherits(output_plot, "girafe"))

      # Check that the plot contains the expected data
      plot_data <- output$line_chart
      plot_data_list <- jsonlite::fromJSON(plot_data)
      plot_data_str <- plot_data_list$x$html

      # Extract all text content from <text> tags
      extracted_text <- gsub("<text[^>]*>([^<]*)</text>", "\\1", plot_data_str)

      # Remove any extra whitespace
      extracted_text <- gsub("\n", " ", extracted_text)
      extracted_text <- gsub("\\s+", " ", extracted_text)

      # Check title
      testthat::expect_true(grepl("LAC - KS4 Average Attainment 8 Score", extracted_text))
    }
  )
})


# LA_BarChartServer() ---------------------------------------------------------
testthat::test_that("LA_BarChartServer creates a ggiraph chart with the correct title", {
  mute_cat(source(here::here("global.R")))

  # Mocking app_inputs and other required data
  app_inputs <- shiny::reactiveValues(
    la = reactive({
      "Wirral"
    }),
    topic = reactive({
      "Children with SEN"
    }),
    indicator = reactive({
      "Newly issued statements and plans in LA maintained mainstream schools"
    })
  )

  # Running the test server
  shiny::testServer(
    LA_BarChartServer,
    args = list(app_inputs, bds_metrics, stat_n_la, covid_affected_indicators),
    {
      # Trigger reactivity to simulate the app environment
      session$flushReact()

      # Retrieve the output plot
      output_plot <- interactive_bar_chart()

      # Check that the output is a ggiraph object
      testthat::expect_true(inherits(output_plot, "girafe"))

      # Check that the plot contains the expected data
      plot_data <- output$bar_chart
      plot_data_list <- jsonlite::fromJSON(plot_data)
      plot_data_str <- plot_data_list$x$html

      # Extract all text content from <text> tags
      cleaned_plot_str <- gsub("<text[^>]*>([^<]*)</text>", "\\1", plot_data_str)

      # Remove any extra whitespace
      cleaned_plot_str <- gsub("\n", " ", cleaned_plot_str)
      cleaned_plot_str <- gsub("\\s+", " ", cleaned_plot_str)

      # Check title
      testthat::expect_true(
        grepl(
          "Newly issued EHC plans with a placement in LA maintained mainstream schools (%)",
          cleaned_plot_str,
          fixed = TRUE
        )
      )
    }
  )
})
