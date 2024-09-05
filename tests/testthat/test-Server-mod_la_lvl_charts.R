testthat::test_that("LA_LineChartServer creates a ggiraph chart with the correct title", {
  mute_cat(source(here::here("global.R")))

  # Mocking app_inputs and other required data
  app_inputs <- shiny::reactiveValues(
    la = reactive({
      "Barking and Dagenham"
    }),
    topic = reactive({
      "Economic Factors"
    }),
    indicator = reactive({
      "Percentage of children in low income families"
    })
  )

  # Running the test server
  shiny::testServer(LA_LineChartServer, args = list(app_inputs, bds_metrics, stat_n_la), {
    # Trigger reactivity to simulate the app environment
    session$flushReact()

    # Retrieve the output plot
    output_plot <- la_line_chart()

    # Check that the output is a ggiraph object
    testthat::expect_true(inherits(output_plot, "girafe"))

    # Check that the plot contains the expected data
    plot_data <- output$line_chart
    plot_data_list <- jsonlite::fromJSON(plot_data)

    # Check title
    testthat::expect_true(
      grepl("Percentage of children in low income families", plot_data_list$x$html)
    )
  })
})
