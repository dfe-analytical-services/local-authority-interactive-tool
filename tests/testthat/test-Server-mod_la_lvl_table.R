# BDS_FilteredServer ----------------------------------------------------------
test_that("1. BDS_FilteredServer correctly filters bds_metrics", {
  # Simulate the app_inputs object with reactive values
  app_inputs <- shiny::reactiveValues(
    topic = reactive({
      "Economic Factors"
    }),
    indicator = reactive({
      "Percentage of children in low income families"
    })
  )

  shiny::testServer(BDS_FilteredServer, args = list(app_inputs, bds_metrics), {
    # Trigger the observeEvent by setting the indicator input
    session$flushReact()

    # Retrieve the filtered data
    filtered_data <- session$returned()

    # Expected result based on the mock data
    expected_data <- bds_metrics |>
      dplyr::filter(
        Topic == "Economic Factors",
        Measure == "Percentage of children in low income families"
      )

    # Test that the filtered data matches the expected result
    testthat::expect_equal(filtered_data, expected_data)
  })
})

test_that("2. BDS_FilteredServer correctly responds to input changes", {
  # Simulate the app_inputs object with reactive values
  app_inputs <- shiny::reactiveValues(
    topic = reactive({
      "Economic Factors"
    }),
    indicator = reactive({
      "Percentage of children in low income families"
    })
  )

  shiny::testServer(BDS_FilteredServer, args = list(app_inputs, bds_metrics), {
    # Trigger the observeEvent by setting the indicator input
    session$flushReact()

    # Retrieve the filtered data
    filtered_data <- session$getReturned()

    # Check filtered_bds has been updated by getting the Topic
    testthat::expect_equal(
      filtered_data() |>
        pull_uniques("Topic"),
      "Economic Factors"
    )

    # Change the inputs to test another case
    app_inputs$topic <- shiny::reactive({
      "Health and Wellbeing"
    })
    app_inputs$indicator <- shiny::reactive({
      "Infant Mortality"
    })

    # Trigger the observeEvent again
    session$flushReact()

    # Retrieve the filtered data again
    filtered_data <- session$returned()

    # Check filtered_bds has been updated by getting the Topic
    testthat::expect_equal(
      filtered_data |>
        pull_uniques("Topic"),
      "Health and Wellbeing"
    )
  })
})
