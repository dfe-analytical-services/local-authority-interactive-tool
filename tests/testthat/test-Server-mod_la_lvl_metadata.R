# LA_LevelMetaServer() --------------------------------------------------------
test_that("1. LA_LevelMetaServer correctly displays hyperlink", {
  # Simulate the app_inputs object with reactive values
  indicator_input <- shiny::reactive({
    "First Time entrants to the Youth Justice System"
  })

  shiny::testServer(LA_LevelMetaServer, args = list(indicator_input, metrics_clean), {
    # Get the source output
    source_output <- output$source

    # Test that the output is a link
    expect_true(grepl("<a", source_output$html))
    expect_true(grepl("http", source_output$html))
  })
})


# MetadataServer() ------------------------------------------------------------
