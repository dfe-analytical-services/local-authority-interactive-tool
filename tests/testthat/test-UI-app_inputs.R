# -----------------------------------------------------------------------------
# This is an example integration test file
#
# Integration tests in Shiny allow you to test the server.R reactivity without
# needing to load a full app and interact with the UI.
#
# This makes integration tests faster to run than UI tests and makes them a
# more efficient alternative if you don't need to interact with the UI to test
# what you want to.
#
# These examples show some ways you can make use of integration tests.
#
# Add more scripts and checks as appropriate for your app.
# -----------------------------------------------------------------------------

# Load global
source(here::here("global.R"))

# Create minimal app
minimal_ui <- shiny::fluidRow(
  title = "Minimal app",
  appInputsUI("la_level")
)

minimal_server <- function(input, output, session) {
  appInputsServer("la_level")
}

minimal_app <- shinyApp(minimal_ui, minimal_server)

shinytest_app <- shinytest2::AppDriver$new(minimal_app)

test_that("Deafult inputs", {
  # LA
  expect_equal(
    shinytest_app$get_value(input = "la_level-la_name"),
    "Barking and Dagenham"
  )

  # Topic
  expect_equal(
    shinytest_app$get_value(input = "la_level-topic_name"),
    "Health and Wellbeing"
  )

  # Indicator
  expect_equal(
    shinytest_app$get_value(input = "la_level-indicator_name"),
    "Infant Mortality"
  )
})

test_that("Change in topic input leads to a change in indicator input", {
  shinytest_app$set_inputs(`la_level-topic_name` = "Economic Factors")

  # Top of the Economic Factors indicators
  expect_equal(
    shinytest_app$get_value(input = "la_level-indicator_name"),
    "Percentage of children in low income families"
  )

  # Check LA remains the same
  expect_equal(
    shinytest_app$get_value(input = "la_level-la_name"),
    "Barking and Dagenham"
  )
})

shinytest_app$stop()
