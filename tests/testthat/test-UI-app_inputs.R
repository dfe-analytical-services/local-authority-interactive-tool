# -----------------------------------------------------------------------------
# This is an example UI test file
# It includes some tests that show how to check the elements in the template
# have loaded correctly.
#
# In this script we show a two more way to test the UI of the app
# 1. create 'snapshots' of the state of the app (easiest to set up)
# 2. exporting values from the app and using here (recommended long term)
#
# Snapshots form the basic for future test runs to check to see if the app has
# changed at all, if it has it will fail and suggest you review the changes.
#
# Deciding what to check for in snapshots is a tricky balance, often it's best
# to test running the app in the console, and then running app$get_values() to
# see what values exist to check.
#
# Exporting values allows you to make tests that don't care about data updates
# or package updates or any other noise, and you can purely focus on what you
# want to check. They take more effort to set up but are worth it in the long
# run.
#
# You should adapt this script, and create additional scripts as necessary to
# match the needs for your app.
# -----------------------------------------------------------------------------

# appInputsServer() -----------------------------------------------------------
# Load global
mute_cat(source(here::here("global.R")))

# Create minimal app
minimal_ui <- shiny::fluidRow(
  title = "Minimal app",
  appInputsUI("la_level")
)

minimal_server <- function(input, output, session) {
  shared_values <- reactiveValues(
    la = NULL,
    topic = NULL,
    indicator = NULL
  )
  appInputsServer("la_level", shared_values, bds_metrics, topic_indicator_full)
}

minimal_app <- shinyApp(minimal_ui, minimal_server)

shinytest_app <- shinytest2::AppDriver$new(
  minimal_app,
  load_timeout = 45 * 10000,
  timeout = 20 * 10000,
  wait = TRUE
)

test_that("Deafult inputs", {
  # LA
  expect_equal(
    shinytest_app$get_value(input = "la_level-la_name"),
    "Barking and Dagenham"
  )

  # Topic
  expect_equal(
    shinytest_app$get_value(input = "la_level-topic_name"),
    "All Topics"
  )

  # Indicator
  expect_equal(
    shinytest_app$get_value(input = "la_level-indicator_name"),
    "Infant Mortality"
  )
})

test_that("Change in topic input leads to a change in indicator input", {
  # Set Topic input to Economic Factors
  shinytest_app$set_inputs(`la_level-topic_name` = "Economic Factors")
  shinytest_app$wait_for_idle()

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
