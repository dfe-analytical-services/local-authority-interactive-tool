# Load global
source(here::here("global.R"))

# Setup app
shinytest_app <- shinytest2::AppDriver$new(
  name = "la_main_tbl",
  height = 846,
  width = 1445,
  load_timeout = 45 * 1000,
  timeout = 20 * 1000,
  wait = TRUE,
  expect_values_screenshot_args = FALSE # Turn off as we don't need screenshots
)

# Get export values
la_main_tbl <- shinytest_app$get_values(export = c("la_main_tbl"))

# Convert reactable JSON to dataframe
la_main_tbl_list <- jsonlite::fromJSON(la_main_tbl$export$la_main_tbl)
la_table_data <- la_main_tbl_list$x$tag$attribs$data
la_table_df <- as.data.frame(la_table_data, check.names = FALSE)

# Test
testthat::test_that("There are 4 rows in the LA main table", {
  testthat::expect_true(nrow(la_table_df) == 4)
})



# # Load global
# source(here::here("global.R"))
#
# # Create minimal app
# minimal_ui <- shiny::fluidRow(
#   title = "Minimal app",
#
#   # User inputs
#   appInputsUI("la_level"),
#
#   # Main table
#   LA_LevelTableUI("la_table")
# )
#
# minimal_server <- function(input, output, session) {
#   # User Inputs
#   app_inputs <- appInputsServer("la_level")
#
#   # Main table
#   la_main_tbl <- LA_LevelTableServer(
#     "la_table",
#     app_inputs,
#     bds_metrics,
#     stat_n_la
#   )
#
#   # Export values for use in UI tests
#   exportTestValues(
#     la_main_tbl = la_main_tbl()
#   )
# }
#
# minimal_app <- shinyApp(minimal_ui, minimal_server)
#
# shinytest_app <- shinytest2::AppDriver$new(minimal_app)
#
# la_main_tbl <- app$get_values(export = c("la_main_tbl"))
#
# rev_bal_export$export$avg_rev_bal_value
