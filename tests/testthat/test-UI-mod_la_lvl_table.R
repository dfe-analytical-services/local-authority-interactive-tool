# nolint start: commented_code

# # 1. Uses root app for setup --------------------------------------------------
# # Load global
# source(here::here("global.R"))
#
# # Setup app
# shinytest_app <- shinytest2::AppDriver$new(
#   name = "la_main_tbl",
#   height = 846,
#   width = 1445,
#   load_timeout = 45 * 1000,
#   timeout = 20 * 1000,
#   wait = TRUE,
#   expect_values_screenshot_args = FALSE # Turn off as we don't need screenshots
# )
#
# # Get export values
# la_main_tbl <- shinytest_app$get_values(export = c("la_main_tbl"))
#
# # Convert reactable JSON to dataframe
# la_main_tbl_list <- jsonlite::fromJSON(la_main_tbl$export$la_main_tbl)
# la_table_data <- la_main_tbl_list$x$tag$attribs$data
# la_table_df <- as.data.frame(la_table_data, check.names = FALSE)
#
# # Test
# testthat::test_that("There are 4 rows in the LA main table", {
#   testthat::expect_true(nrow(la_table_df) == 4)
# })
#
# shinytest_app$stop()


# 2. Uses minimal test-app .R file for setup ----------------------------------
testthat::test_that("There are 4 rows in the LA main table", {
  shinytest_app <- shinytest2::AppDriver$new(
    app_dir = here::here("tests/testthat/test-app_mod_la_lvl_table/"),
    load_timeout = 45 * 1000,
    timeout = 20 * 1000,
    wait = TRUE
  )

  # Get export values
  la_main_tbl <- shinytest_app$get_values(export = c("la_main_tbl"))

  # Convert reactable JSON to dataframe
  la_main_tbl_list <- jsonlite::fromJSON(la_main_tbl$export$la_main_tbl)
  la_table_data <- la_main_tbl_list$x$tag$attribs$data
  la_table_df <- as.data.frame(la_table_data, check.names = FALSE)

  # Test
  testthat::expect_true(nrow(la_table_df) == 4)

  shinytest_app$stop()
})


# ### 3. Uses minimal test-app in-file for setup  -------------------------------
# # Load global
# source(here::here("global.R"))
# source(here::here("R/lait_modules/test-mod_la_lvl_table.R"))
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
# shinytest_app <- shinytest2::AppDriver$new(
#   minimal_app,
#   load_timeout = 45 * 1000
# )
#
# # Get export values
# la_main_tbl <- shinytest_app$get_values(export = c("la_main_tbl"))
#
# # Convert reactable JSON to dataframe
# la_main_tbl_list <- jsonlite::fromJSON(la_main_tbl$export$la_main_tbl)
# la_table_data <- la_main_tbl_list$x$tag$attribs$data
# la_table_df <- as.data.frame(la_table_data, check.names = FALSE)
#
# # Test
# testthat::test_that("There are 4 rows in the LA main table", {
#   testthat::expect_true(nrow(la_table_df) == 4)
# })
#
# shinytest_app$stop()

# nolint end


# Testing LA charts - made using shinytest2::record_test()
test_that("Check LA charts behave as expected", {
  app <- AppDriver$new(
    name = "la-charts",
    height = 1059,
    width = 1461,
    load_timeout = 45 * 1000,
    timeout = 20 * 1000,
    wait = TRUE,
    variant = platform_variant()
  )

  # Get export values
  la_linechart <- app$get_values(export = c("la_linechart"))
  la_linechart_list <- jsonlite::fromJSON(la_linechart$export$la_linechart)

  # Check is a line chart
  testthat::expect_true(
    grepl("stroke='none'", la_linechart_list$x$html)
  )
  testthat::expect_false(
    grepl("linejoin='miter'", la_linechart_list$x$html)
  )

  # Check hover css
  testthat::expect_true(
    grepl(
      "stroke-dasharray:5,5;stroke:black;stroke-width:2px;",
      la_linechart_list$x$settings$hover$css
    )
  )

  # Check title
  testthat::expect_true(
    grepl("Infant Mortality - Rate per 1000 live births", la_linechart_list$x$html)
  )

  # Check visual of line chart
  app$expect_screenshot(
    selector = "#la_chart-line_chart",
    name = "la_line_chart"
  )

  # Change to different topic
  app$set_inputs(
    `la_level-topic_name` = "Key Stage 1",
    la_charts = "Bar chart"
  )

  # Get export values
  la_barchart <- app$get_values(export = c("la_barchart"))
  la_barchart_list <- jsonlite::fromJSON(la_barchart$export$la_barchart)

  # Check is a bar chart
  testthat::expect_true(
    grepl("linejoin='miter'", la_barchart_list$x$html)
  )
  testthat::expect_false(
    grepl("stroke='none'", la_barchart_list$x$html)
  )

  # Check hover css
  testthat::expect_true(
    grepl(
      "fill:orange;stroke:black;cursor:pointer;",
      la_barchart_list$x$settings$hover$css
    )
  )

  # Check title
  testthat::expect_true(
    grepl(
      "Key Stage 1 Reading Expected Standard - % achieving expected level",
      la_barchart_list$x$html
    )
  )

  # Check visual of bar chart
  app$expect_screenshot(
    selector = "#la_chart-bar_chart",
    name = "la_bar_chart"
  )

  app$stop()
})
