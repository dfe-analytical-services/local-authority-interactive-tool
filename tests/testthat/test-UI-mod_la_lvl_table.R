# 1. Uses root app for setup --------------------------------------------------
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

shinytest_app$stop()


# 2. Uses minimal test-app .R file for setup ----------------------------------
shinytest_app <- shinytest2::AppDriver$new(
  app_dir = here::here("tests/testthat/test-app_mod_la_lvl_table/"),
  load_timeout = 45 * 1000
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

shinytest_app$stop()
