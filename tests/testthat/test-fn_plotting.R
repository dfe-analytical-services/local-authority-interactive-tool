# get_yaxis_title() -----------------------------------------------------------
# Dummy data
y_axis_data <- data.frame(
  y_axis_name = c("Temperature", "Pressure", "Temperature", "Humidity"),
  other_column = c(1, 2, 3, 4),
  Chart_title = c("Title Temperature", "Title Pressure", "Title Temperature", "Title Humidity")
)

# Test 1: Check if the function returns unique y_axis_name values
test_that("1. get_yaxis_title returns unique y_axis_name values", {
  temp_data <- y_axis_data[y_axis_data$y_axis_name == "Temperature", ]
  result <- get_yaxis_title(temp_data)
  expected <- c("Temperature")
  expect_equal(result, expected)
})

# Test 2: Check if the function handles data frame with no y_axis_name column
test_that("2. get_yaxis_title handles data frame with no y_axis_name column", {
  no_y_axis_data <- data.frame(other_column = c(1, 2, 3, 4))
  expect_error(get_yaxis_title(no_y_axis_data), "Column `y_axis_name` doesn't exist.")
})


# get_plot_title() ------------------------------------------------------------
# Test 1: Check if the function returns the correct plot title
test_that("1. get_plot_title returns the correct plot title", {
  result <- get_plot_title(
    y_axis_data |>
      dplyr::filter(y_axis_name == "Temperature")
  )
  expected <- "Title Temperature"
  expect_equal(result, expected)
})

# Test 2: Check if the function handles empty strings
test_that("2. get_plot_title handles empty strings", {
  y_axis_data$Chart_title <- ""
  result <- get_plot_title(y_axis_data)
  expected <- ""
  expect_equal(result, expected)
})

# Test 3: Check if the function handles NULL values
test_that("3. get_plot_title handles NULL values", {
  y_axis_data$Chart_title <- NA
  result <- get_plot_title(y_axis_data)
  expected <- NA
  expect_equal(result, expected)
})

# Test 4: Check if the function handles numeric values
test_that("4. get_plot_title handles numeric values", {
  y_axis_data$Chart_title <- 123
  result <- get_plot_title(y_axis_data)
  expected <- 123
  expect_equal(result, expected)
})

# create_plot_colours()---------------------------------------------------------


# Mock data for testing
test_data <- dplyr::tibble(
  `LA and Regions` = c("Region1", "Region2", "Region1", "Region3", "Region2", "Region4")
)

# Test for create_plot_colours
test_that("1. create_plot_colours returns a named vector of colors", {
  result <- create_plot_colours(test_data)

  # Test 1: The output should be a named vector
  expect_true(is.vector(result))
  expect_true(!is.null(names(result)))

  # Test 2: The names of the vector should match unique groups in `LA and Regions`
  unique_groups <- unique(test_data$`LA and Regions`)
  expect_equal(sort(names(result)), sort(unique_groups))

  # Test 3: The length of the color vector should match the number of unique groups
  expect_equal(length(result), length(unique_groups))

  # Test 4: Check for no NA values in the colors
  expect_false(any(is.na(result)))
})

# Additional test for color assignments (if afcolours is deterministic)
test_that("2. create_plot_colours assigns colors consistently", {
  result <- create_plot_colours(test_data)

  # Check if specific group has a consistent color
  color_for_region1 <- result["Region1"]

  # Re-run function and check color consistency
  result_again <- create_plot_colours(test_data)
  expect_equal(result_again["Region1"], color_for_region1)
})

# create_focus_plot_colours()--------------------------------------------------------------

# Load testthat for testing
library(testthat)

# Define focus and neutral colors (replace with actual hex codes if available)
focus_color <- "#FF0000" # Example focus color (replace as needed)
neutral_color <- "#CCCCCC" # Example neutral color (replace as needed)

# Define the tests
test_that("1. create_focus_plot_colours returns a named vector of colors with correct length", {
  # Sample data as tibble
  test_data <- tibble::tibble(
    `LA and Regions` = c("Region1", "Region2", "Region1", "Region3", "Region4")
  )

  # Run the function with a focus group
  focus_group <- "Region2"
  result_colours <- create_focus_plot_colours(test_data, focus_group)

  # Check if result is a named vector
  expect_true(is.vector(result_colours))
  expect_true(!is.null(names(result_colours)))

  # Check if all unique groups are in the names of the result
  unique_groups <- pull_uniques(test_data, "LA and Regions")
  expect_equal(sort(names(result_colours)), sort(unique_groups))

  # Check the length of the color vector matches the number of unique groups
  expect_equal(length(result_colours), length(unique_groups))
})

# create_focus_plot_sizes()-------------------------------------------------------

# Load testthat for testing
library(testthat)

# Define the tests
test_that("1. create_focus_plot_sizes returns a named vector of sizes with correct length", {
  # Sample data as tibble
  test_data <- tibble::tibble(
    `LA and Regions` = c("Region1", "Region2", "Region1", "Region3", "Region4")
  )

  # Run the function with a focus group
  focus_group <- "Region2"
  result_sizes <- create_focus_plot_sizes(test_data, focus_group)

  # Check if result is a named vector
  expect_true(is.vector(result_sizes))
  expect_true(!is.null(names(result_sizes)))

  # Check if all unique groups are in the names of the result
  unique_groups <- pull_uniques(test_data, "LA and Regions")
  expect_equal(sort(names(result_sizes)), sort(unique_groups))

  # Check the length of the size vector matches the number of unique groups
  expect_equal(length(result_sizes), length(unique_groups))
})
