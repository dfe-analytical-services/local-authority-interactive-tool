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


# Additional test for color assignments (if afcolours is deterministic)
test_that("1. create_plot_colours assigns colors consistently", {
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

##calculate_y_range()--------------------------------------------

# Create some sample data for testing
sample_data <- tibble::tibble(
  values_num = c(1, 5, 10, NA, 20)
)

empty_data <- tibble::tibble(
  values_num = numeric(0)
)

all_na_data <- tibble::tibble(
  values_num = c(NA, NA, NA)
)

single_value_data <- tibble::tibble(
  values_num = 7
)

# Test that calculate_y_range returns the correct range for sample data
test_that("1. calculate_y_range returns correct range for sample data", {
  result <- calculate_y_range(sample_data)
  expect_equal(result, c(1, 20))
})

# Test that calculate_y_range handles data with NA values correctly
test_that("2. calculate_y_range ignores NA values", {
  result <- calculate_y_range(sample_data)
  expect_equal(result, c(1, 20)) # NA values should be ignored
})



# Test that calculate_y_range returns the same value twice for single-value data
test_that("3. calculate_y_range returns the same value for single-value data", {
  result <- calculate_y_range(single_value_data)
  expect_equal(result, c(7, 7))
})

# Test that calculate_y_range works with negative values
negative_data <- tibble::tibble(
  values_num = c(-10, -5, 0, 5, 10)
)
test_that("4. calculate_y_range returns correct range for negative and positive values", {
  result <- calculate_y_range(negative_data)
  expect_equal(result, c(-10, 10))
})

##pretty_y_gridlines{}------------------------------------------------------------------


# Create sample data for testing
sample_data <- tibble::tibble(
  values_num = c(1, 5, 10, NA, 20)
)

negative_data <- tibble::tibble(
  values_num = c(-20, -10, -5, NA, -1)
)

positive_data <- tibble::tibble(
  values_num = c(0, 5, 10, 15, NA)
)

mixed_data <- tibble::tibble(
  values_num = c(-10, 0, 10, NA, 20)
)

all_zero_data <- tibble::tibble(
  values_num = c(0, 0, 0, NA)
)

# Test that pretty_y_gridlines generates breaks extending beyond data range
test_that("1. pretty_y_gridlines generates breaks beyond data range", {
  result <- pretty_y_gridlines(sample_data)
  expect_true(min(result) <= min(c(1, 5, 10, 20), na.rm = TRUE))
  expect_true(max(result) >= max(c(1, 5, 10, 20), na.rm = TRUE))
})

# Test that pretty_y_gridlines handles negative values and extends above and below range
test_that("2. pretty_y_gridlines handles negative values correctly", {
  result <- pretty_y_gridlines(negative_data)
  expect_true(min(result) <= -20)
  expect_true(max(result) >= 0)  # Ensure it includes zero
})

# Test that pretty_y_gridlines includes zero for all-positive data
test_that("3. pretty_y_gridlines includes zero for all-positive data", {
  result <- pretty_y_gridlines(positive_data)
  expect_true(min(result) <= 0)  # Ensure it includes zero
  expect_true(max(result) >= 15)
})

# Test that pretty_y_gridlines correctly handles mixed positive and negative values
test_that("4. pretty_y_gridlines handles mixed positive and negative values", {
  result <- pretty_y_gridlines(mixed_data)
  expect_true(min(result) <= -10)
  expect_true(max(result) >= 20)
})

##get_years()--------------------------------------------------------------------------

# Create sample data for testing
sample_data <- tibble::tibble(
  Years_num = c(2015, 2016, 2015, 2017, NA, 2018),
  Years = c("2015", "2016", "2015", "2017", NA, "2018")
)

# Test that get_years returns unique numeric years in order
test_that("1. get_years returns unique numeric years in ascending order", {
  result <- get_years(sample_data, type = "numeric")
  expect_equal(result, c(2015, 2016, 2017, 2018, NA))
})

# Test that get_years returns unique character years in order
test_that("2. get_years returns unique character years in ascending order", {
  result <- get_years(sample_data, type = "character")
  expect_equal(result, c("2015", "2016", "2017", "2018", NA))
})

##format_axes()--------------------------------------------------------------
