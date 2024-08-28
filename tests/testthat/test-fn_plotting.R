# get_yaxis_title() -----------------------------------------------------------
# Dummy data
y_axis_data <- data.frame(
  y_axis_name = c("Temperature", "Pressure", "Temperature", "Humidity"),
  other_column = c(1, 2, 3, 4)
)

# Test 1: Check if the function returns unique y_axis_name values
test_that("1. get_yaxis_title returns unique y_axis_name values", {
  result <- get_yaxis_title(y_axis_data)
  expected <- c("Temperature", "Pressure", "Humidity")
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
  result <- get_plot_title("Temperature", "Time")
  expected <- "Temperature - Time"
  expect_equal(result, expected)
})

# Test 2: Check if the function handles empty strings
test_that("2. get_plot_title handles empty strings", {
  result <- get_plot_title("", "")
  expected <- " - "
  expect_equal(result, expected)
})

# Test 3: Check if the function handles NULL values
test_that("3. get_plot_title handles NULL values", {
  result <- get_plot_title(NA, NA)
  expected <- "NA - NA"
  expect_equal(result, expected)
})

# Test 4: Check if the function handles numeric values
test_that("4. get_plot_title handles numeric values", {
  result <- get_plot_title(123, 456)
  expected <- "123 - 456"
  expect_equal(result, expected)
})
