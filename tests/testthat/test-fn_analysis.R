# calculate_change_from_prev_yr -----------------------------------------------
# Sample data frame for testing
data_change <- data.frame(
  `LA and Regions` = c(rep("Region1", 3), rep("Region2", 3)),
  Years = c("2023", "2022", "2021", "2023", "2022", "2021"),
  Years_num = c(2023, 2022, 2021, 2023, 2022, 2021),
  values_num = c(100, 90, 80, 190, 195, 200),
  check.names = FALSE
)
# Test 1: Basic Functionality Test
test_that("1. calculate_change_from_prev_yr computes changes correctly", {
  expected_result <- data.frame(
    `LA and Regions` = c("Region1", "Region2"),
    Years = c("Change from previous year", "Change from previous year"),
    Years_num = c(2022, 2022),
    values_num = c(10, -5), # 2022-2021 change: Region1 = 90-80, Region2 = 195-190
    check.names = FALSE
  )
  result <- calculate_change_from_prev_yr(data_change)
  expect_equal(result, expected_result)
})

# Test 2: Value is NA
test_that("2. calculate_change_from_prev_yr handles NA values in values_num", {
  data_with_na <- data_change
  data_with_na$values_num[2] <- NA
  expected_result_na <- data.frame(
    `LA and Regions` = c("Region1", "Region2"),
    Years = c("Change from previous year", "Change from previous year"),
    Years_num = c(2022, 2022),
    values_num = c(NA, -5), # NA handling should propagate to the calculation
    check.names = FALSE
  )
  result_na <- calculate_change_from_prev_yr(data_with_na)
  expect_equal(result_na, expected_result_na)
})

# Test 3: Test with NA values in `Years_num`
test_that("3. calculate_change_from_prev_yr handles NA values in Years_num", {
  data_with_na_years <- data_change
  data_with_na_years$Years_num[2] <- NA # Introduce an NA in the Years_num column for Region1
  expected_result_na_years <- data.frame(
    `LA and Regions` = c("Region1", "Region2"),
    Years = c("Change from previous year"),
    Years_num = c(2021, 2022),
    values_num = c(NA, -5), # Region2 should still be calculated correctly
    check.names = FALSE
  )
  result_na_years <- calculate_change_from_prev_yr(data_with_na_years)
  expect_equal(result_na_years, expected_result_na_years)
})

# Test 4: Test with only one year of data
test_that("4. calculate_change_from_prev_yr returns empty result with only one year of data", {
  data_one_year <- data_change |> dplyr::filter(Years_num == 2023)
  result_one_year <- calculate_change_from_prev_yr(data_one_year)
  expect_true(nrow(result_one_year) == 0)
})

# Test 5: Test with an empty data frame
test_that("5. calculate_change_from_prev_yr returns empty result with an empty data frame", {
  empty_data <- data_change[0, ]
  result_empty <- calculate_change_from_prev_yr(empty_data)
  expect_true(nrow(result_empty) == 0)
})

# Test 6: Test with multiple LAs and Regions (already covered in the sample data)
test_that("6. calculate_change_from_prev_yr processes multiple LAs and Regions correctly", {
  result <- calculate_change_from_prev_yr(data_change)
  expect_equal(nrow(result), 2) # Two regions should be processed correctly
})



# calculate_trend -------------------------------------------------------------
# Test 1: Positive values
test_that("1. calculate_trend returns 'Increase' for positive values", {
  positive_change <- c(5)
  expected_positive <- c("Increase")
  expect_equal(calculate_trend(positive_change), expected_positive)
})

# Test 2: Negative values
test_that("2. calculate_trend returns 'Decrease' for negative values", {
  negative_change <- c(-2)
  expected_negative <- c("Decrease")
  expect_equal(calculate_trend(negative_change), expected_negative)
})

# Test 3: Zero values
test_that("3. calculate_trend returns 'No change' for zero values", {
  zero_change <- c(0)
  expected_zero <- c("No change") # Adjusted to match the function output
  expect_equal(calculate_trend(zero_change), expected_zero)
})

# Test 4: NA values
test_that("4. calculate_trend returns NA_character_ for NA values", {
  na_change <- c(NA)
  expected_na <- NA_character_
  expect_equal(calculate_trend(na_change), expected_na)
})

# Test 5: Empty vector
test_that("5.calculate_trend returns an empty vector for an empty input and warns", {
  empty_change <- numeric(0)
  expected_empty <- character(0)
  expected_warning <- "The change_since_prev value looks wrong:"
  expect_warning(
    expect_equal(calculate_trend(empty_change), expected_empty),
    expected_warning
  )
})

# Test 6: Multiple values
test_that("6. calculate_trend returns a warning for multiple values and no trend", {
  multiple_val <- c(-3, 5)
  expected_warning <- "The change_since_prev value looks wrong:"
  expect_warning(calculate_trend(multiple_val), expected_warning)
})


# calculate_quartile_band -----------------------------------------------------
# Define quartile bands for testing
quartile_bands <- c(
  "0%" = 0,
  "25%" = 25,
  "50%" = 50,
  "75%" = 75,
  "100%" = 100
)

# 1. Test when indicator_val is within each quartile
test_that("1. calculate_quartile_band returns correct quartile band for values within each quartile", {
  expect_equal(calculate_quartile_band(10, quartile_bands), "A") # within 0%-25%
  expect_equal(calculate_quartile_band(30, quartile_bands), "B") # within 25%-50%
  expect_equal(calculate_quartile_band(60, quartile_bands), "C") # within 50%-75%
  expect_equal(calculate_quartile_band(80, quartile_bands), "D") # within 75%-100%
})

# 2. Test edge cases at the boundaries
test_that("2. calculate_quartile_band handles boundary values correctly", {
  expect_equal(calculate_quartile_band(0, quartile_bands), "A") # exactly 0%
  expect_equal(calculate_quartile_band(25, quartile_bands), "A") # exactly 25%
  expect_equal(calculate_quartile_band(50, quartile_bands), "B") # exactly 50%
  expect_equal(calculate_quartile_band(75, quartile_bands), "C") # exactly 75%
  expect_equal(calculate_quartile_band(100, quartile_bands), "D") # exactly 100%
})

# 3. Test when indicator_val is NA
test_that("3. calculate_quartile_band returns NA_character_ for NA values", {
  expect_equal(calculate_quartile_band(NA, quartile_bands), NA_character_)
})

# 4. Test when indicator_val is outside the defined quartile bands
test_that("4. calculate_quartile_band returns 'Error' for values outside the defined quartile bands", {
  expect_warning(
    expect_equal(calculate_quartile_band(-10, quartile_bands), "Error"), # less than 0%
    "Unexpected Quartile Banding"
  )
  expect_warning(
    expect_equal(calculate_quartile_band(110, quartile_bands), "Error"), # more than 100%
    "Unexpected Quartile Banding"
  )
})

# 5. Test when quartile_bands are non-standard (e.g., non-uniform)
test_that("5. calculate_quartile_band handles non-standard quartile bands correctly", {
  custom_quartile_bands <- c(
    "0%" = 10,
    "25%" = 20,
    "50%" = NA,
    "75%" = 40,
    "100%" = 50
  )

  expect_warning(
    {
      expect_equal(calculate_quartile_band(15, custom_quartile_bands), "A") # within 10%-20%
      expect_equal(calculate_quartile_band(25, custom_quartile_bands), "Error") # within 20%-NA%
      expect_equal(calculate_quartile_band(35, custom_quartile_bands), "Error") # within NA%-40%
      expect_equal(calculate_quartile_band(45, custom_quartile_bands), "D") # within 40%-50%
      expect_equal(calculate_quartile_band(5, custom_quartile_bands), "Error") # less than 10%
      expect_equal(calculate_quartile_band(55, custom_quartile_bands), "Error") # more than 50%
    },
    "Unexpected Quartile Banding"
  )
})

# 6. Test with an empty vector
test_that("6. calculate_quartile_band returns an empty vector for an empty input and warns", {
  expect_warning(
    expect_equal(calculate_quartile_band(numeric(0), quartile_bands), character(0)),
    regexp = "Indicator value is empty; returning an empty character vector."
  )
})


# get_quartile_band_cell_colour -----------------------------------------------
# Sample data for table_stats
table_stats <- data.frame(
  Polarity = c("Low", "High", NA, "-", "Low"),
  `Quartile Banding` = c("A", "D", "B", "C", "D"),
  check.names = FALSE
)

polarity_colours <- polarity_colours_df() # Assuming this function is defined elsewhere

# Expected results
expected_colours <- c("green", "green", "none", "none", "red")

# 1. Test with matching polarity and quartile bands
test_that("1. get_quartile_band_cell_colour returns correct colours for matching polarity and quartile bands", {
  # Low A
  result <- get_quartile_band_cell_colour(polarity_colours, table_stats[1, ])
  expect_equal(result, expected_colours[1])

  # High D
  result <- get_quartile_band_cell_colour(polarity_colours, table_stats[2, ])
  expect_equal(result, expected_colours[2])

  # NA B
  result <- get_quartile_band_cell_colour(polarity_colours, table_stats[3, ])
  expect_equal(result, expected_colours[3])

  # - C
  result <- get_quartile_band_cell_colour(polarity_colours, table_stats[4, ])
  expect_equal(result, expected_colours[4])

  # Low D
  result <- get_quartile_band_cell_colour(polarity_colours, table_stats[5, ])
  expect_equal(result, expected_colours[5])
})

# 2. Test with multiple polarity values in table_stats
test_that("2. get_quartile_band_cell_colour throws error when multiple polarities", {
  table_stats_multiple_polarity <- data.frame(
    Polarity = c("Low", "High")
    # Missing `Quartile Banding` column
  )
  expect_error(
    get_quartile_band_cell_colour(polarity_colours, table_stats_multiple_polarity),
    "the condition has length > 1"
  )
})

# 3. Test when there is no matching row for Polarity
test_that("3. get_quartile_band_cell_colour returns empty vector and a warning when no matching polarity", {
  table_stats_no_match <- data.frame(
    Polarity = c("Unknown"),
    `Quartile Banding` = c("A"),
    check.names = FALSE
  )
  expect_warning(
    expect_equal(
      get_quartile_band_cell_colour(polarity_colours, table_stats_no_match),
      NULL
    ),
    "Unexpected polarity value: Unknown"
  )
})

# 4. Test when there is no matching row for Quartile band
test_that("4. get_quartile_band_cell_colour returns empty vector with warning when no matching quartile band ", {
  table_stats_no_match <- data.frame(
    Polarity = c("High"),
    `Quartile Banding` = c("F"),
    check.names = FALSE
  )
  expect_warning(
    expect_equal(
      get_quartile_band_cell_colour(polarity_colours, table_stats_no_match),
      NULL
    ),
    "Unexpected Quartile Banding: F"
  )
})

# 5. Test with missing values in polarity_colours
test_that("5. get_quartile_band_cell_colour handles NA values in polarity_colours correctly", {
  polarity_colours_na <- polarity_colours
  polarity_colours_na$cell_colour[is.na(polarity_colours_na$polarity)] <- NA

  table_stats_with_na <- data.frame(
    Polarity = c(NA),
    `Quartile Banding` = c("D"),
    check.names = FALSE
  )
  result_na <- get_quartile_band_cell_colour(polarity_colours_na, table_stats_with_na)
  expect_equal(result_na, c(NA_character_))
})


# calculate_rank --------------------------------------------------------------
# 1. Test with normal ranking
test_that("1. calculate_rank assigns correct ranks for normal values", {
  df_normal <- data.frame(
    values_num = c(10, 20, 30, 40, 50)
  )
  expected_normal <- data.frame(
    values_num = c(10, 20, 30, 40, 50),
    rank = c(1, 2, 3, 4, 5)
  )
  result_normal <- calculate_rank(df_normal)
  expect_equal(result_normal, expected_normal)
})

# 2. Test handling of ties
test_that("2. calculate_rank handles ties correctly", {
  df_ties <- data.frame(
    values_num = c(10, 20, 20, 30, 40)
  )
  expected_ties <- data.frame(
    values_num = c(10, 20, 20, 30, 40),
    rank = c(1, 2, 2, 4, 5)
  )
  result_ties <- calculate_rank(df_ties)
  expect_equal(result_ties, expected_ties)
})

# 3. Test with missing values
test_that("3. calculate_rank handles missing values properly", {
  df_missing <- data.frame(
    values_num = c(10, NA, 30, 20, NA)
  )
  expected_missing <- data.frame(
    values_num = c(10, NA, 30, 20, NA),
    rank = c(1, NA, 3, 2, NA)
  )
  result_missing <- calculate_rank(df_missing)
  expect_equal(result_missing, expected_missing)
})

# 4. Test with an empty data frame
test_that("4. calculate_rank returns an empty data frame when input is empty", {
  df_empty <- data.frame(
    values_num = numeric(0)
  )
  expected_empty <- data.frame(
    values_num = numeric(0),
    rank = numeric(0)
  )
  result_empty <- calculate_rank(df_empty)
  expect_equal(result_empty, expected_empty)
})

# 5. Test with all missing values
test_that("5. calculate_rank returns NA for rank when all values are missing", {
  df_all_missing <- data.frame(
    values_num = c(NA, NA, NA)
  )
  expected_all_missing <- data.frame(
    values_num = c(NA, NA, NA),
    rank = c(NA_real_, NA_real_, NA_real_)
  )
  result_all_missing <- calculate_rank(df_all_missing)
  expect_equal(result_all_missing, expected_all_missing)
})
