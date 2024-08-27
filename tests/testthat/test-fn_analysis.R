library(testthat)

# Define a test suite for calculate_change_from_prev_yr
test_that("calculate_change_from_prev_yr works correctly", {
  # Sample data frame for testing
  data <- data.frame(
    `LA and Regions` = c(rep("Region1", 3), rep("Region2", 3)),
    Years = c("2023", "2022", "2021", "2023", "2022", "2021"),
    Years_num = c(2023, 2022, 2021, 2023, 2022, 2021),
    values_num = c(100, 90, 80, 190, 195, 200),
    check.names = FALSE
  )

  # 1. Basic Functionality Test
  # Expected result for the provided sample data
  expected_result <- data.frame(
    `LA and Regions` = c("Region1", "Region2"),
    Years = c("Change from previous year", "Change from previous year"),
    Years_num = c(2022, 2022),
    values_num = c(10, -5), # 2022-2021 change: Region1 = 90-80, Region2 = 195-190
    check.names = FALSE
  )
  result <- calculate_change_from_prev_yr(data)
  expect_equal(result, expected_result)


  # 2. Value is NA
  data_with_na <- data
  # Introduce an NA in the values_num column for Region1 in 2022
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

  # 3. Test with NA values in `Years_num`
  data_with_na_years <- data
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

  # 4. Test with only one year of data (should return an empty result)
  data_one_year <- data |>
    dplyr::filter(Years_num == 2023)
  result_one_year <- calculate_change_from_prev_yr(data_one_year)
  expect_true(nrow(result_one_year) == 0)

  # 5. Test with an empty data frame (should return an empty result)
  empty_data <- data[0, ]
  result_empty <- calculate_change_from_prev_yr(empty_data)
  expect_true(nrow(result_empty) == 0)

  # 6. Test with multiple LAs and Regions (already covered in the sample data)
  expect_equal(nrow(result), 2) # Two regions should be processed correctly
})

test_that("calculate_trend works correctly", {
  # 1. Test with positive values
  positive_change <- c(5)
  expected_positive <- c("Increase")
  expect_equal(calculate_trend(positive_change), expected_positive)

  # 2. Test with negative values
  negative_change <- c(-2)
  expected_negative <- c("Decrease")
  expect_equal(calculate_trend(negative_change), expected_negative)

  # 3. Test with zero values
  zero_change <- c(0)
  expected_zero <- c("No change")
  expect_equal(calculate_trend(zero_change), expected_zero)

  # 5. Test with NA values
  na_change <- c(NA)
  expected_na <- NA_character_
  expect_equal(calculate_trend(na_change), expected_na)

  # 6. Test with an empty vector
  empty_change <- numeric(0)
  expected_empty <- character(0)
  expected_warning <- "The change_since_prev value looks wrong:"
  expect_warning(
    expect_equal(calculate_trend(empty_change), expected_empty),
    expected_warning
  )


  # 7. Test with multiple values
  multiple_val <- c(-3, 5)
  expect_warning(calculate_trend(multiple_val), expected_warning)
})


# Define test cases for calculate_quartile_band
test_that("calculate_quartile_band works correctly", {
  # Define quartile bands for testing
  quartile_bands <- c(
    "0%" = 0,
    "25%" = 25,
    "50%" = 50,
    "75%" = 75,
    "100%" = 100
  )

  # 1. Test when indicator_val is within each quartile
  expect_equal(calculate_quartile_band(10, quartile_bands), "A") # within 0%-25%
  expect_equal(calculate_quartile_band(30, quartile_bands), "B") # within 25%-50%
  expect_equal(calculate_quartile_band(60, quartile_bands), "C") # within 50%-75%
  expect_equal(calculate_quartile_band(80, quartile_bands), "D") # within 75%-100%

  # 2. Test edge cases at the boundaries
  expect_equal(calculate_quartile_band(0, quartile_bands), "A") # exactly 0%
  expect_equal(calculate_quartile_band(25, quartile_bands), "A") # exactly 25%
  expect_equal(calculate_quartile_band(50, quartile_bands), "B") # exactly 50%
  expect_equal(calculate_quartile_band(75, quartile_bands), "C") # exactly 75%
  expect_equal(calculate_quartile_band(100, quartile_bands), "D") # exactly 100%

  # 3. Test when indicator_val is NA
  expect_equal(calculate_quartile_band(NA, quartile_bands), NA_character_)

  # 4. Test when indicator_val is outside the defined quartile bands
  expect_equal(calculate_quartile_band(-10, quartile_bands), "Error") # less than 0%
  expect_equal(calculate_quartile_band(110, quartile_bands), "Error") # more than 100%

  # 5. Test when quartile_bands are non-standard (e.g., non-uniform)
  custom_quartile_bands <- c(
    "0%" = 10,
    "25%" = 20,
    "50%" = NA,
    "75%" = 40,
    "100%" = 50
  )
  expect_equal(calculate_quartile_band(15, custom_quartile_bands), "A") # within 10%-20%
  expect_equal(calculate_quartile_band(25, custom_quartile_bands), "Error") # within 20%-NA%
  expect_equal(calculate_quartile_band(35, custom_quartile_bands), "Error") # within NA%-40%
  expect_equal(calculate_quartile_band(45, custom_quartile_bands), "D") # within 40%-50%
  expect_equal(calculate_quartile_band(5, custom_quartile_bands), "Error") # less than 10%
  expect_equal(calculate_quartile_band(55, custom_quartile_bands), "Error") # more than 50%

  # 6. Test with an empty vector
  expect_warning(
    expect_equal(calculate_quartile_band(numeric(0), quartile_bands), character(0)),
    regexp = "Indicator value is empty; returning an empty character vector."
  )
})


library(testthat)
library(dplyr)

test_that("get_quartile_band_cell_colour works correctly", {
  # Sample data for table_stats
  table_stats <- data.frame(
    Polarity = c("Low", "High", NA, "-", "Low"),
    `Quartile Banding` = c("A", "D", "B", "C", "D"),
    check.names = FALSE
  )

  polarity_colours <- polarity_colours_df()

  # Expected results
  expected_colours <- c("green", "green", "none", "none", "red")

  # Test with matching polarity and quartile bands
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

  # Test where multiple polarity in table_stats
  table_stats_multiple_polarity <- data.frame(
    Polarity = c("Low", "High")
    # Missing `Quartile Banding` column
  )
  expect_error(
    get_quartile_band_cell_colour(polarity_colours, table_stats_multiple_polarity),
    "the condition has length > 1"
  )

  # Test when there is no matching row
  table_stats_no_match <- data.frame(
    Polarity = c("Unknown"),
    `Quartile Banding` = c("A"),
    check.names = FALSE
  )
  expect_warning(
    expect_equal(
      get_quartile_band_cell_colour(
        polarity_colours,
        table_stats_no_match
      ),
      character(0)
    ),
    "Unexpected polarity value"
  )

  # Test with missing values in polarity_colours
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
