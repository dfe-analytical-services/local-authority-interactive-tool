# calculate_change_from_prev_yr() ---------------------------------------------
# Sample data frame for testing
data_change <- data.frame(
  `LA and Regions` = c(rep("Region1", 3), rep("Region2", 3)),
  Measure = rep("Measure1", 6),
  Years = c("2023", "2022", "2021", "2023", "2022", "2021"),
  Years_num = c(2023, 2022, 2021, 2023, 2022, 2021),
  values_num = c(100, 90, 80, 190, 195, 200),
  check.names = FALSE
)
# Test 1: Basic Functionality Test
test_that("1. calculate_change_from_prev_yr computes changes correctly", {
  expected_result <- data.frame(
    `LA and Regions` = c("Region1", "Region2"),
    Measure = rep("Measure1", 2),
    Years = c("Change from previous year", "Change from previous year"),
    Years_num = c(2022, 2022),
    values_num = c(10, -5), # 2022-2021 change: Region1 = 90-80, Region2 = 195-190
    Values = c(NA, NA),
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
    Measure = rep("Measure1", 2),
    Years = c("Change from previous year", "Change from previous year"),
    Years_num = c(2022, 2022),
    values_num = c(NA, -5), # NA handling should propagate to the calculation
    Values = c(NA, NA),
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
    Measure = rep("Measure1", 2),
    Years = c("Change from previous year"),
    Years_num = c(2021, 2022),
    values_num = c(20, -5),
    Values = c(NA, NA),
    check.names = FALSE
  )

  expect_warning(
    expect_equal(
      calculate_change_from_prev_yr(data_with_na_years),
      expected_result_na_years
    ),
    "Missing year found in the change from previous year calculation,"
  )
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


# calculate_quartile_band() ---------------------------------------------------
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
  expect_equal(calculate_quartile_band(10, quartile_bands, "Low"), "A") # within 0%-25%
  expect_equal(calculate_quartile_band(30, quartile_bands, "Low"), "B") # within 25%-50%
  expect_equal(calculate_quartile_band(60, quartile_bands, "Low"), "C") # within 50%-75%
  expect_equal(calculate_quartile_band(80, quartile_bands, "Low"), "D") # within 75%-100%
  expect_equal(calculate_quartile_band(10, quartile_bands, "High"), "D")
  expect_equal(calculate_quartile_band(30, quartile_bands, "High"), "C")
  expect_equal(calculate_quartile_band(60, quartile_bands, "High"), "B")
  expect_equal(calculate_quartile_band(80, quartile_bands, "High"), "A")
})

# 2. Test edge cases at the boundaries
test_that("2. calculate_quartile_band handles boundary values correctly", {
  expect_equal(calculate_quartile_band(0, quartile_bands, "Low"), "A") # exactly 0%
  expect_equal(calculate_quartile_band(25, quartile_bands, "Low"), "A") # exactly 25%
  expect_equal(calculate_quartile_band(50, quartile_bands, "Low"), "B") # exactly 50%
  expect_equal(calculate_quartile_band(75, quartile_bands, "Low"), "C") # exactly 75%
  expect_equal(calculate_quartile_band(100, quartile_bands, "Low"), "D") # exactly 100%
})

# 3. Test when indicator_val is NA
test_that("3. calculate_quartile_band returns NA_character_ for NA values", {
  expect_equal(calculate_quartile_band(NA, quartile_bands, "High"), NA_character_)
})

# 4. Test when indicator_val is outside the defined quartile bands
test_that("4. calculate_quartile_band returns 'Error' for values outside the defined quartile bands", {
  expect_warning(
    expect_equal(calculate_quartile_band(-10, quartile_bands, "Low"), "Error"), # less than 0%
    "Unexpected Quartile Banding"
  )
  expect_warning(
    expect_equal(calculate_quartile_band(110, quartile_bands, "High"), "Error"), # more than 100%
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
      expect_equal(calculate_quartile_band(15, custom_quartile_bands, "Low"), "A") # within 10%-20%
      expect_equal(calculate_quartile_band(25, custom_quartile_bands, "Low"), "Error") # within 20%-NA%
      expect_equal(calculate_quartile_band(35, custom_quartile_bands, "High"), "Error") # within NA%-40%
      expect_equal(calculate_quartile_band(45, custom_quartile_bands, "Low"), "D") # within 40%-50%
      expect_equal(calculate_quartile_band(5, custom_quartile_bands, "High"), "Error") # less than 10%
      expect_equal(calculate_quartile_band(55, custom_quartile_bands, "Low"), "Error") # more than 50%
      expect_equal(calculate_quartile_band(20, custom_quartile_bands, "High"), "D") # more than 50%
    },
    "Unexpected Quartile Banding"
  )
})

# 6. Test with an empty vector
test_that("6. calculate_quartile_band returns an empty vector for an empty input and warns", {
  expect_warning(
    expect_equal(calculate_quartile_band(numeric(0), quartile_bands, "High"), character(0)),
    regexp = "Indicator value is empty; returning an empty character vector."
  )
})


# get_quartile_band_cell_colour() ---------------------------------------------
# Sample data for table_stats
table_stats <- data.frame(
  Polarity = c("Low", "High", NA, "-", "Low"),
  `Quartile Banding` = c("A", "A", "B", "C", "D"),
  check.names = FALSE
)

# Expected results
expected_colours <- c("#00703c", "#00703c", "none", "none", "#d4351c")

# 1. Test with matching polarity and quartile bands
test_that("1. get_quartile_band_cell_colour returns correct colours for matching polarity and quartile bands", {
  # Low A
  result <- get_quartile_band_cell_colour(
    table_stats[1, "Polarity"],
    table_stats[1, "Quartile Banding"]
  )
  expect_equal(result, expected_colours[1])

  # High D
  result <- get_quartile_band_cell_colour(
    table_stats[2, "Polarity"],
    table_stats[2, "Quartile Banding"]
  )
  expect_equal(result, expected_colours[2])

  # NA B
  result <- get_quartile_band_cell_colour(
    table_stats[3, "Polarity"],
    table_stats[3, "Quartile Banding"]
  )
  expect_equal(result, expected_colours[3])

  # - C
  result <- get_quartile_band_cell_colour(
    table_stats[4, "Polarity"],
    table_stats[4, "Quartile Banding"]
  )
  expect_equal(result, expected_colours[4])

  # Low D
  result <- get_quartile_band_cell_colour(
    table_stats[5, "Polarity"],
    table_stats[5, "Quartile Banding"]
  )
  expect_equal(result, expected_colours[5])
})

# 2. Test with multiple polarity values in table_stats
test_that("2. get_quartile_band_cell_colour throws error when missing quartile bands", {
  table_stats_multiple_polarity <- data.frame(
    Polarity = c("Low", "High")
    # Missing `Quartile Banding` column
  )
  expect_error(
    get_quartile_band_cell_colour(
      table_stats_multiple_polarity$Polarity,
      table_stats_multiple_polarity$`Quartile Banding`
    ),
    "argument is of length zero"
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
      get_quartile_band_cell_colour(
        table_stats_no_match$Polarity,
        table_stats_no_match$`Quartile Banding`
      ),
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
      get_quartile_band_cell_colour(
        table_stats_no_match$Polarity,
        table_stats_no_match$`Quartile Banding`
      ),
      NULL
    ),
    "Unexpected Quartile Banding value: F"
  )
})

# 5. Test with missing values in polarity_colours
test_that("5. get_quartile_band_cell_colour handles NA values in polarity_colours correctly", {
  table_stats_with_na <- data.frame(
    Polarity = c(NA),
    `Quartile Banding` = c("D"),
    check.names = FALSE
  )
  result_none <- get_quartile_band_cell_colour(
    table_stats_with_na$Polarity,
    table_stats_with_na$`Quartile Banding`
  )
  expect_equal(result_none, "none")
})


# calculate_rank() ------------------------------------------------------------
# 1. Test with normal ranking
test_that("1. calculate_rank assigns correct ranks for normal values", {
  df_normal <- data.frame(
    values_num = c(10, 20, 30, 40, 50)
  )
  expected_normal <- data.frame(
    values_num = c(10, 20, 30, 40, 50),
    rank = c("1", "2", "3", "4", "5")
  )
  result_normal <- calculate_rank(df_normal, "Low")
  expect_equal(result_normal, expected_normal)
})

# 2. Test handling of ties
test_that("2. calculate_rank handles ties correctly", {
  df_ties <- data.frame(
    values_num = c(10, 20, 20, 30, 40)
  )
  expected_ties <- data.frame(
    values_num = c(10, 20, 20, 30, 40),
    rank = c("5", "3", "3", "2", "1")
  )
  result_ties <- calculate_rank(df_ties, "High")
  expect_equal(result_ties, expected_ties)
})

# 3. Test with missing values
test_that("3. calculate_rank handles missing values properly", {
  df_missing <- data.frame(
    values_num = c(10, NA, 30, 20, NA)
  )
  expected_missing <- data.frame(
    values_num = c(10, NA, 30, 20, NA),
    rank = c("1", NA, "3", "2", NA)
  )
  result_missing <- calculate_rank(df_missing, "Low")
  expect_equal(result_missing, expected_missing)
})

# 4. Test with an empty data frame
test_that("4. calculate_rank returns an empty data frame when input is empty", {
  df_empty <- data.frame(values_num = numeric(0))
  expected_empty <- data.frame(values_num = numeric(0), rank = numeric(0))

  # Test that a warning is raised and the result matches the expected empty data frame
  expect_warning(
    result_empty <- calculate_rank(df_empty, "High"),
    "The filtered data frame is empty; returning an empty result."
  )
  expect_equal(result_empty, expected_empty)
})

# 5. Test with all missing values
test_that("5. calculate_rank returns NA for rank when all values are missing", {
  df_all_missing <- data.frame(
    values_num = c(NA, NA, NA)
  )
  expected_all_missing <- data.frame(
    values_num = c(NA, NA, NA),
    rank = c(NA_character_, NA_character_, NA_character_)
  )
  result_all_missing <- calculate_rank(df_all_missing, "Low")
  expect_equal(result_all_missing, expected_all_missing)
})
