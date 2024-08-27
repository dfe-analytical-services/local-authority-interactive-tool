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
