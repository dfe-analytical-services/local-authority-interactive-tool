# filter_la_regions() ---------------------------------------------------------
# Dummy data
filter_la_data <- data.frame(
  `LA and Regions` = c("LA1", "Region1", "LA2", "Region2"),
  Years = c(2020, 2021, 2020, 2021),
  Population = c(1000, 2000, 1500, 2500),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

test_that("1. filter_la_regions filters data based on LA and Regions", {
  result <- filter_la_regions(filter_la_data, filter_col = c("LA1", "Region1"))
  expected <- data.frame(
    `LA and Regions` = c("LA1", "Region1"),
    Years = c(2020, 2021),
    Population = c(1000, 2000),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(result, expected)
})

test_that("2. filter_la_regions filters data based on the latest year", {
  result <- filter_la_regions(filter_la_data,
    filter_col = c("LA1", "Region1"),
    latest = TRUE
  )
  expected <- data.frame(
    `LA and Regions` = c("Region1"),
    Years = c(2021),
    Population = c(2000),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(result, expected)
})

test_that("3. filter_la_regions returns a specific column", {
  result <- filter_la_regions(filter_la_data,
    filter_col = c("LA1", "Region1"),
    pull_col = "Population"
  )
  expected <- c(1000, 2000)

  expect_equal(result, expected)
})

test_that("4. filter_la_regions returns an empty data frame if no matches", {
  expected <- filter_la_data |>
    dplyr::slice(0)

  expect_warning(
    expect_equal(
      filter_la_regions(filter_la_data, filter_col = c("LA3")),
      expected
    ),
    "Filter value doesn't exist in LA and Regions"
  )
})

test_that("5. filter_la_regions filters by latest year and returns a specific column", {
  result <- filter_la_regions(filter_la_data,
    filter_col = c("LA1", "Region1"),
    latest = TRUE, pull_col = "Population"
  )
  expected <- 2000

  expect_equal(result, expected)
})

test_that("6. filter_la_regions returns full data frame when pull_col is NA", {
  result <- filter_la_regions(filter_la_data,
    filter_col = c("LA2", "Region2"),
    latest = FALSE, pull_col = NA
  )
  expected <- data.frame(
    `LA and Regions` = c("LA2", "Region2"),
    Years = c(2020, 2021),
    Population = c(1500, 2500),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(result, expected)
})


test_that("7. filter_la_regions returns error when wrong pull_col", {
  expect_error(
    filter_la_regions(filter_la_data,
      filter_col = c("LA2", "Region2"),
      latest = FALSE,
      pull_col = "non_existent"
    ),
    "Column `non_existent` doesn't exist."
  )
})

test_that("8. filter_la_regions returns warning when data is empty", {
  empty_df <- filter_la_data |>
    dplyr::slice(0)

  expect_warning(
    expect_equal(
      filter_la_regions(empty_df,
        filter_col = c("LA2", "Region2"),
        latest = FALSE
      ),
      empty_df
    ),
    "Dataframe seems empty"
  )
})

test_that("8. filter_la_regions returns an empty df when all years are NA", {
  data_missing_yr <- filter_la_data
  data_missing_yr$Years[2] <- NA

  expect_equal(
    filter_la_regions(data_missing_yr,
      filter_col = c("Region1"),
      latest = TRUE
    ),
    filter_la_data |>
      dplyr::slice(0)
  )
})


# pretty_num_table() ----------------------------------------------------------
# Dummy data
# Sample data frame
pretty_tbl_data <- data.frame(
  A = c(1234.56, 7890.12),
  B = c(345.67, 8901.23),
  C = c("foo", "bar"),
  stringsAsFactors = FALSE
)

test_that("1. pretty_num_table formats all numeric cols when no include/ exclude cols", {
  # Apply pretty_num_table
  result <- pretty_num_table(pretty_tbl_data)

  # Expected result
  expected <- data.frame(
    A = c("1,234.56", "7,890.12"),
    B = c("345.67", "8,901.23"),
    C = c("foo", "bar"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("2. pretty_num_table processes only the specified columns with include_columns", {
  # Apply pretty_num_table
  result <- pretty_num_table(pretty_tbl_data, include_columns = c("A"))

  # Expected result
  expected <- data.frame(
    A = c("1,234.56", "7,890.12"),
    B = c(345.67, 8901.23),
    C = c("foo", "bar"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("3. pretty_num_table excludes specified columns with exclude_columns", {
  # Apply pretty_num_table
  result <- pretty_num_table(pretty_tbl_data,
    exclude_columns = c("A")
  )

  # Expected result
  expected <- data.frame(
    A = c(1234.56, 7890.12),
    B = c("345.67", "8,901.23"),
    C = c("foo", "bar"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})


test_that("4. pretty_num_table handles both include_columns and exclude_columns", {
  # Sample data frame
  data <- pretty_tbl_data |>
    cbind(
      data.frame(
        D = c(4567.89, 1234.56),
        stringsAsFactors = FALSE
      )
    )

  # Apply pretty_num_table
  result <- pretty_num_table(data,
    include_columns = c("A", "D"),
    exclude_columns = c("B")
  )

  # Expected result
  expected <- data.frame(
    A = c("1,234.56", "7,890.12"),
    B = c(345.67, 8901.23),
    C = c("foo", "bar"),
    D = c("4,567.89", "1,234.56"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("5. pretty_num_table handles an empty data frame", {
  # Empty data frame
  data <- pretty_tbl_data |>
    dplyr::slice(0)

  expect_warning(
    pretty_num_table(data),
    "Data seems to be empty"
  )
})

test_that("6. pretty_num_table handles NAs", {
  # Empty data frame
  data_na <- pretty_tbl_data
  data_na$A[2] <- NA

  expect_equal(
    pretty_num_table(data_na)$A[2],
    NA_character_
  )
})

test_that("7. pretty_num_table can take pretty_num arguements", {
  # Empty data frame
  result <- pretty_num_table(pretty_tbl_data,
    suffix = "_test",
    dp = 5,
    gbp = TRUE
  )

  expected_result <- data.frame(
    A = c("£1,234.56_test", "£7,890.12_test"),
    B = c("£345.67_test", "£8,901.23_test"),
    C = c("foo", "bar")
  )

  expect_equal(
    result,
    expected_result
  )
})
