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


# dfe_reactable() -------------------------------------------------------------
test_that("1. dfe_reactable basic functionality", {
  data <- data.frame(
    A = c(1, 2, 3),
    B = c("X", "Y", "Z")
  )

  result <- dfe_reactable(data)

  expect_s3_class(result, "reactable")
  expect_true("reactable" %in% class(result))
})

test_that("2. dfe_reactable with custom options", {
  data <- data.frame(
    A = c(1, 2, 3),
    B = c("X", "Y", "Z")
  )

  result <- dfe_reactable(data, sortable = TRUE, filterable = TRUE)

  expect_s3_class(result, "reactable")

  options <- result$x$tag

  # Check if the 'sortable' and 'filterable' options are present and set correctly
  expect_true(is.list(options))
  expect_true(grepl('filterable="TRUE"', options))
  expect_true(grepl("headerClassName = &quot;bar-sort-header&quot;", options))
})


test_that("3. dfe_reactable with an empty data frame", {
  data <- data.frame(A = numeric(), B = character(), stringsAsFactors = FALSE)

  result <- dfe_reactable(data)
  result_data <- result$x$tag$attribs$data |>
    jsonlite::fromJSON() |>
    data.frame()

  expect_s3_class(result, "reactable")
  expect_true(nrow(result_data) == 0)
})

test_that("4. dfe_reactable with HTML content", {
  data <- data.frame(
    A = c("<b>Bold</b>", "<i>Italic</i>"),
    B = c("<u>Underline</u>", "<span style='color:red;'>Red Text</span>")
  )

  result <- dfe_reactable(data)
  options <- result$x$tag

  expect_s3_class(result, "reactable")
  expect_true(grepl("html = TRUE", options))
})


# create_stats_table() --------------------------------------------------------
test_that("1. create_stats_table works with standard inputs", {
  main_table <- data.frame(
    "LA Number" = 123,
    "LA and Regions" = "LA1",
    check.names = FALSE
  )
  selected_la <- "LA1"
  trend <- "Increase"
  change_since_prev <- 5.2
  rank <- 1
  quartile <- "A"
  quartile_bands <- c("25%" = 10, "50%" = 20, "75%" = 30, "100%" = 40)
  indicator_polarity <- "High"

  result <- create_stats_table(
    main_table,
    selected_la,
    trend,
    change_since_prev,
    rank,
    quartile,
    quartile_bands,
    indicator_polarity
  )

  expected <- data.frame(
    "LA Number" = 123,
    "LA and Regions" = "LA1",
    "Trend" = "Increase",
    "Change from previous year" = 5.2,
    "Latest National Rank" = 1,
    "Quartile Banding" = "A",
    "(A) Up to and including" = 10,
    "(B) Up to and including" = 20,
    "(C) Up to and including" = 30,
    "(D) Up to and including" = 40,
    "Polarity" = "High",
    check.names = FALSE
  ) |>
    pretty_num_table(dp = 1)

  expect_equal(result, expected)
})

test_that("2. create_stats_table handles empty inputs gracefully", {
  main_table <- data.frame(
    "LA Number" = numeric(0),
    "LA and Regions" = character(0),
    check.names = FALSE
  )
  selected_la <- character(0)
  trend <- character(0)
  change_since_prev <- numeric(0)
  rank <- numeric(0)
  quartile <- character(0)
  quartile_bands <- c("25%" = 0, "50%" = 0, "75%" = 0, "100%" = 0)
  indicator_polarity <- character(0)

  expect_warning(
    expect_error(
      create_stats_table(
        main_table,
        selected_la,
        trend,
        change_since_prev,
        rank,
        quartile,
        quartile_bands,
        indicator_polarity
      ),
      "arguments imply differing number of rows: 0, 1"
    ),
    "Dataframe seems empty"
  )
})

test_that("3. create_stats_table handles NAs gracefully", {
  main_table <- data.frame(
    "LA Number" = NA,
    "LA and Regions" = "LA1",
    check.names = FALSE
  )
  selected_la <- "LA1"
  trend <- NA
  change_since_prev <- NA
  rank <- NA
  quartile <- NA
  quartile_bands <- c("25%" = 0, "50%" = 0, "75%" = 0, "100%" = 0)
  indicator_polarity <- NA

  expected <- data.frame(
    "LA Number" = NA,
    "LA and Regions" = "LA1",
    "Trend" = NA,
    "Change from previous year" = NA,
    "Latest National Rank" = NA,
    "Quartile Banding" = NA,
    "(A) Up to and including" = 0,
    "(B) Up to and including" = 0,
    "(C) Up to and including" = 0,
    "(D) Up to and including" = 0,
    "Polarity" = NA,
    check.names = FALSE
  ) |>
    pretty_num_table(dp = 1)

  expect_warning(
    expect_equal(
      create_stats_table(
        main_table,
        selected_la,
        trend,
        change_since_prev,
        rank,
        quartile,
        quartile_bands,
        indicator_polarity
      ),
      expected
    ),
    "Suprise NA value in stats table"
  )
})

test_that("4. create_stats_table handles NA Quartile Banding gracefully", {
  main_table <- data.frame(
    "LA Number" = 123,
    "LA and Regions" = "LA1",
    check.names = FALSE
  )
  selected_la <- "LA1"
  trend <- "Increase"
  change_since_prev <- 5.2
  rank <- 1
  quartile <- "A"
  quartile_bands <- c("25%" = NA, "50%" = 20, "75%" = 30, "100%" = 40)
  indicator_polarity <- "High"

  expected <- data.frame(
    "LA Number" = 123,
    "LA and Regions" = "LA1",
    "Trend" = "Increase",
    "Change from previous year" = 5.2,
    "Latest National Rank" = 1,
    "Quartile Banding" = "A",
    "(A) Up to and including" = NA_real_,
    "(B) Up to and including" = 20,
    "(C) Up to and including" = 30,
    "(D) Up to and including" = 40,
    "Polarity" = "High",
    check.names = FALSE
  ) |>
    pretty_num_table(dp = 1)

  expect_warning(
    expect_equal(
      create_stats_table(
        main_table,
        selected_la,
        trend,
        change_since_prev,
        rank,
        quartile,
        quartile_bands,
        indicator_polarity
      ),
      expected
    ),
    "Suprise NA value in stats table"
  )
})


# polarity_colours_df() ------------------------------------------------------
test_that("1. polarity_colours_df returns a df with the correct structure", {
  result <- polarity_colours_df()

  # Check that result is a data frame
  expect_true(is.data.frame(result))

  # Check that the df has three cols: polarity, quartile_band, and cell_colour
  expect_equal(names(result), c("polarity", "quartile_band", "cell_colour"))

  # Check the dimensions of the df (16 rows for 4 polarities * 4 quartile bands)
  expect_equal(nrow(result), 16)
  expect_equal(ncol(result), 3)
})

test_that("2. polarity_colours_df assigns 'none' when polarity is NA or '-'", {
  result <- polarity_colours_df()

  # Filter rows where polarity is NA or "-"
  na_or_dash <- result[result$polarity %in% c(NA, "-"), ]

  # Expect all cell_colour values to be "none"
  expect_true(all(na_or_dash$cell_colour == "none"))
})

test_that("3. polarity_colours_df assigns 'none' for B and C", {
  result <- polarity_colours_df()

  # Filter rows where quartile_band is B or C
  bands_b_c <- result[result$quartile_band %in% c("B", "C"), ]

  # Expect all cell_colour values to be "none"
  expect_true(all(bands_b_c$cell_colour == "none"))
})

test_that("4. polarity_colours_df assigns 'green' for Low and A", {
  result <- polarity_colours_df()

  # Filter rows where quartile_band is A and polarity is Low
  green_cells <- result |>
    dplyr::filter(quartile_band == "A" & polarity == "Low")

  # Expect all cell_colour values to be "green"
  expect_true(all(green_cells$cell_colour == "green"))
})

test_that("5. polarity_colours_df assigns 'red' for Low and D", {
  result <- polarity_colours_df()

  # Filter rows where quartile_band is D and polarity is Low
  red_cells_low <- result |>
    dplyr::filter(quartile_band == "D" & polarity == "Low")

  # Expect all cell_colour values to be "red"
  expect_true(all(red_cells_low$cell_colour == "red"))
})

test_that("5. polarity_colours_df is empty for NA quartile band", {
  result <- polarity_colours_df()
  qb_filter_val <- NA

  # Filter rows where quartile_band is D and polarity is Low
  red_cells_low <- result |>
    dplyr::filter(quartile_band == qb_filter_val & polarity == "Low")

  # Expect all cell_colour values to be "red"
  expect_equal(red_cells_low$cell_colour, character(0))
})
