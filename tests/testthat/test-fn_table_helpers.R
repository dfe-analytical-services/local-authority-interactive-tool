test_that("1. build_la_stats_table works with standard inputs", {
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
  quartile_bands <- c("0%" = 0, "25%" = 10, "50%" = 20, "75%" = 30, "100%" = 40)
  indicator_dps <- 1
  indicator_polarity <- "High"
  no_show_qb <- FALSE

  result <- build_la_stats_table(
    main_table,
    selected_la,
    trend,
    change_since_prev,
    rank,
    quartile,
    quartile_bands,
    indicator_dps,
    indicator_polarity,
    no_show_qb
  )

  # Basic structure checks
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Required columns should exist
  expected_cols <- c(
    "LA Number", "LA and Regions", "Trend",
    "Change from previous year", "Polarity",
    "Latest National Rank", "Quartile Banding"
  )
  expect_true(all(expected_cols %in% names(result)))

  # Spot-check key fields
  expect_equal(result$`LA Number`[1], 123)
  expect_equal(result$`LA and Regions`[1], "LA1")
  expect_equal(result$Trend[1], "Increase")
  expect_equal(result$`Change from previous year`[1], 5.2)
  expect_equal(result$Polarity[1], "High")
  expect_equal(result$`Latest National Rank`[1], 1)
  expect_equal(result$`Quartile Banding`[1], "A")
})


test_that("2. build_la_stats_table handles empty inputs gracefully", {
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
  indicator_dps <- 1
  indicator_polarity <- character(0)
  no_show_qb <- FALSE

  result <- build_la_stats_table(
    main_table,
    selected_la,
    trend,
    change_since_prev,
    rank,
    quartile,
    quartile_bands,
    indicator_dps,
    indicator_polarity,
    no_show_qb
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "LA Number", "LA and Regions", "Trend",
    "Change from previous year", "Polarity"
  ) %in% names(result)))
  expect_equal(nrow(result), 1)
})

test_that("3. build_la_stats_table handles NAs gracefully", {
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
  quartile_bands <- c("0%" = 0, "25%" = 0, "50%" = 0, "75%" = 0, "100%" = 0)
  indicator_dps <- 1
  indicator_polarity <- NA
  no_show_qb <- FALSE

  result <- build_la_stats_table(
    main_table,
    selected_la,
    trend,
    change_since_prev,
    rank,
    quartile,
    quartile_bands,
    indicator_dps,
    indicator_polarity,
    no_show_qb
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c(
    "LA Number", "LA and Regions", "Trend",
    "Change from previous year", "Polarity",
    "Latest National Rank", "Quartile Banding"
  ) %in% names(result)))
})

test_that("4. build_la_stats_table handles NA Quartile Banding gracefully", {
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
  quartile_bands <- c("0%" = 0, "25%" = 0, "50%" = 0, "75%" = 0, "100%" = 0)
  indicator_dps <- 1
  indicator_polarity <- "Low"
  no_show_qb <- FALSE

  result <- build_la_stats_table(
    main_table,
    selected_la,
    trend,
    change_since_prev,
    rank,
    quartile,
    quartile_bands,
    indicator_dps,
    indicator_polarity,
    no_show_qb
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c(
    "LA Number", "LA and Regions", "Trend",
    "Change from previous year", "Polarity",
    "Latest National Rank", "Quartile Banding"
  ) %in% names(result)))
})
