# build_la_stats_table() --------------------------------------------------------
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

  expected <- data.frame(
    "LA Number" = 123,
    "LA and Regions" = "LA1",
    "Trend" = "Increase",
    "Change from previous year" = 5.2,
    "Polarity" = "High",
    "Latest National Rank" = 1,
    "Quartile Banding" = "A",
    "A" = "40 to 30.1",
    "B" = "30 to 20.1",
    "C" = "20 to 10.1",
    "D" = "10 to 0",
    check.names = FALSE
  )

  # Compare only columns that exist in both
  expect_equal(result[, names(expected)], expected)
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
  expect_equal(nrow(result), 1)

  # All NA or "-" as appropriate
  expect_true(all(is.na(result[1, c("LA Number", "Trend", "Change from previous year")])))
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

  expect_true(all(c("Latest National Rank", "Quartile Banding") %in% names(result)))
  expect_equal(result$`Latest National Rank`[1], "-")
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

  # Expect quartile ranges even if values are 0
  expect_equal(result$A[1], "0 to 0")
  expect_equal(result$D[1], "0.1 to 0")
})
