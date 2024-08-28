# clean_snp_colnames() --------------------------------------------------------
test_that("1. clean_snp_colnames with a simple case", {
  data <- data.frame(
    SN1 = c(1, 2),
    SNP = c(3, 4),
    SN2 = c(5, 6),
    SNP.1 = c(7, 8)
  )

  result <- clean_snp_colnames(data)
  expected_colnames <- c("SN1", "SNP1", "SN2", "SNP2")

  expect_equal(colnames(result), expected_colnames)
})

test_that("2. clean_snp_colnames with no SNP columns", {
  data <- data.frame(
    SN1 = c(1, 2),
    SN2 = c(3, 4)
  )

  expect_error(
    clean_snp_colnames(data),
    "SNP columns do not seem to be in the right format"
  )
})

test_that("3. clean_snp_colnames with no SN columns", {
  data <- data.frame(
    SNP = c(1, 2),
    SNP.1 = c(3, 4)
  )

  expect_error(
    clean_snp_colnames(data),
    "SN columns do not seem to be in the right format"
  )
})

test_that("4. clean_snp_colnames with mixed SN, SNP, and other columns", {
  data <- data.frame(
    SN1 = c(1, 2),
    SNP = c(3, 4),
    Other = c(5, 6),
    SN2 = c(7, 8),
    SNP.1 = c(9, 10)
  )

  result <- clean_snp_colnames(data)
  expected_colnames <- c("SN1", "SNP1", "Other", "SN2", "SNP2")

  expect_equal(colnames(result), expected_colnames)
})

test_that("5. clean_snp_colnames with missing SN numbers", {
  data <- data.frame(
    SN1 = c(1, 2),
    SNP = c(3, 4),
    SN3 = c(5, 6),
    SNP.1 = c(7, 8)
  )

  result <- clean_snp_colnames(data)
  expected_colnames <- c("SN1", "SNP1", "SN3", "SNP3")

  expect_equal(colnames(result), expected_colnames)
})

test_that("6. clean_snp_colnames with duplicate SNP columns", {
  data <- data.frame(
    SN1 = c(1, 2),
    SNP = c(3, 4),
    SNP.1 = c(5, 6),
    SN2 = c(7, 8),
    SNP.2 = c(9, 10)
  )

  expect_warning(
    expect_equal(
      colnames(clean_snp_colnames(data)),
      c("SN1", "SNP1", "SNP2", "SN2", "SNP1")
    ),
    "number of items to replace is not a multiple of replacement length"
  )
})

test_that("7. clean_snp_colnames with special characters in column names", {
  data <- data.frame(
    `SN-1` = c(1, 2),
    SNP = c(3, 4),
    `SN_2` = c(5, 6),
    SNP.1 = c(7, 8)
  )

  # Expect error since SN columns don't match the exact pattern "SN#"
  expect_error(
    clean_snp_colnames(data),
    "SN columns do not seem to be in the right format"
  )
})

test_that("8. clean_snp_colnames with no SN and no SNP columns (both warnings expected)", {
  data <- data.frame(
    Column1 = c(1, 2),
    Column2 = c(3, 4)
  )

  expect_error(
    clean_snp_colnames(data),
    "SN columns do not seem to be in the right format"
  )
})


# create_measure_key() --------------------------------------------------------
test_that("1. create_measure_key with basic input", {
  data <- data.frame(
    Topic = c("Topic1", "Topic2"),
    Measure_short = c("Measure1", "Measure2"),
    stringsAsFactors = FALSE
  )

  result <- create_measure_key(data)
  expected <- data.frame(
    Topic = c("Topic1", "Topic2"),
    Measure_short = c("Measure1", "Measure2"),
    measure_key = c("topic1_measure1", "topic2_measure2"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("2. create_measure_key with spaces in Topic and Measure_short", {
  data <- data.frame(
    Topic = c("My Topic", "Another Topic"),
    Measure_short = c("Short Measure", "Long Measure"),
    stringsAsFactors = FALSE
  )

  result <- create_measure_key(data)
  expected <- data.frame(
    Topic = c("My Topic", "Another Topic"),
    Measure_short = c("Short Measure", "Long Measure"),
    measure_key = c("my_topic_short_measure", "another_topic_long_measure"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("3. create_measure_key with special characters", {
  data <- data.frame(
    Topic = c("Topic#1", "Topic@2"),
    Measure_short = c("Measure*1", "Measure&2"),
    stringsAsFactors = FALSE
  )

  result <- create_measure_key(data)
  expected <- data.frame(
    Topic = c("Topic#1", "Topic@2"),
    Measure_short = c("Measure*1", "Measure&2"),
    measure_key = c("topic#1_measure*1", "topic@2_measure&2"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("4. create_measure_key with empty columns", {
  data <- data.frame(
    Topic = c("", "Topic2"),
    Measure_short = c("Measure1", ""),
    stringsAsFactors = FALSE
  )

  result <- create_measure_key(data)
  expected <- data.frame(
    Topic = c("", "Topic2"),
    Measure_short = c("Measure1", ""),
    measure_key = c("_measure1", "topic2_"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("5. create_measure_key with all NA columns", {
  data <- data.frame(
    Topic = NA,
    Measure_short = NA,
    stringsAsFactors = FALSE
  )

  result <- create_measure_key(data)
  expected <- data.frame(
    Topic = NA,
    Measure_short = NA,
    measure_key = "na_na",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})
