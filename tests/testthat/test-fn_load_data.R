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
