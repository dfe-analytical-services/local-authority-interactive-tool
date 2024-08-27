generate_bds_dummy_data <- function(bds_data, n_measure = 10, n_years = 4, n_las = 4) {
  set.seed(1)

  # Length of column
  n_repeats <- n_measure * n_years * n_las

  # Years
  current_yr <- as.numeric(format(Sys.Date(), "%Y"))
  seq_yr <- seq(current_yr - n_years + 1, current_yr)
  year_col <- rep(seq_yr, times = n_measure * n_las)

  # Indicators and topics
  measures_n_topics <- bds_data |>
    dplyr::distinct(Topic, Measure, Measure_short, y_axis_name) |>
    dplyr::sample_n(n_measure) |>
    {
      \(x) dplyr::slice(x, rep(1:nrow(x), each = n_years * n_las))
    }()

  # Local Authorities
  local_authorities <- bds_data |>
    dplyr::distinct(`LA and Regions`, Region) |>
    dplyr::sample_n(n_las) |>
    {
      \(x) dplyr::slice(x, rep(1:nrow(x), years = n_years * n_measure))
    }()

  # Values
  measure_test <- measures_n_topics |>
    pull_uniques("Measure")

  sampled_values_lst <- lapply(measure_test, function(measure) {
    # Sample the values for each measure
    sampled_values <- bds_metrics |>
      dplyr::filter(Measure == measure) |>
      dplyr::pull(Values) |>
      sample(size = n_years * n_las, replace = TRUE)

    sampled_values_clean <- ifelse(sampled_values %in% c("-", "c"), NA, sampled_values)

    sampled_values_num <- as.numeric(sampled_values_clean)

    # Create a data frame with Measure and sampled Values
    data.frame(
      Measure_value = measure,
      Values = sampled_values,
      values_clean = sampled_values_clean,
      values_num = sampled_values_num
    )
  })
  values_col <- do.call(rbind, sampled_values_lst)


  # Build dependent dataset
  dummy_bds_dependent <- measures_n_topics |>
    cbind(local_authorities, year_col, values_col) |>
    dplyr::mutate(rand_order = sample(n_repeats)) |>
    dplyr::arrange(rand_order) |>
    dplyr::select(-rand_order)

  # Check Measures line up
  testthat::test_that("Measures line up", {
    testthat::expect_true(
      all(dummy_bds_dependent$Measure == dummy_bds_dependent$Measure_value)
    )
  })


  # Other cols
  measure_code_col <- replicate(n_repeats, paste0(sample(1:9, 3, replace = TRUE), collapse = ""))
  la_number_col <- replicate(n_repeats, paste0(sample(1:9, 3, replace = TRUE), collapse = ""))
  type_col <- replicate(n_repeats, paste0(sample(LETTERS, 3, replace = TRUE), collapse = ""))
  combined_code_col <- paste(measure_code_col, dummy_bds_dependent$Measure_short, dummy_bds_dependent$year_col, sep = "_")

  # Build dataframe
  data.frame(
    Topic = dummy_bds_dependent$Topic,
    Measure_code = measure_code_col,
    Measure = dummy_bds_dependent$Measure,
    Measure_short = dummy_bds_dependent$Measure_short,
    Polarity = sample(c("High", "Low"), n_repeats, replace = TRUE),
    y_axis_name = dummy_bds_dependent$y_axis_name,
    Years = dummy_bds_dependent$year_col,
    `LA and Regions` = dummy_bds_dependent$`LA and Regions`,
    Values = dummy_bds_dependent$Values,
    `LA Number` = la_number_col,
    Region = dummy_bds_dependent$Region,
    Type = type_col,
    combined_code = combined_code_col,
    values_clean = dummy_bds_dependent$values_clean,
    values_num = dummy_bds_dependent$values_num
  )
}





generate_bds_dummy_data <- function(bds_data, n_measure = 10, n_years = 4, n_las = 4) {
  set.seed(1)

  # Length of column

  # Topic

  # Measure_cod3
  # Measure
  # Measure_short
  # Polarity
  # y_axis_name
  # Years
  # LA and Regions
  `LA and Regions` <- c(rep("Region1", 3), rep("Region2", 3))

  # Values
  # LA Number
  # Region
  # Type
  # combined_code
  # values_clean
  # values_num

  # Years
  current_yr <- as.numeric(format(Sys.Date(), "%Y"))
  seq_yr <- seq(current_yr - n_years + 1, current_yr)
  year_col <- rep(seq_yr, times = n_measure * n_las)

  # Indicators and topics
  measures_n_topics <- bds_data |>
    dplyr::distinct(Topic, Measure, Measure_short, y_axis_name) |>
    dplyr::sample_n(n_measure) |>
    {
      \(x) dplyr::slice(x, rep(1:nrow(x), each = n_years * n_las))
    }()

  # Local Authorities
  local_authorities <- bds_data |>
    dplyr::distinct(`LA and Regions`, Region) |>
    dplyr::sample_n(n_las) |>
    {
      \(x) dplyr::slice(x, rep(1:nrow(x), years = n_years * n_measure))
    }()

  # Values
  measure_test <- measures_n_topics |>
    pull_uniques("Measure")

  sampled_values_lst <- lapply(measure_test, function(measure) {
    # Sample the values for each measure
    sampled_values <- bds_metrics |>
      dplyr::filter(Measure == measure) |>
      dplyr::pull(Values) |>
      sample(size = n_years * n_las, replace = TRUE)

    sampled_values_clean <- ifelse(sampled_values %in% c("-", "c"), NA, sampled_values)

    sampled_values_num <- as.numeric(sampled_values_clean)

    # Create a data frame with Measure and sampled Values
    data.frame(
      Measure_value = measure,
      Values = sampled_values,
      values_clean = sampled_values_clean,
      values_num = sampled_values_num
    )
  })
  values_col <- do.call(rbind, sampled_values_lst)


  # Build dependent dataset
  dummy_bds_dependent <- measures_n_topics |>
    cbind(local_authorities, year_col, values_col) |>
    dplyr::mutate(rand_order = sample(n_repeats)) |>
    dplyr::arrange(rand_order) |>
    dplyr::select(-rand_order)

  # Check Measures line up
  testthat::test_that("Measures line up", {
    testthat::expect_true(
      all(dummy_bds_dependent$Measure == dummy_bds_dependent$Measure_value)
    )
  })


  # Other cols
  measure_code_col <- replicate(n_repeats, paste0(sample(1:9, 3, replace = TRUE), collapse = ""))
  la_number_col <- replicate(n_repeats, paste0(sample(1:9, 3, replace = TRUE), collapse = ""))
  type_col <- replicate(n_repeats, paste0(sample(LETTERS, 3, replace = TRUE), collapse = ""))
  combined_code_col <- paste(measure_code_col, dummy_bds_dependent$Measure_short, dummy_bds_dependent$year_col, sep = "_")

  # Build dataframe
  data.frame(
    Topic = dummy_bds_dependent$Topic,
    Measure_code = measure_code_col,
    Measure = dummy_bds_dependent$Measure,
    Measure_short = dummy_bds_dependent$Measure_short,
    Polarity = sample(c("High", "Low"), n_repeats, replace = TRUE),
    y_axis_name = dummy_bds_dependent$y_axis_name,
    Years = dummy_bds_dependent$year_col,
    `LA and Regions` = dummy_bds_dependent$`LA and Regions`,
    Values = dummy_bds_dependent$Values,
    `LA Number` = la_number_col,
    Region = dummy_bds_dependent$Region,
    Type = type_col,
    combined_code = combined_code_col,
    values_clean = dummy_bds_dependent$values_clean,
    values_num = dummy_bds_dependent$values_num
  )
}




# Source functions (all scripts in R/ with prefix 'fn_') ----------------------
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)

# Test coverage ---------------------------------------------------------------
library(testthat)

# Define a function to calculate coverage for each file
calculate_coverage <- function(fn_file) {
  coverage <- covr::file_coverage(paste0("R/", fn_file), paste0("tests/testthat/test-", fn_file))
  coverage
}

list.files("R/") |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(calculate_coverage)
