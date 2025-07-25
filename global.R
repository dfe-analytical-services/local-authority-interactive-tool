# -----------------------------------------------------------------------------
# This is the global file.
#
# Use it to store functions, library calls, source files etc.
#
# Moving these out of the server file and into here improves performance as the
# global file is run only once when the app launches and stays consistent
# across users whereas the server and UI files are constantly interacting and
# responsive to user input.
#
# Library calls ===============================================================
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

# Core shiny and R packages
shhh(library(shiny))

# Creating charts and tables
shhh(library(ggplot2))
shhh(library(ggiraph))

# Dependencies needed for testing or CI but not for the app -------------------
# Including them here keeps them in renv but avoids the app needlessly loading
# them, saving on load time.
if (FALSE) {
  # Testing
  shhh(library(testthat))
  shhh(library(shinytest2))
  # Continuous Integration
  shhh(library(styler))
  shhh(library(lintr))
  shhh(library(roxygen2))
  shhh(library(rstudioapi))
  # Housekeeping
  shhh(library(devtools))
  shhh(library(usethis))
  shhh(library(covr))
  # For {arrow} to not give warning
  shhh(library(tzdb))
  # DfE packages
  shhh(library(dfeR))
  shhh(library(dfeshiny))
  shhh(library(shinyGovstyle))
  # Plotting
  shhh(library(svglite)) # For saving plots as svg
}

# Source scripts ==============================================================

# Source any scripts here. Scripts may be needed to process data before it gets
# to the server file or to hold custom functions to keep the main files shorter
#
# It's best to do this here instead of the server file, to improve performance.

# Source functions (all scripts in R/ with prefix 'fn_') ----------------------
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)

# Source modules (all scripts in R/lait_modules) ------------------------------
lapply(list.files(here::here("R/lait_modules/"), full.names = TRUE), source)

# Source ui components (all scripts in R/ui_panels) ---------------------------
lapply(list.files(here::here("R/ui_panels/"), full.names = TRUE), source)


# Set admin global variables ==================================================

site_title <- "Local Authority Interactive Tool (LAIT)" # name of app
parent_pub_name <- "LAIT GitHub repository (files named bds_long)"
# link to source publication
parent_publication <- "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait"

# Set the URLs that the site will be published to
site_primary <- "https://department-for-education.shinyapps.io/local-authority-interactive-tool/"

# Set the key for Google Analytics tracking
google_analytics_key <- "4VMNMFG2R3"

# Enable bookmarking so that input choices are shown in the url ---------------
shiny::enableBookmarking("url")


# Loading data ================================================================
# BDS - London regions changed to match SN
# - Christ Church to Christchurch, Westmoreland to Westmorland
# - London (Inner) & London (Outer), England_all_schools & England_state_funded
# - Added LA nums for Englands and London (inner/outer)
bds <- arrow::read_parquet(
  here::here("01_data/02_prod/bds_long_0.parquet")
)

# Statistical Neighbours
stat_n_raw <- readxl::read_xlsx(
  here::here("01_data/02_prod/sn_may_2025.xlsx"),
  sheet = "LA SN Groups",
  col_names = TRUE,
  skip = 2,
  .name_repair = "unique_quiet",
  col_types = "text"
)



# Data dictionary
metrics_raw <- read.csv(
  here::here("01_data/02_prod/lait_data_dictionary.csv"),
  check.names = FALSE
)

# For the Updates and Data Sources page
# Development update log
development_update_log <- read.csv(
  here::here("01_data/02_prod/development_update_log.csv")
)

# Banner message
banner_update_msg <- read.csv(
  here::here("01_data/02_prod/banner_update.csv"),
  check.names = FALSE
) |>
  dplyr::arrange(Date) |>
  dplyr::slice_head(n = 1) |>
  dplyr::mutate(
    Date = format(as.Date(Date), "%d %B %Y")
  ) |>
  unlist() |>
  paste(collapse = " ")

# Useful links
useful_links <- read.csv(
  here::here("01_data/02_prod/useful_links.csv"),
  check.names = FALSE
)

# Cleaning data ===============================================================
# BDS
# Convert values to numeric - doing the most to stop a coerce to NA warning
# Remove any rows where years are NA
bds_clean <- bds |>
  dplyr::mutate(
    values_clean = dplyr::case_when(
      Values == "-" ~ NA,
      Values == "c" ~ NA,
      Values == ".." ~ NA,
      is.na(Values) ~ NA,
      TRUE ~ Values
    ),
    values_num = as.numeric(values_clean)
  ) |>
  dplyr::filter(!is.na(Years))


# Statistical neighbours
# Clean dataframe - remove cols with all NA,
# rename SNP cols & remove rows where LA num is NA
stat_n <- stat_n_raw |>
  dplyr::select(dplyr::where(not_all_na)) |>
  clean_snp_colnames() |>
  dplyr::filter(!is.na(`LA num`))

# Transforming long
stat_n_long <- stat_n |>
  tidyr::pivot_longer(dplyr::starts_with("SN"),
    names_to = c(".value", "SN_SNP"),
    names_pattern = "^(.*?)(\\d+)$"
  )

# Extract LAs and LA nums
stat_n_geog <- stat_n |>
  dplyr::select(!dplyr::starts_with("SN")) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.)))


# Metrics
# Filter out discontinued metrics
metrics_included <- metrics_raw |>
  dplyr::filter(!grepl("DISCONTINUE", Table_status))

# Topic and indicators pairs (full - no duplicates filtered out)
topic_indicator_full <- metrics_included |>
  dplyr::distinct(Topic, Measure)

# Duplicate indicators across topics
dupes_across_topics <- topic_indicator_full |>
  dplyr::filter(dplyr::n() > 1, .by = "Measure")

# For each dupe combine topic names
dupes_combined_topics <- dupes_across_topics |>
  dplyr::summarise(
    Topic = stringr::str_c(unique(Topic), collapse = " / "),
    .by = "Measure"
  )

# Cleaning
# Remove whitesapce from key
# Set any NA decimal place column values to 1
# Convert Last and Next updated to Format Month Year
# Add in combined topic names for duplicate indicators
# Remove duplicates
metrics_clean <- metrics_included |>
  dplyr::mutate(
    Measure_short = trimws(Measure_short),
    dps = ifelse(is.na(dps), 1, dps),
    # Have to supress warnings due to mixed datatypes
    `Last Update` = dplyr::case_when(
      grepl("^[0-9]+$", `Last Update`) ~ suppressWarnings(
        as.numeric(`Last Update`) |>
          as.Date(origin = "1899-12-30") |>
          format("%B %Y") |>
          as.character()
      ),
      TRUE ~ as.character(`Last Update`)
    ),
    `Next Update` = dplyr::case_when(
      grepl("^[0-9]+$", `Next Update`) ~ suppressWarnings(
        as.numeric(`Next Update`) |>
          as.Date(origin = "1899-12-30") |>
          format("%B %Y") |>
          as.character()
      ),
      TRUE ~ as.character(`Next Update`)
    )
  ) |>
  dplyr::left_join(
    dupes_combined_topics,
    by = "Measure",
    suffix = c("", "_dupe_combined")
  ) |>
  # Update Topic where combined values exist
  dplyr::mutate(
    Topic = dplyr::case_when(
      !is.na(Topic_dupe_combined) ~ Topic_dupe_combined,
      TRUE ~ Topic
    )
  ) |>
  dplyr::select(-Topic_dupe_combined) |>
  dplyr::filter(!duplicated(Measure))

metrics_discontinued <- metrics_raw |>
  dplyr::filter(Measure_short %notin% metrics_clean$Measure_short) |>
  pull_uniques("Measure_short")

# Joining data ================================================================

# BDS & metrics (left join as have cleaned metrics for discontinued)
# Many-to-many join due to duplicates Measure_short from Data Dict
# (as topics can share measures)
# Some creation and cleaning of important cols
bds_metrics <- metrics_clean |>
  dplyr::select(
    Topic, Measure_code, Measure, Measure_short, state_funded_flag,
    Polarity, y_axis_name, Year_Type, Chart_title, dps
  ) |>
  dplyr::left_join(bds_clean,
    by = c("Measure_short" = "Short Desc"),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    Years_num = as.numeric(substr(Years, start = 1, stop = 4)),
    `LA Number` = as.character(`LA Number`)
  )


# Testing many-to-many join
metrics_duplicates <- metrics_clean |>
  dplyr::filter(
    duplicated(metrics_clean$Measure_short) |
      duplicated(metrics_clean$Measure_short, fromLast = FALSE)
  ) |>
  dplyr::pull(Measure_short)

bds_metrics_dupes <- bds_metrics |>
  dplyr::filter(Measure_short %in% metrics_duplicates)

# PROOF 1: Number of rows in bds == rows in bds (many-to-many) minus dupes
testthat::test_that(
  "Rows in BDS and BDS post merge are equal (minus the dupes)",
  {
    testthat::expect_equal(
      bds_clean |>
        nrow(),
      nrow(bds_metrics) - (nrow(bds_metrics_dupes) / 2)
    )
  }
)
# Using waldo to do same as test
waldo::compare(
  x = bds_clean |>
    nrow() |>
    as.numeric(),
  y = (nrow(bds_metrics) - (nrow(bds_metrics_dupes) / 2)) |>
    as.numeric()
)

# Measures that have not been joined by BDS clean
bds_metrics |>
  dplyr::filter(is.na(`LA and Regions`)) |>
  pull_uniques("Measure_short")

# Measures in BDS metrics but not in BDS clean
setdiff(
  bds_metrics |> pull_uniques("Measure_short"),
  bds_clean |> pull_uniques("Short Desc")
)


# PROOF 2: The unique values of Measure Short + Topic are the same
testthat::test_that(
  "Unique vals of measure_short + topic are the same in BDS & Metrics",
  {
    testthat::expect_equal(
      bds_metrics |>
        create_measure_key() |>
        pull_uniques("measure_key"),
      metrics_clean |>
        create_measure_key() |>
        pull_uniques("measure_key")
    )
  }
)

# PROOF 3: Number of topics per duplicate is 2
testthat::test_that("Number of topics per duplicate is 2", {
  testthat::expect_no_error(
    local({
      metric_topics_lst <- lapply(metrics_duplicates, function(metric) {
        metric_topics <- bds_metrics |>
          dplyr::filter(Measure_short == metric) |>
          pull_uniques("Topic")

        stopifnot(length(metric_topics) == 2)

        metric_topics
      })
    }),
    message = "length(metric_topics) > 2 is not TRUE"
  )
})


# Join stat nieghbours LA names to SN dataframe
stat_n_la <- stat_n_long |>
  dplyr::left_join(
    stat_n_geog |>
      dplyr::mutate(`LA number` = as.numeric(`LA number`)) |>
      dplyr::mutate(`LA num` = as.character(`LA num`)) |>
      dplyr::select(`LA num`, "LA Name_sn" = `LA Name`),
    by = c("SN" = "LA num")
  )


# Mini datasets ===============================================================

# LA names - statistical neighbours
la_names_sn <- pull_uniques(stat_n_geog, "LA Name")

# Non LAs
non_la_names_bds <- bds_clean |>
  dplyr::filter(`LA Number` >= 970) |>
  pull_uniques("LA and Regions")

# LAs
la_names_bds <- bds_clean |>
  dplyr::filter(`LA and Regions` %notin% non_la_names_bds) |>
  pull_uniques("LA and Regions")

# PROOF: Same LAs in BDS and Statistical Neighbours
testthat::test_that("Same LAs in both BDS and Stat Neighbours", {
  # Join is perfect
  testthat::expect_length(
    data.frame("la" = la_names_bds) |>
      dplyr::left_join(
        data.frame(
          "la" = la_names_sn,
          "la_sn" = la_names_sn
        ),
        by = "la"
      ) |>
      dplyr::filter(is.na(la_sn)) |>
      dplyr::pull(la),
    0
  )

  # Equal set
  testthat::expect_setequal(
    la_names_bds,
    la_names_sn
  )
})


# LAs
region_names_bds <- bds_clean |>
  dplyr::filter(`LA and Regions` %notin% c(la_names_bds, "England")) |>
  pull_uniques("LA and Regions")

# PROOF: 11 Regions and same Regions in BDS and Statistical Neighbours
testthat::test_that("Ther are 11 Region names & match Stat Neighbours", {
  testthat::expect_length(
    region_names_bds,
    11
  )

  testthat::expect_setequal(
    setdiff(region_names_bds, "London"),
    stat_n_la |>
      pull_uniques("GOReg") |>
      na.omit()
  )
})

# Metric topics
metric_topics <- pull_uniques(topic_indicator_full, "Topic")

# Metric names (alphabetically ordered)
metric_names <- tibble::tibble(
  Measure = topic_indicator_full |>
    pull_uniques("Measure")
) |>
  order_alphabetically(Measure)

# All Years across string and num Years
# (for Create Your Own year range choices - initially)
all_year_types <- unique(c(
  bds_metrics |>
    pull_uniques("Years"),
  bds_metrics |>
    pull_uniques("Years_num")
))

# Indicators that are impacted by COVID
# (aka missing data across all LAs for a whole year between 2091-2022)
covid_affected_data <- bds_metrics |>
  dplyr::filter(Years_num >= 2019, Years_num <= 2022) |>
  dplyr::group_by(Topic, Measure, Years_num) |>
  dplyr::summarise(all_na = all(is.na(values_num)), .groups = "keep") |>
  dplyr::filter(all_na) |>
  dplyr::ungroup()

# Indicators with too small a range for QB'ing
no_qb_indicators <- metrics_clean |>
  dplyr::filter(No_Quartile == "N") |>
  pull_uniques("Measure")

# Successful load of global.R message
cat(crayon::green("global.R successfully loaded!"))
