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
  # For {arrow} to not give warning
  shhh(library(tzdb))
  # DfE packages
  shhh(library(dfeR))
  shhh(library(dfeshiny))
  shhh(library(shinyGovstyle))
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
parent_pub_name <- "LAIT publication" # name of source publication
parent_publication <- # link to source publication
  "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait"

# Set the URLs that the site will be published to
site_primary <- "https://department-for-education.shinyapps.io/local-authority-interactive-tool/"
site_overflow <- "https://department-for-education.shinyapps.io/local-authority-interactive-tool-overflow/"

# Combine URLs into list for disconnect function
# We can add further mirrors where necessary. Each one can generally handle
# about 2,500 users simultaneously
sites_list <- c(site_primary, site_overflow)

# Set the key for Google Analytics tracking
google_analytics_key <- "Z967JJVQQX"

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
  here::here("01_data/02_prod/SN_April 2021.xlsx"),
  sheet = "LA SN Groups",
  col_names = TRUE,
  skip = 2,
  .name_repair = "unique_quiet"
)

# Data dictionary
metrics_raw <- readxl::read_xlsx(
  here::here("01_data/02_prod/LAIT Data Dictionary.xlsx"),
  sheet = "Data_prod"
)


# Cleaning data ===============================================================
# BDS
# Convert values to numeric - suppress warnings?
bds_clean <- bds |>
  dplyr::mutate(
    values_clean = dplyr::case_when(
      Values == "-" ~ NA,
      Values == "c" ~ NA,
      is.na(Values) ~ NA,
      TRUE ~ Values
    ),
    values_num = as.numeric(values_clean)
  )

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
# Remove whitesapce from key & filter out discontinued metrics
metrics_clean <- metrics_raw |>
  dplyr::mutate(Measure_short = trimws(Measure_short)) |>
  dplyr::filter(!grepl("DISCONTINUE", Table_status))

metrics_discontinued <- metrics_raw |>
  dplyr::filter(Measure_short %notin% metrics_clean$Measure_short) |>
  pull_uniques("Measure_short")


# Joining data ================================================================

# BDS & metrics (left join as have cleaned metrics for discontinued)
# Many-to-many join due to duplicates Measure_short from Data Dict
# (as topics can share measures)
bds_metrics <- metrics_clean |>
  dplyr::select(
    Topic, Measure_code, Measure, Measure_short,
    Polarity, y_axis_name
  ) |>
  dplyr::left_join(bds_clean,
    by = c("Measure_short" = "Short Desc"),
    relationship = "many-to-many"
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
        dplyr::filter(`Short Desc` %notin% metrics_discontinued) |>
        nrow(),
      nrow(bds_metrics) - (nrow(bds_metrics_dupes) / 2)
    )
  }
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

        return(metric_topics)
      })
    }),
    message = "length(metric_topics) > 2 is not TRUE"
  )
})


# Join stat nieghbours LA names to SN dataframe
stat_n_la <- stat_n_long |>
  dplyr::right_join(
    stat_n_geog |>
      dplyr::select(`LA num`,
        `LA Name_sn` = `LA Name`
      ),
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


# Englands
national_names_bds <- bds_clean |>
  dplyr::filter(grepl("England \\(", `LA and Regions`)) |>
  pull_uniques("LA and Regions")

# PROOF: 2 England names
testthat::test_that("There are 2 England names", {
  testthat::expect_length(
    national_names_bds,
    2
  )
})


# LAs
region_names_bds <- bds_clean |>
  dplyr::filter(`LA and Regions` %notin% c(la_names_bds, national_names_bds)) |>
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
metric_topics <- pull_uniques(metrics_clean, "Topic")

# Metric names
metric_names <- pull_uniques(metrics_clean, "Measure")

# Creating indicator polarity cell colour dataframe
# Define the possible values for each column
polarity_options <- c(NA, "-", "Low", "High")
quartile_band_options <- c("A", "B", "C", "D")
cell_colour_options <- c("red", "green", "none")

# Create all combinations of polarity and quartile band
polarity_colours <- expand.grid(
  polarity = polarity_options,
  quartile_band = quartile_band_options,
  stringsAsFactors = FALSE
)

# Initialize cell_colour column with "none"
polarity_colours$cell_colour <- "none"

# Apply the conditions to determine the cell colour
polarity_colours$cell_colour <- with(polarity_colours, ifelse(
  (is.na(polarity) | polarity == "-") | (quartile_band == "B" | quartile_band == "C"),
  "none", ifelse(
    (quartile_band == "A" & polarity == "Low"),
    "green", ifelse(
      (quartile_band == "D" & polarity == "Low"),
      "red", ifelse(
        (quartile_band == "A" & polarity == "High"),
        "red", ifelse(
          (quartile_band == "D" & polarity == "High"),
          "green",
          "none"
        )
      )
    )
  )
))
