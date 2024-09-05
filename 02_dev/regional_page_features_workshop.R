# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


# LAIT Regional Level LA table ------------------------------------------------
# - Regional Authorities
# Set user inputs
selected_topic <- "Health and Wellbeing"
selected_indicator <- "Low birth weight"
selected_la <- "Barking and Dagenham"

# Get the LA region
region_la <- stat_n_geog |>
  dplyr::filter(`LA Name` == selected_la) |>
  dplyr::pull(GOReg)

# Get other LAs in the region
region_la_la <- stat_n_geog |>
  dplyr::filter(GOReg == region_la) |>
  pull_uniques("LA Name")


# Filter BDS for topic and indicator
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic == selected_topic,
    Measure == selected_indicator
  )

# Then filter for selected LA and regional LAs
region_la_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(selected_la, region_la_la)
  )

# Region LA levels long
region_la_long <- region_la_filtered_bds |>
  # dplyr::filter(`LA and Regions` %notin% c(la_sns)) |>
  dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
  # dplyr::bind_rows(sn_avg) |>
  dplyr::mutate(
    `LA and Regions` = factor(
      `LA and Regions`
      # levels = c(
      #   selected_la, la_region,
      #   "Statistical Neighbours", la_national
      # )
    ),
    Years_num = as.numeric(substr(Years, start = 1, stop = 4))
  )

# Difference between last two years
region_la_diff <- region_la_long |>
  calculate_change_from_prev_yr()

# Most recent year
current_year <- region_la_long |>
  dplyr::filter(Years_num == max(Years_num)) |>
  pull_uniques("Years")

# Join difference and pivot wider to recreate Regional LA table
region_la_table <- region_la_long |>
  dplyr::bind_rows(region_la_diff) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num
  ) |>
  pretty_num_table(dp = 1) |>
  dplyr::arrange(.data[[current_year]], `LA and Regions`)


# Regional Level Regions table ------------------------------------------------
# Get national term
region_national <- filtered_bds |>
  dplyr::filter(`LA and Regions` %in% national_names_bds & !is.na(values_num)) |>
  pull_uniques("LA and Regions")

# Filter for all regions and England
region_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(region_names_bds, region_national)
  )

# Region levels long
region_long <- region_filtered_bds |>
  # dplyr::filter(`LA and Regions` %notin% c(la_sns)) |>
  dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
  # dplyr::bind_rows(sn_avg) |>
  dplyr::mutate(
    `LA and Regions` = factor(
      `LA and Regions`
      # levels = c(
      #   selected_la, la_region,
      #   "Statistical Neighbours", la_national
      # )
    ),
    Years_num = as.numeric(substr(Years, start = 1, stop = 4))
  )

# Difference between last two years
region_diff <- region_long |>
  calculate_change_from_prev_yr()

# Most recent year
current_year <- region_long |>
  dplyr::filter(Years_num == max(Years_num)) |>
  pull_uniques("Years")

# Join difference and pivot wider to recreate Region table
region_table <- region_long |>
  dplyr::bind_rows(region_diff) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num
  ) |>
  pretty_num_table(dp = 1) |>
  dplyr::arrange(.data[[current_year]], `LA and Regions`) |>
  # Places England row at the bottom of the table
  dplyr::mutate(is_england = ifelse(grepl("England \\(", `LA and Regions`), 1, 0)) |>
  dplyr::arrange(is_england, .by_group = FALSE) |>
  dplyr::select(-is_england)


# Regional Level Stats table --------------------------------------------------
# Get LA numbers
# Selected LA
region_la_la_num <- region_la_table |>
  filter_la_regions(selected_la, pull_col = "LA Number")

# Region and England
region_la_num <- region_table |>
  filter_la_regions(c(region_la, region_national), pull_col = "LA Number")

# Get change in previous year
# Selected LA
region_la_change_prev <- region_la_table |>
  filter_la_regions(selected_la,
    latest = FALSE,
    pull_col = "Change from previous year"
  )

# Region and England
region_change_prev <- region_table |>
  filter_la_regions(c(region_la, region_national),
    latest = FALSE,
    pull_col = "Change from previous year"
  )

# Creating the stats table cols
region_stats_la_num <- c(region_la_la_num, region_la_num)
region_stats_name <- c(selected_la, region_la, region_national)
region_stats_change <- c(region_la_change_prev, region_change_prev)

# Creating the trend descriptions
region_trend <- dplyr::case_when(
  is.na(region_stats_change) ~ NA_character_,
  region_stats_change > 0 ~ "Increase",
  region_stats_change < 0 ~ "Decrease",
  TRUE ~ "No trend"
)

# Build stats table
region_stats_table <- data.frame(
  "LA Number" = region_stats_la_num,
  "LA and Regions" = region_stats_name,
  "Trend" = region_trend,
  "Change from previous year" = region_stats_change
)
