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

# Then filter for selected LA, region, stat neighbours and relevant national
region_la_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(selected_la, region_la_la)
  )

# LA levels long
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

# Join difference and pivot wider to recreate LAIT table
region_la_able <- region_la_long |>
  dplyr::bind_rows(region_la_diff) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num
  ) |>
  pretty_num_table(dp = 1) |>
  dplyr::arrange(.data[[current_year]])


# Regional Level Regions table ------------------------------------------------
