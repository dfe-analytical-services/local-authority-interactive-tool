# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


# LAIT Statistical Neighbour Level page ---------------------------------------
# - Regional Authorities
# Set user inputs
selected_topic <- "Foundation Stage"
selected_indicator <- "Foundation Stage - % achieving a good level of development"
selected_la <- "Barking and Dagenham"

# Filter BDS for topic and indicator
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic == selected_topic,
    Measure == selected_indicator,
    !is.na(Years)
  )

# Decimal point setting
indicator_dps <- filtered_bds |>
  get_indicator_dps()

# Get statistical neighbours
# Pull sn names
stat_n_sns <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la) |>
  pull_uniques("LA Name_sn")

# Get selected (Ldn cleaned) LA region
stat_n_region <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la) |>
  pull_uniques("GOReg") |>
  clean_ldn_region(filtered_bds)

# Keep relevant areas (LA, SN, region and England) from filtered BDS
stat_n_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(selected_la, stat_n_sns, stat_n_region, "England")
  )

# Calculate SN average
stat_n_sn_avg <- stat_n_filtered_bds |>
  dplyr::filter(`LA and Regions` %in% stat_n_sns) |>
  dplyr::summarise(
    values_num = mean(values_num, na.rm = TRUE),
    .by = c("Years", "Years_num")
  ) |>
  dplyr::mutate(
    "LA Number" = "-",
    "LA and Regions" = "Statistical Neighbours",
    .before = "Years"
  )

# Statistical Neighbours long data
stat_n_long <- stat_n_filtered_bds |>
  dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values) |>
  dplyr::bind_rows(stat_n_sn_avg) |>
  dplyr::mutate(
    `LA and Regions` = factor(
      `LA and Regions`,
      levels = c(
        selected_la, stat_n_sns, "Statistical Neighbours",
        stat_n_region, "England"
      )
    )
  )

# Most recent year
current_year <- stat_n_long |>
  dplyr::filter(Years_num == max(Years_num)) |>
  pull_uniques("Years")

# Difference between last two years
stat_n_diff <- stat_n_long |>
  calculate_change_from_prev_yr()

# Join change from previous year rows, convert to wide and format data
stat_n_table <- stat_n_long |>
  dplyr::bind_rows(stat_n_diff) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num,
  ) |>
  pretty_num_table(
    dp = indicator_dps,
    exclude_columns = "LA Number"
  )

# Statistical Neighbour Level SN table ----------------------------------------
stat_n_sn_table <- stat_n_table |>
  dplyr::filter(`LA and Regions` %in% c(selected_la, stat_n_sns)) |>
  dplyr::arrange(.data[[current_year]], `LA and Regions`)

# Output table
dfe_reactable(
  stat_n_sn_table,
  # Create the reactable with specific column alignments
  columns = align_reactable_cols(stat_n_sn_table, num_exclude = "LA Number"),
  rowStyle = function(index) {
    highlight_selected_row(index, stat_n_sn_table, selected_la)
  },
  pagination = FALSE
)


# Statistical Neighbour Level comparison table --------------------------------
stat_n_comp_table <- stat_n_table |>
  dplyr::filter(`LA and Regions` %in% c(
    "Statistical Neighbours",
    stat_n_region,
    "England"
  )) |>
  dplyr::arrange(`LA and Regions`)

# Output table
dfe_reactable(
  stat_n_comp_table,
  # Create the reactable with specific column alignments
  columns = align_reactable_cols(stat_n_comp_table, num_exclude = "LA Number"),
  pagination = FALSE
)


# Statistical Neighbour Level stats table -------------------------------------
stat_n_stats_geog <- c(selected_la, stat_n_region, "England")

# Extract change from prev year
stat_n_change_prev <- stat_n_diff |>
  filter_la_regions(stat_n_stats_geog,
    pull_col = "values_num"
  )

# Get polarity of indicator
stat_n_indicator_polarity <- filtered_bds |>
  pull_uniques("Polarity")

# Set the trend value
stat_n_trend <- as.numeric(stat_n_change_prev)

# Get latest rank, ties are set to min & NA vals to NA rank
stat_n_rank <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE) |>
  dplyr::mutate(
    rank = dplyr::case_when(
      is.na(values_num) ~ NA,
      # Rank in descending order
      stat_n_indicator_polarity == "High" ~ rank(-values_num, ties.method = "min", na.last = TRUE),
      # Rank in ascending order
      stat_n_indicator_polarity == "Low" ~ rank(values_num, ties.method = "min", na.last = TRUE)
    )
  ) |>
  filter_la_regions(selected_la, pull_col = "rank")

# Calculate quartile bands for indicator
stat_n_quartile_bands <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE, pull_col = "values_num") |>
  quantile(na.rm = TRUE)

# Extracting LA latest value
stat_n_indicator_val <- filtered_bds |>
  filter_la_regions(selected_la, latest = TRUE, pull_col = "values_num")

# Calculating which quartile this value sits in
stat_n_quartile <- calculate_quartile_band(
  stat_n_indicator_val,
  stat_n_quartile_bands,
  stat_n_indicator_polarity
)

# SN stats table
stat_n_stats_table <- data.frame(
  "LA Number" = stat_n_diff |>
    filter_la_regions(stat_n_stats_geog, pull_col = "LA Number"),
  "LA and Regions" = stat_n_stats_geog,
  "Trend" = stat_n_trend,
  "Change from previous year" = stat_n_change_prev,
  "Polarity" = stat_n_indicator_polarity,
  check.names = FALSE
) |>
  pretty_num_table(
    dp = get_indicator_dps(filtered_bds),
    include_columns = c("Change from previous year")
  )

# Rank and QB (get from LA in mods though)
stat_n_statsla_table <- data.frame(
  "Natioanl Rank" = stat_n_rank,
  "Quartile Banding" = stat_n_quartile,
  check.names = FALSE
)


# Statistical Neighbour Level SN focus plot -----------------------------------
