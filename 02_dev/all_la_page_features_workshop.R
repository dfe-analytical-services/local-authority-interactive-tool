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
selected_topic <- "Health and Wellbeing"
selected_indicator <- "Infant Mortality"
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

# Get polarity of indicator
all_la_indicator_polarity <- filtered_bds |>
  pull_uniques("Polarity")

### Could be useful:
# Get selected (Ldn cleaned) LA region
all_la_region <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la) |>
  pull_uniques("GOReg") |>
  clean_ldn_region(filtered_bds)



# Get latest rank, ties are set to min & NA vals to NA rank
all_la_ranked <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE) |>
  dplyr::mutate(
    Rank = dplyr::case_when(
      is.na(values_num) ~ NA,
      # Rank in descending order
      all_la_indicator_polarity == "High" ~ rank(-values_num, ties.method = "min", na.last = TRUE),
      # Rank in ascending order
      all_la_indicator_polarity == "Low" ~ rank(values_num, ties.method = "min", na.last = TRUE)
    )
  ) |>
  dplyr::select(`LA and Regions`, Rank)

# All LAs long data
all_la_long <- filtered_bds |>
  dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values)

# Most recent year
current_year <- all_la_long |>
  dplyr::filter(Years_num == max(Years_num)) |>
  pull_uniques("Years")

# Convert to wide format
all_la_table <- all_la_long |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num,
  ) |>
  dplyr::left_join(all_la_ranked, by = "LA and Regions") |>
  pretty_num_table(
    dp = indicator_dps,
    exclude_columns = c("LA Number", "Rank")
  )
