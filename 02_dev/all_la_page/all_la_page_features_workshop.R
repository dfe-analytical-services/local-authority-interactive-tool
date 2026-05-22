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
selected_topic <- "Children's Service Finance"
selected_indicator <- "Total Services for Young People  (finance) - Gross"
selected_la <- "Barking and Dagenham"


# Filter BDS for topic and indicator
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic == selected_topic,
    Measure == selected_indicator
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

# Difference between last two years
all_la_diff <- all_la_long |>
  calculate_change_from_prev_yr()

# Convert to wide format and join rank column
all_la_table <- all_la_long |>
  rbind(all_la_diff) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num,
  ) |>
  dplyr::left_join(all_la_ranked, by = "LA and Regions")


# All LA Level LA table -------------------------------------------------------
all_la_la_table <- all_la_table |>
  dplyr::filter(`LA and Regions` %in% la_names_bds) |>
  dplyr::arrange(`LA and Regions`)

# Output table
dfe_reactable(
  all_la_la_table,
  # Create the reactable with specific column alignments
  columns = format_num_reactable_cols(all_la_la_table,
    num_exclude = "LA Number",
    categorical = "Rank",
    indicator_dps = indicator_dps
  ),
  rowStyle = function(index) {
    highlight_selected_row(index, all_la_la_table, selected_la)
  },
  pagination = FALSE
)


# All LA Level Region table ---------------------------------------------------
all_la_region_table <- all_la_table |>
  # Keep only Regions and England (remove London Inner/Outer with all NAs)
  dplyr::filter(
    `LA and Regions` %notin% la_names_bds,
    # Sums number of non-NA cols (left of LA and Regions) and checks if = 0
    !(
      `LA and Regions` %in% c("London (Inner)", "London (Outer)") &
        rowSums(
          !is.na(
            dplyr::select(all_la_table, -c(`LA Number`, `LA and Regions`))
          )
        ) == 0
    )
  ) |>
  # Replace Rank with a blank col
  dplyr::mutate(Rank = "") |>
  dplyr::arrange(`LA Number`)

# Output table
dfe_reactable(
  all_la_region_table,
  # Create the reactable with specific column alignments
  columns = modifyList(
    format_num_reactable_cols(
      all_la_region_table,
      get_indicator_dps(filtered_bds),
      num_exclude = "LA Number",
      categorical = "Rank"
    ),
    set_custom_default_col_widths(),
    # Remove all column header names
    setNames(
      lapply(
        names(all_la_region_table),
        function(name) reactable::colDef(name = "")
      ),
      names(all_la_region_table)
    )
  ),
  rowStyle = function(index) {
    highlight_selected_row(index, all_la_region_table, all_la_region)
  },
  pagination = FALSE
)
