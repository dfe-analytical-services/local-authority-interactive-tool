# Load global
source(here::here("global.R"))

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


# LAIT LA Level ----------------------------------
# - Local Authority, Region and England table ---
selected_topic <- "Health and Wellbeing"
selected_indicator <- "Under 18 Hospital Admissions (Alcohol related)"
selected_la <- "Bedford Borough"

# Filter stat neighbour for selected LA
filtered_sn <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la)

# Statistical Neighbours
la_sns <- filtered_sn |>
  pull_uniques("LA Name_sn")

# LA region
la_region <- filtered_sn |>
  pull_uniques("GOReg")

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

# Determine London region to use
la_region_ldn_clean <- clean_ldn_region(la_region, filtered_bds)

# Then filter for selected LA, region, stat neighbours and national
la_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(selected_la, la_region_ldn_clean, la_sns, "England")
  )

# SN average
sn_avg <- la_filtered_bds |>
  dplyr::filter(`LA and Regions` %in% la_sns) |>
  dplyr::summarise(
    values_num = mean(values_num, na.rm = TRUE),
    .by = c("Years", "Years_num")
  ) |>
  dplyr::mutate(
    "LA Number" = "-",
    "LA and Regions" = "Statistical Neighbours",
    .before = "Years"
  )

# LA levels long
la_long <- la_filtered_bds |>
  dplyr::filter(`LA and Regions` %notin% c(la_sns)) |>
  dplyr::select(`LA Number`, `LA and Regions`, Years, Years_num, values_num, Values) |>
  dplyr::bind_rows(sn_avg) |>
  dplyr::mutate(
    `LA and Regions` = factor(
      `LA and Regions`,
      levels = c(
        selected_la, la_region_ldn_clean,
        "Statistical Neighbours", "England"
      )
    )
  )

# Difference between last two years
la_diff <- la_long |>
  calculate_change_from_prev_yr()

# Join difference and pivot wider to recreate LAIT table
la_table <- la_long |>
  dplyr::bind_rows(la_diff) |>
  dplyr::mutate(Values = dplyr::case_when(
    is.na(values_num) ~ Values,
    `Years` == "Change from previous year" ~ as.character(values_num),
    TRUE ~ as.character(values_num)
  )) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num,
  ) |>
  dplyr::arrange(`LA and Regions`)

# Output table
dfe_reactable(
  la_table,
  # Create the reactable with specific column alignments
  columns = utils::modifyList(
    format_num_reactable_cols(
      la_table,
      get_indicator_dps(filtered_bds),
      num_exclude = "LA Number"
    ),
    set_custom_default_col_widths(),
  ),
  rowStyle = function(index) {
    highlight_selected_row(index, la_table, selected_la)
  }
)


# - Local Authority specific stats table --------------------------------------

# Extract change from prev year (from LA table)
la_change_prev <- la_table |>
  filter_la_regions(selected_la,
    latest = FALSE,
    pull_col = "Change from previous year"
  )

# Get polarity of indicator
la_indicator_polarity <- filtered_bds |>
  pull_uniques("Polarity")

# Set the trend value
la_trend <- la_diff |>
  filter_la_regions(selected_la,
    pull_col = "values_num"
  )

# Get latest rank, ties are set to min & NA vals to NA rank
la_rank <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE) |>
  dplyr::mutate(
    rank = dplyr::case_when(
      is.na(values_num) ~ NA,
      # Rank in descending order
      la_indicator_polarity == "High" ~ rank(-values_num, ties.method = "min", na.last = TRUE),
      # Rank in ascending order
      la_indicator_polarity == "Low" ~ rank(values_num, ties.method = "min", na.last = TRUE)
    )
  ) |>
  filter_la_regions(selected_la, pull_col = "rank")

# Calculate quartile bands for indicator
la_quartile_bands <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE, pull_col = "values_num") |>
  quantile(na.rm = TRUE)

# Extracting LA latest value
la_indicator_val <- filtered_bds |>
  filter_la_regions(selected_la, latest = TRUE, pull_col = "values_num")

# Calculating which quartile this value sits in
if (la_indicator_polarity %in% "Low") {
  la_quartile <- dplyr::case_when(
    is.na(la_indicator_val) ~ NA_character_,
    (la_indicator_val >= la_quartile_bands[["0%"]]) &
      (la_indicator_val <= la_quartile_bands[["25%"]]) ~ "A",
    (la_indicator_val > la_quartile_bands[["25%"]]) &
      (la_indicator_val <= la_quartile_bands[["50%"]]) ~ "B",
    (la_indicator_val > la_quartile_bands[["50%"]]) &
      (la_indicator_val <= la_quartile_bands[["75%"]]) ~ "C",
    (la_indicator_val > la_quartile_bands[["75%"]]) &
      (la_indicator_val <= la_quartile_bands[["100%"]]) ~ "D",
    TRUE ~ "Error"
  )
} else if (la_indicator_polarity %in% "High") {
  la_quartile <- dplyr::case_when(
    is.na(la_indicator_val) ~ NA_character_,
    (la_indicator_val >= la_quartile_bands[["0%"]]) &
      (la_indicator_val <= la_quartile_bands[["25%"]]) ~ "D",
    (la_indicator_val > la_quartile_bands[["25%"]]) &
      (la_indicator_val <= la_quartile_bands[["50%"]]) ~ "C",
    (la_indicator_val > la_quartile_bands[["50%"]]) &
      (la_indicator_val <= la_quartile_bands[["75%"]]) ~ "B",
    (la_indicator_val > la_quartile_bands[["75%"]]) &
      (la_indicator_val <= la_quartile_bands[["100%"]]) ~ "A",
    TRUE ~ "Error"
  )
} else {
  la_quartile <- "Not applicable"
}

la_stats_table <- build_la_stats_table(
  la_table,
  selected_la,
  la_trend,
  la_change_prev,
  la_rank,
  la_quartile,
  la_quartile_bands,
  la_indicator_polarity
)


# Build stats table
la_stats_table <- data.frame(
  "LA Number" = la_table |>
    filter_la_regions(selected_la, pull_col = "LA Number"),
  "LA and Regions" = selected_la,
  "Trend" = -la_trend,
  "Change from previous year" = la_change_prev,
  "Latest National Rank" = la_rank,
  "Quartile Banding" = la_quartile,
  "(A) Up to and including" = la_quartile_bands[["25%"]],
  "(B) Up to and including" = la_quartile_bands[["50%"]],
  "(C) Up to and including" = la_quartile_bands[["75%"]],
  "(D) Up to and including" = la_quartile_bands[["100%"]],
  "Polarity" = la_indicator_polarity,
  check.names = FALSE
)

if (la_indicator_polarity %notin% c("High", "Low")) {
  la_stats_table <- la_stats_table |>
    dplyr::mutate(
      "Latest National Rank" = "Not applicable",
      "Quartile Banding" = "Not applicable",
      "(A) Up to and including" = "-",
      "(B) Up to and including" = "-",
      "(C) Up to and including" = "-",
      "(D) Up to and including" = "-"
    )
}


# Format stats table
# Use modifyList to merge the lists properly
dfe_reactable(
  la_stats_table |> dplyr::select(-Polarity),
  columns = modifyList(
    # Create the reactable with specific column alignments
    format_num_reactable_cols(
      la_stats_table |>
        dplyr::select(-Polarity),
      get_indicator_dps(filtered_bds),
      num_exclude = "LA Number",
      categorical = c("Trend", "Quartile Banding", "Latest National Rank")
    ),
    # Define specific formatting for the Trend and Quartile Banding columns
    list(
      set_custom_default_col_widths(),
      `Quartile Banding` = reactable::colDef(
        style = quartile_banding_col_def(la_stats_table)
      ),
      Trend = reactable::colDef(
        cell = trend_icon_renderer
      )
    )
  )
)





# LA line chart plot ----------------------------------------------------------
# Plot
la_line_chart <- la_long |>
  # Filter out NAs to stop warning
  # "Failed setting attribute 'data-id', mismatched lengths of ids and values"
  # dplyr::filter(!is.na(values_num)) |>
  ggplot2::ggplot() +
  ggiraph::geom_point_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      color = `LA and Regions`,
      data_id = `LA and Regions`
    ),
    na.rm = TRUE
  ) +
  ggiraph::geom_line_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      color = `LA and Regions`,
      data_id = `LA and Regions`
    ),
    na.rm = TRUE
  ) +
  format_axes(la_long) +
  set_plot_colours(la_long) +
  set_plot_labs(filtered_bds) +
  custom_theme()


# Creating vertical geoms to make vertical hover tooltip
vertical_hover <- lapply(
  get_years(la_long),
  tooltip_vlines,
  la_long,
  indicator_dps
)

# Plotting interactive graph
ggiraph_test_save <- ggiraph::girafe(
  ggobj = (la_line_chart + vertical_hover),
  width_svg = 8.5,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)

# Saving plot as a png
ggsave(
  "test.svg",
  plot = la_line_chart, # Use the ggplot object inside the girafe output
  width = 8.5,
  height = 6
)

# Save ggiraph plot as standalone html
htmlwidgets::saveWidget(ggiraph_test_save, tempfile(fileext = ".html"))

# LA bar plot -----------------------------------------------------------------
# Plot
la_bar_chart <- la_long |>
  ggplot2::ggplot() +
  ggiraph::geom_col_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      fill = `LA and Regions`,
      tooltip = glue::glue_data(
        la_long |>
          pretty_num_table(include_columns = "values_num", dp = indicator_dps),
        "Year: {Years}\n{`LA and Regions`}: {values_num}"
      ),
      data_id = `LA and Regions`
    ),
    position = "dodge",
    width = 0.6,
    na.rm = TRUE,
    colour = "black"
  ) +
  format_axes(la_long) +
  set_plot_colours(la_long, "fill") +
  set_plot_labs(filtered_bds) +
  custom_theme()

# Plotting interactive graph
ggiraph::girafe(
  ggobj = la_bar_chart,
  width_svg = 8.5,
  options = generic_ggiraph_options(),
  fonts = list(sans = "Arial")
)



# LA Metadata -----------------------------------------------------------------

# Description
metrics_clean |>
  get_metadata(selected_indicator, "Description")

# Methodology
metrics_clean |>
  get_metadata(selected_indicator, "Methodology")

# Last updated
metrics_clean |>
  get_metadata(selected_indicator, "Last Update")

# Next updated
metrics_clean |>
  get_metadata(selected_indicator, "Next Update")

# Source (hyperlink)
metrics_clean |>
  get_metadata(selected_indicator, "Hyperlink(s)")
