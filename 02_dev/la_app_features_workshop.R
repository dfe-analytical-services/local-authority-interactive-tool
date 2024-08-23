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
selected_indicator <- "Low birth weight"
selected_la <- "Barking and Dagenham"

# Filter stat neighbour for selected LA
filtered_sn <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la)

la_sns <- filtered_sn |>
  pull_uniques("LA Name_sn")

# LA region
la_region <- filtered_sn |>
  pull_uniques("GOReg")

# List of areas being compared
la_comparison <- c(selected_la, la_region, "Statistical Neighbours", "England")


# Then for the respective National indicator (all schools or State funded)
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic == selected_topic,
    Measure == selected_indicator
  )

# Get national term
la_national <- filtered_bds |>
  dplyr::filter(grepl("England_", `LA and Regions`) & !is.na(values_num)) |>
  pull_uniques("LA and Regions")

# Then filter for selected LA, region, stat neighbours and relevant national
la_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(selected_la, la_region, la_sns, la_national)
  )

# SN average
sn_avg <- la_filtered_bds |>
  dplyr::filter(`LA and Regions` %in% la_sns) |>
  dplyr::summarise(
    values_num = mean(values_num, na.rm = TRUE),
    .by = c("Years")
  ) |>
  dplyr::mutate(
    "LA Number" = NA,
    "LA and Regions" = "Statistical Neighbours",
    .before = "Years"
  )

# LA levels long
la_long <- la_filtered_bds |>
  dplyr::filter(`LA and Regions` %notin% c(la_sns)) |>
  dplyr::select(`LA Number`, `LA and Regions`, Years, values_num) |>
  dplyr::bind_rows(sn_avg) |>
  dplyr::mutate(
    `LA and Regions` = factor(
      `LA and Regions`,
      levels = c(
        selected_la, la_region,
        "Statistical Neighbours", la_national
      )
    ),
    Years_num = as.numeric(substr(Years, start = 1, stop = 4))
  )

# Difference between last two years
la_diff <- la_long |>
  calculate_change_from_prev_yr()

# Join difference and pivot wider to recreate LAIT table
la_table <- la_long |>
  dplyr::bind_rows(la_diff) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions"),
    names_from = Years,
    values_from = values_num
  ) |>
  pretty_num_table(dp = 1) |>
  dplyr::arrange(`LA and Regions`)



# - Local Authority specific stats table ---

# Extract change from prev year (from LA table)
la_change_prev <- la_table |>
  filter_la_regions(selected_la,
    latest = FALSE,
    pull_col = "Change from previous year"
  )
# Set the trend value
la_trend <- dplyr::case_when(
  is.na(la_change_prev) ~ NA_character_,
  la_change_prev > 0 ~ "increase",
  la_change_prev < 0 ~ "decrease",
  TRUE ~ "No trend"
)

# Get latest rank, ties are set to min & NA vals to NA rank
la_rank <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE) |>
  dplyr::mutate(
    rank = dplyr::case_when(
      is.na(values_num) ~ NA,
      TRUE ~ rank(values_num, ties.method = "min", na.last = TRUE)
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

# Get polarity of indicator
la_indicator_polarity <- filtered_bds |>
  pull_uniques("Polarity")

# Build stats table
la_table_stats <- data.frame(
  "LA Number" = la_table |>
    filter_la_regions(selected_la, pull_col = "LA Number"),
  "LA and Regions" = selected_la,
  "Trend" = la_trend,
  "Change from previous year" = la_change_prev,
  "Latest National Rank" = la_rank,
  "Quartile Banding" = la_quartile,
  "(A) Up to and including" = la_quartile_bands[["25%"]],
  "(B) Up to and including" = la_quartile_bands[["50%"]],
  "(C) Up to and including" = la_quartile_bands[["75%"]],
  "(D) Up to and including" = la_quartile_bands[["100%"]],
  check.names = FALSE
)

# Format stats table
dfe_reactable(
  la_table_stats,
  columns = list(
    `Quartile Banding` = reactable::colDef(
      style = reactablefmtr::cell_style(
        background_color = polarity_colours |>
          dplyr::filter(
            polarity == la_indicator_polarity,
            quartile_band == la_quartile
          ) |>
          dplyr::pull(cell_colour)
      )
    )
  )
)









# LA line chart plot ----------------------------------
# Plot
la_line_chart <- la_long |>
  ggplot2::ggplot() +
  ggiraph::geom_point_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      color = `LA and Regions`,
      shape = `LA and Regions`,
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
  set_plot_labs(filtered_bds, selected_indicator) +
  custom_theme()


# Creating vertical geoms to make vertical hover tooltip
vertical_hover <- lapply(
  get_years(la_long),
  tooltip_vlines,
  la_long
)

# Plotting interactive graph
ggiraph::girafe(
  ggobj = (la_line_chart + vertical_hover),
  width_svg = 8,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  )
)


# LA bar plot ----------------------------------
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
          pretty_num_table(include_columns = "values_num", dp = 1),
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
  set_plot_labs(filtered_bds, selected_indicator) +
  custom_theme()

# Plotting interactive graph
ggiraph::girafe(
  ggobj = la_bar_chart,
  width_svg = 8,
  options = generic_ggiraph_options()
)



# LA Metadata ----------------------------------

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
