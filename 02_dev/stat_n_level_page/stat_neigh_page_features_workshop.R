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
selected_indicator <- "Low birth weight"
selected_la <- "Barnet"

# Filter BDS for topic and indicator
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic == selected_topic,
    Measure == selected_indicator
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
    values_num = dplyr::na_if(mean(values_num, na.rm = TRUE), NaN),
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
  )

# Statistical Neighbour Level SN table ----------------------------------------
stat_n_sn_table <- stat_n_table |>
  dplyr::filter(`LA and Regions` %in% c(selected_la, stat_n_sns)) |>
  dplyr::arrange(.data[[current_year]], `LA and Regions`)

# Output table
dfe_reactable(
  stat_n_sn_table,
  # Create the reactable with specific column alignments
  columns = utils::modifyList(
    format_num_reactable_cols(
      stat_n_sn_table,
      get_indicator_dps(filtered_bds),
      num_exclude = "LA Number"
    ),
    set_custom_default_col_widths()
  ),
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
  columns = utils::modifyList(
    format_num_reactable_cols(
      stat_n_comp_table,
      get_indicator_dps(filtered_bds),
      num_exclude = "LA Number"
    ),
    set_custom_default_col_widths()
  ),
  rowStyle = function(index) {
    highlight_selected_row(index, stat_n_comp_table)
  },
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
  calculate_rank(stat_n_indicator_polarity) |>
  filter_la_regions(selected_la, pull_col = "rank")

# Calculate quartile bands for indicator
stat_n_quartile_bands <- filtered_bds |>
  filter_la_regions(la_names_bds, latest = TRUE, pull_col = "values_num") |>
  quantile(na.rm = TRUE)

# Extracting LA latest value
stat_n_indicator_val <- filtered_bds |>
  filter_la_regions(selected_la, latest = TRUE, pull_col = "values_num")

# Boolean as to whether to include Quartile Banding
no_show_qb <- selected_indicator %in% no_qb_indicators

# Calculating which quartile this value sits in
stat_n_quartile <- calculate_quartile_band(
  stat_n_indicator_val,
  stat_n_quartile_bands,
  stat_n_indicator_polarity,
  no_show_qb
)

# SN stats table
stat_n_stats_table <- build_sn_stats_table(
  stat_n_diff,
  stat_n_stats_geog,
  stat_n_trend,
  stat_n_change_prev,
  stat_n_rank,
  stat_n_quartile,
  stat_n_indicator_polarity
)

# Output stats table
dfe_reactable(
  stat_n_stats_table,
  rowStyle = function(index) {
    highlight_selected_row(index, stat_n_stats_table, selected_la)
  },
  columns = modifyList(
    format_num_reactable_cols(
      stat_n_stats_table,
      get_indicator_dps(filtered_bds),
      num_exclude = "LA Number",
      categorical = c("Trend", "Quartile Banding", "Latest National Rank")
    ),
    # Define specific formatting for the Trend and Quartile Banding columns
    list(
      set_custom_default_col_widths(),
      Trend = reactable::colDef(
        cell = trend_icon_renderer,
        style = function(value) {
          get_trend_colour(value, stat_n_stats_table$Polarity[1])
        }
      ),
      # Just colour the QB cell
      `Quartile Banding` = reactable::colDef(
        style = function(value, index) {
          quartile_banding_col_def(stat_n_stats_table[index, ])
        }
      ),
      Polarity = reactable::colDef(show = FALSE)
    )
  )
)


# Statistical Neighbour Level SN focus plot -----------------------------------
# Set selected LA to last level so appears at front of plot
focus_line_data <- stat_n_long |>
  dplyr::filter(`LA and Regions` %in% c(selected_la, stat_n_sns)) |>
  reorder_la_regions(selected_la, after = Inf)

region_line_chart <- focus_line_data |>
  ggplot2::ggplot() +
  ggiraph::geom_line_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      color = `LA and Regions`,
      size = `LA and Regions`,
      data_id = `LA and Regions`,
    ),
    na.rm = TRUE
  ) +
  format_axes(focus_line_data) +
  set_plot_colours(focus_line_data, colour_type = "focus", focus_group = selected_la) +
  set_plot_labs(filtered_bds) +
  ggrepel::geom_label_repel(
    data = subset(focus_line_data, Years == current_year),
    aes(
      x = Years_num,
      y = values_num,
      label = `LA and Regions`
    ),
    color = "black",
    segment.colour = NA,
    label.size = NA,
    max.overlaps = Inf,
    nudge_x = 2,
    direction = "y",
    hjust = 1,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  custom_theme() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 66, 5.5, 5.5)) +
  guides(color = "none", size = "none")


# Creating vertical geoms to make vertical hover tooltip
vertical_hover <- lapply(
  get_years(focus_line_data),
  tooltip_vlines,
  focus_line_data,
  indicator_dps,
  selected_la
)

# Plotting interactive graph
ggiraph::girafe(
  ggobj = (region_line_chart + vertical_hover),
  width_svg = 12,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)


# Statistical Neighbour Level SN multi-choice line plot -----------------------
# Randomly select up to 6 Geog areas for plotting
set.seed(123) # Set seed for reproducibility (optional)
stat_n_random_selection <- stat_n_long |>
  pull_uniques("LA and Regions") |>
  as.character() |>
  sample(size = 3) # Select up to 6 randomly

# Filter Statistical Neighbour data for these areas
stat_n_line_chart_data <- stat_n_long |>
  # Filter for random areas - simulate user choosing up to 6 areas
  dplyr::filter(
    (`LA and Regions` %in% stat_n_random_selection) |
      (`LA and Regions` %in% selected_la)
  ) |>
  # Set area orders so selection order starts on top of plot
  reorder_la_regions(rev(c(selected_la, stat_n_random_selection)), after = Inf)

# Plot - selected areas
stat_n_line_chart <- stat_n_line_chart_data |>
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
  set_plot_colours(
    data.frame(
      `LA and Regions` = c(selected_la, stat_n_random_selection),
      check.names = FALSE
    ),
    "colour",
    selected_la
  ) +
  set_plot_labs(filtered_bds) +
  custom_theme() +
  # Revert order of the legend so goes from right to left
  ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))


# Creating vertical geoms to make vertical hover tooltip
vertical_hover <- lapply(
  get_years(stat_n_line_chart_data),
  tooltip_vlines,
  stat_n_line_chart_data |>
    reorder_la_regions(c(selected_la, stat_n_random_selection)),
  indicator_dps
)

# Plotting interactive graph
ggiraph::girafe(
  ggobj = (stat_n_line_chart + vertical_hover),
  width_svg = 8.5,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)


# Statistical Neighbour focus bar plot ----------------------------------------
focus_bar_data <- focus_line_data |>
  reorder_la_regions(selected_la)

stat_n_focus_bar_chart <- focus_bar_data |>
  ggplot2::ggplot() +
  ggiraph::geom_col_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      fill = `LA and Regions`,
      tooltip = tooltip_bar(
        focus_bar_data,
        indicator_dps,
        selected_la
      ),
      data_id = `LA and Regions`
    ),
    position = "dodge",
    width = 0.6,
    na.rm = TRUE,
    colour = "black"
  ) +
  format_axes(focus_bar_data) +
  set_plot_colours(focus_bar_data, "focus-fill", selected_la) +
  set_plot_labs(filtered_bds) +
  custom_theme() +
  guides(fill = "none")

# Plotting interactive graph
ggiraph::girafe(
  ggobj = stat_n_focus_bar_chart,
  width_svg = 8.5,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)


# Statistical Neighbour multi-choice bar plot ---------------------------------
stat_n_bar_multi_data <- stat_n_line_chart_data |>
  reorder_la_regions(selected_la)

stat_n_multi_bar_chart <- stat_n_bar_multi_data |>
  ggplot2::ggplot() +
  ggiraph::geom_col_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      fill = `LA and Regions`,
      tooltip = tooltip_bar(stat_n_bar_multi_data, indicator_dps),
      data_id = `LA and Regions`
    ),
    position = "dodge",
    width = 0.6,
    na.rm = TRUE,
    colour = "black"
  ) +
  format_axes(stat_n_bar_multi_data) +
  set_plot_colours(stat_n_bar_multi_data, "fill", selected_la) +
  set_plot_labs(filtered_bds) +
  custom_theme()

# Plotting interactive graph
ggiraph::girafe(
  ggobj = stat_n_multi_bar_chart,
  width_svg = 8.5,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)
