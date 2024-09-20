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

# Get the LA region
region_la <- stat_n_geog |>
  dplyr::filter(`LA Name` == selected_la) |>
  dplyr::pull(GOReg)

# Get other LAs in the region
region_la_la <- stat_n_geog |>
  dplyr::filter(GOReg == region_la) |>
  pull_uniques("LA Name")

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
      #   "Statistical Neighbours", "England"
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
  pretty_num_table(
    dp = indicator_dps,
    exclude_columns = "LA Number"
  ) |>
  dplyr::arrange(.data[[current_year]], `LA and Regions`)

# Render reactable table
dfe_reactable(
  region_la_table,
  columns = align_reactable_cols(region_la_table, num_exclude = "LA Number"),
  rowStyle = function(index) {
    highlight_selected_row(index, region_la_table, selected_la)
  }
)


# Regional Level Regions table ------------------------------------------------
# Filter for all regions and England
region_filtered_bds <- filtered_bds |>
  dplyr::filter(
    `LA and Regions` %in% c(region_names_bds, "England")
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
      #   "Statistical Neighbours", "England"
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
  pretty_num_table(
    dp = indicator_dps,
    exclude_columns = "LA Number"
  ) |>
  dplyr::arrange(.data[[current_year]], `LA and Regions`) |>
  # Places England row at the bottom of the table
  dplyr::mutate(is_england = ifelse(grepl("^England", `LA and Regions`), 1, 0)) |>
  dplyr::arrange(is_england, .by_group = FALSE) |>
  dplyr::select(-is_england)


# Regional Level Stats table --------------------------------------------------
# Determines which London to use
# Some indicators are not provided at (Inner)/ (Outer) level
region_la_ldn_clean <- clean_ldn_region(region_la, filtered_bds)

# Get LA numbers
# Selected LA
region_la_la_num <- region_la_table |>
  filter_la_regions(selected_la, pull_col = "LA Number")

# Region and England
region_la_num <- region_table |>
  filter_la_regions(c(region_la_ldn_clean, "England"), pull_col = "LA Number")

# Get change in previous year
# Selected LA
region_la_change_prev <- region_la_table |>
  filter_la_regions(selected_la,
    latest = FALSE,
    pull_col = "Change from previous year"
  )

# Region and England
region_change_prev <- region_table |>
  filter_la_regions(c(region_la_ldn_clean, "England"),
    latest = FALSE,
    pull_col = "Change from previous year"
  )

# Creating the stats table cols
region_stats_la_num <- c(region_la_la_num, region_la_num)
region_stats_name <- c(selected_la, region_la_ldn_clean, "England")
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
  "Change from previous year" = region_stats_change,
  check.names = FALSE
)


# Region line chart plot ------------------------------------------------------
# Filter any regions with all NA values and England
region_long_plot <- region_long |>
  dplyr::group_by(`LA and Regions`) |>
  # Remove any "London (" regions where all values_num are NA
  dplyr::filter(
    !(grepl("^London \\(", `LA and Regions`) & dplyr::n() == sum(is.na(values_num))),
    `LA and Regions` %notin% "England"
  )


# Randomly select up to 6 Regions for plotting
set.seed(123) # Set seed for reproducibility (optional)
region_random_selection <- region_long_plot |>
  pull_uniques("LA and Regions") |>
  as.character() |>
  sample(size = 3) # Select up to 6 randomly

region_line_chart_data <- region_long_plot |>
  # Filter for random Regions - simulate user choosing up to 6 regions
  dplyr::filter(
    (`LA and Regions` %in% region_random_selection) |
      (`LA and Regions` %in% region_la_ldn_clean)
  ) |>
  # Set region orders so selection order starts on top of plot
  reorder_la_regions(rev(c(region_la_ldn_clean, region_random_selection)), after = Inf)

# Plot - selected Regions
region_line_chart <- region_line_chart_data |>
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
  format_axes(region_line_chart_data) +
  manual_colour_mapping(
    c(region_la_ldn_clean, region_random_selection),
    type = "line"
  ) +
  set_plot_labs(filtered_bds) +
  custom_theme() +
  # Revert order of the legend so goes from right to left
  ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))


# Creating vertical geoms to make vertical hover tooltip
vertical_hover <- lapply(
  get_years(region_line_chart_data),
  tooltip_vlines,
  region_line_chart_data,
  indicator_dps
)

# Plotting interactive graph
ggiraph::girafe(
  ggobj = (region_line_chart + vertical_hover),
  width_svg = 8,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)

# Focus plot
# Set selected region to last level so appears at front of plot
focus_line_data <- region_long_plot |>
  reorder_la_regions(region_la_ldn_clean, after = Inf)

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
  set_plot_colours(focus_line_data, colour_type = "focus", focus_group = region_la_ldn_clean) +
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
    show.legend = FALSE
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
  indicator_dps
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


# Region bar plot -----------------------------------------------------------------
# Focus plot
focus_bar_data <- region_long_plot |>
  reorder_la_regions(region_la_ldn_clean)

la_bar_chart <- focus_bar_data |>
  ggplot2::ggplot() +
  ggiraph::geom_col_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      fill = `LA and Regions`,
      tooltip = glue::glue_data(
        focus_bar_data |>
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
  format_axes(focus_bar_data) +
  set_plot_colours(focus_bar_data, "focus-fill", region_la_ldn_clean) +
  set_plot_labs(filtered_bds) +
  custom_theme() +
  guides(fill = "none")

# Plotting interactive graph
ggiraph::girafe(
  ggobj = la_bar_chart,
  width_svg = 8,
  options = generic_ggiraph_options(),
  fonts = list(sans = "Arial")
)
