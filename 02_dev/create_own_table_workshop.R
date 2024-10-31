# Load global
if (exists("bds_metrics")) {
  source(here::here("global.R"))
}

# Load functions
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)


# LAIT Statistical Neighbour Level page ---------------------------------------
# - Regional Authorities
# Set user inputs
selected_la <- c("Barking and Dagenham", "Barnet")
selected_topic <- "Children's Service Finance"
selected_indicator <- "Total Services for Young People  (finance) - Gross"

# Other input groupings
input <- list()
input$la_groups <- "none" # la_stat_ns, region_las, all_las
input$all_regions <- FALSE
input$inc_england <- TRUE
input$stat_ns <- FALSE
input$year_range <- c(2021, 2022)
input$geog_input <- selected_la
input$topic_input <- selected_topic
input$indicator <- selected_indicator




# Filter the dataset for selected indicators and get distinct years
years_dict <- bds_metrics |>
  dplyr::filter(Measure %in% input$indicator, !is.na(Years)) |>
  dplyr::distinct(Years, Years_num)

# Boolean to check for consistent year suffixes
consistent_year_suffix <- years_dict |>
  check_year_suffix_consistency()

# Assign choices based on suffix consistency
years_choices <- if (consistent_year_suffix) {
  sort(years_dict$Years)
} else {
  sort(years_dict$Years_num)
}


# Reactive-style functions without using `observeEvent` or `reactive`
# Simulating selected indicators based on topic
filtered_topic_bds <- bds_metrics |>
  dplyr::filter(
    Topic %in% input$topic_input,
    Measure %in% input$indicator
  ) |>
  dplyr::select(Topic, Measure)

# Simulate updating indicator choices
selected_indicators <- unique(filtered_topic_bds)

# Simulating geog_inputs logic without reactives
geog_inputs <- {
  inputs <- input$geog_input
  if (input$la_groups == "all_las") {
    inputs <- unique(c(inputs, la_names_bds))
  }
  if (input$all_regions) {
    inputs <- unique(c(inputs, region_names_bds))
  }
  if (input$inc_england) {
    inputs <- unique(c(inputs, "England"))
  }
  if (input$la_groups == "region_las") {
    selected_la_regions <- get_la_region(stat_n_geog, input$geog_input)
    all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)
    inputs <- unique(c(inputs, all_region_las))
  }
  if (input$la_groups == "la_stat_ns") {
    selected_la_stat_n <- unlist(lapply(input$geog_input, get_la_stat_neighbrs, data = stat_n_la))
    inputs <- c(inputs, selected_la_stat_n)
  }
  unique(inputs)
}

# Simulating stat_n_association logic without reactives
stat_n_association <- {
  association_table <- data.frame(`LA and Regions` = character(), `sn_parent` = character(), check.names = FALSE)
  if (input$la_groups == "la_stat_ns") {
    input_las <- intersect(input$geog_input, la_names_bds)
    stat_n_groups <- lapply(input_las, function(la) {
      data.frame(
        `LA and Regions` = c(la, get_la_stat_neighbrs(stat_n_la, la)),
        `sn_parent` = la,
        check.names = FALSE
      )
    })
    if (length(input_las) > 0) {
      association_table <- do.call(rbind, stat_n_groups)
    }
  }
  association_table
}

# Filtered BDS
# Step 1: Filter by topic, indicator, and geography
filtered_bds <- dplyr::semi_join(
  bds_metrics,
  selected_indicators,
  by = c("Topic" = "Topic", "Measure" = "Measure")
) |>
  dplyr::filter(
    `LA and Regions` %in% geog_inputs,
    !is.na(Years)
  )

# Step 2: Check if year suffixes are consistent
consistent_str_years <- check_year_suffix_consistency(filtered_bds)

# Step 3: Update to numeric years if inconsistent suffixes
if (!consistent_str_years) {
  filtered_bds <- filtered_bds |>
    dplyr::mutate(Years = Years_num)
}

# Step 4: Apply year range filter
if (length(input$year_range) == 1) {
  filtered_bds <- filtered_bds |>
    dplyr::filter(Years == input$year_range[1])
} else if (length(input$year_range) == 2) {
  filtered_bds <- filtered_bds |>
    dplyr::filter(Years >= input$year_range[1])
}


# Staging table
# Step 1: Select relevant columns and pivot to wide format
wide_table <- filtered_bds |>
  dplyr::select(
    `LA Number`, `LA and Regions`, Topic,
    Measure, Years, Years_num, values_num, Values
  ) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
    names_from = Years,
    values_from = values_num
  ) |>
  dplyr::left_join(
    stat_n_geog |>
      dplyr::select(`LA num`, GOReg),
    by = c("LA Number" = "LA num")
  ) |>
  dplyr::mutate(GOReg = dplyr::case_when(
    `LA and Regions` %in% c("England", region_names_bds) ~ `LA and Regions`,
    TRUE ~ GOReg
  ))

# Step 2: Order columns and sort year columns
wide_table_ordered <- wide_table |>
  dplyr::select(
    `LA Number`, `LA and Regions`,
    "Region" = "GOReg",
    Topic, Measure,
    dplyr::all_of(sort_year_columns(wide_table))
  )

# Step 3: Add sn association
# If SNs included, add SN LA association column
# Multi-join as want to include an association for every row (even duplicates)
if (isTRUE(input$la_groups == "la_stat_ns")) {
  wide_table_ordered <- wide_table_ordered |>
    dplyr::left_join(
      stat_n_association,
      by = "LA and Regions",
      relationship = "many-to-many"
    ) |>
    dplyr::relocate(sn_parent, .after = "Measure") |>
    dplyr::rename("Statistical Neighbour Group" = "sn_parent")
}

staging_table <- wide_table_ordered

# Staging table output
dfe_reactable(
  staging_table,
  columns = utils::modifyList(
    format_num_reactable_cols(
      staging_table,
      get_indicator_dps(filtered_bds),
      num_exclude = c("LA Number", "Measure")
    ),
    list(
      set_custom_default_col_widths(
        Measure = set_min_col_width(90)
      ),
      # Truncates long cell values and displays hover with full value
      Measure = reactable::colDef(
        html = TRUE,
        cell = function(value, index, name) {
          render.reactable.cell.with.tippy(text = value, tooltip = value)
        }
      )
    )
  ),
  defaultPageSize = 3,
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(3, 5, 10, 25),
  compact = TRUE
)



# Query data frame
query <- list(
  data = data.frame(
    Topic = I(list()),
    Indicator = I(list()),
    `LA and Regions` = I(list()),
    `Year range` = I(list()),
    `Click to remove query` = character(),
    `.query_id` = numeric(),
    check.names = FALSE
  ),
  output = data.frame(
    `LA Number` = character(),
    `LA and Regions` = character(),
    Region = character(),
    Topic = character(),
    Measure = character(),
    check.names = FALSE
  )
)

input$add_query <- TRUE

# Query table
# Step 1: When "Add table" button clicked - add query to saved queries
if (input$add_query) {
  # Check if anything selected
  if (length(geog_inputs) > 0 && nrow(selected_indicators) > 0) {
    # Create a unique identifier for the new query
    new_q_id <- max(c(0, query$data$.query_id), na.rm = TRUE) + 1

    # Creating year range info
    available_years <- range(years_choices)

    # Define the year range info logic
    year_range_display <- dplyr::case_when(
      length(input$year_range) == 0 ~ paste0(
        "All years (", available_years[1], " to ", available_years[2], ")"
      ),
      length(input$year_range) == 2 ~ paste(
        input$year_range[1], "to", input$year_range[2]
      ),
      length(input$year_range) == 1 ~ paste0("", input$year_range[1])
    )

    # Create query information
    new_query <- data.frame(
      .query_id = new_q_id,
      Topic = I(list(selected_indicators$Topic)),
      Indicator = paste(selected_indicators$Measure, collapse = ",<br>"),
      `LA and Regions` = paste(
        get_geog_selection(input, la_names_bds, region_names_bds, stat_n_geog),
        collapse = ",<br>"
      ),
      `Year range` = year_range_display,
      `Click to remove query` = "Remove",
      check.names = FALSE
    )

    # Append new query to the existing queries
    query$data <- rbind(query$data, new_query)

    # Appending the data of the new query to the output table
    # Adding new query ID to staging data
    staging_to_append <- staging_table
    staging_to_append$.query_id <- new_q_id

    # Example check for year consistency
    consistent_staging_final_yrs <- TRUE # Assume suffixes are consistent for this example

    # Append to query output based on consistency
    if (!consistent_staging_final_yrs && nrow(query$output) > 0) {
      # Logic for cleaning year columns if needed
      # Not implemented here; assume consistency for simplicity
      query$output <- dplyr::bind_rows(query$output, staging_to_append)
    } else {
      query$output <- dplyr::bind_rows(query$output, staging_to_append)
    }
  }
}


# Final output table
# Check if the output indicators have consistent suffixes and set column names accordingly
output_indicators <- query$output |>
  pull_uniques("Measure")

# Boolean for if the output indicators share suffixes
share_year_suffix <- bds_metrics |>
  dplyr::filter(
    Measure %in% output_indicators,
    !is.na(Years)
  ) |>
  check_year_suffix_consistency()

# If suffixes are shared then reapply the years with suffix to col names
if (share_year_suffix) {
  # Get the years with suffixes
  years_dict <- bds_metrics |>
    dplyr::filter(
      Measure %in% output_indicators,
      !is.na(Years)
    ) |>
    dplyr::distinct(Years, Years_num)

  # Replace the matching year col names with respective year suffix
  new_col_names <- colnames(query$output) |>
    (\(cols) ifelse(cols %in% years_dict$Years_num,
      years_dict$Years[match(cols, years_dict$Years_num)],
      cols
    ))()

  # Apply the new year suffix names to query$output
  colnames(query$output) <- new_col_names
}

# Final query output table with ordered columns, (SN parent if selected) and
# Sorted year columns
clean_final_table <- query$output |>
  dplyr::select(
    `LA Number`, `LA and Regions`,
    Region, Topic, Measure,
    tidyselect::any_of("Statistical Neighbour Group"),
    dplyr::all_of(sort_year_columns(query$output))
  )

# Final filtered BDS
output_table_filters <- clean_final_table |>
  dplyr::distinct(`LA and Regions`, Topic, Measure)

final_filtered_bds <- bds_metrics |>
  dplyr::semi_join(
    output_table_filters,
    by = c("LA and Regions", "Topic", "Measure")
  )


number_of_indicators <- length(pull_uniques(clean_final_table, "Measure"))

number_of_geogs <- length(pull_uniques(clean_final_table, "LA and Regions"))

# Create chart data ----------------------------------------------------------
if (number_of_indicators > 3 || number_of_geogs > 4) {
  print("TOO MANY SELECTIONS")
}

# Pull order that geogs and indicators are added
# This is used to set the levels of the factor (so display in order in chart)
geog_chart_order <- query$data |>
  get_query_table_values(`LA and Regions`)

indicator_chart_order <- query$data |>
  get_query_table_values(Indicator)

# Coerce final output table to long data (for plotting)
# Recreate Years_num & values_num, also factor `LA and Regions` & Measure
chart_plotting_data <- clean_final_table |>
  dplyr::distinct() |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("20"),
    names_to = "Years",
    values_to = "Values"
  ) |>
  dplyr::mutate(
    Years_num = as.numeric(substr(Years, start = 1, stop = 4)),
    values_num = Values,
    `LA and Regions` = factor(`LA and Regions`, levels = geog_chart_order),
    Measure = factor(Measure, levels = indicator_chart_order)
  )

# Line chart -----------------------------------------------------------------
# Count year cols - used to determine if to show geom_point
# (If only one year then no line will show so point needed)
num_year_cols <- chart_plotting_data |>
  dplyr::distinct(Years) |>
  nrow()


# Build static main plot
# Plot data - colour represents Geographies & linetype represents Indicator
line_chart <- chart_plotting_data |>
  ggplot2::ggplot() +
  ggiraph::geom_line_interactive(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      color = `LA and Regions`,
      linetype = Measure,
      data_id = `LA and Regions`
    ),
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = Years_num,
      y = values_num,
      color = `LA and Regions`
    ),
    na.rm = TRUE,
    size = ifelse(num_year_cols == 1, 3, 0),
    shape = 16
  ) +
  format_axes(chart_plotting_data) +
  set_plot_colours(chart_plotting_data) +
  set_plot_labs(final_filtered_bds) +
  custom_theme() +
  # Setting legend title at top
  ggplot2::theme(
    legend.title = ggplot2::element_text(),
    legend.title.position = "top",
    legend.spacing.x = unit(5, "lines")
  ) +
  # Creating nice looking legend content
  ggplot2::guides(
    color = ggplot2::guide_legend(
      order = 1,
      ncol = 1,
      title = "Geographies (colour):",
      override.aes = list(size = 3, shape = 15, linetype = NULL)
    ),
    linetype = ggplot2::guide_legend(
      order = 2,
      ncol = 1,
      title = "Indicators (line-type):"
    )
  )

# Build interactive line chart
# Creating vertical geoms to make vertical hover tooltip
vertical_hover <- lapply(
  get_years(chart_plotting_data),
  tooltip_vlines,
  chart_plotting_data,
  get_indicator_dps(final_filtered_bds),
  TRUE
)

# Plotting interactive graph
ggiraph::girafe(
  ggobj = (line_chart + vertical_hover),
  width_svg = 8.5,
  options = generic_ggiraph_options(
    opts_hover(
      css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
    )
  ),
  fonts = list(sans = "Arial")
)
