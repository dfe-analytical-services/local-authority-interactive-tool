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
input$all_las <- FALSE
input$all_regions <- FALSE
input$region_las <- FALSE
input$stat_ns <- TRUE


# User inputs which depend off selected LA and indicator
selectable_years <- filtered_bds |>
  pull_uniques("Years")

# Setting combination of user input choices (regarding geographies)
input$geog_input <- selected_la
user_geog_inputs <- c()

# Append all LAs
if (isTRUE(input$all_las)) {
  user_geog_inputs <- c(user_geog_inputs, la_names_bds)
}

# Append all Regions
if (isTRUE(input$all_regions)) {
  user_geog_inputs <- c(user_geog_inputs, region_names_bds)
}

# Append Regions LAs
if (isTRUE(input$region_las)) {
  # LAs in same region as selected LA
  selected_la_regions <- get_la_region(stat_n_geog, input$geog_input)
  all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)

  user_geog_inputs <- c(user_geog_inputs, all_region_las)
}

# Append LA statistical neighbours
if (isTRUE(input$stat_ns)) {
  # LA statistical neighbours
  selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, input$geog_input)

  user_geog_inputs <- c(user_geog_inputs, selected_la_stat_n)
}

user_geog_inputs <- unique(user_geog_inputs)


# Filtering BDS for inputs
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic %in% selected_topic,
    Measure %in% selected_indicator,
    `LA and Regions` %in% user_geog_inputs,
    !is.na(Years)
  )

# Reactive value "query" used to store query data
# Uses lists to store multiple inputs (geographies)
query <- data.frame(
  Topic = I(list()),
  Measure = I(list()),
  `LA and Regions` = I(list()),
  `Click to remove query` = character(),
  check.names = FALSE
)

# Build the staging table (select data and make wide)
staging_table <- filtered_bds |>
  dplyr::select(
    `LA Number`, `LA and Regions`, Topic,
    Measure, Years, Years_num, values_num, Values
  ) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
    names_from = Years,
    values_from = values_num,
  )

# Output staging table
reactable::reactable(
  staging_table
)

# Get query information
new_query <- data.frame(
  Topic = I(list(selected_topic)),
  Indicator = I(list(selected_indicator)),
  `LA and Regions` = I(list(
    get_geog_selection(input, la_names_bds, region_names_bds, stat_n_geog)
  )),
  `Click to remove query` = "Remove",
  check.names = FALSE
)

# Append query to existing query data
query <- query |>
  # Remove row identifier as not needed yet (and not available initially)
  # dplyr::select(-.internal_uuid) |>
  rbind(new_query)


# Check if there are any selected measures
if (nrow(query) == 0) {
  return(reactable::reactable(
    data.frame(
      `Message from tool` = "Please add queries.",
      check.names = FALSE
    )
  ))
}

# Main logic for query data processing
selected_indicators <- unique(unlist(query$Indicator))
no_selected_indicators <- length(selected_indicators)

if (no_selected_indicators == 0) {
  return(reactable::reactable(
    data.frame(
      `Message from tool` = "No years available for selected measures.",
      check.names = FALSE
    )
  ))
}

# Filter BDS for selected indicators
raw_indicator_filtered_bds <- filter_bds_for_indicators(bds_metrics, selected_indicators)

# Check if all years have consistent suffix
query_str_years <- check_year_suffix_consistency(raw_indicator_filtered_bds)

# Get min and max years
year_bounds <- get_min_max_years(raw_indicator_filtered_bds)
all_years <- create_years_df(year_bounds$min_year, year_bounds$max_year)

final_query_data <- data.frame()

# Loop through each row in the query table and process data
for (i in seq_len(nrow(query))) {
  current_topic <- query$Topic[[2]]
  current_measure <- query$Indicator[[2]]
  current_geog <- query$`LA and Regions`[[2]]

  # Sorting geography filters
  # Adding all LAs in selected LA regions
  if (any(grepl("LAs in ", current_geog))) {
    current_la_regions <- current_geog |>
      stringr::str_subset("^LAs in ") |> # Subset strings that start with "LAs in "
      stringr::str_remove("^LAs in ") # Remove the "LAs in " prefix

    # Extract all LAs in the region and add to current geog
    current_region_las <- get_las_in_regions(stat_n_geog, current_la_regions)
    current_geog <- c(current_geog, current_region_las)
  }
  # Adding statistical neighbours of selected LAs
  if (any(grepl(" statistical neighbours", current_geog))) {
    current_la_sns <- current_geog |>
      stringr::str_subset(" statistical neighbours$") |>
      stringr::str_remove(" statistical neighbours$")

    # Extract all statistical neighbours
    selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, current_la_sns)
    current_geog <- c(current_geog, selected_la_stat_n)
  }
  # Append all LAs or Regions to the current geography if needed
  if ("All LAs" %in% current_geog) current_geog <- c(current_geog, la_names_bds)
  if ("All Regions" %in% current_geog) current_geog <- c(current_geog, region_names_bds)

  current_geog <- unique(current_geog)

  # Filter BDS for the current query
  raw_query_data <- bds_metrics |>
    dplyr::filter(
      Topic %in% current_topic,
      Measure %in% current_measure,
      `LA and Regions` %in% current_geog,
      !is.na(Years)
    ) |>
    dplyr::select(
      `LA Number`, `LA and Regions`, Topic, Measure, Years, Years_num, values_num, Values
    )

  # Join the data with the full years to fill gaps
  full_query_data <- dplyr::full_join(raw_query_data, all_years, by = "Years_num") |>
    tidyr::pivot_wider(
      id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
      names_from = ifelse(query_str_years || no_selected_indicators == 1, "Years", "Years_num"),
      values_from = values_num
    )

  # Sort year columns with full names preserved
  sorted_year_cols <- sort_year_columns(full_query_data)

  clean_query_data <- full_query_data |>
    dplyr::filter(!dplyr::if_all(everything(), is.na)) |>
    dplyr::select(
      `LA Number`, `LA and Regions`, Topic, Measure,
      dplyr::all_of(sorted_year_cols)
    )

  # Append the cleaned query data to final query data
  final_query_data <- dplyr::bind_rows(final_query_data, clean_query_data)
}

# Check if final_data has any rows before rendering
if (nrow(final_query_data) == 0) {
  return(reactable::reactable(
    data.frame(
      `Message from tool` = "No data available based on the selected queries.",
      check.names = FALSE
    )
  ))
}

# Output final table
reactable::reactable(final_query_data)




















test_data <- data.frame(
  # Store as a list to allow multiple topics
  Topic = I(list(selected_topic)),
  Measure = I(list(selected_indicator)),
  `LA and Regions` = I(list(selected_la)),
  `Click to remove query` = "Remove",
  .internal_uuid = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)


reactable::reactable(
  test_data,
  columns = list(
    # JS used from `reactable.extras::button_extra()` to create btn in table
    # Uses the row identifier to know which row to remove
    `Click to remove query` = reactable::colDef(
      cell = reactable::JS(
        "function(cellInfo) {
            // Use the unique row identifier as the button ID
            const buttonId = 'remove_' + cellInfo.row['.internal_uuid'];
            return React.createElement(ButtonExtras, {
              id: buttonId,
              label: 'Remove',
              uuid: cellInfo.row['.internal_uuid'],
              column: cellInfo.column.id,
              class: 'govuk-button--warning',
              className: 'govuk-button--warning'
            }, cellInfo.index);
          }"
      )
    ),
    # Don't show row identifier
    .internal_uuid = reactable::colDef(show = FALSE)
  )
)



test_bds_metrics <- data.frame(
  `LA Number` = c(1, 1, 1, 2, 2, 2, 3, 3),
  `LA and Regions` = c(
    "East Midlands", "East Midlands", "East Midlands",
    "London", "London", "London",
    "North West", "North West"
  ),
  Topic = c("Health", "Health", "Health", "Education", "Education", "Education", "Economy", "Economy"),
  Measure = c(
    "Measure A", "Measure A", "Measure A",
    "Measure B", "Measure B", "Measure B",
    "Measure C", "Measure C"
  ),
  Years = c("2016 (prov)", "2014", "2015", "2016 (prov)", "2015", "2014", "2016 (prov)", "2014"),
  Years_num = c(2016, 2014, 2015, 2016, 2015, 2014, 2016, 2014),
  values_num = c(100, 80, 90, 95, 85, 75, 110, 70),
  Values = c("100k", "80k", "90k", "95k", "85k", "75k", "110k", "70k"),
  check.names = FALSE
)

test_query <- list(
  data = data.frame(
    Topic = I(list("Health", "Education")),
    Measure = I(list("Measure A", "Measure B")),
    `LA and Regions` = I(list(c("East Midlands"), c("London"))),
    check.names = FALSE
  )
)

test_bds_metrics |>
  pull_uniques("Years") |>
  substring(5) |>
  stringr::str_replace_all("\\d", "") |> # Remove any digits from position 5 onwards
  {
    \(x) all(x == x[1])
  }()

test_all_years <- data.frame(Years_num = seq(2014, 2016))

# Get the current query values
current_topic <- test_query$data$Topic[1]
current_measure <- test_query$data$Measure[1]
current_geog <- test_query$data$`LA and Regions`[1]

# Set geography filters
# Append all LAs
if ("All LAs" %in% current_geog) {
  current_geog <- c(current_geog, la_names_bds)
}
# Append all Regions
if ("All Regions" %in% current_geog) {
  current_geog <- c(current_geog, region_names_bds)
}
# Return unique geogs
current_geog <- unique(current_geog)

# Filter BDS for the current query
raw_query_data <- test_bds_metrics |>
  dplyr::filter(
    Topic %in% current_topic,
    Measure %in% current_measure,
    `LA and Regions` %in% current_geog
  ) |>
  dplyr::select(
    `LA Number`, `LA and Regions`, Topic,
    Measure, Years, Years_num, values_num, Values
  )

# Create the cleaned query
# Merge the temp_data with the all_years data to ensure query has
# year cols to match other queries (for easy row join)
full_query_data <- dplyr::full_join(
  raw_query_data,
  test_all_years,
  by = c("Years_num")
) |>
  # Use string Years if all suffix is the same
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
    names_from = ifelse(TRUE, "Years", "Years_num"),
    values_from = values_num
  )

# Extract and sort year columns with full names preserved
full_query_year_cols <- names(full_query_data)[grepl("^\\d{4}", names(full_query_data))]

# Create a named vector for sorting
full_query_sorted_year_cols <- full_query_year_cols |>
  purrr::set_names() |>
  purrr::map_chr(~ stringr::str_sub(.x, 1, 4)) |> # Extract first 4 characters
  sort() |> # Sort numerically
  names()

clean_query_data <- full_query_data |>
  # Remove any all NA rows
  # (created from join where indicator has missing year)
  dplyr::filter(!dplyr::if_all(everything(), is.na)) |>
  # Order years cols suitably
  dplyr::select(
    `LA Number`,
    `LA and Regions`,
    Topic,
    Measure,
    dplyr::all_of(full_query_sorted_year_cols) # Select sorted year columns
  )

# Combine the current query into the final query data frame
final_query_data <- dplyr::bind_rows(final_query_data, clean_query_data)
