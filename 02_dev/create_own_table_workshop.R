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


# Filter BDS for topic and indicator
filtered_bds <- bds_metrics |>
  dplyr::filter(
    Topic == selected_topic,
    Measure == selected_indicator,
    !is.na(Years)
  )

# User inputs which depend off selected LA and indicator
selectable_years <- filtered_bds |>
  pull_uniques("Years")

# Cleaned selected LA region
selected_region <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la) |>
  pull_uniques("GOReg") |>
  clean_ldn_region(filtered_bds)

# LAs in same region as selected LA
selectable_region_las <- filtered_bds |>
  # Apply cleaning London region to df
  dplyr::mutate(clean_region = sapply(Region, clean_ldn_region)) |>
  dplyr::filter(clean_region %in% selected_region)

# LA statistical neighbours
selectable_sns <- stat_n_la |>
  dplyr::filter(`LA Name` == selected_la) |>
  pull_uniques("LA Name_sn")

# General categories
la_names_bds
region_names_bds
"England"
core_cities <- c(
  "Birmingham", "Bristol", "Leeds", "Liverpool",
  "Manchester", "Newcastle", "Nottingham", "Sheffield"
)

# Creating the table ready for
create_own_bds <- filtered_bds |>
  dplyr::filter(`LA and Regions` %in% selected_la) |>
  dplyr::select(`LA Number`, `LA and Regions`, Topic, Measure, Years, Years_num, values_num, Values) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
    names_from = Years,
    values_from = values_num,
  )

create_own_bds <- create_own_bds |>
  dplyr::mutate(
    Include = rep(TRUE, nrow(create_own_bds)),
    `Region LAs` = rep(FALSE, nrow(create_own_bds)),
    `Remove Row` = rep("Remove", nrow(create_own_bds))
  )




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
