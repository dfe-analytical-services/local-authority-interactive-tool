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



bds_metrics <- data.frame(
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
  Years = c("2016", "2014", "2015", "2016", "2015", "2014", "2016", "2014"),
  Years_num = c(2016, 2014, 2015, 2016, 2015, 2014, 2016, 2014),
  values_num = c(100, 80, 90, 95, 85, 75, 110, 70),
  Values = c("100k", "80k", "90k", "95k", "85k", "75k", "110k", "70k"),
  check.names = FALSE
)

query <- list(
  data = data.frame(
    Topic = I(list("Health", "Education")),
    Measure = I(list("Measure A", "Measure B")),
    `LA and Regions` = I(list(c("East Midlands"), c("London"))),
    check.names = FALSE
  )
)

all_years <- data.frame(Years_num = seq(2014, 2016))

# Get the current query values
current_topic <- query$data$Topic[[1]]
current_measure <- query$data$Measure[[1]]
current_geog <- query$data$`LA and Regions`[[1]]

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
raw_query_data <- bds_metrics |>
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
clean_query_data <- dplyr::full_join(
  raw_query_data,
  all_years,
  by = c("Years_num")
) |>
  tidyr::pivot_wider(
    id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
    names_from = Years_num,
    values_from = values_num
  ) |>
  # Remove any all NA rows
  # (created from join where indicator has missing year)
  dplyr::filter(!dplyr::if_all(everything(), is.na)) |>
  dplyr::select(
    `LA Number`,
    `LA and Regions`,
    Topic,
    Measure,
    tidyselect::num_range("", min(all_years):max(all_years))
  )

# Combine the current query into the final query data frame
final_query_data <- dplyr::bind_rows(final_query_data, clean_query_data)
