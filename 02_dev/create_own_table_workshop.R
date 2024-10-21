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
  Remove = "Remove",
  check.names = FALSE,
  stringsAsFactors = FALSE
)


test <- reactable::reactable(
  test_data,
  columns = list(
    Remove = reactable::colDef(
      cell = reactable::JS(
        "function(cellInfo, rowInfo) {
            return React.createElement(ButtonExtras,
            {id: 'remove_' + rowInfo.index, label: cellInfo.value,
            uuid: cellInfo.row['.internal_uuid'] ? cellInfo.row['.internal_uuid'] : (Number(cellInfo.id) + 1), column: cellInfo.column.id }, cellInfo.id)
            }"
      )
    )
  )
)
