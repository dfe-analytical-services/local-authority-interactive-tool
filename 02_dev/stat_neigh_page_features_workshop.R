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
