generate_dummy_data <- function(df, n = 10) {
  dummy_df <- as.data.frame(lapply(df, function(column) {
    if (is.numeric(column)) {
      # Generate numeric data based on the range of the original column
      runif(n, min = min(column, na.rm = TRUE), max = max(column, na.rm = TRUE))
    } else if (is.character(column)) {
      # Generate random character strings of similar length
      replicate(n, paste0(sample(letters, 5, replace = TRUE), collapse = ""))
    } else if (is.factor(column)) {
      # Generate random factor levels
      factor(sample(levels(column), n, replace = TRUE), levels = levels(column))
    } else if (is.logical(column)) {
      # Generate random logical values
      sample(c(TRUE, FALSE), n, replace = TRUE)
    } else if (inherits(column, "Date")) {
      # Generate random dates within the range of the original column
      as.Date(runif(n, min = as.numeric(min(column, na.rm = TRUE)), max = as.numeric(max(column, na.rm = TRUE))), origin = "1970-01-01")
    } else {
      # Default case for any other types (e.g., complex, raw, etc.)
      NA
    }
  }))

  return(dummy_df)
}


generate_dummy_data(bds_metrics)
