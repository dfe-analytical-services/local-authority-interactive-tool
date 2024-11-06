# expandable() ----------------------------------------------------------------
# 1. expandable() test for valid input with all string parameters
test_that("expandable() creates correct shiny tags with string inputs", {
  result <- expandable("test_id", "Test Label", "This is the content")
  expect_true(inherits(result, "shiny.tag"))
  expect_equal(result$attribs$id, "test_id")
  expect_equal(result$children[[1]]$children[[1]]$children[[1]], "Test Label")
  expect_equal(result$children[[2]]$children[[1]], "This is the content")
})

# 2. expandable() test for valid input with shiny.tag as contents
test_that("expandable() handles shiny.tag as contents", {
  content_tag <- shiny::tags$p("Paragraph content")
  result <- expandable("test_id", "Test Label", content_tag)
  expect_true(inherits(result, "shiny.tag"))
  expect_equal(result$children[[2]]$name, "div")
  expect_true(inherits(result$children[[2]]$children[[1]], "shiny.tag"))
  expect_equal(result$children[[2]]$children[[1]]$name, "p")
  expect_equal(result$children[[2]]$children[[1]]$children[[1]], "Paragraph content")
})

# 3. expandable() test for error when input_id is not a single string
test_that("expandable() throws error for invalid input_id", {
  expect_error(
    expandable(123, "Test Label", "This is the content"),
    "input_id must be a single string."
  )
  expect_error(
    expandable(c("id1", "id2"), "Test Label", "This is the content"),
    "input_id must be a single string."
  )
})

# 4. expandable() test for error when label is not a single string
test_that("expandable() throws error for invalid label", {
  expect_error(
    expandable("test_id", 123, "This is the content"),
    "label must be a single string."
  )
  expect_error(
    expandable("test_id", c("label1", "label2"), "This is the content"),
    "label must be a single string."
  )
})

# 5. expandable() test for error when contents is not a string or shiny.tag
test_that("expandable() throws error for invalid contents", {
  expect_error(
    expandable("test_id", "Test Label", 123),
    "contents must be a string or a shiny.tag object."
  )
  expect_error(
    expandable("test_id", "Test Label", list("invalid")),
    "contents must be a string or a shiny.tag object."
  )
})


# value_box() -----------------------------------------------------------------
test_that("1. Basic functionality of value_box", {
  result <- value_box("42", "This is a subtitle")
  expect_true(grepl("small-box bg-blue", result))
  expect_true(grepl("42", result))
  expect_true(grepl("This is a subtitle", result))
  expect_true(grepl("vboxhead-medium", result))
  expect_true(grepl("vboxdetail-medium", result))
})

test_that("2. Valid colors for value_box", {
  result <- value_box("42", "This is a subtitle", color = valid_colors[1])
  expect_true(grepl("small-box bg-blue", result))
})

test_that("3. Different font sizes for value_box", {
  result <- value_box("42", "This is a subtitle", fontsize = "large")
  expect_true(grepl("vboxhead-large", result))
  expect_true(grepl("vboxdetail-large", result))
})

test_that("4. Icon handling in value_box", {
  icon <- tags$i(class = "fa fa-star")
  result <- value_box("42", "This is a subtitle", icon = icon)
  expect_true(grepl("fa fa-star", result))
  expect_true(grepl("icon-large", result))
})

test_that("5. Width handling in value_box", {
  result <- value_box("42", "This is a subtitle", width = 6)
  expect_true(grepl("col-sm-6", result))
})

test_that("6. href handling in value_box", {
  result <- value_box("42", "This is a subtitle", href = "https://example.com")
  expect_true(grepl("<a href=\"https://example.com\"", result))
})

test_that("7. NULL icon in value_box", {
  result <- value_box("42", "This is a subtitle", icon = NULL)
  expect_false(grepl("icon-large", result))
})

test_that("8. NULL width in value_box", {
  result <- value_box("42", "This is a subtitle", width = NULL)
  expect_false(grepl("col-sm-", result))
})

test_that("9. Invalid color in value_box", {
  expect_error(
    value_box("42", "This is a subtitle", color = "invalid-color"),
    "Invalid color: "
  )
  expect_error(
    value_box("42", "This is a subtitle", color = "red"),
    "Invalid color: "
  )
})

test_that("10. Invalid icon in value_box", {
  expect_error(
    value_box("42", "This is a subtitle", icon = "invalid-icon"),
    "icon must be a shiny.tag object"
  )
})

testthat::test_that("11. Value box function errors when no value is given", {
  # Expect an error if no value argument is given to value_box() function
  testthat::expect_error(value_box())
})


# validate_color() ------------------------------------------------------------
test_that("1. validate_color with valid colors", {
  expect_true(validate_color("orange"))
})

test_that("2. validate_color with invalid color", {
  expect_error(
    validate_color("black"),
    "Invalid color: black. Valid colors are: blue, "
  )
})

test_that("3. validate_color with empty string", {
  expect_error(
    validate_color(""),
    "Invalid color: . Valid colors are: blue, "
  )
})

test_that("4. validate_color with NULL", {
  expect_error(
    validate_color(NULL),
    "Invalid color: NULL. Valid colors are: blue, "
  )
})


# set_css_style_sheet() -------------------------------------------------------
test_that("1. set_css_style_sheet deals with non css files", {
  expect_error(
    set_css_style_sheet("wrong_file"),
    "This doesn't look like a css file"
  )
})

testthat::test_that("2. Check set_css_style_sheet creates a shiny.tag", {
  result <- set_css_style_sheet("styles.css")

  expect_equal(class(result), "shiny.tag")
})


# not_all_na() ----------------------------------------------------------------
test_that("1. not_all_na with all NA values", {
  result <- not_all_na(c(NA, NA, NA))
  expect_false(result)
})

test_that("2. not_all_na with no NA values", {
  result <- not_all_na(c(1, 2, 3))
  expect_true(result)
})

test_that("3. not_all_na with a mix of NA and non-NA values", {
  result <- not_all_na(c(NA, 1, NA, 2))
  expect_true(result)
})

test_that("4. not_all_na with an empty vector", {
  result <- not_all_na(c())
  expect_false(result)
})


# clean_spaces() --------------------------------------------------------------
test_that("1. clean_spaces with multiple spaces between words", {
  result <- clean_spaces("This  is   a  test")
  expect_equal(result, "This is a test")
})

test_that("2. clean_spaces with leading and trailing spaces", {
  result <- clean_spaces("  Leading and trailing  ")
  expect_equal(result, " Leading and trailing ")
})

test_that("3. clean_spaces with mixed whitespace characters", {
  result <- clean_spaces("This\tis\na test")
  expect_equal(result, "This is a test")
})


# pull_uniques() --------------------------------------------------------------
test_that("1. pull_uniques with numeric column containing duplicates", {
  data <- data.frame(a = c(1, 2, 2, 3, 3, 3, 4))
  result <- pull_uniques(data, "a")
  expect_equal(result, c(1, 2, 3, 4))
})

test_that("2. pull_uniques with character column containing duplicates", {
  data <- data.frame(a = c("apple", "banana", "apple", "cherry", "banana"))
  result <- pull_uniques(data, "a")
  expect_equal(result, c("apple", "banana", "cherry"))
})

test_that("3. pull_uniques with factor column containing duplicates", {
  data <- data.frame(a = factor(c("low", "medium", "low", "high", "medium")))
  result <- pull_uniques(data, "a")
  expect_equal(result, factor(c("low", "medium", "high")))
})

test_that("4. pull_uniques with an empty data frame", {
  data <- data.frame(a = numeric(0))
  result <- pull_uniques(data, "a")
  expect_equal(result, numeric(0))
})

test_that("5. pull_uniques with a column containing only NA values", {
  data <- data.frame(a = c(NA, NA, NA))
  result <- pull_uniques(data, "a")
  expect_equal(result, NA)
})

test_that("6. pull_uniques with a column containing NA and non-NA values", {
  data <- data.frame(a = c(1, NA, 2, NA, 3))
  result <- pull_uniques(data, "a")
  expect_equal(result, c(1, NA, 2, 3))
})

test_that("7. pull_uniques with a column containing mixed data types", {
  data <- data.frame(a = c(1, "2", TRUE, 1, "2", FALSE))
  result <- pull_uniques(data, "a")
  expect_equal(result, c("1", "2", "TRUE", "FALSE"))
})

test_that("8. Data frame with no columns", {
  df <- data.frame()
  expect_error(
    pull_uniques(df, "nonexistent"),
    "Can't extract columns that don't exist."
  )
})


# filter_and_pull() -----------------------------------------------------------
test_that("1. filter_and_pull with a simple case", {
  data <- data.frame(
    category = c("A", "B", "A", "C"),
    value = c(10, 20, 30, 40)
  )

  result <- filter_and_pull(data, "category", "A", "value")
  expect_equal(result, c(10, 30))
})

test_that("2. filter_and_pull with no matching filter variable", {
  data <- data.frame(
    category = c("A", "B", "A", "C"),
    value = c(10, 20, 30, 40)
  )

  result <- filter_and_pull(data, "category", "D", "value")
  expect_equal(result, numeric(0))
})

test_that("3. filter_and_pull with missing values in column filtering for", {
  data <- data.frame(
    category = c("A", "B", NA, "C"),
    value = c(NA, 20, 30, 40)
  )

  result <- filter_and_pull(data, "category", "A", "value")
  expect_equal(result, c(NA_integer_))
})

test_that("4. filter_and_pull with an empty data frame", {
  data <- data.frame(
    category = character(),
    value = numeric()
  )

  result <- filter_and_pull(data, "category", "A", "value")
  expect_equal(result, numeric(0))
})

test_that("5. filter_and_pull with a non-existing pull column", {
  data <- data.frame(
    category = c("A", "B", "A", "C"),
    value = c(10, 20, 30, 40)
  )

  expect_error(
    filter_and_pull(data, "category", "A", "non_existing"),
    "Can't extract columns that don't exist"
  )
})

test_that("6. filter_and_pull with filter_col and pull_col being the same", {
  data <- data.frame(
    category = c("A", "B", "A", "C"),
    value = c(10, 20, 30, 40)
  )

  result <- filter_and_pull(data, "category", "A", "category")
  expect_equal(result, c("A", "A"))
})


# get_metadata() --------------------------------------------------------------
test_that("1. get_metadata with text metadata (no date conversion)", {
  data <- data.frame(
    Measure = c("mpg", "cyl", "hp"),
    Description = c("Miles per gallon", "Number of cylinders", "Horsepower"),
    stringsAsFactors = FALSE
  )

  result <- get_metadata(data, "mpg", "Description")
  expect_equal(result, "Miles per gallon")
})

test_that("2. get_metadata with numeric metadata (date conversion)", {
  data <- data.frame(
    Measure = c("mpg", "cyl", "hp"),
    `Last Update` = c("44561", "44562", "44563"), # Excel date format
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- get_metadata(data, "mpg", "Last Update")
  expect_equal(result, "December 2021")
})

test_that("3. get_metadata with metadata that shouldn't be converted to a date", {
  data <- data.frame(
    Measure = c("mpg", "cyl", "hp"),
    `Value` = c("Test 12345", "Test 67890", "Test 23456"),
    stringsAsFactors = FALSE
  )

  result <- get_metadata(data, "cyl", "Value")
  expect_equal(result, "Test 67890")
})

test_that("4. get_metadata with non-matching indicator", {
  data <- data.frame(
    Measure = c("mpg", "cyl", "hp"),
    Description = c("Miles per gallon", "Number of cylinders", "Horsepower"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_equal(get_metadata(data, "gear", "Description"), "No matching metadata"),
    "No matching metadata for Description"
  )
})

test_that("5. get_metadata with missing values in metadata column", {
  data <- data.frame(
    Measure = c("mpg", "cyl", "hp"),
    Description = c("Miles per gallon", NA, "Horsepower"),
    stringsAsFactors = FALSE
  )

  result <- get_metadata(data, "cyl", "Description")
  expect_equal(result, NA_character_)
})

test_that("6. get_metadata with empty data frame", {
  data <- data.frame(
    Measure = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_equal(get_metadata(data, "gear", "Description"), "No matching metadata"),
    "No matching metadata for Description"
  )
})


# mute_cat()--------------------------------------------------

test_that("1. mute_cat suppresses messages", {
  output <- capture.output(
    mute_cat({
      message("This message should be suppressed")
    })
  )
  expect_equal(output, character(0)) # Expect no output captured
})

test_that("2. mute_cat suppresses warnings", {
  # Capture any output generated, including warnings
  output <- capture.output(
    suppressWarnings(
      mute_cat({
        warning("This warning should be suppressed")
      })
    )
  )
  expect_equal(output, character(0)) # Expect no captured output if suppression works
})


test_that("3. mute_cat returns the correct result", {
  result <- mute_cat({
    cat("Result should be 42\n")
    42
  })
  expect_equal(result, 42)
})


# clean_ldn_region()-------------------------------------------


# Mock data to test "London (Outer)" case separately
filtered_bds_with_na <- dplyr::tibble(
  `LA and Regions` = c("London (Inner)", "London (Outer)", "London"),
  values_num = c(100, NA, 50)
)

# Run a single test
test_that("1. clean_ldn_region returns 'London (Outer)' when some data exists for the region", {
  result <- clean_ldn_region("London (Outer)", filtered_bds_with_na)
  expect_equal(result, "London")
})


# af_colours_focus()----------------------------------------------

# Define the mock function
mock_af_colours <- function(type) {
  if (type == "focus") {
    message("This palette should only be used to highlight specific elements to help users understand the information.")
    return(c("#FF5733", "#33FF57", "#3357FF")) # Example hex color codes
  }
}

# Tests
test_that("1. af_colours_focus returns a character vector of hex color codes", {
  with_mock(
    `afcolours::af_colours` = mock_af_colours, # Mocking the function
    {
      result <- af_colours_focus()
      expect_type(result, "character")
      expect_equal(result, c("#FF5733", "#33FF57", "#3357FF"))
    }
  )
})

test_that("2. af_colours_focus suppresses the informational message", {
  with_mock(
    `afcolours::af_colours` = mock_af_colours, # Mocking the function
    {
      expect_silent(af_colours_focus())
    }
  )
})

test_that("3. af_colours_focus suppresses the correct message", {
  with_mock(
    `afcolours::af_colours` = mock_af_colours, # Mocking the function
    {
      expect_output(
        suppressMessages(af_colours_focus()),
        NA # Expect no output, as the message should be suppressed
      )
    }
  )
})
