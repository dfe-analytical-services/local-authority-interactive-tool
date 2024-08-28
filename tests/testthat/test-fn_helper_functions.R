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
