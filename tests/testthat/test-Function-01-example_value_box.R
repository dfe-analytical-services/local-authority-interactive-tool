# nolint start: commented_code

# -----------------------------------------------------------------------------
# This is an example unit test file.
#
# These examples show some ways you can make use of unit tests.
#
# Add more scripts and checks to test any other functions or non-shiny R code.
#
# Unit tests are easy to write and quick to run, make use of them where you can
# For more information, look at the testthat package documentation.
# -----------------------------------------------------------------------------
# testthat::test_that("Example - two plus two equals four", {
#   # Expect two objects to be the same
#
testthat::expect_equal(2 + 2, 4)
#   # Expect comparisons to be TRUE or FALSE
#   testthat::expect_true(2 + 2 == 4)
#   # Expect code to execute without error
#   testthat::expect_no_error(2 + 2)
# })
#
# testthat::test_that("Value box function errors when no value is given", {
#   # Note the path to the file is adjusted so that it works
#   # when running shinytest2::test_app()
#   source(here::here("R/helper_functions.R"))
#
#   # Expect an error if no value argument is given to value_box() function
#   testthat::expect_error(value_box())
# })

# nolint end
