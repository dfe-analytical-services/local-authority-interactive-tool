# ---------------------------------------------------------
# This is the .Rprofile file
#
# Use it to include any functions you want to run before any other code is run.
# For example, using renv automatically sources its activate script to the .RProfile file
# This ensures that all renv checks on package versions happens before any code is run.
#
# ---------------------------------------------------------

message("Sourcing .Rprofile...")

source("renv/activate.R")

# Welcome message
.First <- function() {
  # Check if praise installed
  if (system.file(package = "praise") != "") {
    welcome_msg <- message(
      praise::praise("${Exclamation}-${Exclamation}! "),
      "Welcome to your dfeR project.",
      praise::praise("Time for some ${adjective} R coding...")
    )

    welcome_msg
  } else {
    warning("praise package is not installed, please run renv::restore() to set up the necessary package environment")
  }
}

# Check if dfeshiny installed
if (system.file(package = "dfeshiny") != "") {
  library(dfeshiny)
} else {
  warning("dfeshiny package is not installed, please run renv::restore() to set up the necessary package environment")
}

# Install commit-hooks locally
statusWriteCommit <- file.copy(".hooks/pre-commit.R", ".git/hooks/pre-commit", overwrite = TRUE)
