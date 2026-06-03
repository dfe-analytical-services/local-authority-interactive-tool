#!/usr/bin/env Rscript
cat("Running commit hooks...", fill = TRUE)

message("\n")

message("1. Checking for unpublished data...\n")

error_flag <- FALSE

datalog <- here::here("datafiles_log.csv")
log_files <- read.csv(datalog, stringsAsFactors = FALSE)
gitignore_content <- read.csv(
  here::here(".gitignore"),
  header = FALSE,
  stringsAsFactors = FALSE
)
ign_files <- gitignore_content |>
  dplyr::filter(!grepl("^#", V1))
colnames(ign_files)[1] <- "filename"

message("Contents of the .gitignore file:")
print(ign_files)

# Run a pass through the .gitignore files and look for any issues
if (ncol(ign_files) > 1) {
  message(
    "ERROR: It looks like you've got commas in the .gitignore. Please correct the .gitignore file and try again."
  )
  error_flag <- TRUE
} else {
  for (i in 1:nrow(ign_files)) {
    if (grepl(" ", ign_files$filename[i])) {
      message(
        "ERROR: It looks like you've got spaces in filenames in the .gitignore. Please rename your files if they contain spaces and update the .gitignore file accordingly."
      )
      error_flag <- TRUE
    }
  }
}

suffixes <- "xlsx$|dat$|csv$|tex$|pdf$"

current_files <- data.frame(
  files = list.files(here::here(), recursive = TRUE)
) |>
  dplyr::filter(grepl(suffixes, files), !grepl("renv|datafiles_log.csv", files))

for (file in current_files$files) {
  if (!file %in% log_files$filename) {
    message("Error: ", file, " is not recorded in datafiles_log.csv.\n\n")
    message(
      "Please add an entry to datafiles_log.csv for this file and mark it as unpublished, published or reference.\n\n"
    )
    error_flag <- TRUE
  } else {
    file_status <- (log_files |> dplyr::filter(filename == file))$status
    if (
      !file_status %in%
        c("published", "Published", "reference", "Reference", "dummy", "Dummy")
    ) {
      if (!file %in% ign_files$filename & !grepl("unpublished", file)) {
        message(
          "Error: ",
          file,
          " is not logged as published or reference data in datafiles_log.csv and is not found in .gitignore.\n\n"
        )
        message(
          "If the file contains published or reference data then update its entry in datafiles_log.csv.\n\n"
        )
        message(
          "If the file contains unpublished data then add it to the .gitignore file.\n\n"
        )
        error_flag <- TRUE
      } else {
        message(
          file,
          " is recorded in the logfile as unpublished data and in .gitignore and so will not be included as part of the commit.\n\n"
        )
      }
    }
  }
}


if (error_flag) {
  message(
    "Warning, aborting commit. Unrecognised data files found, please update .gitignore or datafiles_log.csv.\n"
  )
  quit(save = "no", status = 1, runLast = FALSE)
} else {
  message("...all good!")
}

message("\n")

message("2. Checking Google Analytics tag...\n")

if (
  grepl("G-Z967JJVQQX", htmltools::includeHTML(("google-analytics.html"))) &
    !(toupper(Sys.getenv("USERNAME")) %in%
      c("CFOSTER4", "CRACE", "LSELBY", "RBIELBY", "JMACHIN"))
) {
  message("...cleaning out the template's Google Analytics tag.")
  xfun::gsub_file(
    "google-analytics.html",
    pattern = "G-Z967JJVQQX",
    replacement = "G-XXXXXXXXXX"
  )
  xfun::gsub_file("ui.R", pattern = "Z967JJVQQX", replacement = "XXXXXXXXXX")
  system2(command = "git", args = c("add", "google-analytics.html"))
} else {
  message("...all good!")
}

message("\n")

message("3. Checking code styling with air...\n")

# Run air format (assuming this is the correct command)
air_output <- system("air format .", ignore.stdout = TRUE, ignore.stderr = TRUE)

# Get the list of staged files
staged_files <- system("git diff --cached --name-only", intern = TRUE)

# Get the list of all changes (to detect if anything changed, including unstaged)
changed_files <- system("git status --porcelain", intern = TRUE)

# Check for any modified (unstaged) files — lines starting with " M", "MM", etc.
unstaged_files <- changed_files[grepl("^.M ", changed_files)]

if (length(unstaged_files) > 0) {
  message("Warning: Code formatting issues detected and fixed by air.\n")

  # Stage only the originally staged files again (preserves intended scope)
  if (length(staged_files) > 0) {
    for (f in staged_files) {
      system(paste("git add", shQuote(f)))
    }
    message(
      "Changes have been formatted, staged, and committed automatically.\n"
    )
  } else {
    message("No originally staged files found. Skipping commit.\n")
  }
} else {
  message("...code styling checks passed\n")
}

# End of hooks
