# nolint start: object_name
#
# General modules =============================================================
# These are nested within other modules
#



# Get All LA table name
Get_AllLATableNameUI <- function(id) {
  ns <- NS(id)

  shiny::uiOutput(ns("table_name"))
}

Get_AllLATableNameServer <- function(id, filtered_bds) {
  moduleServer(id, function(input, output, session) {
    # Pull chart title
    output$table_name <- shiny::renderUI({
      filtered_bds() |>
        pull_uniques("Chart_title")
    })
  })
}
