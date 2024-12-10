create_your_own_panel <- function() {
  bslib::nav_panel(
    "create_your_own",
    full_data_on_github_noti(),
    div(
      class = "well",
      style = "overflow-y: visible; padding: 1rem;",
      bslib::layout_column_wrap(
        Create_MainInputsUI("create_inputs")["Main choices"]
      ),
      bslib::layout_column_wrap(
        Create_MainInputsUI("create_inputs")["LA grouping"],
        Create_MainInputsUI("create_inputs")["Other grouping"],
        YearRangeUI("year_range"),
        Create_MainInputsUI("create_inputs")["Clear all current selections"]
      )
    ),
    StagingTableUI("staging_table"),
    QueryTableUI("query_table"),
    CreateOwnTableUI("create_own_table"),
    div(
      class = "well",
      style = "overflow-y: visible;",
      shiny::h3(
        "Output Charts",
        create_tooltip_icon("Charts showing data from all the saved selections")
      ),
      shiny::p("Note a maximum of 4 geographies and 3 indicators can be shown."),
      bslib::navset_tab(
        CreateOwnLineChartUI("create_own_line"),
        CreateOwnBarChartUI("create_own_bar")
      )
    )
  )
}
