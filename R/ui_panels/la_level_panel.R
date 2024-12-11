la_level_panel <- function() {
  bslib::nav_panel(
    "la_level",
    PageHeaderUI("la_header"),
    appInputsUI("la_inputs"),
    LA_LevelTableUI("la_table"),
    LA_StatsTableUI("la_stats"),
    div(
      class = "well",
      style = "overflow-y: visible;",
      role = "presentation",
      `aria-label` = "Line and bar charts showing data from the first table including the
                        selected Local Authority, Region, Statistical Neighbour and England.",
      bslib::navset_card_tab(
        id = "la_charts",
        LA_LineChartUI("la_line_chart"),
        LA_BarChartUI("la_bar_chart")
      )
    ),
    LA_LevelMetaUI("la_meta")
  )
}
