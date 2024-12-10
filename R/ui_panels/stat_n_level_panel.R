stat_n_level_panel <- function() {
  bslib::nav_panel(
    "statistical_neighbour_level",
    PageHeaderUI("stat_n_header"),
    appInputsUI("stat_n_inputs"),
    StatN_TablesUI("stat_n_tables"),
    div(
      class = "well",
      style = "overflow-y: visible;",
      bslib::navset_card_underline(
        id = "stat_n_charts",
        StatN_FocusLineChartUI("stat_n_focus_line"),
        StatN_MultiLineChartUI("stat_n_multi_line"),
        StatN_FocusBarChartUI("stat_n_focus_bar"),
        StatN_MultiBarChartUI("stat_n_multi_bar")
      )
    ),
    LA_LevelMetaUI("stat_n_meta")
  )
}
