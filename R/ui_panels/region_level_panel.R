region_level_panel <- function() {
  bslib::nav_panel(
    "regional_level",
    PageHeaderUI("region_header"),
    appInputsUI("region_inputs"),
    RegionLevel_TableUI("region_tables"),
    div(
      class = "well",
      style = "overflow-y: visible;",
      `aria-hidden` = "true",
      bslib::navset_card_tab(
        id = "region_charts",
        Region_FocusLineChartUI("region_focus_line"),
        Region_MultiLineChartUI("region_multi_line"),
        Region_FocusBarChartUI("region_focus_bar"),
        Region_MultiBarChartUI("region_multi_bar")
      )
    ),
    LA_LevelMetaUI("region_meta")
  )
}
