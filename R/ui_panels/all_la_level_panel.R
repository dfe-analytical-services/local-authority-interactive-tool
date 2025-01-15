all_la_level_panel <- function() {
  bslib::nav_panel(
    "all_la_level",
    PageHeaderUI("all_la_header"),
    appInputsUI("all_la_inputs"),
    AllLA_TableUI("all_la_table"),
    LA_LevelMetaUI("all_la_meta")
  )
}
