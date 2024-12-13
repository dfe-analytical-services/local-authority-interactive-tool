accessibility_panel <- function() {
  shiny::tags$div(
    # Add in back link
    actionLink(
      class = "govuk-back-link",
      style = "margin-top: 0.2rem; margin-bottom: 1.2rem;",
      "accessibility_to_dashboard",
      "Back to dashboard"
    ),
    dfeshiny::a11y_panel(
      dashboard_title = site_title,
      dashboard_url = "https://department-for-education.shinyapps.io/local-authority-interactive-tool/",
      date_tested = "9th December 2024",
      date_prepared = "9th December 2024",
      date_reviewed = "9th December 2024",
      issues_contact = "https://github.com/dfe-analytical-services/local-authority-interactive-tool/issues",
      non_accessible_components = "TBC",
      specific_issues = "TBC"
    )
  )
}
