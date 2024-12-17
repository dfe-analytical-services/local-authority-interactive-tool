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
      date_prepared = "17th December 2024",
      date_reviewed = "17th December 2024",
      issues_contact = "https://github.com/dfe-analytical-services/local-authority-interactive-tool/issues",
      non_accessible_components = c(
        "Keyboard navigation within some tables may be limited",
        "Navigation between pages doesn't trigger automatic screen reader response"
      ),
      specific_issues = c(
        "Some ARIA roles within tables lack particular children elements",
        "The dashboard lacks a \"skip to main content\" link",
        "Tables don't have horizontal scroll at all zoom levels where it's needed, cutting off some content",
        "Context focus does not work fully when navigating between pages"
      )
    )
  )
}
