support_panel <- function() {
  bslib::nav_panel(
    value = "support",
    # Add in back link
    actionLink(
      class = "govuk-back-link",
      style = "margin-top: 0.2rem; margin-bottom: 1.2rem;",
      "support_to_dashboard",
      "Back to dashboard"
    ),
    title = shiny::HTML("Support and feedback<br>(Feedback form)"),
    dfeshiny::support_panel(
      team_email = "Darlington.BRIDGE@education.gov.uk",
      repo_name = "https://github.com/dfe-analytical-services/local-authority-interactive-tool",
      ees_publication = FALSE,
      alt_href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait",
      form_url = "https://forms.office.com/e/gTNw1EBgsn",
      custom_data_info = HTML(
        paste0(
          "The full dataset is available in the ",
          dfeshiny::external_link(
            href = paste0(
              "https://github.com/dfe-analytical-services/",
              "local-authority-interactive-tool/tree/main/01_data/02_prod"
            ),
            link_text = "data directory of the LAIT GitHub repository",
            add_warning = TRUE
          )
        ),
        ". The files beginning with 'bds_long' store the main dataset for the tool. ",
        "You will also find several other datasets here. ",
        "These help build the tool, feel free to check them out."
      )
    )
  )
}
