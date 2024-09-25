user_guide_panel <- function() {
  shiny::tabPanel(
    "User guide",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          h1("Local Authority Interactive Tool"),
          h2("Introduction"),
          p("The Department has developed the Local Authority Interactive Tool
              (LAIT) to provide easy access to a wide range of data related to
              children and young people sourced from various departments across
              government.

              The app is designed and maintained by the DFE's Regions Group LA
              Performance & Data (LAPD) Team."),
          p(
            "We might want to add some brief introductory text here alongside
              some links to different tabs within your dashboard. Here's an
              example of a link working:",
            InternalLinkUI("la_level_link")
          ),
          h2("Context and purpose"),
          p("The Tool provides the functionality to ‘benchmark’ an authority
              nationally and against either its Region or Statistical
              Neighbours. The data items are presented largely in the form of
              tables and charts, which can be varied and ‘downloaded’ from the
              system for use elsewhere. (Download feature arriving soon...)"),
          p("We will add some relevant background information here. For example
              some useful LAIT resources."),
          h2("Using the app"),
          p("Here we will add detailed instructions of how to use the LAIT app.
              This will include screenshots and guidance. For example, how to
              investigate an indicator using the Regional Level page."),
          h2("Guidance sources"),
          p("For example, here we'll add some of the key resources we draw
                  on to guide styling and vizualisation...")
        )
      )
    )
  )
}
