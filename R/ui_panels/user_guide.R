user_guide_panel <- function() {
  shiny::tabPanel(
    "User guide",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          shinyGovstyle::banner(
            "beta banner",
            "beta",
            paste0(
              "This page is in beta phase and we are still reviewing the content.
               We will provide a much more detailed user guide when the tool is
               published."
            )
          ),
          shiny::br(),
          h1("Local Authority Interactive Tool"),


          # Introduction =======================================================
          h2("Introduction"),
          p("The Department has developed the Local Authority Interactive Tool
              (LAIT) to provide easy access to a wide range of data related to
              children and young people sourced from various departments across
              government.The app is designed and maintained by the DFE's Regions
              Group LA Performance & Data (LAPD) Team."),
          p(
            "We might want to add some brief introductory text here alongside
              some links to different tabs within your dashboard. Here's an
              example of a link working:",
            InternalLinkUI("la_level_link")
          ),


          # Context and Purpose ================================================
          h2("Context and purpose"),
          p("The tool facilitates local authority ‘benchmarking’ and trend
              analysis by allowing users to compare individual local authority
              (LA) data alongside national, regional and Statistical Neighbour
              (SN) groupings over time (up to 10 years)."),
          p("The tool consists of five data views which present data items
              largely in the form of tables and charts, which can be downloaded
              and used elsewhere.  Rank and quartile positions are also included
              to ease the assessment of local authority positioning in a
              national context."),
          p("The tool holds in the region of 400 children's services measures
              covering:"),
          tags$ul(
            tags$li("children’s health and wellbeing;"),
            tags$li("children in need, child protection, children’s services workforce, and children looked after (inc. adoption);"),
            tags$li("early years;"),
            tags$li("key stage attainment (inc. by age 19);"),
            tags$li("children with SEN;"),
            tags$li("further and higher education;"),
            tags$li("behaviour and attendance;"),
            tags$li("economic factors;"),
            tags$li("youth offending;"),
            tags$li("children’s services finance.")
          ),
          p("We will add some relevant background information here. For example
              some useful LAIT resources."),


          # Using the app ======================================================
          h2("Using the app"),
          p("Here we will add detailed instructions of how to use the LAIT app.
              This will include screenshots and guidance. For example, how to
              investigate an indicator using the Regional Level page."),
          shinyGovstyle::accordion(
            inputId = "user-guide-how-to",
            titles = c(
              "Local Authority View",
              "Regional View",
              "Statistical Neighbour View",
              "All Local Authorities View",
              "Create Your Own"
            ),
            descriptions = list(
              "Local Authority View",
              "Regional View",
              "Statistical Neighbour View",
              tagList(
                HTML(
                  "The All LAs view presents all 153 Local Authorities, in
                  alphabetical order, plus previous authorities for those that
                  have recently undergone a local government reorganisation,
                  (figure 9). The table is downloadable.<br>"
                ),
                tags$img(
                  src = "images/user_guide/all_LAs_view_top_table.png",
                  alt = "Figure 9 showing an example table of all Local Authorities.",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                )
              ),
              "Create Your Own"
            )
          ),


          # Guidance sources ===================================================
          h2("Guidance sources"),
          p("For example, here we'll add some of the key resources we draw
                  on to guide styling and visualisation...")
        )
      )
    )
  )
}
