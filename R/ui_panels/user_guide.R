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
              tagList(
                HTML(
                  "This view begins with a choose local authority (LA), topic
                    and indicator section.  Use the three dropdowns to select a
                    LA and measure of choice.
                   <br>
                   <br>
                   As this is an interactive system, the tables and chart (and
                    relevant titling) change automatically depending on the
                    indicator chosen.
                   <br>
                   <br>
                   The table beneath the dropdowns will show time series data
                    for the chosen indicator in respect of the LA, its Region,
                    Statistical Neighbours and England averages - for
                    comparison/benchmarking purposes (figure 2).
                   <br>
                   <br>
                   Note that your selected LA is highlighted in light blue
                    within the data table, and that there is an option to
                    download the data.
                   <br>
                   <br>
                   The figures presented are simple averages for the selected
                    LA’s 10 statistical neighbours (SNs).  They provide a simple
                    comparator of the measure without placing too much emphasis
                    on any one authority.  Where data does not exist for one or
                    more of the selected LA’s SN group, the information is
                    excluded from the mean calculation.
                   <br>
                   <br>
                   <b>Figure 2: Data Table for Chosen Authority/Data Item</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_lvl_main_table.png",
                  alt = "Figure 2: Data Table for Chosen Authority/Data Item",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Beneath this is a sub-table showing latest year on year
                    trend, change from previous year and national rank.
                    If applicable the authority is placed into a quartile
                    banding and formatting will flag up upper (A) and Lower (D)
                    quartiles for clarity.
                   <br>
                   <br>
                   <b>Figure 3: Trends and Quartile bandings</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_lvl_stats_table.png",
                  alt = "Figure 3: Trends and Quartile bandings",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   This table is followed by a chart (figure 4), which plots the
                    authority, national, region and statistical neighbour
                    average data over the same period as the data table,
                    providing an interactive visual representation. The data is
                    displayed as a line chart by default but can be changed to
                    a bar chart, both are downloadable.
                   <br>
                   <br>
                   <b>Figure 4: Charting the Data</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_lvl_charts.png",
                  alt = "Figure 4: Charting the Data",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Beneath the chart a description of the data item is provided
                    (figure 5), as well as an indication of last and next update
                    dates, alongside a hyperlink to the data source (if clicked,
                    the source will open in new tab).
                   <br>
                   <br>
                   <b>Figure 5: Data Description</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_lvl_metadata.png",
                  alt = "Figure 5: Data Description",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                )
              ),
              "Regional View",
              "Statistical Neighbour View",
              tagList(
                p(
                  "The All LAs view presents all 153 Local Authorities, in
                  alphabetical order, plus previous authorities for those that
                  have recently undergone a local government reorganisation,
                  (figure 9). The table is downloadable."
                ),
                p(HTML("<b>Figure 9 showing an example table of all Local Authorities.</b>")),
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
