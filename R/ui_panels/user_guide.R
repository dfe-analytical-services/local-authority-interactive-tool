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
            tags$li("children in need, child protection,
                    children’s services workforce,
                    and children looked after (inc. adoption);"),
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
              # LA Level View ==================================================
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
                  src = "images/user_guide/LA_view_main_table.png",
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
                  src = "images/user_guide/LA_view_stats_table.png",
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
                  src = "images/user_guide/LA_view_charts.png",
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
                  src = "images/user_guide/LA_view_metadata.png",
                  alt = "Figure 5: Data Description",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                )
              ),
              # Regional View ==================================================
              shiny::tagList(
                HTML(
                  "This presents the selected authority’s data alongside its
                    Regional local authority neighbours and the selected
                    authority’s region data against other regional data (and
                    England).  The selected authority and region are highlighted
                    in light blue.
                   <br>
                   <br>
                   <b>Figure 6: Regional Level View</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Region_view_main_table.png",
                  alt = "Figure 6: Regional Level View",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Following the similar pattern as the Local Authority view, a
                    chart is displayed which presents each region’s data. The
                    selected authority’s region is highlighted in blue, whilst
                    other regions are coloured grey.
                   <br>
                   <br>
                   <b>Figure 7: Regional Level Charting</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Region_view_focus_chart.png",
                  alt = "Figure 7: Regional Level Charting",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                  The chart is again interactive and includes an option to
                   create a ‘mini’ chart with a max of three regions of your
                   choice presented alongside the selected LA’s region.
                   <br>
                   <br>
                   <b>Figure 8: User choice regional chart</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Region_view_multi_chart.png",
                  alt = "Figure 8: User choice regional chart",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                )
              ),
              # Statistical Neighbour View =====================================
              shiny::tagList(
                HTML(
                  "Statistical neighbours provide a method for benchmarking
                    progress. For each local authority (LA), these models
                    designate several other LAs deemed to have similar
                    characteristics. These designated LAs are known as
                    statistical neighbours.  The National Foundation for
                    Educational Research (NFER) was commissioned in 2007 by the
                    Department to identify and group similar LAs in terms of
                    the socio-economic characteristics, each LA was assigned 10
                    such neighbours.  See [link on gov.uk] for further details.
                  <br>
                  <br>
                  These neighbour groupings are used in this tool to allow
                   comparison of the selected authority with the authorities in
                   its Statistical Neighbour group.
                  <br>
                  <br>
                  <b>Figure 9: Statistical Neighbour View</b>
                  <br>"
                ),
                tags$img(
                  src = "images/user_guide/Stat_N_view_main_table.png",
                  alt = "Figure 9: Statistical Neighbour View",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Following the data tables is a chart that presents the data
                    for the selected authority (highlighted in blue) alongside
                    its Statistical Neighbours (grey) (figure 10).
                   <br>
                   <br>
                   <b>Figure 10: Statistical Neighbour Charting</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Stat_N_view_charts.png",
                  alt = "Figure 10: Statistical Neighbour Charting",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "The chart is interactive and although set by default as a
                    line chart, it can be changed to a bar chart and/or altered
                    to display the selected authority against a maximum of
                    three if its statistical neighbours."
                )
              ),
              # All LA View ====================================================
              tagList(
                HTML(
                  "The All LAs view presents all 153 Local Authorities, in
                    alphabetical order, plus previous authorities for those that
                    have recently undergone a local government reorganisation,
                    (figure 11). The table is downloadable.
                   <br>
                   <br>
                   <b>Figure 11: The view of all England's upper tier Local
                       Authorities</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/All_LAs_view_top_table.png",
                  alt = "Figure 11: The view of all England's upper tier Local Authorities",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                )
              ),
              # Create Your Own ================================================
              shiny::tagList(
                HTML(
                  "Provides functionality to create your own data table/s and
                    associated chart/s.
                   <br>
                   <br>
                   <b>Figure 12: Overview of create your own table and charts</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Create_Own_overview.png",
                  alt = "Figure 12: Overview of create your own table and charts",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   There are a few simple steps you need to take to choose and
                    extract the information required."
                ),
                tagList(
                  tags$ol(
                    tags$li("Using the dropdown menus, choose the level of data
                              i.e., LA/s, Region/s, England, or combinations
                              thereof."),
                    tags$br(),
                    tags$li(tagList(
                      "Choose measure/s. You can choose multiple measures
                        from any topic area or combination of topics.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_choose_measure.png",
                        alt = "Create Own Page: Select inputs to build table and charts",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "The choices made will appear in a ‘staging area’:",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_appear_staging.png",
                        alt = "Create Own Page: The Staging Table",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "There are also a few pre-set selections for ease. Simply
                        choose the option/s, and the information will appear in
                        the staging area.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_pre_set_groups.png",
                        alt = "Create Own Page: Pre-set selections",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "Once content with your choices, click the green ‘Add
                        Selections’ button. Your choices will now appear as a
                        query in a summary of selections section and an
                        associated output table, which can be downloaded.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_summary_selections.png",
                        alt = "Create Own Page: Summary of Selections Table",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      ),
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_output_table.png",
                        alt = "Create Own Page: Output Table containing all selections",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "You can create numerous queries but please note that
                        these can not be stored in the tool and can only be used
                        during your live session.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_store_selections.png",
                        alt = "Create Own Page: Can add multiple sets of selections",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "Finally, queries can be output to a chart (line and/or
                        bar), downloaded, or copied to the clipboard. However,
                        note restrictions: charts can only be generated for a
                        maximum of four geographies and three measures/indicators.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_output_charts.png",
                        alt = "Create Own Page: Output charts",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    ))
                  ),
                  tags$div(
                    style = "margin-left: 1.5em; margin-top: 10px;",
                    "Message will be displayed if chart cannot be generated:"
                  ),
                  tags$img(
                    src = "images/user_guide/Create_Own_output_charts_error.png",
                    alt = "Create Own Page: Erro message for output charts",
                    style = "width:100%; max-width:600px; margin-left: 1.5em; margin-top:10px;"
                  )
                )
              )
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
