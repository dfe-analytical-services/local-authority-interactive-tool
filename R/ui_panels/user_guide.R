user_guide_panel <- function() {
  shiny::tabPanel(
    "User guide",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          shiny::br(),
          h1("LAIT User Guide"),


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
            InternalLinkUI("la_level_link", "LA Level page")
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
          p("Below provides some guidance on how to use the LAIT app.
              The tool should be easy to use and consulting this guidance
              is not necessarily required. However it does given written detail
              on the functionality which may be of use."),
          p("The Hints and Tips section will be of most interest as here it is
              explained how to harness the full features of the app. For
              example, how to save your selections on the Create Your Own page."),
          shinyGovstyle::accordion(
            inputId = "user-guide-how-to",
            titles = c(
              "Hints and Tips",
              "Local Authority View",
              "Regional View",
              "Statistical Neighbour View",
              "All Local Authorities View",
              "Create Your Own"
            ),
            descriptions = list(
              # Hints and Tips =================================================
              shiny::tagList(
                HTML(
                  "Statistical Neighbour Averages: the figures presented are
                    simple averages for the selected LA’s 10 statistical
                    neighbours (SNs). They provide a simple comparator of the
                    measure without placing too much emphasis on any one
                    authority. Where data does not exist for one or more of the
                    selected LA’s SN group, the information is excluded from the
                    mean calculation.
                  "
                )
              ),
              # LA Level View ==================================================
              tagList(
                HTML(
                  "Use the three dropdowns to select a local authority (LA)
                    and measure.
                   <br>
                   <br>
                   As this is an interactive tool, the tables and chart (and
                    relevant titling) change automatically depending on the
                    indicator chosen.
                   <br>
                   <br>
                   The table displays time series data
                    for the chosen indicator in respect of the LA, its Region,
                    Statistical Neighbours and England averages - for
                    comparison/benchmarking purposes (figure ).
                   <br>
                   <br>
                   Note that the selected LA is highlighted in orange
                    within the data table, and that there is an option to
                    download the data as a CSV or XLSX file type.
                   <br>
                   <br>
                   For the Statistical Neighbour values, the
                    figures presented are simple averages for the selected
                    LA’s 10 statistical neighbours (SNs). They provide a simple
                    comparator of the measure without placing too much emphasis
                    on any one authority. Where data does not exist for one or
                    more of the selected LA’s SN group, the information is
                    excluded from the mean calculation.
                   <br>
                   <br>
                   <b>Figure : Data Table for Chosen Authority/Data Item</b>
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
                    trend, change from previous year and the latest year's
                    national rank (figure ).
                    If applicable the authority is placed into a quartile
                    banding (based on the latest year's data)
                    and colour formatting will flag up upper (A) and Lower (D)
                    quartiles for clarity.
                   <br>
                   <br>
                   <b>Figure : Trends and Quartile bandings</b>
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
                   This table is followed by a chart (figure ), which plots the
                    local authority, national, region and statistical neighbour
                    average data over the same period as the data table.
                    The data is displayed as a line chart by default but can be
                    changed to a bar chart by switching to the bar chart tab.
                    Both charts are downloadable (as SVG or HTML) or can be
                    copied to your clipboard.
                   <br>
                   <br>
                   <b>Figure : Charting the Data</b>
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
                   Beneath the chart, the metadata of the indicator is provided
                    (figure ). This includes a description and methodology,
                    as well as an indication of last and next update
                    dates. Finally, a hyperlink to the data source (if clicked,
                    the source will open in new tab).
                   <br>
                   <br>
                   <b>Figure : Metadata</b>
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
                  "This presents the selected local authority’s data alongside its
                    Regional local authority neighbours. Then the selected local
                    authority’s region's data against other regions data (and
                    England). The selected local authority and region are highlighted
                    in orange (figure ).
                   <br>
                   <br>
                   <b>Figure : Regional Level View</b>
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
                   Differing to the Local Authority view, the charts are split
                    into two types, 'Focus' and 'User selection' (figure ).
                    There are line and bar charts for each of these types.
                   <br>
                   <br>
                   The 'Focus' chart presents each regions' data (figure ).
                    The selected local authority’s region is highlighted in blue,
                    whilst other regions are coloured grey. This is to give a clear
                    contrast of how the region of interest compares to other
                    regions.
                   <br>
                   <br>
                   The 'User selection' chart displays the same data but its default
                    is to only show the selected local authority's region's data
                    (figure ). Users can then add up to three other regions
                    (or England). This allows users to make more specific
                    comparisons between regions.
                   <br>
                   <br>
                   <b>Figure : Regional Level Charting</b>
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
                  The chart area is again interactive and includes an option to
                   create a ‘mini’ chart with a max of three regions of your
                   choice presented alongside the selected LA’s region. ###### BIN
                   <br>
                   <br>
                   <b>Figure : User choice regional chart</b>
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
                    such neighbours. See ",
                  dfeshiny::external_link(
                    href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait",
                    link_text = "Local Authority interactive tool (LAIT) GOV.UK",
                    add_warning = TRUE
                  ),
                  " ladning page for further details.
                  <br>
                  <br>
                  These neighbour groupings are used in this tool to allow
                   comparison of the selected local authority alongside the
                   local authorities in it's statistical neighbour group.
                  <br>
                  <br>
                  <b>Figure : Statistical Neighbour View</b>
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
                   Following the data tables are 'Focus' and 'User selection'
                    charts that present the statistical neighbour data. There
                    are line and bar charts for each of these types.
                   <br>
                   <br>
                    The 'Focus' charts show the selected local authority
                     (highlighted in blue) alongside its statistical neighbours
                     (grey) (figure ). This is to give a clear contrast of how
                     the selected local authoity compares to it's neighbours.
                   <br>
                   <br>
                   <b>Figure : Statistical Neighbour Charting</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Stat_N_view_charts.png",
                  alt = "Figure 10: Statistical Neighbour Charting",
                  style = "width:100%; max-width:600px; margin-top:10px;"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   The 'User selection' chart displays the same data but its default
                    is to only show the selected local authority's data
                    (figure ). Users can then add up to three other statistical
                    neighbours (or the selected local authority's region or England).
                    This allows users to make more specific comparisons between
                    the geographies."
                )
              ),
              # All LA View ====================================================
              tagList(
                HTML(
                  "The All LAs view presents all 153 Local Authorities, in
                    alphabetical order, plus previous authorities for those that
                    have recently undergone a local government reorganisation,
                    (figure ). Each of the tables are downloadable.
                   <br>
                   <br>
                   <b>Figure : The view of all England's upper tier Local
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
                  "The Create Your Own page provides functionality to create
                    your own data table(s) and associated chart(s).
                   <br>
                   <br>
                   <b>Figure : Overview of create your own table and charts</b>
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
                    extract the data you would like."
                ),
                tagList(
                  tags$ol(
                    tags$li("Using the dropdown menus, choose the level of data
                              i.e., LA(s), Region(s), England, or combinations
                              of these."),
                    tags$br(),
                    tags$li(tagList(
                      "Choose measure(s). You can choose multiple measures
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
                      "There are also a few pre-set selections for easy use. Simply
                        choose the option(s), and the data will appear in
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
                      "The choices made will appear in a ‘Staging Table’:",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_appear_staging.png",
                        alt = "Create Own Page: The Staging Table",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "Once you have made your selections, click the green ‘Add
                        Selections’ button (to the righ of the Staging Table header).
                        Your choices will now appear in the 'Summary of Selections'
                        table and the resulting data will be added to the
                        'Output Table', which can be downloaded.",
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
                      "You can create and add numerous series of selections but
                        please note that if you are downloading lots of data
                        this may take some time to load. Also, you can download
                        the full dataset (in long format) from the ",
                      dfeshiny::external_link(
                        href = "https://github.com/dfe-analytical-services/local-authority-interactive-tool/tree/main/01_data/02_prod",
                        link_text = "data folder in the LAIT GitHub",
                        add_warning = TRUE
                      ),
                      " in the files which names start with 'bds_long'.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_store_selections.png",
                        alt = "Create Own Page: Can add multiple sets of selections",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "Selections can be saved so that on entering the tool
                       your selections are pre-populated. Then all you need to
                       do is click 'Add Selections'. To do this you must first
                       make the selections you want to save. Then copy the
                       URL (webpage link in the search bar usually at the top
                       of your screen) and keep this somewhere safe. If you
                       use this link to open the app, your selections will load in
                       automatically. Here is an example:<br>",
                      dfeshiny::external_link(
                        href = paste0(
                          "https://department-for-education.shinyapps.io/",
                          "local-authority-interactive-tool/?_inputs_&year_range-",
                          "year_range=null&pages=%22dashboard%22&left_nav=%22create_your_own",
                          "%22&la_inputs-la_name=%22Barking%20and%20Dagenham%22&la_inputs-",
                          "indicator_name=%22A%20level%20cohort%20Average%20point%20score%20",
                          "per%20entry%22&region_inputs-la_name=%22Barking%20and%20Dagenham%22",
                          "&region_inputs-indicator_name=%22A%20level%20cohort%20Average%20point%20",
                          "score%20per%20entry%22&stat_n_inputs-la_name=%22Barking%20and%20",
                          "Dagenham%22&stat_n_inputs-indicator_name=%22A%20level%20cohort%20",
                          "Average%20point%20score%20per%20entry%22&all_la_inputs-la_name=%22",
                          "Barking%20and%20Dagenham%22&all_la_inputs-indicator_name=%22A%20level",
                          "%20cohort%20Average%20point%20score%20per%20entry%22&create_inputs-",
                          "geog_input=%5B%22Barking%20and%20Dagenham%22%2C%22Barnsley%22%5D&create_inputs",
                          "-indicator=%5B%22A%20levels%203%2B%20A%20grades%20%2F%20Double%20awards%22%2C%22A%20",
                          "levels%20AAB%20grades%20%2F%20Applied%20%2F%20Double%20awards%22%5D&create_inputs-",
                          "la_group=%22all_las%22&create_inputs-inc_regions=false&create_inputs-inc_england=true"
                        ),
                        link_text = "Test link to show Create Your Own pre-populated",
                        add_warning = TRUE
                      ),
                      ".",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_store_selections.png",
                        alt = "Create Own Page: Can add multiple sets of selections",
                        style = "width:100%; max-width:600px; margin-top:10px;"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "Finally, saved selections will be displayed as a chart
                        (line and bar). These can be downloaded. However, note
                        the restrictions are that charts can only be generated
                        for a maximum of four geographies and three indicators.",
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
                    "A message will display when these retrictions and breached
                      and so the charts do not appear."
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
          p(
            "Follow this link to find a list of ",
            internal_nav_link(
              id = "info_page_useful_resources",
              link_text = "links to useful or related resources",
              target_tab = "information_page",
              target_id = "useful_links_link"
            ),
            "."
          )
        )
      )
    )
  )
}
