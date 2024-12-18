user_guide_panel <- function() {
  shiny::tabPanel(
    "User guide",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          style = "max-width: 1200px",
          shiny::br(),
          h1("LAIT User Guide"),


          # Introduction =======================================================
          h2("Introduction"),
          p("The Department has developed the Local Authority Interactive Tool
              (LAIT) to provide easy access to a wide range of data related to
              children and young people sourced from various departments across
              government.The app is designed and maintained by the DFE's Regions
              Group LA Performance & Data (LAPD) Team."),


          # Context and Purpose ================================================
          h2("Context and purpose"),
          p("The tool facilitates local authority ‘benchmarking’ and trend
              analysis by allowing users to compare individual local authority
              (LA) data alongside national, regional and Statistical Neighbour
              groupings over time (up to 10 years)."),
          p("The tool consists of five data views which present data items
              largely in the form of tables and charts, which can be downloaded
              and used elsewhere.  Rank and quartile positions are also included
              to ease the assessment of local authority positioning in a
              national context."),
          p("The tool holds in the region of 400 children's services measures
              covering:"),
          tags$ul(
            tags$li("Children’s health and wellbeing;"),
            tags$li("Children in need, Child protection,
                    Children’s services workforce,
                    and Children looked after (inc. adoption);"),
            tags$li("Early years;"),
            tags$li("Key stage attainment (inc. by age 19);"),
            tags$li("Children with SEN;"),
            tags$li("Further and higher education;"),
            tags$li("Behaviour and attendance;"),
            tags$li("Economic factors;"),
            tags$li("Youth offending;"),
            tags$li("Children’s services finance.")
          ),
          p("We will add some relevant background information here. For example
              some useful LAIT resources."),


          # Using the app ======================================================
          h2("Using the app"),
          p("Below provides some guidance on how to use the LAIT app.
              The tool should be easy to use and consulting this guidance
              is not necessarily required. However it does given written detail
              and screenshots of the app to describe the functionality, which
              may give further clarity."),
          p("The Hints and Tips section explains how to harness the full
              features of the app. For example, how to save your selections
              on the Create Your Own page and the statsitical neighbour
              average calculation explanation."),
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
                  "
                   <b>Statistical Neighbour Averages:</b>
                   <br>
                   <br>
                   The figures presented are simple averages for the selected
                    LA’s 10 statistical neighbours.
                   <br>
                   <br>
                   They provide a simple comparator of the measure without
                    placing too much emphasis on any one authority. Where data
                    does not exist for one or more of the selected LA’s statistical
                    neighbour group, the information is excluded from the mean
                    calculation.
                  <br>
                  <br>
                  <br>
                  <b>Save selections on the Create Your own page:</b>
                  <br>
                  <br>
                  Selections can be saved so that on entering the tool
                   your selections are pre-populated. Then all you need to
                   do is click 'Add selections'.
                  <br>
                  <br>
                  To do this you must first make the selections you want to save.
                   Then copy the URL (webpage link in the search bar usually at the top
                   of your screen - figure 1.01) and keep this somewhere safe.
                  <br>
                  <br>
                  Now, if you use this link to open the app, your selections
                  will load in automatically! Simply click 'Add selections' and
                  download or view your data.
                  <br>
                  <br>
                  Below is an example link which has the following selections:"
                ),
                tags$ul(
                  tags$li("Geographies: Barking and Dagenham, Barnsley"),
                  tags$li(paste(
                    "Indicators: A levels 3+ A grades / Double awards,",
                    "A levels AAB grades / Applied / Double awards"
                  )),
                  tags$li("LA Groupings: Include All LAs"),
                  tags$li("Other Groupings: England")
                ),
                HTML("<br>Try the link out and see the selections prepopulate!"),
                tags$br(),
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
                tags$br(),
                tags$br(),
                HTML(
                  "<b>Figure 1.01: Copy the URL to save selections on Create Your Own</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Create_Own_save_selections.png",
                  alt = paste(
                    "A screenshot showing the Create Your Own page. The webpage URL",
                    "is highlighted and an arrow points at it to emphasize where to",
                    "copy the URL from."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "hint_create_own_link",
                  link_text = "(Link to the Create Your Own page)",
                  target_tab = "create_your_own",
                  target_id = "create_own-page_anchor"
                ),
                HTML(
                  "
                   <br>
                   <br>
                   <br>
                   <b>General tips:</b>
                   <br>
                   <br>
                   <b>Missing data</b>
                   <br>
                   <br>
                   NA - Data is missing or supressed.
                   <br>
                   <br>
                   Blank cells in tables - Data does not exist (only the case
                    on the Create Your Own page where indicators can be mixed,
                    causing extra year columns to be added).
                   <br>
                   <br>
                   - (dash) - Ranking and/or quartile banding is not applicable
                    to this indicator.
                   <br>
                   <br>
                   <br>
                   <b>Downloading Data</b>
                   <br>
                   <br>
                   Most of the data in the tool has been rounded for ease of
                    display. However, you can access the raw values by downloading
                    the datasets (as CSV or XLSX).
                   <br>
                   <br>
                   If you want to download the interactive part of the
                    charts you should use the HTML datatype. Although this is
                    a sligthly larger filetype (no bigger than 500 KB).
                   <br>
                   <br>
                   Finally, the copy-to-clipbaord functionality is relatively
                    innovative feature and is still in development. It should
                    work well on laptops but is not currently supported on
                    mobile devices. But, you can still download the charts as SVG
                    or HTML.
                   <br>
                   <br>
                   <br>
                   <b>Functionality Hints</b>
                   <br>
                   <br>
                  "
                ),
                tags$img(
                  src = "images/user_guide/tooltip_hint.png",
                  alt = "Screenshot of the tooltip icon.",
                  class = "user-guide-image",
                  style = "width: auto;"
                ),
                HTML(
                  "
                   - A tooltip that provides further information.
                   <br>
                   <br>
                  "
                ),
                tags$img(
                  src = "images/user_guide/expand_button_hint.png",
                  alt = paste(
                    "Screenshot of the expand button found when hovering the mouse",
                    "over the bottom right of a chart card."
                  ),
                  class = "user-guide-image",
                  style = "width: auto;"
                ),
                HTML(
                  "
                   - Expands the chart to full-screen for better visability.
                   <br>
                   <br>
                   (The button can be found by hovering over the bottom-right of
                   the chart cards.)
                   <br>
                   <br>
                  "
                ),
                tags$img(
                  src = "images/user_guide/x_button_hint.png",
                  alt = paste(
                    "Screenshot of the Local Authority input box,",
                    "showing the small x button found to the right of the selected",
                    "LA."
                  ),
                  class = "user-guide-image",
                  style = "width: auto;"
                ),
                HTML(
                  "
                  - Click 'x' symbol to remove/delete selections.
                  <br>
                  <br>
                  <br>
                  If there is anything you think we have missed, please let us
                   know by emailing Darlington.BRIDGE@education.gov.uk or
                   explore.statistics@education.gov.uk, or by
                 "
                ),
                internal_nav_link(
                  id = "hint_feedback_link",
                  link_text = "filling out the feedback form",
                  target_tab = "support",
                  target_id = "support_to_dashboard"
                ),
                "."
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
                   The main table displays time series data
                    for the chosen indicator in respect of the LA, its Region,
                    Statistical Neighbours and England averages - for
                    comparison/benchmarking purposes (figure 2.01).
                   <br>
                   <br>
                   Note that the selected LA is highlighted in orange
                    within the data table, and that there is an option to
                    download the data as a CSV or XLSX file type.
                   <br>
                   <br>
                   For the Statistical Neighbour values, the
                    figures presented are simple averages for the selected
                    LA’s 10 statistical neighbours. They provide a simple
                    comparator of the measure without placing too much emphasis
                    on any one authority. Where data does not exist for one or
                    more of the selected LA’s statistical neighbour group,
                    the information is excluded from the mean calculation.
                   <br>
                   <br>
                   <b>Figure 2.01: LA Level View</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_view_main_table.png",
                  alt = "Screenshot of the header and first table of the LA Level view.",
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_la_level_link",
                  link_text = "(Link to the LA Level main data table)",
                  target_tab = "la_level",
                  target_id = "la_header-page_anchor"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Beneath this is a sub-table showing latest year on year
                    trend, change from previous year and the latest year's
                    national rank (figure 2.02).
                    If applicable the authority is placed into a quartile
                    banding (based on the latest year's data)
                    and colour formatting will flag up upper (A) and Lower (D)
                    quartiles for clarity.
                   <br>
                   <br>
                   <b>Figure 2.02: LA Level Summary Table</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_view_stats_table.png",
                  alt = "Screenshot of the summary table of the LA Level view.",
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_la_level_summary_link",
                  link_text = "(Link to the LA Level Summary data table)",
                  target_tab = "la_level",
                  target_id = "la_table-la_table"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   This table is followed by a chart (figure 2.03), which plots the
                    LA, national, region and statistical neighbour
                    average data over the same period as the data table.
                    The data is displayed as a line chart by default but can be
                    changed to a bar chart by switching to the bar chart tab.
                    Both charts are downloadable (as SVG or HTML) or can be
                    copied to your clipboard.
                   <br>
                   <br>
                   <b>Figure 2.03: LA Level Charts</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_view_charts.png",
                  alt = "Screenshot of the line chart on the LA Level view.",
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_la_level_charts_link",
                  link_text = "(Link to the LA Level charts)",
                  target_tab = "la_level",
                  target_id = "la_stats-la_stats"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Beneath the chart, the metadata of the indicator is provided
                    (figure 2.04). This includes a description and methodology,
                    as well as an indication of last and next update
                    dates. Finally, a hyperlink to the data source (if clicked,
                    the source will open in new tab). The metadata is the same
                    for each indicator across the different views.
                   <br>
                   <br>
                   <b>Figure 2.04: Metadata</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/LA_view_metadata.png",
                  alt = "Screenshot of the metatdata on the LA Level view.",
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_la_level_metadata_link",
                  link_text = "(Link to the LA Level metadata)",
                  target_tab = "la_level",
                  target_id = "la_meta-metadata"
                )
              ),
              # Regional View ==================================================
              shiny::tagList(
                HTML(
                  "This presents the selected LA’s data alongside its
                    Regional LA neighbours. Then the selected local
                    authority’s region's data against other regions data (and
                    England). The selected LA and region are highlighted
                    in orange (figure 3.01).
                   <br>
                   <br>
                   <b>Figure 3.01: Regional Level View</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Region_view_main_table.png",
                  alt = paste(
                    "Screenshot of the first two tables (LAs and Regions)",
                    "on the Regional Level view."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_region_view_link",
                  link_text = "(Link to the Regional Level View)",
                  target_tab = "regional_level",
                  target_id = "region_header-page_anchor"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   Differing to the Local Authority view, the charts are split
                    into two types, 'Focus' and 'User selection'.
                    There are line and bar charts for each of these types.
                   <br>
                   <br>
                   The 'Focus' chart presents each regions' data (figure 3.02).
                    The selected LA’s region is highlighted in blue,
                    whilst other regions are coloured grey. This is to give a clear
                    contrast of how the region of interest compares to other
                    regions.
                   <br>
                   <br>
                   <b>Figure 3.02: Regional Level 'Focus' Line Chart</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Region_view_focus_chart.png",
                  alt = paste(
                    "Screenshot of the focus line chart",
                    "on the Regional Level view."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_region_focus_link",
                  link_text = "(Link to the Regional Level Charts)",
                  target_tab = "regional_level",
                  target_id = "region_stats_mod-stats_table"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   The 'User selection' chart displays the same data but its default
                    is to only show the selected LA's region's data
                    (figure 3.03). Users can then add up to three other regions
                    (or England). This allows users to make more specific
                    comparisons between regions.
                   <br>
                   <br>
                   <b>Figure 3.03: Regional Level 'User selection' Line Chart</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Region_view_multi_chart.png",
                  alt = paste(
                    "Screenshot of the User selection line chart",
                    "on the Regional Level view."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_region_multi_link",
                  link_text = "(Link to the Regional Level Charts)",
                  target_tab = "regional_level",
                  target_id = "region_stats_mod-stats_table"
                ),
              ),
              # Statistical Neighbour View =====================================
              shiny::tagList(
                "Statistical neighbours provide a method for benchmarking
                 progress. For each LA, these models
                 designate several other LAs deemed to have similar
                 characteristics. These designated LAs are known as
                 statistical neighbours.  The National Foundation for
                 Educational Research (NFER) was commissioned in 2007 by the
                 Department to identify and group similar LAs in terms of
                 the socio-economic characteristics, each LA was assigned 10
                 such neighbours. See the ",
                dfeshiny::external_link(
                  href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait",
                  link_text = "Local Authority interactive tool (LAIT) GOV.UK",
                  add_warning = TRUE
                ),
                HTML(
                  " landing page for further details.
                  <br>
                  <br>
                  These neighbour groupings are used in this tool to allow
                   comparison of the selected LA alongside the
                   LAs in it's statistical neighbour group (figure 4.01).
                  <br>
                  <br>
                  <b>Figure 4.01: Statistical Neighbour View</b>
                  <br>"
                ),
                tags$img(
                  src = "images/user_guide/Stat_N_view_main_table.png",
                  alt = paste(
                    "Screenshot of the tables (Statistical Neighbours, Other",
                    " Geographies, SUmmary) on the Statistical Neighbour view."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_sn_view_link",
                  link_text = "(Link to the Statistical Neighbour View)",
                  target_tab = "statistical_neighbour_level",
                  target_id = "stat_n_header-page_anchor"
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
                    The 'Focus' charts show the selected LA
                     (highlighted in blue) alongside its statistical neighbours
                     (grey) (figure 4.02). This is to give a clear contrast of how
                     the selected LA compares to it's neighbours.
                   <br>
                   <br>
                   <b>Figure 4.02: Statistical Neighbour 'Focus' Bar Chart</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Stat_N_view_focus_bar_chart.png",
                  alt = paste(
                    "Screenshot of the focus bar chart",
                    "on the Statistical Neighbour view."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_sn_focus_link",
                  link_text = "(Link to the Statistical Neighbour Charts)",
                  target_tab = "statistical_neighbour_level",
                  target_id = "stat_n_stats_mod-output_table"
                ),
                HTML(
                  "<br>
                   <br>
                   <br>
                   The 'User selection' chart displays the same data but its default
                    is to only show the selected LA's data
                    (figure 4.03). Users can then add up to three other statistical
                    neighbours (or the selected LA's region or England).
                    This allows users to make more specific comparisons between
                    the geographies.
                   <br>
                   <br>
                   <b>Figure 4.03: Statistical Neighbour 'User selection' Bar Chart</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Stat_N_view_multi_bar_chart.png",
                  alt = paste(
                    "Screenshot of the User selection bar chart",
                    "on the Statistical Neighbour view."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_sn_multi_link",
                  link_text = "(Link to the Statistical Neighbour Charts)",
                  target_tab = "statistical_neighbour_level",
                  target_id = "stat_n_stats_mod-output_table"
                )
              ),
              # All LA View ====================================================
              tagList(
                HTML(
                  "The All LAs view presents all 153 Local Authorities, in
                    alphabetical order, plus previous authorities for those that
                    have recently undergone a local government reorganisation,
                    (figure 5.01). Each of the tables are downloadable.
                   <br>
                   <br>
                   <b>Figure 5.01: All LA Level View</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/All_LAs_view_top_table.png",
                  alt = paste(
                    "Screenshot of the top of the All LA view, showing the",
                    "first several rows of the LA table."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_all_la_view_link",
                  link_text = "(Link to the All LA view)",
                  target_tab = "all_la_level",
                  target_id = "all_la_header-page_anchor"
                )
              ),
              # Create Your Own ================================================
              shiny::tagList(
                HTML(
                  "The Create Your Own page provides functionality to create
                    your own data table(s) and associated chart(s) (figure 6.01).
                   <br>
                   <br>
                   <b>Figure 6.01: Overview of Create Your Own page</b>
                   <br>"
                ),
                tags$img(
                  src = "images/user_guide/Create_Own_overview.png",
                  alt = paste(
                    "Screenshot of the top of the Create Your Own page.",
                    "Shows the notification banner pointing towards GitHub for",
                    "bigger data downloads, the selection choices, the Staging Table,",
                    "Summary of Selections table and the Output Table."
                  ),
                  class = "user-guide-image"
                ),
                tags$br(),
                tags$br(),
                internal_nav_link(
                  id = "user_guide_create_own_link",
                  link_text = "(Link to the Create Your Own page)",
                  target_tab = "create_your_own",
                  target_id = "create_own-page_anchor"
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
                        alt = paste(
                          "Screenshot of the main selections available to build the",
                          "Create Your Own table/charts. These are Geographies (LA",
                          "Regions, England), Topic and Indicator."
                        ),
                        class = "user-guide-image"
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
                        alt = paste(
                          "Screenshot of pre-set selections available.",
                          "These are geographies (include all LAs, LAs in the selected",
                          "LA regions, LAs that are statistical neighbours. Add England",
                          "or Regions. Choose the year range. Finally, a button to clear",
                          "all selections."
                        ),
                        class = "user-guide-image"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "The choices made will appear in a ‘Staging Table’:",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_appear_staging.png",
                        alt = paste(
                          "Screenshot of the Staging Table populated with a few",
                          "selections."
                        ),
                        class = "user-guide-image"
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
                        alt = paste(
                          "Screenshot of the Summary of Selections table,",
                          "showing a row of selections that have been added."
                        ),
                        class = "user-guide-image"
                      ),
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_output_table.png",
                        alt = paste(
                          "Screenshot of the Output Table showing the",
                          "data that has been added from the saved selections."
                        ),
                        class = "user-guide-image"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "You can create and add numerous series of selections but
                        please note that if you are downloading lots of data
                        this may take some time to load. Also, you can download
                        the full dataset (in long format) from the ",
                      dfeshiny::external_link(
                        href = paste(
                          "https://github.com/dfe-analytical-services/",
                          "local-authority-interactive-tool/tree/main/01_data/02_prod"
                        ),
                        link_text = "data folder in the LAIT GitHub",
                        add_warning = TRUE
                      ),
                      " in the files which names start with 'bds_long'.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_store_selections.png",
                        alt = paste(
                          "Screenshot of the Summary of Selections Table showing",
                          "several rows of saved selections."
                        ),
                        class = "user-guide-image"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "Selections can be saved so that on entering the tool
                       your selections are pre-populated. Then all you need to
                       do is click 'Add selections'. To do this you must first
                       make the selections you want to save. Then copy the
                       URL (webpage link in the search bar usually at the top
                       of your screen) and keep this somewhere safe. If you
                       use this link to open the app, your selections will load in
                       automatically. Here is an example:",
                      tags$br(),
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
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_save_selections.png",
                        alt = paste(
                          "A screenshot showing the Create Your Own page. The webpage URL",
                          "is highlighted and an arrow points at it to emphasize where to",
                          "copy the URL from."
                        ),
                        class = "user-guide-image"
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
                        alt = paste(
                          "A screenshot showing the Create Your Own page line chart.",
                          "It displays the data added to the Output Table in the first",
                          "row of saved selections."
                        ),
                        class = "user-guide-image"
                      )
                    )),
                    tags$br(),
                    tags$li(tagList(
                      "A message will display when these retrictions and breached
                        and so the charts do not appear.",
                      tags$br(),
                      tags$img(
                        src = "images/user_guide/Create_Own_output_charts_error.png",
                        alt = paste(
                          "A screenshot showing the Create Your Own page line chart with",
                          "an error message saying 'No plot as too many Geographies selected",
                          "If you think this is incorrect, please report so in the feedback form'."
                        ),
                        class = "user-guide-image"
                      )
                    ))
                  )
                )
              )
            )
          ),
          # Feedback wanted ====================================================
          h2("Feedback"),
          p(
            "If you think something is unclear or would benefit from having
              extra guidance then please let us know by ",
            internal_nav_link(
              id = "user_guide_feedback_link",
              link_text = "filling out the feedback form",
              target_tab = "support",
              target_id = "support_to_dashboard"
            ),
            ". Thanks in advance!"
          ),
          # Guidance sources ===================================================
          h2("Guidance sources"),
          p(
            "Follow this link to find a list of ",
            internal_nav_link(
              id = "info_page_useful_resources",
              link_text = "links to useful or related resources",
              target_tab = "updates_and_sources",
              target_id = "useful_links_link"
            ),
            "."
          )
        )
      )
    )
  )
}
