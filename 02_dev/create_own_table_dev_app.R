# Load global ==================================================================
source(here::here("global.R"))

# Load functions ===============================================================
list.files("R/", full.names = TRUE) |>
  (\(x) {
    x[grepl("fn_", x)]
  })() |>
  purrr::walk(source)

# UI ===========================================================================
ui <- bslib::page_fillable(
  ## Other language dependencies ===============================================
  shiny::includeCSS(here::here("www/dfe_shiny_gov_style.css")),
  tags$head(htmltools::includeScript("www/custom_js.js")),
  # Makes the remove button work
  reactable.extras::reactable_extras_dependency(),

  # Main selections ============================================================
  h1("Create your own"),
  div(
    class = "well",
    style = "overflow-y: visible; padding: 1rem;",
    bslib::layout_column_wrap(
      # Geographic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = "geog",
          label = "LAs, Regions, and England:",
          choices = c(la_names_bds, region_names_bds, "England"),
          multiple = TRUE,
          options = list(
            "placeholder" = "Select a LA, Region or England"
          )
        )
      ),
      # Topic input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = "topic_input",
          label = "Topic:",
          choices = metric_topics
        )
      ),
      # Indicator input
      div(
        style = "margin-bottom: 1rem;",
        shiny::selectizeInput(
          inputId = "indicator",
          label = "Indicator:",
          choices = metric_names,
          multiple = TRUE,
          options = list(
            "placeholder" = "Select an indicator"
          )
        )
      )
    ),
    bslib::layout_column_wrap(
      # Checkbox inputs for LAs, Regions, etc
      shiny::radioButtons(
        inputId = "la_group",
        label = "LA Groupings (choose one):",
        choices = list(
          "None" = "no_groups",
          "Include All LAs" = "all_las",
          "Include LAs in the same Region" = "region_las",
          "Include statistical neighbours" = "la_stat_ns"
        ),
        selected = NULL,
        inline = FALSE
      ),
      # Other groupings
      div(
        style = "width: fit-content;",
        shiny::p("Other groupings:"),
        shiny::checkboxInput("inc_regions", "Include All Regions", FALSE),
        shiny::checkboxInput("inc_england", "Include England", FALSE),
        shinyWidgets::pickerInput(
          inputId = "year_range",
          label = "Select year range:",
          choices = NULL,
          multiple = TRUE
        ),
      )
    ),
    shiny::br(),
    # Add selections button
    shiny::actionButton("add_query", "Add selections", class = "gov-uk-button")
  ),

  # Staging table ==============================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Staging Table (View of current selections)"),
    bslib::card(
      reactable::reactableOutput("staging_table")
    )
  ),

  # Selections table ===========================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Summary of Selections"),
    bslib::card(
      reactable::reactableOutput("query_table")
    )
  ),

  # Output data table ==========================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Output Table (View of all saved selections)"),
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Output Table",
        reactable::reactableOutput("output_table")
      ),
      # Download tab
      bslib::nav_panel(
        title = "Download",
        file_type_input_btn("file_type"),
        Download_DataUI("table_download", "Output Table")
      )
    )
  ),

  # Charts =====================================================================
  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Output Charts (Charts showing data from saved selections)"),
    p("Note a maximum of 4 geographies and 3 indicators can be shown."),

    # Line chart ---------------------------------------------------------------
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Line chart",
        # Line chart plot with download buttons
        div(
          style = "display: flex;
                   justify-content: space-between;
                   align-items: center;
                   background: white;",
          bslib::card(
            bslib::card_body(
              ggiraph::girafeOutput("line_chart")
            ),
            full_screen = TRUE,
            style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
          ),
          div(
            # Download button to trigger chart download modal
            shiny::tagAppendAttributes(
              DownloadChartBtnUI("download_btn_line"),
              style = "max-width: none; margin-left: 0;  align-self: auto;"
            ),
            br(),
            shiny::tagAppendAttributes(
              actionButton(
                "copybtn_line",
                "Copy Chart to Clipboard",
                icon = icon("copy"),
                class = "gov-uk-button"
              ),
              style = "max-width: none;"
            ),
            style = "display: flex; flex-direction: column; align-self: flex-start; margin: 15px;"
          )
        ),
        # Hidden static plot for copy-to-clipboard
        div(
          shiny::plotOutput("copy_plot_line"),
          style = "content-visibility: hidden;"
        )
      ),

      # Bar chart --------------------------------------------------------------
      bslib::nav_panel(
        title = "Bar chart",
        # Line chart plot with download buttons
        div(
          style = "display: flex;
                   justify-content: space-between;
                   align-items: center;
                   background: white;",
          bslib::card(
            bslib::card_body(
              ggiraph::girafeOutput("bar_chart")
            ),
            full_screen = TRUE,
            style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
          ),
          div(
            # Download button to trigger chart download modal
            shiny::tagAppendAttributes(
              DownloadChartBtnUI("download_btn_bar"),
              style = "max-width: none; margin-left: 0; align-self: auto;"
            ),
            br(),
            shiny::tagAppendAttributes(
              actionButton(
                "copybtn_bar",
                "Copy Chart to Clipboard",
                icon = icon("copy"),
                class = "gov-uk-button"
              ),
              style = "max-width: none;"
            ),
            style = "display: flex; flex-direction: column; align-self: flex-start; margin: 15px;"
          )
        )
      ),
      # Hidden static plot for copy-to-clipboard
      div(
        shiny::plotOutput("copy_plot_bar"),
        style = "content-visibility: hidden;"
      )
    )
  )
)


server <- function(input, output, session) {
  # Dynamic input choices ======================================================
  # Year range -----------------------------------------------------------------
  # Compute years choices available based on selected indicator
  years_choices <- reactive({
    # Get distinct years
    years_dict <- bds_metrics |>
      dplyr::filter(Measure %in% input$indicator, !is.na(Years)) |>
      dplyr::distinct(Years, Years_num)

    # Boolean for matching years' suffixes
    consistent_year_suffix <- years_dict |>
      check_year_suffix_consistency()

    # Display string years if matching suffix (numeric/clean if not)
    if (consistent_year_suffix) {
      years_dict$Years |>
        sort()
    } else {
      years_dict$Years_num |>
        sort()
    }
  })

  # Update the year range choices based on the selected indicator
  observeEvent(input$indicator, {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "year_range",
      choices = years_choices(),
      options = shinyWidgets::pickerOptions(
        maxOptions = 2,
        maxOptionsText = "Deselect a year",
        multipleSeparator = " to ",
        noneSelectedText = "All years available"
      )
    )
  })

  # When no indicators selected year range displays "Select an indicator"
  observe({
    if (is.null(input$indicator) || length(input$indicator) == 0) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "year_range",
        choices = "Please select an indicator first",
        options = shinyWidgets::pickerOptions(
          noneSelectedText = "Select an indicator to see year range"
        )
      )
    }
  })


  # Geography and Indicator inputs ---------------------------------------------
  # Reactive to store all selected indicators along with their topics
  selected_indicators <- reactiveVal({
    data.frame(
      Topic = character(),
      Measure = character()
    )
  })

  # Filter indicator choices based on the selected topic
  shiny::observeEvent(input$topic_input, {
    # Available indicators (based on topic chosen)
    filtered_topic_bds <- bds_metrics |>
      dplyr::filter(Topic %in% input$topic_input) |>
      dplyr::select(Topic, Measure)

    # Get the currently selected topic-indicator pairs
    current_selection <- selected_indicators()

    # Combine currently selected topic-indicator pairs with the new filtered ones
    combined_choices <- unique(rbind(current_selection, filtered_topic_bds))

    # Update the choices with new topic whilst retaining the
    # already selected indicators
    shiny::updateSelectizeInput(
      session = session,
      inputId = "indicator",
      choices = combined_choices$Measure,
      selected = current_selection$Measure
    )
  })

  # Update the selected_indicators reactive for newly selected topic-indicator pairs
  # This keeps selection consistent across topics
  shiny::observeEvent(input$indicator,
    {
      # Get the new topic-indicator pairs
      current_filtered <- bds_metrics |>
        dplyr::filter(
          Topic %in% input$topic_input,
          Measure %in% input$indicator
        ) |>
        dplyr::distinct(Topic, Measure)

      # Get previously selected indicators
      previous_selection <- selected_indicators()

      # Remove any topic-indicator pairs that have been deselected
      deselected_measures <- setdiff(previous_selection$Measure, input$indicator)
      updated_selection <- previous_selection |>
        dplyr::filter(!Measure %in% deselected_measures)

      # Combine the new topic-indicator pairs with the previous selections
      combined_selection <- unique(rbind(updated_selection, current_filtered))

      # Update the reactive value for all topic-indicator pairs
      selected_indicators(combined_selection)
    },
    ignoreNULL = FALSE
  )

  # Collating user selections ==================================================
  # Geography inputs -----------------------------------------------------------
  geog_inputs <- reactive({
    # Value from LA & Region input
    inputs <- input$geog

    # Add geography groupings (if selected)
    # All LAs
    if (isTRUE(input$la_group == "all_las")) {
      inputs <- unique(c(inputs, la_names_bds))
    }

    # All Regions
    if (isTRUE(input$inc_regions)) {
      inputs <- unique(c(inputs, region_names_bds))
    }

    # Include England
    if (isTRUE(input$inc_england)) {
      inputs <- unique(c(inputs, "England"))
    }

    # All LAs from selected LA region
    if (isTRUE(input$la_group == "region_las")) {
      selected_la_regions <- get_la_region(stat_n_geog, input$geog)
      all_region_las <- get_las_in_regions(stat_n_geog, selected_la_regions)

      inputs <- unique(c(inputs, all_region_las))
    }

    # LA statistical neighbours
    if (isTRUE(input$la_group == "la_stat_ns")) {
      selected_la_stat_n <- get_la_stat_neighbrs(stat_n_la, input$geog)

      inputs <- c(inputs, selected_la_stat_n)
    }

    # Return unique geographies
    unique(inputs)
  })

  # Statistical neighbour input ------------------------------------------------
  # Assign LA statistical neighbours their selected LA association
  stat_n_association <- reactive({
    # Create mini association df of SN LAs and their parent SN LA
    association_table <- data.frame(
      `LA and Regions` = character(),
      `sn_parent` = character(),
      check.names = FALSE
    )

    # If SN selected fill out the above SN association df
    if (isTRUE(input$la_group == "la_stat_ns")) {
      # Get LAs from geogs selected
      input_las <- intersect(input$geog, la_names_bds)

      # Build association df (include LA itself too)
      stat_n_groups <- lapply(input_las, function(la) {
        data.frame(
          `LA and Regions` = c(la, get_la_stat_neighbrs(stat_n_la, la)),
          `sn_parent` = la,
          check.names = FALSE
        )
      })

      # Combine all statistical neighbour associations into a single data frame
      if (length(input_las) > 0) {
        association_table <- do.call(rbind, stat_n_groups)
      }
    }

    # Return the association data
    association_table
  })


  # Staging table ==============================================================
  # Filter BDS for geographies and
  # topic-indicator pairs in the selected_values reactive
  filtered_bds <- reactive({
    req(input$topic_input, input$indicator)
    req(nrow(selected_indicators()) > 0)

    # Filter the bds_metrics by topic, indicator, and geography
    bds_filtered <- bds_metrics |>
      dplyr::semi_join(
        selected_indicators(),
        by = c(
          "Topic" = "Topic",
          "Measure" = "Measure"
        )
      ) |>
      dplyr::filter(
        `LA and Regions` %in% geog_inputs(),
        !is.na(Years)
      )

    # Cleaning Years
    # Check if all years have consistent suffix
    consistent_str_years <- check_year_suffix_consistency(bds_filtered)

    # If not consistent suffix use the cleaned year cols (numeric years)
    if (!consistent_str_years) {
      bds_filtered <- bds_filtered |>
        dplyr::mutate(
          Years = Years_num
        )
    }

    # Apply the year range filter
    # If only one year selected then show just that year
    if (length(input$year_range) == 1) {
      bds_filtered <- bds_filtered |>
        dplyr::filter(
          Years == input$year_range[1]
        )
    } else if (length(input$year_range) == 2) {
      bds_filtered <- bds_filtered |>
        dplyr::filter(
          Years >= input$year_range[1],
          Years <= input$year_range[2]
        )
    }

    # Return the user selection filtered data for staging table
    bds_filtered
  })

  # Build the staging table
  staging_table <- reactive({
    # Selected relevant cols
    # Coerce to wide format
    # Join region col (set regions and England as themselves for Region)
    wide_table <- filtered_bds() |>
      dplyr::select(
        `LA Number`, `LA and Regions`, Topic,
        Measure, Years, Years_num, values_num, Values
      ) |>
      tidyr::pivot_wider(
        id_cols = c("LA Number", "LA and Regions", "Topic", "Measure"),
        names_from = Years,
        values_from = values_num,
      ) |>
      dplyr::left_join(
        stat_n_geog |>
          dplyr::select(`LA num`, GOReg),
        by = c("LA Number" = "LA num")
      ) |>
      dplyr::mutate(GOReg = dplyr::case_when(
        `LA and Regions` %in% c("England", region_names_bds) ~ `LA and Regions`,
        TRUE ~ GOReg
      ))

    # Order columns (and sort year cols order)
    wide_table_ordered <- wide_table |>
      dplyr::select(
        `LA Number`, `LA and Regions`,
        "Region" = "GOReg",
        Topic, Measure,
        dplyr::all_of(sort_year_columns(wide_table))
      )


    # If SNs included, add SN LA association column
    # Multi-join as want to include an association for every row (even duplicates)
    if (isTRUE(input$la_group == "la_stat_ns")) {
      wide_table_ordered <- wide_table_ordered |>
        dplyr::left_join(
          stat_n_association(),
          by = "LA and Regions",
          relationship = "many-to-many"
        ) |>
        dplyr::relocate(sn_parent, .after = "Measure") |>
        dplyr::rename("Statistical Neighbour Group" = "sn_parent")
    }

    # Staging table formatted and ready for output
    wide_table_ordered
  })

  # Staging table output -------------------------------------------------------
  output$staging_table <- reactable::renderReactable({
    # Display messages if there are incorrect selections
    if (is.null(geog_inputs()) && is.null(input$geog)) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add selections (above).",
          check.names = FALSE
        )
      ))
    } else if (is.null(input$indicator)) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add an indicator selection (above).",
          check.names = FALSE
        )
      ))
    } else if (is.null(geog_inputs())) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "Please add a geography selection (above).",
          check.names = FALSE
        )
      ))
    }

    # Output table - formatting numbers, long text and page settings
    dfe_reactable(
      staging_table(),
      columns = utils::modifyList(
        format_num_reactable_cols(
          staging_table(),
          get_indicator_dps(filtered_bds()),
          num_exclude = c("LA Number", "Measure")
        ),
        list(
          set_custom_default_col_widths(
            Measure = set_min_col_width(90)
          ),
          # Truncates long cell values and displays hover with full value
          Measure = reactable::colDef(
            html = TRUE,
            cell = function(value, index, name) {
              truncate_cell_with_hover(text = value, tooltip = value)
            }
          )
        )
      ),
      defaultPageSize = 3,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(3, 5, 10, 25),
      compact = TRUE
    )
  })

  # Selection (query) table ====================================================
  # Reactive value "query" used to store query data
  # Uses lists to store multiple inputs (Geographies & Indicators)
  query <- reactiveValues(
    data = data.frame(
      Topic = I(list()),
      Indicator = I(list()),
      `LA and Regions` = I(list()),
      `Year range` = I(list()),
      `Click to remove query` = character(),
      `.query_id` = numeric(),
      check.names = FALSE
    ),
    output = data.frame(
      `LA Number` = character(),
      `LA and Regions` = character(),
      Region = character(),
      Topic = character(),
      Measure = character(),
      check.names = FALSE
    )
  )

  # When "Add table" button clicked - add query to saved queries
  observeEvent(input$add_query, {
    # Check if anything selected
    req(length(geog_inputs()) > 0 && nrow(selected_indicators()) > 0)

    # Create a unique identifier for the new query (current no of queries + 1)
    new_q_id <- max(c(0, query$data$.query_id), na.rm = TRUE) + 1

    # Creating year range info
    # Get the range of available years
    available_years <- range(years_choices())

    # Define the year range info logic
    # None selected - all years - "All years (x to y)"
    # Range selected - "x to y"
    # One year selected - "x"
    year_range_display <- dplyr::case_when(
      length(input$year_range) == 0 ~ paste0(
        "All years (", available_years[1], " to ", available_years[2], ")"
      ),
      length(input$year_range) == 2 ~ paste(
        input$year_range[1], "to", input$year_range[2]
      ),
      length(input$year_range) == 1 ~ paste0("", input$year_range[1])
    )

    # Create query information
    # Split multiple input choices with commas and line breaks
    # (indicator x, indicator y)
    # Assign the new query ID, selected topic-indicator pairs,
    # create the geog selections (special formatting  for groupings),
    # year range (with logic from above) and the remove col
    new_query <- data.frame(
      .query_id = new_q_id,
      Topic = I(list(selected_indicators()$Topic)),
      Indicator = paste(selected_indicators()$Measure, collapse = ",<br>"),
      `LA and Regions` = paste(
        get_geog_selection(input, la_names_bds, region_names_bds, stat_n_geog),
        collapse = ",<br>"
      ),
      `Year range` = year_range_display,
      `Click to remove query` = "Remove",
      check.names = FALSE
    )

    # Append new query to the existing queries
    query$data <- query$data |>
      rbind(new_query)

    # Appending the data of the new query to the output table
    # Adding new query ID to staging data
    # (so remove button also removes relevant data from output table)
    staging_to_append <- staging_table()
    staging_to_append$.query_id <- new_q_id

    # Comparing staging table and output table year cols
    # This checks if suffixes are consistent across the 2 dfs
    # (Pulls the suffixes from both)
    consistent_staging_final_yrs <- data.frame(
      Years = c(
        colnames(query$output)[grepl("^\\d{4}", colnames(query$output))],
        colnames(staging_to_append)[grepl("^\\d{4}", colnames(staging_to_append))]
      )
    ) |>
      check_year_suffix_consistency()

    # If not consistent suffixes then clean both dfs year cols
    # Otherwise add the suffix years
    if (!consistent_staging_final_yrs && nrow(query$output) > 0) {
      query$output <- query$output |>
        rename_columns_with_year() |>
        dplyr::bind_rows(rename_columns_with_year(staging_to_append))
    } else {
      query$output <- query$output |>
        dplyr::bind_rows(staging_to_append)
    }
  })

  # Query table output ---------------------------------------------------------
  output$query_table <- reactable::renderReactable({
    req(nrow(query$data))

    # Display messages if there are no saved selections
    if (nrow(query$data) == 0) {
      return(reactable::reactable(
        data.frame(
          `Message from tool` = "No saved selections.",
          check.names = FALSE
        )
      ))
    }

    # Output table - Allow html (for <br>),
    # add the JS from reactable.extras::button_extra() for remove button
    # Show only unique topics and remove the query ID col
    dfe_reactable(
      query$data,
      columns = list(
        Indicator = html_col_def(),
        `LA and Regions` = html_col_def(),
        `Click to remove query` = reactable::colDef(
          cell = reactable::JS(
            "function(cellInfo) {
            const buttonId = 'remove_' + cellInfo.row['.query_id'];
            return React.createElement(ButtonExtras, {
              id: buttonId,
              label: 'Remove',
              uuid: cellInfo.row['.query_id'],
              column: cellInfo.column.id,
              class: 'govuk-button--warning',
              className: 'govuk-button--warning'
            }, cellInfo.index);
          }"
          )
        ),
        # Customise the other columns (e.g., Topic)
        Topic = reactable::colDef(
          cell = function(value) {
            unique_values <- unique(unlist(value))
            if (length(unique_values) > 0) {
              return(paste(unique_values, collapse = ",<br>"))
            } else {
              return("")
            }
          },
          html = TRUE
        ),
        .query_id = reactable::colDef(show = FALSE)
      ),
      defaultPageSize = 5,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 25),
      compact = TRUE
    )
  })

  # Remove query button
  observe({
    req(nrow(query$data))

    # Create button observers for each row using the query ID
    lapply(query$data$.query_id, function(q_id) {
      # Create matching query ID for each remove button
      remove_button_id <- paste0("remove_", q_id)

      # Observe the button click
      observeEvent(input[[remove_button_id]],
        {
          # Remove the corresponding row (query) from query$data using the query ID
          query$data <- query$data[query$data$.query_id != q_id, , drop = FALSE]

          # Also remove the corresponding rows from query$output
          query$output <- query$output[query$output$.query_id != q_id, , drop = FALSE]

          # If no rows (queries) left then also remove the years cols
          # This is so that if a user wants a range of years next
          # the legacy years aren't still there
          if (nrow(query$output) == 0) {
            query$output <- query$output |>
              dplyr::select(`LA Number`, `LA and Regions`, Region, Topic, Measure)
          }
        },
        ignoreInit = TRUE
      )
    })
  })

  # Output data table ==========================================================
  # Cleaned final table
  clean_final_table <- reactive({
    req(query$data)

    # Check if there are any saved queries
    if (nrow(query$data) == 0) {
      return(
        data.frame(
          `Message from tool` = "No saved selections.",
          check.names = FALSE
        )
      )
    }

    # Logic to reset the year cols to have year suffixes if they match
    # (As they may have been cleaned from the code logic at end of the new query chunk)
    # Get output indicators
    output_indicators <- query$output |>
      pull_uniques("Measure")

    # Boolean for if the output indicators share suffixes
    share_year_suffix <- bds_metrics |>
      dplyr::filter(
        Measure %in% output_indicators,
        !is.na(Years)
      ) |>
      check_year_suffix_consistency()

    # If suffixes are shared then reapply the years with suffix to col names
    if (share_year_suffix) {
      # Get the years with suffixes
      years_dict <- bds_metrics |>
        dplyr::filter(
          Measure %in% output_indicators,
          !is.na(Years)
        ) |>
        dplyr::distinct(Years, Years_num)

      # Replace the matching year col names with respective year suffix
      new_col_names <- colnames(query$output) |>
        (\(cols) {
          ifelse(
            cols %in% years_dict$Years_num,
            years_dict$Years[match(cols, years_dict$Years_num)],
            cols
          )
        })()

      # Apply the new year suffix names to query$output
      colnames(query$output) <- new_col_names
    }

    # Final query output table with ordered columns, (SN parent if selected) and
    # Sorted year columns
    query$output |>
      dplyr::select(
        `LA Number`, `LA and Regions`,
        Region, Topic, Measure,
        tidyselect::any_of("Statistical Neighbour Group"),
        dplyr::all_of(sort_year_columns(query$output))
      )
  })

  # Filtering BDS for all topic-indicator pairs in the final output table
  # (The filtered_bds only has the staging topic-indicator pairs)
  final_filtered_bds <- reactive({
    output_table_filters <- clean_final_table() |>
      dplyr::distinct(`LA and Regions`, Topic, Measure)

    bds_metrics |>
      dplyr::semi_join(
        output_table_filters,
        by = c("LA and Regions", "Topic", "Measure")
      )
  })

  # Download the output table --------------------------------------------------
  Download_DataServer(
    "table_download",
    reactive(input$file_type),
    reactive(clean_final_table()),
    reactive("LAIT-create-your-own-table")
  )

  # Final output table (based on saved queries) --------------------------------
  output$output_table <- reactable::renderReactable({
    # Display the final query table data
    # Format numeric cols (using dps based of output table indicators),
    # Truncate measure with hover and page settings
    dfe_reactable(
      clean_final_table(),
      columns = utils::modifyList(
        format_num_reactable_cols(
          clean_final_table(),
          get_indicator_dps(final_filtered_bds()),
          num_exclude = c("LA Number", "Measure")
        ),
        list(
          set_custom_default_col_widths(),
          Measure = reactable::colDef(
            html = TRUE,
            cell = function(value, index, name) {
              truncate_cell_with_hover(text = value, tooltip = value)
            }
          )
        )
      ),
      defaultPageSize = 5,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 25),
      compact = TRUE
    )
  })

  # Charts =====================================================================
  # Compute number of indicators & geographies - used to determine whether data
  # is displayed or computed
  # Allowing a max of 3 and 4 respectively
  number_of_indicators <- reactive({
    length(pull_uniques(clean_final_table(), "Measure"))
  })

  number_of_geogs <- reactive({
    length(pull_uniques(clean_final_table(), "LA and Regions"))
  })

  # Create chart data ----------------------------------------------------------
  chart_plotting_data <- reactive({
    req(
      "Message from tool" %notin% colnames(clean_final_table()),
      number_of_indicators() <= 3,
      number_of_geogs() <= 4
    )

    # Pull order that geogs and indicators are added
    # This is used to set the levels of the factor (so display in order in chart)
    geog_chart_order <- query$data |>
      get_query_table_values(`LA and Regions`)

    indicator_chart_order <- query$data |>
      get_query_table_values(Indicator)

    # Coerce final output table to long data (for plotting)
    # Recreate Years_num & values_num, also factor `LA and Regions` & Measure
    clean_final_table() |>
      dplyr::distinct() |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("20"),
        names_to = "Years",
        values_to = "Values"
      ) |>
      dplyr::mutate(
        Years_num = as.numeric(substr(Years, start = 1, stop = 4)),
        values_num = Values,
        `LA and Regions` = factor(`LA and Regions`, levels = geog_chart_order),
        Measure = factor(Measure, levels = indicator_chart_order)
      )
  })

  # Line chart -----------------------------------------------------------------
  # Build static main plot
  line_chart <- reactive({
    req(
      "Message from tool" %notin% colnames(clean_final_table()),
      number_of_indicators() <= 3,
      number_of_geogs() <= 4
    )

    # Count year cols - used to determine if to show geom_point
    # (If only one year then no line will show so point needed)
    num_year_cols <- chart_plotting_data() |>
      dplyr::distinct(Years) |>
      nrow()

    # Plot data - colour represents Geographies & linetype represents Indicator
    chart_plotting_data() |>
      ggplot2::ggplot() +
      ggiraph::geom_line_interactive(
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          color = `LA and Regions`,
          linetype = Measure,
          data_id = `LA and Regions`
        ),
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          color = `LA and Regions`
        ),
        na.rm = TRUE,
        # Show points if only one year data selected
        size = ifelse(num_year_cols == 1, 3, 0),
        shape = 16
      ) +
      format_axes(chart_plotting_data()) +
      set_plot_colours(chart_plotting_data()) +
      set_plot_labs(final_filtered_bds()) +
      custom_theme() +
      # Setting legend title at top
      ggplot2::theme(
        legend.title = ggplot2::element_text(),
        legend.title.position = "top",
        legend.spacing.x = unit(5, "lines")
      ) +
      # Creating nice looking legend content
      ggplot2::guides(
        color = ggplot2::guide_legend(
          order = 1,
          ncol = 1,
          title = "Geographies (colour):",
          override.aes = list(size = 3, shape = 15, linetype = NULL)
        ),
        linetype = ggplot2::guide_legend(
          order = 2,
          ncol = 1,
          title = "Indicators (line-type):"
        )
      )
  })

  # Build interactive line chart
  interactive_line_chart <- reactive({
    req(
      "Message from tool" %notin% colnames(clean_final_table()),
      number_of_indicators() <= 3,
      number_of_geogs() <= 4
    )
    # Creating vertical geoms to make vertical hover tooltip
    vertical_hover <- lapply(
      get_years(chart_plotting_data()),
      tooltip_vlines,
      chart_plotting_data(),
      get_indicator_dps(final_filtered_bds()),
      TRUE
    )

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = (line_chart() + vertical_hover),
      width_svg = 8.5,
      options = generic_ggiraph_options(
        opts_hover(
          css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
        )
      ),
      fonts = list(sans = "Arial")
    )
  })

  # Line chart plot output -----------------------------------------------------
  output$line_chart <- ggiraph::renderGirafe({
    # Error messages for missing selections
    if ("Message from tool" %in% colnames(clean_final_table())) {
      ggiraph::girafe(
        ggobj = display_no_data_plot("No plot as not enough selections made"),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )

      # Error messages for too many selections
    } else if (
      number_of_geogs() > 4
    ) {
      ggiraph::girafe(
        ggobj = display_no_data_plot(label = "No plot as too many Geographies selected"),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    } else if (
      number_of_indicators() > 3
    ) {
      ggiraph::girafe(
        ggobj = display_no_data_plot(label = "No plot as too many Indicators selected"),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )

      # Plot line chart
    } else {
      interactive_line_chart()
    }
  })

  # Line chart download --------------------------------------------------------
  # Initialise server logic for download button and modal
  DownloadChartBtnServer("download_btn_line", "line", "Line")

  # Set up the download handlers for the chart
  Download_DataServer(
    "line-chart_download",
    reactive(input$`line-file_type`),
    reactive(list("svg" = line_chart(), "html" = interactive_line_chart())),
    reactive(c("LAIT-create-your-own-line-chart"))
  )

  # Plot used for copy to clipboard (hidden)
  output$copy_plot_line <- shiny::renderPlot(
    {
      line_chart()
    },
    res = 200,
    width = 24 * 96,
    height = 12 * 96
  )


  # Bar chart ------------------------------------------------------------------
  # Build main bar static plot
  bar_chart <- reactive({
    req(
      "Message from tool" %notin% colnames(clean_final_table()),
      number_of_indicators() <= 3,
      number_of_geogs() <= 4
    )

    # Giving facet_wrap charts the correct chart names
    # Get chart names for each indicator
    chart_names <- final_filtered_bds() |>
      dplyr::distinct(Measure, Chart_title)

    # Wrap the chart names (dependent on number of indicators -
    # more narrows width available)
    chart_names_wrapped <- chart_names |>
      dplyr::mutate(Chart_title = stringr::str_wrap(
        Chart_title,
        width = 60 - length(chart_names$Measure) * 10
      ))

    # Create a named vector for custom titles for each indicator
    custom_titles <- setNames(
      chart_names_wrapped$Chart_title,
      chart_names_wrapped$Measure
    )


    # Plot chart - split by indicators, colours represent Geographies
    chart_plotting_data() |>
      ggplot2::ggplot() +
      ggiraph::geom_col_interactive(
        ggplot2::aes(
          x = Years_num,
          y = values_num,
          fill = `LA and Regions`,
          tooltip = glue::glue_data(
            chart_plotting_data() |>
              pretty_num_table(
                include_columns = "values_num",
                dp = get_indicator_dps(final_filtered_bds())
              ),
            "Measure: {Measure}\nYear: {Years}\n\n{`LA and Regions`}: {values_num}"
          )
        ),
        position = position_dodge(width = 0.6),
        width = 0.6,
        na.rm = TRUE,
        color = "black"
      ) +
      format_axes(chart_plotting_data()) +
      set_plot_colours(chart_plotting_data(), "fill") +
      set_plot_labs(final_filtered_bds()) +
      custom_theme() +
      ggplot2::theme(
        legend.title = ggplot2::element_text(),
        legend.title.position = "top"
      ) +
      guides(
        fill = ggplot2::guide_legend(ncol = 2, title = "Geographies:")
      ) +
      ggplot2::labs(title = "Bar charts showing selected indicators") +
      ggplot2::facet_wrap(
        ~Measure,
        labeller = labeller(Measure = as_labeller(custom_titles)),
      ) +
      # Gives space between the charts so x-axis labels don't overlap
      theme(
        panel.spacing.x = unit(15, "mm"),
        plot.margin = ggplot2::margin(r = 30)
      )
  })

  # Build interactive line chart
  interactive_bar_chart <- reactive({
    req(
      "Message from tool" %notin% colnames(clean_final_table()),
      number_of_indicators() <= 3,
      number_of_geogs() <= 4
    )

    # Plotting interactive graph
    ggiraph::girafe(
      ggobj = (bar_chart()),
      width_svg = 8.5,
      options = generic_ggiraph_options(),
      fonts = list(sans = "Arial")
    )
  })

  # Bar chart plot output ------------------------------------------------------
  output$bar_chart <- ggiraph::renderGirafe({
    # Error messages for missing or too many selections
    if ("Message from tool" %in% colnames(clean_final_table())) {
      ggiraph::girafe(
        ggobj = display_no_data_plot("No plot as not enough selections made"),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    } else if (
      number_of_geogs() > 4
    ) {
      ggiraph::girafe(
        ggobj = display_no_data_plot(label = "No plot as too many Geographies selected"),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )
    } else if (
      number_of_indicators() > 3
    ) {
      ggiraph::girafe(
        ggobj = display_no_data_plot(label = "No plot as too many Indicators selected"),
        width_svg = 8.5,
        options = generic_ggiraph_options(
          opts_hover(
            css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"
          )
        ),
        fonts = list(sans = "Arial")
      )

      # Plot chart
    } else {
      interactive_bar_chart()
    }
  })

  # Bar chart download ---------------------------------------------------------
  # Initialise server logic for download button and modal
  DownloadChartBtnServer("download_btn_bar", "bar", "Bar")

  # Set up the download handlers for the chart
  Download_DataServer(
    "bar-chart_download",
    reactive(input$`bar-file_type`),
    reactive(list("svg" = bar_chart(), "html" = interactive_bar_chart())),
    reactive(c("LAIT-create-your-own-bar-chart"))
  )

  # Plot used for copy to clipboard (hidden)
  output$copy_plot_bar <- shiny::renderPlot(
    {
      bar_chart()
    },
    res = 200,
    width = 24 * 96,
    height = 12 * 96
  )
}

shinyApp(ui, server)
