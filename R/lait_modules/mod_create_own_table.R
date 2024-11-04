# nolint start: object_name
#
# Staging table ================================================================
# Staging BDS ------------------------------------------------------------------
# Filter the BDS for current user input selections
# (used to create the staging table)
StagingBDSServer <- function(id,
                             create_inputs,
                             geog_groups,
                             year_input,
                             bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Filter BDS for topic-indicator pairs in the selected_values reactive
    topic_indicator_bds <- reactive({
      req(nrow(create_inputs$selected_indicators()) > 0)
      bds_metrics |>
        dplyr::semi_join(
          create_inputs$selected_indicators(),
          by = c(
            "Topic" = "Topic",
            "Measure" = "Measure"
          )
        )
    })

    # Now filter BDS for geographies and year range
    # Split from above so if indicator doesn't change then don't recompute
    staging_bds <- reactive({
      req(geog_groups(), topic_indicator_bds())
      # Filter by full geography inputs
      filtered_bds <- topic_indicator_bds() |>
        dplyr::filter(
          `LA and Regions` %in% geog_groups(),
          !is.na(Years)
        )

      # Cleaning Years
      # Check if all years have consistent suffix
      consistent_str_years <- check_year_suffix_consistency(filtered_bds)

      # If not consistent suffix use the cleaned year cols (numeric years)
      if (!consistent_str_years) {
        filtered_bds <- filtered_bds |>
          dplyr::mutate(
            Years = Years_num
          )
      }

      # Apply the year range filter
      # If only one year selected then show just that year
      if (length(year_input$range()) == 1) {
        filtered_bds <- filtered_bds |>
          dplyr::filter(
            Years == year_input$range()[1]
          )
      } else if (length(year_input$range()) == 2) {
        filtered_bds <- filtered_bds |>
          dplyr::filter(
            Years >= year_input$range()[1],
            Years <= year_input$range()[2]
          )
      }

      # Return the user selection filtered data for staging table
      filtered_bds
    })


    # Return staging BDS
    staging_bds
  })
}


# Staging data -----------------------------------------------------------------
StagingDataServer <- function(
    id, create_inputs, staging_bds, region_names_bds, la_names_bds, stat_n_la) {
  moduleServer(id, function(input, output, session) {
    # Make statistical neighbour association table available
    stat_n_association <- StatN_AssociationServer(
      "stat_n_association",
      create_inputs,
      la_names_bds,
      stat_n_la
    )

    # Build the staging table
    staging_table <- reactive({
      # Selected relevant cols
      # Coerce to wide format
      # Set regions and England as themselves for Region
      wide_table <- staging_bds() |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Region, Topic,
          Measure, Years, Years_num, values_num, Values
        ) |>
        tidyr::pivot_wider(
          id_cols = c("LA Number", "LA and Regions", "Region", "Topic", "Measure"),
          names_from = Years,
          values_from = values_num,
        ) |>
        dplyr::mutate(Region = dplyr::case_when(
          `LA and Regions` %in% c("England", region_names_bds) ~ `LA and Regions`,
          TRUE ~ Region
        ))

      # Order columns (and sort year cols order)
      wide_table_ordered <- wide_table |>
        dplyr::select(
          `LA Number`, `LA and Regions`, Region,
          Topic, Measure,
          dplyr::all_of(sort_year_columns(wide_table))
        )


      # If SNs included, add SN LA association column
      # Multi-join as want to include an association for every row (even duplicates)
      if (isTRUE(create_inputs$la_group() == "la_stat_ns")) {
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

    # Return staging table
    staging_table
  })
}


# Staging table UI -------------------------------------------------------------
# Simple reactable table inside a well div
StagingTableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Staging Table (View of current selections)"),
    bslib::card(
      reactable::reactableOutput(ns("staging_table"))
    )
  )
}

# Staging table Server ---------------------------------------------------------
# Output a formatted reactable table of the staging data
# Few error message table outputs for incorrect/ missing selections
StagingTableServer <- function(id,
                               create_inputs,
                               region_names_bds,
                               la_names_bds,
                               stat_n_la,
                               geog_groups,
                               year_input,
                               bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Staging table reactable ouput
    output$staging_table <- reactable::renderReactable({
      # Display messages if there are incorrect selections
      if (length(create_inputs$indicator()) == 0 && is.null(create_inputs$geog())) {
        return(reactable::reactable(
          data.frame(
            `Message from tool` = "Please add selections (above).",
            check.names = FALSE
          )
        ))
      } else if (length(create_inputs$indicator()) == 0) {
        return(reactable::reactable(
          data.frame(
            `Message from tool` = "Please add an indicator selection (above).",
            check.names = FALSE
          )
        ))
      } else if (is.null(create_inputs$geog())) {
        return(reactable::reactable(
          data.frame(
            `Message from tool` = "Please add a geography selection (above).",
            check.names = FALSE
          )
        ))
      }

      # Filtering BDS for staging data
      staging_bds <- StagingBDSServer(
        "staging_bds",
        create_inputs,
        geog_groups,
        year_input,
        bds_metrics
      )

      # Build staging data
      staging_data <- StagingDataServer(
        "staging_data",
        create_inputs,
        staging_bds,
        region_names_bds,
        la_names_bds,
        stat_n_la
      )

      # Output table - formatting numbers, long text and page settings
      dfe_reactable(
        staging_data(),
        columns = utils::modifyList(
          format_num_reactable_cols(
            staging_data(),
            get_indicator_dps(staging_bds()),
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
  })
}


# Query table ==================================================================
# Query data -------------------------------------------------------------------
QueryDataServer <- function(id,
                            create_inputs,
                            geog_groups,
                            year_input,
                            staging_data) {
  moduleServer(id, function(input, output, session) {
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
    observeEvent(create_inputs$add_query(),
      {
        # Check if anything selected
        req(length(geog_groups()) > 0 && nrow(create_inputs$selected_indicators()) > 0)

        # Create a unique identifier for the new query (current no of queries + 1)
        new_q_id <- max(c(0, query$data$.query_id), na.rm = TRUE) + 1

        # Creating year range info
        # Get the range of available years
        available_years <- range(year_input$choices())

        # Define the year range info logic
        # None selected - all years - "All years (x to y)"
        # Range selected - "x to y"
        # One year selected - "x"
        year_range_display <- dplyr::case_when(
          length(year_input$range()) == 0 ~ paste0("All years (", available_years[1], " to ", available_years[2], ")"),
          length(year_input$range()) == 2 ~ paste(year_input$range()[1], "to", year_input$range()[2]),
          length(year_input$range()) == 1 ~ paste0("", year_input$range()[1])
        )

        # Evaluate user inputs for get_geog_selection()
        evaluated_inputs <- list(
          geog = create_inputs$geog(),
          la_group = create_inputs$la_group(),
          inc_regions = create_inputs$inc_regions(),
          inc_england = create_inputs$inc_england()
        )

        # Create query information
        # Split multiple input choices with commas and line breaks
        # (indicator x, indicator y)
        # Assign the new query ID, selected topic-indicator pairs,
        # create the geog selections (special formatting  for groupings),
        # year range (with logic from above) and the remove col
        new_query <- data.frame(
          .query_id = new_q_id,
          Topic = I(list(create_inputs$selected_indicators()$Topic)),
          Indicator = paste(create_inputs$selected_indicators()$Measure, collapse = ",<br>"),
          `LA and Regions` = paste(
            get_geog_selection(evaluated_inputs, la_names_bds, region_names_bds, stat_n_geog),
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
        staging_to_append <- staging_data()
        staging_to_append$.query_id <- new_q_id
        consistent_staging_final_yrs <- data.frame(
          Years = c(
            colnames(query$output)[grepl("^\\d{4}", colnames(query$output))],
            colnames(staging_to_append)[grepl("^\\d{4}", colnames(staging_to_append))]
          )
        ) |> check_year_suffix_consistency()

        # If not consistent suffixes then clean both dfs year cols
        # Otherwise add the suffix years
        if (!consistent_staging_final_yrs && nrow(query$output) > 0) {
          query$output <- query$output |>
            rename_columns_with_year() |>
            dplyr::bind_rows(rename_columns_with_year(staging_to_append))
        } else {
          query$output <- query$output |> dplyr::bind_rows(staging_to_append)
        }
      },
      ignoreInit = TRUE
    )

    query
  })
}


# Query Table UI ---------------------------------------------------------------
QueryTableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Summary of Selections"),
    bslib::card(
      reactable::reactableOutput(ns("query_table"))
    )
  )
}

# Query Table Server -----------------------------------------------------------
# Renders the query table and manages removal actions
QueryTableServer <- function(id, query) {
  moduleServer(id, function(input, output, session) {
    # Display message if there are no saved selections
    output$query_table <- reactable::renderReactable({
      req(nrow(query$data))
      if (nrow(query$data) == 0) {
        return(reactable::reactable(
          data.frame(`Message from tool` = "No saved selections.", check.names = FALSE)
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
                const buttonId = 'query_table-remove-' + cellInfo.row['.query_id'];
                console.log('Generated button ID:', buttonId);  // Confirm buttonId in console
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

    # Remove query button logic
    observe({
      req(nrow(query$data))

      # Create button observers for each row using the query ID
      lapply(query$data$.query_id, function(q_id) {
        # Create matching query ID for each remove button
        remove_button_id <- paste0("remove-", q_id)

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

    # Output updated query (which is up-to-date with any removed rows)
    query
  })
}


# Create Own Table =============================================================
# Create Own Data --------------------------------------------------------------
CreateOwnDataServer <- function(id, query, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Building data for the output of all saved queries
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
          (\(cols) ifelse(cols %in% years_dict$Years_num,
            years_dict$Years[match(cols, years_dict$Years_num)],
            cols
          ))()

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

    # Return data ready to render as output of Create Own Table
    clean_final_table
  })
}


# Create Own BDS ---------------------------------------------------------------
CreateOwnBDSServer <- function(id, create_own_table, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Filtering BDS for all topic-indicator pairs in the final output table
    # (The filtered_bds only has the staging topic-indicator pairs)
    final_filtered_bds <- reactive({
      output_table_filters <- create_own_table() |>
        dplyr::distinct(`LA and Regions`, Topic, Measure)

      bds_metrics |>
        dplyr::semi_join(
          output_table_filters,
          by = c("LA and Regions", "Topic", "Measure")
        )
    })

    final_filtered_bds
  })
}


# Create Own Table UI ----------------------------------------------------------
CreateOwnTableUI <- function(id) {
  ns <- NS(id)

  div(
    class = "well",
    style = "overflow-y: visible;",
    h3("Output Table (View of all saved selections)"),
    bslib::navset_card_tab(
      # Create Own Table -------------------------------------------------------
      bslib::nav_panel(
        title = "Output Table",
        reactable::reactableOutput(ns("output_table"))
      ),
      # Create Own Download ----------------------------------------------------
      bslib::nav_panel(
        title = "Download",
        file_type_input_btn(ns("file_type")),
        Download_DataUI(ns("table_download"), "Output Table")
      )
    )
  )
}

# Create Own Table Server ------------------------------------------------------
CreateOwnTableServer <- function(id, query, bds_metrics) {
  moduleServer(id, function(input, output, session) {
    # Load data for Create Own Table
    create_own_data <- CreateOwnDataServer(
      "create_own_table",
      query,
      bds_metrics
    )

    # Load BDS made from Create Own data
    create_own_bds <- CreateOwnBDSServer(
      "create_own_bds",
      create_own_data,
      bds_metrics
    )

    # Final output table (based on saved queries) ------------------------------
    output$output_table <- reactable::renderReactable({
      # Display the final query table data
      # Format numeric cols (using dps based of output table indicators),
      # Truncate measure with hover and page settings
      dfe_reactable(
        create_own_data(),
        columns = utils::modifyList(
          format_num_reactable_cols(
            create_own_data(),
            get_indicator_dps(create_own_bds()),
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

    # Download the output table ------------------------------------------------
    Download_DataServer(
      "table_download",
      reactive(input$file_type),
      reactive(create_own_data()),
      reactive("LAIT-create-your-own-table")
    )
  })
}

# nolint end
