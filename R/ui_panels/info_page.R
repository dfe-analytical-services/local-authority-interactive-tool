info_page_panel <- function() {
  shiny::tabPanel(
    "Information Page",
    shinyGovstyle::gov_main_layout(
      shinyGovstyle::gov_row(
        shiny::column(
          12,
          h1("Information Page"),

          # Latest Updates =====================================================
          h2("Latest Updates"),
          p(
            "Please note that 'latest' updates may not always be time recent,
              so be sure to check the update dates."
          ),
          # Data updates -------------------------------------------------------
          LatestDataUpdateUI("latest_indicator_update"),
          br(),
          # Development updates ------------------------------------------------
          LatestDevUpdateUI("latest_dev_update"),
          br(),
          br(),

          # Indicators metadata ===============================================
          h2("Indicator Information"),
          p(
            "While this information is available in the metadata section of
              each indicator, it is consolidated here for convenience."
          ),
          IndicatorInfoTableUI("indicator_info_table"),
          br(),
          br(),


          # Guidance sources ===================================================
          h2("Links to useful or related resources (opens in new tab)"),
          UsefulLinksUI("useful_links")
        )
      )
    )
  )
}


# Display Indicator Information table
IndicatorInfoTableUI <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("indicator_info_table"))
}


# Compute Indicator Information table
IndicatorInfoTableServer <- function(id, metrics_data) {
  moduleServer(id, function(input, output, session) {
    output$indicator_info_table <- reactable::renderReactable({
      # Select columns to show indicator information
      indicator_info <- metrics_data |>
        dplyr::select(
          Topic,
          Measure,
          `Data Owner (DO) /Supplier and Contact Details`,
          `Last Update`,
          `Next Update`,
          `Hyperlink(s)`
        ) |>
        # Convert to nice looking links
        dplyr::rowwise() |>
        dplyr::mutate(`Hyperlink(s) (opens in new tab)` = as.character(
          dfeshiny::external_link(
            href = `Hyperlink(s)`,
            link_text = Measure,
            add_warning = FALSE
          )
        )) |>
        dplyr::ungroup() |>
        order_alphabetically(Measure)

      # Output table
      dfe_reactable(
        indicator_info,
        columns = list(`Hyperlink(s)` = reactable::colDef(show = FALSE)),
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 25),
        compact = TRUE,
        searchable = TRUE
      )
    })
  })
}



LatestDataUpdateUI <- function(id) {
  ns <- NS(id)

  shinyGovstyle::noti_banner(
    inputId = ns("latest_update_indicator"),
    title_txt = "Latest Updated Indicator(s)",
    body_txt = as.character(
      shiny::tagList(
        shiny::p("These indicators were most recently updated:"),
        # Adding reactableOutput for the table
        reactable::reactableOutput(ns("latest_update_table"))
      )
    )
  )
}


LatestDataUpdateServer <- function(id, metrics_data) {
  moduleServer(id, function(input, output, session) {
    # Prepare the data
    latest_updated_indicator <- metrics_data |>
      dplyr::mutate(latest_update_date = as.Date(paste(`Last Update`, "01"),
        format = "%B %Y %d"
      )) |>
      dplyr::filter(latest_update_date == max(latest_update_date)) |>
      dplyr::select(Indicator = Measure, `Last Update`) |>
      order_alphabetically(Indicator)

    # Render the reactable table with scrollable rows
    output$latest_update_table <- reactable::renderReactable({
      dfe_reactable(
        latest_updated_indicator,
        pagination = FALSE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE,
        height = "220px",
        searchable = TRUE
      )
    })
  })
}



LatestDevUpdateUI <- function(id) {
  ns <- NS(id)

  # Use bslib::card() for a clean and modern collapsible card structure
  bslib::card(
    class = "dev-update-card",
    style = "
      border: 1px solid #ccc;
      border-radius: 12px;
      padding: 20px;
      margin-bottom: 20px;
      background-color: #f9f9f9;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    ",
    # Card header with title, spinning gear icon, and collapse toggle
    bslib::card_header(
      shiny::tags$div(
        style = "
          display: flex;
          align-items: center;
          justify-content: space-between;
        ",
        # Title text
        shiny::tags$div(
          style = "display: flex; align-items: center;",
          shiny::tags$h3(
            "Latest Development Updates",
            style = "
              margin: 0;
              color: #1d70b8;
              font-weight: bold;
            "
          ),
          # Spinning gear icon
          shiny::tags$div(
            style = "
              width: 40px;
              height: 40px;
              border-radius: 50%;
              display: flex;
              align-items: center;
              justify-content: center;
              margin-left: 1rem;
            ",
            shiny::tags$i(
              class = "fas fa-gear", # Font Awesome icon
              style = "
                color: #1d70b8;
                font-size: 20px;
                animation: rotateIcon 2s infinite linear;
              "
            )
          )
        ),
        # Collapse toggle button
        shiny::tags$button(
          class = "btn btn-link",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("collapseBody")),
          `aria-expanded` = "true",
          `aria-controls` = ns("collapseBody"),
          style = "font-size: 20px; color: #1d70b8;",
          shiny::tags$i(class = "fas fa-chevron-down")
        )
      )
    ),
    # Card body with collapsible content
    shiny::tags$div(
      id = ns("collapseBody"),
      class = "collapse show", # Default to expanded
      shiny::tags$div(
        class = "card-body",
        # Animated text for description
        shiny::tags$p(
          "Below are the most recent development updates related to the tool:"
        ),
        # Latest development details
        shiny::tags$div(
          style = "margin-bottom: 10px;",
          shiny::uiOutput(ns("latest_update_table"))
        )
      )
    ),
    # Card footer with external link to GitHub
    bslib::card_footer(
      style = "border: none;",
      shiny::HTML(paste0(
        "For more information, please visit the ",
        dfeshiny::external_link(
          href = "https://github.com/dfe-analytical-services/local-authority-interactive-tool",
          link_text = "LAIT GitHub",
          add_warning = TRUE
        ),
        "."
      ))
    ),
    # Add the keyframe animation for spinning
    shiny::tags$style(
      shiny::HTML("
        @keyframes rotateIcon {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        .btn-link {
          text-decoration: none;
        }
      ")
    )
  )
}


LatestDevUpdateServer <- function(id, dev_update_log) {
  moduleServer(id, function(input, output, session) {
    # Extract the most recent development update
    latest_dev_update <- dev_update_log |>
      dplyr::filter(Date == max(Date)) |>
      dplyr::slice_min(1)

    # Render the update content inside a styled card
    output$latest_update_table <- shiny::renderUI({
      htmltools::tags$div(
        style = "line-height: 1.6;",
        htmltools::tags$p(
          htmltools::tags$b("Type:"),
          paste(latest_dev_update$Type)
        ),
        htmltools::tags$p(
          htmltools::tags$b("Summary:"),
          paste(latest_dev_update$Summary)
        ),
        htmltools::tags$p(
          htmltools::tags$b("Details:"),
          shiny::br(),
          paste(latest_dev_update$Details)
        ),
        htmltools::tags$p(
          htmltools::tags$b("Date Updated:"),
          paste(latest_dev_update$Date)
        )
      )
    })
  })
}


UsefulLinksUI <- function(id) {
  ns <- NS(id)

  # UI container for useful links
  shiny::uiOutput(ns("useful_links_lst"))
}

UsefulLinksServer <- function(id, useful_links) {
  moduleServer(id, function(input, output, session) {
    # Prepare the data for display
    useful_links_formatted <- useful_links |>
      dplyr::rowwise() |>
      dplyr::mutate(nice_useful_link = as.character(
        dfeshiny::external_link(
          href = Link,
          link_text = Tool_Name,
          add_warning = FALSE
        )
      )) |>
      dplyr::ungroup()

    # Render the UI
    output$useful_links_lst <- shiny::renderUI({
      # Create a styled container for the links
      htmltools::tags$div(
        style = "
          line-height: 1.6;
          max-width: 800px;
          width: 100%;
          min-width: 400px;
          background-color: #f9f9f9;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 0 20px 20px 20px;
          margin-bottom: 20px;
          box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
        ",
        # Group by 'Type' and render each group in a single card
        purrr::map(unique(useful_links_formatted$Type), function(type) {
          # Subset links of the same type
          links_by_type <- useful_links_formatted |> dplyr::filter(Type == type)

          # Wrap the entire group in a card
          htmltools::tags$div(
            # Type Header
            htmltools::tags$h3(
              type,
              style = "margin-bottom: 15px; padding-top: 15px;"
            ),
            # List the links for the type
            htmltools::tags$div(
              purrr::map(1:nrow(links_by_type), function(i) {
                htmltools::tags$div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  # Left: The link
                  htmltools::tags$div(
                    style = "flex: 2;",
                    shiny::HTML(links_by_type$nice_useful_link[i])
                  ),
                  # Right: The owner
                  htmltools::tags$div(
                    style = "
                      flex: 1;
                      text-align: left;
                    ",
                    links_by_type$Owner[i]
                  )
                )
              })
            )
          )
        })
      )
    })
  })
}
