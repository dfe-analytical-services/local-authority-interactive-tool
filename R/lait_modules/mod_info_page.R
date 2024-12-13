# nolint start: object_name
# Modules to produce the information page
#
#' Latest Data Update UI
#'
#' Creates a UI card displaying the latest updated indicators in a notification banner style.
#'
#' @param id A string specifying the module's unique ID.
#'
#' @return A UI element that contains a card styled as a notification banner,
#'         including a heading and a reactable table with a loading spinner.
#'
#' @details
#' - The UI includes a `reactable` table to list the most recently updated indicators.
#' - Styled using GOV.UK notification banner classes and additional CSS for aesthetics.
#'
#' @examples
#' library(shiny)
#' ui <- fluidPage(
#'   LatestDataUpdateUI("latest_update")
#' )
#'
#' server <- function(input, output, session) {
#'   metrics_data <- data.frame(
#'     Measure = c("Budget", "Attendance"),
#'     `Last Update` = c("March 2023", "April 2023")
#'   )
#'
#'   LatestDataUpdateServer("latest_update", metrics_data)
#' }
#' shinyApp(ui, server)
#'
LatestDataUpdateUI <- function(id) {
  ns <- NS(id)

  bslib::card(
    full_screen = FALSE,
    class = "govuk-notification-banner",
    style = "border-radius: 12px; overflow: hidden;", # Add curved corners
    bslib::card_body(
      style = "gap: 0; padding: 0.7rem;",
      div(
        class = "govuk-notification-banner__header",
        tags$h2(
          class = "govuk-notification-banner__title",
          id = ns("latest_update_indicator"),
          "Latest Updated Indicator(s)"
        )
      ),
      div(
        class = "govuk-notification-banner__content",
        style = "border-radius: 9px;",
        tags$p(
          class = "govuk-notification-banner__heading",
          "These indicators were most recently updated:"
        ),
        with_gov_spinner(
          reactable::reactableOutput(ns("latest_update_table")),
          size = 0.6
        )
      )
    )
  )
}


#' Latest Data Update Server
#'
#' Computes and renders a table displaying the indicators that were most recently updated.
#'
#' @param id A string specifying the module's unique ID.
#' @param metrics_data A data frame containing metrics data with at least the following columns:
#'   - `Measure`: The name of the indicator/measure.
#'   - `Last Update`: The last update date in a "Month Year" format (e.g., "March 2023").
#'
#' @return No return value. Outputs a `reactable` table to the UI.
#'
#' @details
#' - The function calculates the most recently updated indicators by converting
#'   the `Last Update` column to a date format and filtering the latest date.
#' - Outputs a scrollable table using the `dfe_reactable()` function with enhanced styling.
#'
#' @examples
#' # See `LatestDataUpdateUI` for example usage with UI integration.
#'
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
        height = "220px",
        searchable = TRUE
      )
    })
  })
}


#' Latest Development Update UI Module
#'
#' Creates a user interface (UI) for displaying the latest development updates
#' in a collapsible card layout. The card includes a spinning gear icon, a
#' toggle button for expanding/collapsing the content, and a footer with a link
#' to the LAIT GitHub repository.
#'
#' @param id The unique module ID for this UI element. It is used to namespace
#'   input and output elements for this specific instance of the module.
#'
#' @return A `bslib::card` element that displays the latest development update
#'   content, along with a collapsible feature and external link to the GitHub
#'   repository for further information.
#'
#' @details This function uses `bslib::card()` to construct a visually appealing
#'   card with a modern collapsible design. The card displays:
#'   - A header with the title "Latest Development Updates" and a spinning gear
#'     icon that animates with CSS.
#'   - A body that shows a summary of the latest development updates in a
#'     collapsible content section, which is dynamically populated using a
#'     spinner and the latest update data.
#'   - A footer with a link to the LAIT GitHub repository for more details.
#'   - Keyframe animation is used to create a spinning effect on the gear icon.
#'
#' @examples
#' \dontrun{
#' LatestDevUpdateUI("latest_updates")
#' }
#'
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
              class = "fas fa-gear",
              `aria-hidden` = "true",
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
          with_gov_spinner(
            shiny::uiOutput(ns("latest_update_table")),
            color = "#0b0c0c",
            size = 0.7,
            spinner_type = 7
          )
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


#' Latest Development Update Server Module
#'
#' A server-side module that handles the logic for rendering the most recent
#' development update content within the `LatestDevUpdateUI` module. The function
#' extracts the latest update from a provided log and formats it into a dynamic
#' UI output.
#'
#' @param id The unique module ID used to namespace input and output elements
#'   for this specific instance of the module.
#' @param dev_update_log A data frame or tibble containing development update logs.
#'   The log should include columns such as `Date`, `Type`, `Summary`, and `Details`.
#'
#' @return This function does not return a value directly. It renders a UI element
#'   (using `shiny::renderUI`) that dynamically displays the most recent development
#'   update from the `dev_update_log`. The content includes:
#'   - The type of update.
#'   - A summary of the update.
#'   - Detailed information about the update.
#'   - The date of the update.
#'
#' @details This module filters the `dev_update_log` data to extract the most
#'   recent development update based on the `Date` field. It then formats the
#'   content for display in a styled HTML structure, with each field (type, summary,
#'   details, and date) clearly labelled. The `format_text()` function is used to
#'   format the details text before rendering.
#'
#' @examples
#' \dontrun{
#' LatestDevUpdateServer("latest_update", dev_update_log)
#' }
LatestDevUpdateServer <- function(id, dev_update_log) {
  moduleServer(id, function(input, output, session) {
    # Extract the most recent development update
    latest_dev_update <- development_update_log |>
      dplyr::filter(Date == max(Date)) |>
      dplyr::slice_min(1)

    # Render the update content inside a styled card
    output$latest_update_table <- shiny::renderUI({
      htmltools::tags$div(
        style = "line-height: 1.6;",
        htmltools::tags$p(
          htmltools::tags$b("Type:"),
          latest_dev_update$Type
        ),
        htmltools::tags$p(
          htmltools::tags$b("Summary:"),
          latest_dev_update$Summary
        ),
        htmltools::tags$p(
          htmltools::tags$b("Details:"),
          shiny::br(),
          format_text(latest_dev_update$Details)
        ),
        htmltools::tags$p(
          htmltools::tags$b("Date Updated:"),
          latest_dev_update$Date
        )
      )
    })
  })
}


#' Indicator Information Table UI
#'
#' Creates a UI element for displaying an indicator information table with
#' a loading spinner. The table is rendered using the `reactable` package.
#'
#' @param id A string specifying the module's unique ID.
#'
#' @return A UI element for the indicator information table wrapped with
#'   a spinner to indicate loading.
#'
#' @details
#' The table UI is created using `reactable::reactableOutput()` and wrapped
#' with a spinner using `with_gov_spinner()`.
#'
#' @examples
#' library(shiny)
#' ui <- fluidPage(
#'   IndicatorInfoTableUI("indicator_table")
#' )
#'
#' server <- function(input, output, session) {
#'   metrics_data <- data.frame(
#'     Topic = c("Finance", "Education"),
#'     Measure = c("Budget", "Attendance"),
#'     `Data Owner (DO) /Supplier and Contact Details` = c("Owner A", "Owner B"),
#'     `Last Update` = c("2023-01-01", "2023-02-01"),
#'     `Next Update` = c("2024-01-01", "2024-02-01"),
#'     `Hyperlink(s)` = c("https://example.com/finance", "https://example.com/edu")
#'   )
#'
#'   IndicatorInfoTableServer("indicator_table", metrics_data)
#' }
#' shinyApp(ui, server)
#'
IndicatorInfoTableUI <- function(id) {
  ns <- NS(id)
  with_gov_spinner(
    reactable::reactableOutput(ns("indicator_info_table"))
  )
}


#' Indicator Information Table Server
#'
#' Computes and renders an indicator information table using the given
#' metrics data. The table includes information about topics, measures,
#' data ownership, and update schedules.
#'
#' @param id A string specifying the module's unique ID.
#' @param metrics_data A data frame containing metrics data with the
#'   following columns:
#'   - `Topic`: The topic of the indicator.
#'   - `Measure`: The name of the measure.
#'   - `Data Owner (DO) /Supplier and Contact Details`: Contact details
#'      of the data owner or supplier.
#'   - `Last Update`: The last update date for the indicator.
#'   - `Next Update`: The next update date for the indicator.
#'   - `Hyperlink(s)`: A URL linking to additional resources.
#'
#' @return No return value. Outputs a `reactable` table to the UI.
#'
#' @details
#' - The table selects specific columns from the `metrics_data` and formats
#'   hyperlinks using `dfeshiny::external_link()`.
#' - The column `Hyperlink(s)` is hidden, and a new column,
#'   `Hyperlink(s) (opens in new tab)`, is added with formatted links.
#' - The table is styled and rendered using the `dfe_reactable()` function.
#' - Supports searching, pagination, and customisable page sizes.
#'
#' @examples
#' # See `IndicatorInfoTableUI` for example usage with UI integration.
#'
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
        searchable = TRUE
      )
    })
  })
}


#' Useful Links UI Module
#'
#' Creates a user interface (UI) for displaying useful links in a styled
#' container with a loading spinner. The links are dynamically rendered based
#' on the data provided by the server-side module.
#'
#' @param id The unique module ID for this UI element. It is used to namespace
#'   input and output elements for this specific instance of the module.
#'
#' @return A UI element containing the useful links list wrapped in a styled
#'   container with a loading spinner.
#'
#' @details This function uses `with_gov_spinner()` to display a loading spinner
#'   while the useful links are being rendered. The links are displayed using
#'   `shiny::uiOutput()` which will be populated with content from the server-side
#'   module.
#'
#' @examples
#' \dontrun{
#' UsefulLinksUI("useful_links")
#' }
UsefulLinksUI <- function(id) {
  ns <- NS(id)

  # UI container for useful links
  with_gov_spinner(
    shiny::uiOutput(ns("useful_links_lst")),
    spinner_type = 7,
    color = "#0b0c0c"
  )
}


#' Useful Links Server Module
#'
#' A server-side module that handles the logic for rendering useful links
#' in the UI. It formats the data, groups links by their type, and displays
#' them with relevant metadata such as the owner.
#'
#' @param id The unique module ID used to namespace input and output elements
#'   for this specific instance of the module.
#' @param useful_links A data frame or tibble containing useful links with
#'   columns such as `Type`, `Link`, `Tool_Name`, and `Owner`.
#'
#' @return This function does not return a value directly. It renders a UI
#'   element (using `shiny::renderUI`) that dynamically displays the useful
#'   links, grouped by their `Type`. Each link is formatted as an external
#'   link with the tool name as the clickable text and includes the owner's
#'   name next to it.
#'
#' @details This module processes the `useful_links` data by creating a
#'   formatted link for each entry and then groups them by the `Type`. The
#'   links are rendered within a styled container, and each group is displayed
#'   within its own section (card). The first group includes an "Owner" column
#'   next to each link.
#'
#' @examples
#' \dontrun{
#' UsefulLinksServer("useful_links", useful_links_data)
#' }
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
        purrr::imap(unique(useful_links_formatted$Type), function(type, index) {
          # Subset links of the same type
          links_by_type <- useful_links_formatted |> dplyr::filter(Type == type)

          # Wrap the entire group in a card
          htmltools::tags$div(
            # Type Header with "Owner" inline for the first type
            if (index == 1) {
              htmltools::tags$div(
                style = "
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                  margin-bottom: 15px;
                  padding-top: 15px;
                ",
                htmltools::tags$div(
                  style = "flex: 2; font-weight: bold;",
                  type
                ),
                htmltools::tags$div(
                  style = "flex: 1; text-align: left; font-weight: bold;",
                  "Owner"
                )
              )
            } else {
              htmltools::tags$h3(
                type,
                style = "margin-bottom: 15px; padding-top: 15px;"
              )
            },
            # List the links for the type
            htmltools::tags$div(
              purrr::map(seq_len(nrow(links_by_type)), function(i) {
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

# nolint end
