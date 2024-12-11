#' Generate Styled Line Chart Card
#'
#' Creates a card layout for displaying a line chart with full-screen support.
#'
#' @param output_id The Shiny output ID for the chart.
#' @return A card UI component with predefined styling.
#'
create_chart_card_ui <- function(output_id) {
  bslib::card(
    bslib::card_body(
      with_gov_spinner(
        ggiraph::girafeOutput(output_id)
      )
    ),
    full_screen = TRUE,
    style = "flex-grow: 1; display: flex; justify-content: center; padding: 0 10px;"
  )
}


#' Generate Styled Download Options
#'
#' Creates a UI layout for download and copy buttons with predefined styles.
#'
#' @param download_btn UI component for the download button.
#' @param copy_btn UI component for the copy button.
#' @return A div containing download and copy buttons with predefined styling.
#'
create_download_options_ui <- function(download_id, copy_clipboard_id) {
  div(
    shiny::tagAppendAttributes(
      DownloadChartBtnUI(download_id),
      style = "max-width: none; margin-left: 0; align-self: auto;"
    ),
    br(),
    shiny::tagAppendAttributes(
      actionButton(
        copy_clipboard_id,
        "Copy Chart to Clipboard",
        icon = icon("copy"),
        class = "gov-uk-button"
      ),
      style = "max-width: none;"
    ),
    style = "display: flex; flex-direction: column; align-self: flex-start; margin: 15px;"
  )
}


#' Create a Hidden Clipboard Plot
#'
#' This function generates a hidden static plot element that can be used
#' for copying the plot to the clipboard. The plot is rendered but not
#' visible in the UI, using the `content-visibility: hidden` CSS property.
#'
#' @param clipboard_plot_id A unique identifier for the plot output to be
#'                           rendered.
#' @return A `div` containing a hidden `plotOutput` element with the
#'         specified ID.
#' @details The plot is intended for copying to the clipboard, without
#'          being displayed to the user directly on the screen. It can
#'          be used in conjunction with functionality that allows users
#'          to copy or export plots.
#'
create_hidden_clipboard_plot <- function(clipboard_plot_id) {
  # Hidden static plot for copy-to-clipboard
  div(
    shiny::plotOutput(clipboard_plot_id),
    style = "content-visibility: hidden;"
  )
}


#' Create a Closable Notification Banner
#'
#' This function generates a notification banner styled with the GOV.UK
#' notification style. The banner includes a close button (an "×" icon)
#' that removes the banner from the page when clicked.
#'
#' @param input_id A unique ID for the notification banner. This ID is used
#'   for JavaScript interactions to handle the banner's close functionality.
#' @param title_txt A character string representing the title of the
#'   notification banner. This text appears prominently at the top of the banner.
#' @param body_txt HTML content or a character string representing the main
#'   content of the notification banner. Can include links or other formatted text.
#' @param type A character string specifying the type of notification. Options
#'   are `"standard"`, `"success"`, `"warning"`, or `"error"`. Defaults to `"standard"`.
#'
#' @return A Shiny `tagList` object containing the notification banner with
#'   a close button. The banner is styled for GOV.UK aesthetics and functionality.
#'
#' @examples
#' # Create a simple closable notification banner in a Shiny app
#' ui <- fluidPage(
#'   closable_noti_banner(
#'     input_id = "example_banner",
#'     title_txt = "Information",
#'     body_txt = shiny::HTML("This is a sample notification banner."),
#'     type = "standard"
#'   )
#' )
#'
closable_noti_banner <- function(input_id, title_txt, body_txt, type = "standard") {
  shiny::tagList(
    shiny::tags$div(
      id = paste0(input_id, "-banner"),
      class = "govuk-notification-banner",
      style = "position: relative; padding-right: 2rem;", # Adjust padding for close button
      shinyGovstyle::noti_banner(
        inputId = input_id,
        title_txt = title_txt,
        body_txt = body_txt,
        type = type
      ),
      # Close button
      shiny::tags$button(
        type = "button",
        class = "govuk-notification-banner__close",
        style = paste(
          "position: absolute; top: 10px; right: 10px; background: none;",
          "border: none; font-size: 18px; cursor: pointer; color: #ffffff;",
          "font-weight: bold; line-height: 1;"
        ),
        "×"
      )
    ),
    # JavaScript to remove banner on click
    shiny::tags$script(
      shiny::HTML(sprintf("
        $(document).on('click', '#%s-banner .govuk-notification-banner__close', function() {
          $('#%s-banner').remove();
        });
      ", input_id, input_id))
    )
  )
}


#' Create a Notification Banner for Full Data on GitHub
#'
#' This function generates a closable notification banner informing users
#' about the availability of the full dataset as a downloadable .csv file
#' on GitHub. The banner includes a link to the dataset and a close button.
#'
#' @return A Shiny `tagList` object containing the notification banner with
#'   a close button. The banner provides information and a clickable link
#'   to the dataset on GitHub.
#'
#' @examples
#' # Add the banner to a Shiny app
#' ui <- fluidPage(
#'   full_data_on_github_noti()
#' )
#'
full_data_on_github_noti <- function() {
  closable_noti_banner(
    input_id = "full_data_on_github",
    title_txt = "Information",
    body_txt = shiny::HTML(paste0(
      "The full dataset is available for download as a CSV on GitHub. ",
      "You can access the file ",
      dfeshiny::external_link(
        href = paste0(
          "https://github.com/dfe-analytical-services/",
          "local-authority-interactive-tool/tree/main/01_data/02_prod"
        ),
        link_text = "bds_long.csv"
      ),
      ".<br>",
      "<span style='font-weight: normal;'>",
      "We recommend this method for downloading large datasets, ",
      "especially if you plan to use the data in your code.</span>"
    )),
    type = "standard"
  )
}
