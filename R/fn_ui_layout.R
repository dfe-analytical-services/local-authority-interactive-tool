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
      ggiraph::girafeOutput(output_id)
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
