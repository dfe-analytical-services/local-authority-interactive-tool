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
        icon = icon("copy", `aria-hidden` = "true"),
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
    style = "content-visibility: hidden;",
    `aria-hidden` = "true"
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


dfe_footer <- function(links_list) {
  # Add the HTML around the link and make an id by snake casing
  create_footer_link <- function(link_text) {
    shiny::tags$li(
      class = "govuk-footer__inline-list-item",
      actionLink(
        class = "govuk-link govuk-footer__link",
        inputId = tolower(gsub(" ", "_", link_text)),
        label = link_text
      )
    )
  }

  # The HTML div to be returned
  shiny::tags$footer(
    class = "govuk-footer ",
    role = "contentinfo",
    shiny::div(class = "govuk-width-container ", shiny::div(
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Add custom links in
      shiny::div(
        class = "govuk-footer__meta-item govuk-footer__meta-item--grow",

        # Set a visually hidden title for accessibility
        shiny::h2(class = "govuk-visually-hidden", "Support links"),
        # Generate as many links as needed
        shiny::tags$ul(
          class = "govuk-footer__inline-list",
          lapply(links_list, create_footer_link)
        )
      ),

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Back to copied code from shinyGovstyle
      shiny::div(class = "govuk-footer__meta", shiny::tagList(
        shiny::div(
          class = "govuk-footer__meta-item govuk-footer__meta-item--grow",
          shiny::tag(
            "svg",
            list(
              role = "presentation",
              focusable = "false",
              class = "govuk-footer__licence-logo",
              xmlns = "http://www.w3.org/2000/svg",
              viewbox = "0 0 483.2 195.7",
              height = "17",
              width = "41",
              shiny::tag("path", list(
                fill = "currentColor",
                d = paste0(
                  "M421.5 142.8V.1l-50.7 32.3v161.1h112.4v-50.7",
                  "zm-122.3-9.6A47.12 47.12 0 0 1 221 97.8c0-26 21",
                  ".1-47.1 47.1-47.1 16.7 0 31.4 8.7 39.7 21.8l42.7",
                  "-27.2A97.63 97.63 0 0 0 268.1 0c-36.5 0-68.3 20.1",
                  "-85.1 49.7A98 98 0 0 0 97.8 0C43.9 0 0 43.9 0 97",
                  ".8s43.9 97.8 97.8 97.8c36.5 0 68.3-20.1 85.1-49.",
                  "7a97.76 97.76 0 0 0 149.6 25.4l19.4 22.2h3v-87.8",
                  "h-80l24.3 27.5zM97.8 145c-26 0-47.1-21.1-47.1-47",
                  ".1s21.1-47.1 47.1-47.1 47.2 21 47.2 47S123.8 145",
                  " 97.8 145"
                )
              ))
            )
          ),
          shiny::tags$span(
            class = "govuk-footer__licence-description",
            "All content is available under the",
            shiny::tags$a(
              class = "govuk-footer__link",
              href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
              rel = "license",
              "Open Government Licence v3.0",
              .noWS = "after"
            ),
            ", except where otherwise stated"
          )
        ),
        shiny::tags$div(
          class = "govuk-footer__meta-item",
          shiny::tags$a(
            class = "govuk-footer__link govuk-footer__copyright-logo",
            href =
              paste0(
                "https://www.nationalarchives.gov.uk/information-management/",
                "re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/"
              ),
            "\u00A9 Crown copyright"
          )
        )
      ))
    ))
  )
}


# left nav ====================================================================
dfe_contents_links <- function(links_list) {
  # Add the HTML around the link and make an id by snake casing
  create_sidelink <- function(link_text) {
    tags$li(
      "—",
      actionLink(tolower(gsub(
        " ", "_", link_text
      )), link_text, class = "contents_link")
    )
  }

  # The HTML div to be returned
  tags$div(
    style = "position: sticky; top: 0.5rem; padding: 0.25rem; word-break: break-word;",
    # Make it stick!
    h2("Contents"),
    # remove the circle bullets
    tags$ol(
      style = "list-style-type: none; padding-left: 0; font-size: 1.188rem;",
      lapply(links_list, create_sidelink)
    )
  )
}


cookies_banner_server_jt <- function(id = "cookies_banner",
                                     input_cookies,
                                     parent_session,
                                     google_analytics_key = NULL,
                                     cookies_link_panel = "cookies_panel_ui") {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(google_analytics_key)) {
      warning("Please provide a valid Google Analytics key")
    }
    shiny::observeEvent(input_cookies(), {
      if (!is.null(input_cookies())) {
        if (!("dfe_analytics" %in% names(input_cookies()))) {
          shinyjs::show(id = "cookies_main")
        } else {
          shinyjs::hide(id = "cookies_main")
          msg <- list(
            name = "dfe_analytics",
            value = input_cookies()$dfe_analytics
          )
          session$sendCustomMessage("analytics-consent", msg)
          if ("cookies" %in% names(input)) {
            if ("dfe_analytics" %in% names(input_cookies())) {
              if (input_cookies()$dfe_analytics == "denied") {
                ga_msg <- list(name = paste0("_ga_", google_analytics_key))
                session$sendCustomMessage("cookie-clear", ga_msg)
              }
            }
          }
        }
      } else {
        shinyjs::hide(id = "cookies_main", asis = TRUE)
        shinyjs::toggle(id = "cookies_div", asis = TRUE)
      }
    })
    shiny::observeEvent(input$cookies_accept, {
      msg <- list(name = "dfe_analytics", value = "granted")
      session$sendCustomMessage("cookie-set", msg)
      session$sendCustomMessage("analytics-consent", msg)
      shinyjs::hide(id = "cookies_main", asis = TRUE)
    })
    shiny::observeEvent(input$cookies_reject, {
      msg <- list(name = "dfe_analytics", value = "denied")
      session$sendCustomMessage("cookie-set", msg)
      session$sendCustomMessage("analytics-consent", msg)
      shinyjs::hide(id = "cookies_main", asis = TRUE)
    })
    shiny::observeEvent(input$cookies_link, {
      shiny::updateTabsetPanel(
        session = parent_session, "pages",
        selected = cookies_link_panel
      )
    })
    return(shiny::renderText({
      cookies_text_stem <- "You have chosen to"
      cookies_text_tail <- "the use of cookies on this website."
      if (!is.null(input_cookies())) {
        if ("dfe_analytics" %in% names(input_cookies())) {
          if (input_cookies()$dfe_analytics == "granted") {
            paste(cookies_text_stem, "accept", cookies_text_tail)
          } else {
            paste(cookies_text_stem, "reject", cookies_text_tail)
          }
        }
      } else {
        "Cookies consent has not been confirmed."
      }
    }))
  })
}
