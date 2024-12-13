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


#' Create a GOV.UK-style Footer for a Shiny Application
#'
#' This function generates a GOV.UK-style footer for a Shiny app. It includes
#' support links, licensing information, and copyright details, styled
#' according to the GOV.UK design system. The footer can dynamically render
#' support links based on the `links_list` provided.
#'
#' @param links_list A character vector of link text to be displayed as support
#' links in the footer. Each link will be generated as a clickable item using
#' Shiny's `actionLink` function.
#'
#' @return An HTML footer element styled with GOV.UK classes, which includes:
#'   - Custom support links specified by `links_list`.
#'   - Licensing information indicating content availability under the Open
#'     Government Licence v3.0.
#'   - Crown copyright information with a clickable link to more details.
#'
#' @details
#' The function uses the GOV.UK design system classes to style the footer
#' elements. Each item in `links_list` is converted to a clickable link in the
#' footer's support links section. The footer also includes metadata about
#' licensing and copyright, which are standard for GOV.UK sites.
#'
#' Accessibility features include:
#' - A visually hidden title for screen readers (`Support links`).
#' - Proper semantic markup for navigation and content information.
#'
#' @examples
#' # Example: Creating a footer with two support links
#' links <- c("Contact us", "Privacy policy")
#' footer <- dfe_footer(links)
#' shiny::div(footer)
#'
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


#' Generate a Sticky Table of Contents Sidebar for a Shiny Application
#'
#' This function creates a sticky "Table of Contents" component for a Shiny app.
#' It renders a list of clickable links, which can be used to navigate within
#' the app. Each link is styled and positioned as a sidebar menu, designed to
#' remain visible while scrolling.
#'
#' @param links_list A character vector of link text items to be displayed in
#' the "Contents" section. Each item in the list is rendered as a clickable
#' `actionLink` in the sidebar.
#'
#' @return An HTML `div` element styled as a sticky sidebar. The contents
#' include:
#'   - A heading (`Contents`) for the sidebar.
#'   - An ordered list (`<ol>`) of clickable links, styled without bullet points.
#'
#' @details
#' - Each item in `links_list` is converted into a clickable link. The link IDs
#'   are generated by converting spaces in the link text to underscores and
#'   converting text to lowercase (snake case).
#' - The sidebar is styled with `position: sticky`, ensuring it remains in view
#'   while scrolling.
#' - Word-breaking is enabled (`word-break: break-word`) to ensure long links
#'   or text do not overflow.
#'
#' @examples
#' # Example: Creating a sticky table of contents with three links
#' links <- c("Introduction", "Analysis Results", "Conclusion")
#' sidebar <- dfe_contents_links(links)
#' shiny::div(sidebar)
#'
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


# Temp function while it is fixed in dfeshiny
#' Cookies Banner Module Server for Shiny Applications
#'
#' This function implements a Shiny module server to manage a cookies consent
#' banner. It handles user interactions for accepting or rejecting cookies and
#' integrates with Google Analytics if a valid key is provided.
#'
#' @param id A character string representing the module's ID. Defaults to
#' `"cookies_banner"`.
#' @param input_cookies A reactive function that provides the cookies consent
#' information. It should return a named list with a `dfe_analytics` field
#' indicating the user's consent (`"granted"` or `"denied"`).
#' @param parent_session The parent Shiny session. Used for updating UI elements
#' outside the module, such as navigating to a cookies policy panel.
#' @param google_analytics_key A character string containing the Google Analytics
#' key. If `NULL`, Google Analytics integration will not be enabled. Defaults
#' to `NULL`.
#' @param cookies_link_panel A character string representing the ID of the tabset
#' panel to navigate to when the "cookies policy" link is clicked. Defaults to
#' `"cookies_panel_ui"`.
#'
#' @return A reactive expression returning a summary message about the user's
#' cookies consent status. The returned text indicates whether the user has
#' accepted, rejected, or not yet confirmed their cookies preference.
#'
#' @details
#' - If `input_cookies()` does not include a `dfe_analytics` field, the cookies
#'   banner is shown (`cookies_main` is visible).
#' - If cookies consent is `"granted"`, a message is sent to the browser to enable
#'   analytics. If `"denied"`, it disables analytics and clears any related
#'   cookies.
#' - Clicking the "cookies policy" link navigates to the specified `cookies_link_panel`.
#' - The returned text updates dynamically to reflect the user's current consent
#'   choice.
#'
#' @examples
#' # Example usage in a Shiny app:
#' ui <- fluidPage(
#'   shinyjs::useShinyjs(),
#'   tabsetPanel(id = "pages", tabPanel("cookies_panel_ui")),
#'   div(id = "cookies_main", "Cookies banner content"),
#'   actionButton("cookies_accept", "Accept Cookies"),
#'   actionButton("cookies_reject", "Reject Cookies")
#' )
#' server <- function(input, output, session) {
#'   cookies <- reactiveVal(list(dfe_analytics = NULL))
#'   cookies_banner_server_jt("cookies_banner", cookies, session)
#' }
#' shinyApp(ui, server)
#'
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
