#' Generate Styled Line Chart Card
#'
#' Creates a card layout for displaying a line chart with full-screen support.
#'
#' @param output_id The Shiny output ID for the chart.
#' @return A card UI component with predefined styling.
#'
create_chart_card_ui <- function(output_id, aria_label) {
  bslib::card(
    bslib::card_body(
      div(
        with_gov_spinner(ggiraph::girafeOutput(output_id)),
        role = "img",
        `aria-label` = aria_label
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
    # Add a custom class for CSS control
    class = "dfe-contents-links",
    style = "
      position: sticky;
      top: 0.5rem;
      padding: 0.25rem;
      display: flex;
      flex-direction: column;
    ",
    h2("Contents"),
    tags$ol(
      style = "list-style-type: none; padding-left: 0; font-size: 1.188rem;",
      lapply(links_list, create_sidelink),
      tags$li(
        # Remove the circle bullets
        style = "list-style-type: none;",
        tags$i(class = "fas fa-chevron-down"),
        tags$a(
          href = "javascript:void(0);", # Prevents the default anchor behavior
          onclick = "window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' });",
          "Go to bottom of page"
        )
      )
    ),
    tags$div(
      class = "return-to-top",
      style = "list-style-type: none; margin-top: auto;",
      tags$li(
        shiny::tags$i(class = "fas fa-chevron-up"),
        tags$a(href = "#top", "Return to top of page")
      )
    )
  )
}


#' Internal Navigation Link UI
#'
#' Creates an internal navigation link in a Shiny app that switches tabs and
#' scrolls to a specific section of the page.
#'
#' This function generates a hyperlink that, when clicked, switches to the
#' specified tab and scrolls smoothly to the specified section within that tab.
#' It is especially useful for navigating long pages or multi-tab Shiny
#' applications.
#'
#' @param id A character string to namespace the module. Ensures the link
#' is unique within the Shiny app.
#' @param link_text A character string specifying the text displayed for the
#' hyperlink.
#' @param target_tab A character string representing the value of the tab to
#' switch to. The `data-target-tab` attribute is used to define the target tab.
#' @param target_id A character string specifying the `id` of the section to
#' scroll to within the target tab.
#'
#' @return An HTML `<a>` tag for the internal navigation link.
#'
#' @examples
#' internal_nav_link(
#'   id = "example_link",
#'   link_text = "Go to section",
#'   target_tab = "info_tab",
#'   target_id = "target_section"
#' )
#'
internal_nav_link <- function(id, link_text, target_tab, target_id) {
  ns <- NS(id) # Namespace the module
  tags$a(
    id = ns("internal_link"),
    href = "#", # Prevent default anchor behavior
    `data-target-tab` = target_tab, # Tab to switch to
    `data-target-id` = target_id, # ID of the section to scroll to
    link_text
  )
}
