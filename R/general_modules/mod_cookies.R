# Cookies UI functions --------------------------------------------------------

# Setting up cookie consent based on a cookie recording the consent:
# https://book.javascript-for-r.com/shiny-cookies.html
#' Set JavaScript Dependencies for Cookie Consent
#'
#' This function generates an HTML `head` tag that includes the necessary
#' JavaScript dependencies for managing cookie consent in a Shiny application.
#' Specifically, it loads the `js-cookie` library and a custom
#' `cookie-consent.js` script to handle cookie consent interactions.
#'
#' @return A `tags$head` object containing `script` tags that reference the
#' JavaScript libraries required for cookie consent functionality.
#' This object can be included in the UI definition of a Shiny application.
#'
#' @details
#' The function is intended to streamline the inclusion of JavaScript files
#' related to cookie consent in Shiny applications.
#' It loads the `js-cookie` library from a CDN (Content Delivery Network) and
#' a custom JavaScript file, `cookie-consent.js`,
#' that is typically used to manage user interactions with cookie banners
#'  and consent settings.
#'
#' This setup is based on practices described in the "shiny-cookies"
#' section of the book
#' [JavaScript for R](https://book.javascript-for-r.com/shiny-cookies.html).
#' The `js-cookie` library is a lightweight JavaScript library for
#' handling cookies, while `cookie-consent.js` is expected to contain
#' the specific logic for your application.
#'
set_javascript_dependencies <- function() {
  tags$head(
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script(src = "cookie-consent.js")
  )
}
