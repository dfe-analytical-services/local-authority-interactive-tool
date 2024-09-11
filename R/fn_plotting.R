#' @title Extract the Unique Y-Axis Title from a Dataset
#' @description This function extracts the unique y-axis title from a
#' given dataset.
#' @param data_full A dataframe containing the full dataset.
#' @return A character vector containing the unique y-axis title(s).
#' @examples
#' \dontrun{
#' get_yaxis_title(data_full = my_data)
#' }
#' @export
get_yaxis_title <- function(data_full) {
  data_full |>
    pull_uniques("y_axis_name")
}


#' @title Generate a Plot Title
#' @description This function generates a plot title based on the
#' selected indicator and the axis indicator.
#' @param selected_indicator A character string representing the
#' selected indicator.
#' @param axis_indicator A character string representing the axis indicator.
#' @return A character string representing the plot title.
#' @examples
#' \dontrun{
#' get_plot_title(selected_indicator = "GDP", axis_indicator = "Population")
#' }
#' @export
get_plot_title <- function(selected_indicator, axis_indicator) {
  glue::glue(
    "{selected_indicator} - {axis_indicator}"
  )
}


#' Create Plot Colours
#'
#' This function generates a named vector of colors for the groups in the data,
#' which can be used in the plot to distinguish different
#' Local Authorities and Regions.
#'
#' @param data_long A data frame in long format containing the data
#' for the plot.
#'
#' @return A named vector of colors, where each color is associated
#' with a specific group
#' in the `LA and Regions` column of the `data_long` dataset.
#'
#' @details
#' The function first extracts the unique groups from the
#' `LA and Regions` column in `data_long`.
#' It then generates a categorical color palette using `afcolours::af_colours`,
#' with a color assigned to each group.
#' The output is a named vector where the names correspond to the groups,
#' and the values are the respective colors.
#'
create_plot_colours <- function(data_long) {
  # Colours
  plot_groups <- data_long |>
    pull_uniques("LA and Regions")

  plot_colours <- afcolours::af_colours(
    type = "categorical",
    n = length(plot_groups)
  )
  names(plot_colours) <- plot_groups

  plot_colours
}


#' Create Plot Colours with Focus Group Highlight
#'
#' This function generates a colour mapping for plot groups,
#' where all groups are assigned a neutral colour,
#' except for a specified focus group that is highlighted with a distinct
#' colour from the `af_colours_focus` palette.
#'
#' @param data_long A data frame in long format, containing a column
#' `LA and Regions` representing the groups for the plot.
#' @param focus_group A character string representing the group that should
#' be highlighted in the plot.
#'
#' @return A named character vector of hex colour codes, where names
#' correspond to the groups in `LA and Regions`
#' and values are the associated colours. The focus group will have
#' a distinct colour.
#'
#' @seealso [af_colours_focus()] for the colour palette used.
#'
#' @examples
#' # Create plot colours with a focus on "London"
#' create_focus_plot_colours(data_long, "London")
#'
#' @export
create_focus_plot_colours <- function(data_long, focus_group) {
  # Colours
  plot_groups <- data_long |>
    pull_uniques("LA and Regions")

  plot_colours <- rep(
    af_colours_focus()[2],
    length(plot_groups)
  )
  names(plot_colours) <- plot_groups

  plot_colours[focus_group] <- af_colours_focus()[1]

  plot_colours
}


create_focus_plot_sizes <- function(data_long, focus_group) {
  # Get plot groups
  plot_groups <- data_long |>
    pull_uniques("LA and Regions")

  # Set default line size
  plot_sizes <- rep(0.5, length(plot_groups))
  names(plot_sizes) <- plot_groups

  # Set focus line size
  plot_sizes[focus_group] <- 1

  plot_sizes
}


#' @title Calculate Y-Axis Range
#' @description This function calculates the range of the 'values_num'
#' column in the provided dataset.
#' @param data_long A dataframe containing the dataset.
#' @return A numeric vector of length two, containing the minimum and
#' maximum of the 'values_num' column.
#' @examples
#' \dontrun{
#' calculate_y_range(data_long = my_data)
#' }
#' @export
calculate_y_range <- function(data_long) {
  data_long |>
    dplyr::pull("values_num") |>
    range(na.rm = TRUE)
}


#' @title Get Lower Limit of Y-Axis
#' @description This function calculates the lower limit of the y-axis
#' for the plot.
#' @param data_long A dataframe containing the dataset.
#' @return A numeric value representing the lower limit of the y-axis.
#' @examples
#' \dontrun{
#' get_ylim_low(data_long = my_data)
#' }
#' @export
get_ylim_low <- function(data_long) {
  y_range <- calculate_y_range(data_long)
  if (y_range[1] >= 0) {
    0
  } else {
    y_range[1] * 1.1
  }
}


#' @title Get Upper Limit of Y-Axis
#' @description This function calculates the upper limit of the y-axis for
#' the plot.
#' @param data_long A dataframe containing the dataset.
#' @return A numeric value representing the upper limit of the y-axis.
#' @examples
#' \dontrun{
#' get_ylim_high(data_long = my_data)
#' }
#' @export
get_ylim_high <- function(data_long) {
  y_range <- calculate_y_range(data_long)
  if (y_range[2] <= 0) {
    0
  } else {
    y_range[2] * 1.1
  }
}


#' @title Get Unique Years from Dataset
#' @description This function extracts the unique years from the
#' 'Years_num' column in the provided dataset.
#' @param data_long A dataframe containing the dataset.
#' @return A numeric vector containing the unique years.
#' @examples
#' \dontrun{
#' get_years(data_long = my_data)
#' }
#' @export
get_years <- function(data_long) {
  data_long |>
    pull_uniques("Years_num")
}


#' @title Get Number of Unique Years
#' @description This function calculates the number of unique years
#' in the provided dataset.
#' @param data_long A dataframe containing the dataset.
#' @return A numeric value representing the number of unique years.
#' @examples
#' \dontrun{
#' get_num_years(data_long = my_data)
#' }
#' @export
get_num_years <- function(data_long) {
  get_years(data_long) |>
    length()
}


#' Format Axes for a ggplot2 Plot
#'
#' This function formats the x and y axes for a ggplot2 plot.
#' It uses the data in long format to determine the limits and
#' breaks for the axes.
#'
#' @param data_long A data frame in long format.
#' The data frame should have a numeric variable for the y-axis
#' and a time variable for the x-axis.
#'
#' @return A list of ggplot2 scale functions for the x and y axes.
#'
#' @examples
#' \dontrun{
#' data_long <- data.frame(year = 2000:2020, value = rnorm(21))
#' p <- ggplot(data_long, aes(x = year, y = value)) +
#'   geom_line() +
#'   format_axes(data_long)
#' print(p)
#' }
#'
#' @export
format_axes <- function(data_long) {
  y_lim_low <- get_ylim_low(data_long)
  y_lim_high <- get_ylim_high(data_long)
  num_years <- get_num_years(data_long)

  list(
    ggplot2::scale_y_continuous(
      limits = c(y_lim_low, y_lim_high),
      expand = expansion(0, 0),
      breaks = scales::breaks_pretty()
    ),
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = num_years)
    )
  )
}


#' Set Plot Colours
#'
#' This function applies a manual color or fill scale to a ggplot2 plot,
#' based on the provided color parameters.
#'
#' @param colour_params A named vector of colors, typically generated by
#' the `create_plot_colours` function.
#' @param colour_type A character string specifying the type of
#' color scale to apply.
#' Options are `"colour"` (for line and point colors) and
#' `"fill"` (for bar fill colors). Default is `"colour"`.
#'
#' @return A `ggplot2` scale function (`scale_colour_manual` or
#' `scale_fill_manual`) that can be added to a ggplot2 plot.
#'
#' @details
#' This function returns the appropriate ggplot2 scale function
#' (`scale_colour_manual` or `scale_fill_manual`) based on the
#' `colour_type` argument.
#' The colors are applied according to the mapping in `colour_params`,
#' ensuring that each group in the data is assigned its corresponding color.
#'
set_plot_colours <- function(data_long,
                             colour_type = "colour",
                             focus_group = NULL) {
  if (colour_type == "colour") {
    ggplot2::scale_colour_manual(values = create_plot_colours(data_long))
  } else if (colour_type == "fill") {
    ggplot2::scale_fill_manual(values = create_plot_colours(data_long))
  } else if (colour_type == "focus") {
    list(
      ggplot2::scale_color_manual(values = create_focus_plot_colours(data_long, focus_group)),
      ggplot2::scale_size_manual(values = create_focus_plot_sizes(data_long, focus_group))
    )
  } else if (colour_type == "focus-fill") {
    ggplot2::scale_fill_manual(values = create_focus_plot_colours(data_long, focus_group))
  }
}


#' Set Plot Labels for a ggplot2 Plot
#'
#' This function sets the x, y, and title labels for a ggplot2 plot.
#' It uses the filtered data and selected indicator to determine the labels.
#'
#' @param filtered_bds A filtered data frame.
#' The data frame should have a numeric variable for the y-axis and a
#' time variable for the x-axis.
#' @param selected_indicator A character string representing the selected
#' indicator for the plot.
#'
#' @return A ggplot2 labs function with the x, y, and title labels set.
#'
#' @examples
#' \dontrun{
#' filtered_bds <- data.frame(
#'   year = 2000:2020, value = rnorm(21),
#'   indicator = "GDP"
#' )
#' p <- ggplot(filtered_bds, aes(x = year, y = value)) +
#'   geom_line() +
#'   set_plot_labs(filtered_bds, "GDP")
#' print(p)
#' }
#'
#' @export
set_plot_labs <- function(filtered_bds, selected_indicator) {
  y_title <- get_yaxis_title(filtered_bds)
  plot_title <- get_plot_title(selected_indicator, y_title)

  ggplot2::labs(
    x = "",
    y = y_title,
    title = plot_title
  )
}


#' Custom ggplot2 Theme
#'
#' This function returns a list of ggplot2 theme elements for
#' customizing the appearance of plots.
#'
#' @return A list of `ggplot2::theme` settings, including a minimal theme,
#' centered plot titles, bottom legend placement,
#' and removal of minor grid lines on the x-axis.
#'
#' @details
#' The custom theme is designed to provide a clean and modern look to the plots.
#' It uses `ggplot2::theme_minimal` as the base theme,
#' with additional customization for plot titles,
#' legend positioning, and grid lines.
#' This theme can be easily applied to any ggplot2 plot to
#' ensure visual consistency.
#'
custom_theme <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  )
}


#' Create Interactive Vertical Lines for Tooltips
#'
#' This function generates vertical lines on a ggplot2 plot that display
#' tooltips when hovered over, using the ggiraph package.
#'
#' @param x A numeric value representing the x-axis position (year)
#' for the vertical line.
#' @param data A data frame containing the data for the plot,
#' filtered for the relevant year.
#'
#' @return A `ggiraph::geom_vline_interactive` object representing the
#' vertical line with attached tooltips.
#'
#' @details
#' The function creates an interactive vertical line at the specified
#' x-axis position.
#' The tooltip displays the year and corresponding values for each
#' `LA and Regions` group present in the data.
#' The line is dashed and only becomes visible upon hovering,
#' enhancing the interactivity of the plot.
#'
tooltip_vlines <- function(x, data) {
  geom_vline_interactive(
    xintercept = x,
    data_id = x,
    tooltip = glue::glue("Year: {x}", paste0(
      "\n",
      glue::glue_data(
        data |>
          pretty_num_table(include_columns = "values_num", dp = 1) |>
          dplyr::filter(Years_num == x, !is.na(values_num)) |>
          dplyr::arrange(dplyr::desc(values_num)),
        "{`LA and Regions`}: {values_num}"
      ),
      collapse = ""
    )),
    hover_nearest = TRUE,
    linetype = "dashed",
    size = 2.5,
    color = "transparent"
  )
}


#' Custom ggiraph Tooltip CSS
#'
#' This function generates a string of custom CSS styles for
#' tooltips used in ggiraph plots.
#'
#' @return A character string containing the CSS styling for the
#' ggiraph tooltips.
#'
#' @details
#' The CSS string includes styles for background color, text color,
#' padding, border radius, font family, font weight, and border styling.
#' This allows for a consistent and visually
#' appealing tooltip appearance across all ggiraph plots in the application.
#'
custom_ggiraph_tooltip <- function() {
  tooltip_css <- paste0(
    "background-color:#ECECEC;",
    "color:black;",
    "padding:5px;",
    "border-radius:3px;",
    "font-family:Arial;",
    "font-weight:500;",
    "border:1px solid black;",
    "z-index: 99999 !important;"
  )

  tooltip_css
}


#' Generate Generic ggiraph Options
#'
#' This function generates a list of commonly used ggiraph options for
#' customizing tooltips and toolbars in `ggiraph` visualizations.
#' It is designed to provide a set of default settings that can be easily
#' extended or overridden.
#'
#' @param ... Additional options to be passed to `ggiraph` functions.
#' These options will be added to the default options provided by this function.
#'
#' @return A list of `ggiraph` options, including tooltip styling,
#' toolbar position, and customization for PNG download functionality.
#'
#' @details
#' The default options generated by this function include:
#' - Tooltip customization via `ggiraph::opts_tooltip()`,
#' with a custom CSS styling and full opacity.
#' - Toolbar customization via `ggiraph::opts_toolbar()`,
#' positioning the toolbar in the top right corner,
#' setting a custom filename for PNG downloads,
#' and hiding unnecessary toolbar options like selection and zoom.
#'
#' These default options can be modified or extended using the `...` argument,
#' allowing you to pass additional options or override the defaults.
#'
generic_ggiraph_options <- function(...) {
  list(
    ggiraph::opts_tooltip(
      css = custom_ggiraph_tooltip(),
      opacity = 1
    ),
    ggiraph::opts_toolbar(
      position = "topright",
      pngname = "lait-png-download",
      hidden = c("selection", "zoom", "misc")
    ),
    ...
  )
}
