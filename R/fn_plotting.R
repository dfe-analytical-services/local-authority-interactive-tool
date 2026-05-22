#' Get Y-Axis Title for Plot
#'
#' This function retrieves the title for the Y-axis based on the `y_axis_name`
#' column of the provided dataset. If there is only one unique value for
#' `y_axis_name`, the title will be formatted with line breaks. If there are
#' multiple unique values, the function returns a combined title indicating
#' mixed units, with the individual units separated by commas and "and" before
#' the last unit.
#'
#' @param data_full A data frame containing the `y_axis_name` column, which will
#'   be used to determine the Y-axis title.
#'
#' @return A character string representing the Y-axis title. This can either
#'   be the formatted title with line breaks or a combined string indicating
#'   mixed units.
#'
#' @details The function uses `pull_uniques` to extract unique values from
#'   the `y_axis_name` column. If a single unique value is found, it formats
#'   it with line breaks via `add_line_breaks`. If multiple unique values are
#'   found, the function combines them with commas and "and" before the last unit.
#'
#' @examples
#' # If there is a single y-axis title
#' get_yaxis_title(data_full = data_frame(y_axis_name = c("Units")))
#'
#' # If there are multiple y-axis titles
#' get_yaxis_title(data_full = data_frame(y_axis_name = c("Units", "Indicator")))
#'
get_yaxis_title <- function(data_full) {
  y_axis_title <- data_full |>
    pull_uniques("y_axis_name")

  # If more than one y-axis title then combine
  if (length(y_axis_title) == 1) {
    add_line_breaks(y_axis_title)
  } else {
    mixed_title <- paste(
      "Mixed units:\n",
      paste(
        paste(y_axis_title[-length(y_axis_title)], collapse = ",\n"),
        y_axis_title[length(y_axis_title)],
        sep = " and\n"
      )
    )
  }
}


#' Get X-Axis Title for Plot
#'
#' This function retrieves the title for the X-axis based on the `Year_Type`
#' column of the provided dataset. If there is only one unique value for
#' `Year_Type`, the title will be formatted with line breaks. If there are
#' multiple unique values, a generic "Mixed Year Types" label is used.
#'
#' @param data_full A data frame containing the `Year_Type` column, which will
#'   be used to determine the X-axis title.
#'
#' @return A character string representing the X-axis title. This can either
#'   be the value of `Year_Type` formatted with line breaks or the string
#'   "Mixed Year Types" if there are multiple unique values.
#'
#' @details The function uses `pull_uniques` to extract unique values from
#'   the `Year_Type` column. If a single unique value is found, it formats
#'   it with line breaks via `add_line_breaks`. If multiple unique values
#'   are found, the title will be "Plain Years".
#'
#' @examples
#' # If there is a single year type
#' get_xaxis_title(data_full = data_frame(Year_Type = c("Fiscal", "Fiscal", "Fiscal")))
#'
#' # If there are multiple year types
#' get_xaxis_title(data_full = data_frame(Year_Type = c("Fiscal", "Calendar")))
get_xaxis_title <- function(data_full) {
  x_axis_title <- data_full |>
    pull_uniques("Year_Type")

  # If more than one y-axis title then give generic
  if (length(x_axis_title) == 0 || all(is.na(x_axis_title))) {
    "Years (no type given)"
  } else if (length(x_axis_title) == 1) {
    add_line_breaks(x_axis_title)
  } else {
    "Mixed Year Types"
  }
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
get_plot_title <- function(data_full) {
  chart_title <- data_full |>
    pull_uniques("Chart_title")

  # If more than one title, format with "," & "and"s
  if (length(chart_title) == 1) {
    chart_title
  } else {
    paste(
      "Chart showing -",
      paste(
        paste(chart_title[-length(chart_title)], collapse = ",<br>"),
        chart_title[length(chart_title)],
        sep = " and<br>"
      ),
      sep = "<br>"
    )
  }
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
create_plot_colours <- function(data_long, focus_group = NULL) {
  # Extract unique groups for plotting
  plot_groups <- data_long |>
    pull_uniques("LA and Regions")

  # Initialise plot colours
  plot_colours <- c()

  # Assign specific colour for focus group if provided & remove from plot groups
  if (!is.null(focus_group) && focus_group %in% plot_groups) {
    plot_colours[focus_group] <- get_selected_la_colour()
    plot_groups <- setdiff(plot_groups, focus_group)
  }

  # Assign specific colour for "England" if present & remove from plot groups
  if ("England" %in% plot_groups) {
    plot_colours["England"] <- get_england_colour()
    plot_groups <- setdiff(plot_groups, "England")
  }

  # Assign colours for remaining groups (ensure now duplicate colours)
  remaining_colours <- setdiff(get_clean_af_colours(), plot_colours)[seq_along(plot_groups)]
  names(remaining_colours) <- plot_groups

  # Combine all colours
  plot_colours <- c(plot_colours, remaining_colours)

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


#' Create Plot Sizes for Focus Group
#'
#' This function generates a vector of line sizes for plotting, where the
#' specified focus group is highlighted with a larger line size. All other
#' groups are assigned a default smaller size.
#'
#' @param data_long A data frame in long format containing the plotting
#'   groups. This should include a column for "LA and Regions".
#' @param focus_group A character string indicating the focus group to be
#'   highlighted in the plot.
#'
#' @return A numeric vector of line sizes, with the focus group having a
#'   larger size (1) and all other groups having a default smaller size (0.5).
#'
#' @examples
#' create_focus_plot_sizes(data_long, "Barking and Dagenham")
#'
create_focus_plot_sizes <- function(data_long, focus_group) {
  # Get plot groups
  plot_groups <- data_long |>
    pull_uniques("LA and Regions")

  # Set default line size
  plot_sizes <- rep(0.5, length(plot_groups))
  names(plot_sizes) <- plot_groups

  # Set focus line size
  plot_sizes[focus_group] <- 1.5

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


#' Calculate Pretty Y-Gridlines
#'
#' This function computes the y-range for the provided data and generates
#' 'pretty' gridline breaks. It ensures that gridlines extend above the
#' maximum and below the minimum values of the dataset, enhancing plot
#' readability.
#'
#' The function first retrieves the data range using `calculate_y_range`.
#' If the maximum value is non-positive, it sets the upper limit to zero,
#' and if the minimum value is non-negative, it sets the lower limit to zero
#' as well. This adjustment guarantees that the y-axis includes zero when
#' necessary.
#'
#' After calculating the range, it generates 'pretty' breaks and checks to
#' ensure breaks extend beyond the maximum and minimum values. If needed, it
#' adds increments to the breaks for clarity.
#'
#' @param data_long A data frame in long format, containing the data
#'   for which the y-axis gridlines are calculated.
#'
#' @return A numeric vector of 'pretty' breaks for y-gridlines, ensuring
#'   that gridlines extend beyond the minimum and maximum values for better
#'   visualisation.
#'
#' @examples
#' # Example usage:
#' gridlines <- pretty_y_gridlines(data_long)
#'
pretty_y_gridlines <- function(data_long) {
  # Get the range of the data
  y_range <- calculate_y_range(data_long)

  # Zero axes if needed
  if (y_range[2] <= 0) {
    y_range[2] <- 0
  }

  if (y_range[1] >= 0) {
    y_range[1] <- 0
  }

  # Generate 'pretty' breaks covering the range
  pretty_breaks <- pretty(y_range)

  # Ensure there's a break above the max value
  if (max(pretty_breaks) <= y_range[2] && y_range[2] != 0) {
    pretty_breaks <- c(pretty_breaks, max(pretty_breaks) + 1.1 * diff(pretty_breaks[1:2]))
  }

  # Ensure there's a break below the min value
  if (min(pretty_breaks) >= y_range[1] && y_range[1] != 0) {
    pretty_breaks <- c(min(pretty_breaks) - 1.1 * diff(pretty_breaks[1:2]), pretty_breaks)
  }

  # Return the expanded range for y-limits
  pretty_breaks
}


#' Extract unique years from a dataset
#'
#' This function retrieves unique years from a long-form dataset, allowing
#' you to specify whether to extract numeric years or character-based labels.
#'
#' @param data_long A data frame in long format that contains year information.
#' @param type A string indicating the type of year to extract.
#'        Use "numeric" for numeric years (default) or "character" for
#'        character-based years.
#'
#' @return A vector of unique years, either numeric or character-based,
#'         depending on the `type` argument.
#'
#' @details The function first arranges the dataset by numeric years
#'          (`Years_num`). It then extracts unique years based on the
#'          column chosen according to the `type` parameter:
#'          either numeric (`Years_num`) or character (`Years`).
#'
#' @examples
#' # Example usage with a long-form dataset:
#' # get_years(data_long, type = "numeric")
#' # get_years(data_long, type = "character")
get_years <- function(data_long, type = "numeric") {
  # Ensure type is valid
  if (!type %in% c("numeric", "character")) {
    stop("Invalid type. Please use 'numeric', 'character' or leave it empty.")
  }

  # Arrange the data by Years_num
  data_ordered <- data_long |>
    dplyr::arrange(Years_num)

  # Choose Years column based on the type
  year_column <- ifelse(type == "numeric", "Years_num", "Years")

  # Return unique years
  data_ordered |>
    pull_uniques(year_column)
}


#' Remove Trailing Zeroes from Formatted Numbers
#'
#' This function takes numeric values, formats them using `pretty_num_large()`
#' and removes any trailing zeroes from the decimal part, but only for values
#' greater than zero.
#'
#' @param x A numeric vector to be formatted.
#' @param dp Integer. The default number of decimal places to be used if the
#'   number has decimals. Default is 0.
#' @param ... Additional arguments passed to `pretty_num_large`.
#'
#' @return A character vector with formatted numeric values and no trailing zeroes,
#'         only for values greater than 0.
#'
#' @examples
#' pretty_num_remove_trailing_zeroes(c(1000000, 1234567.8901, 100.0), dp = 3)
#' pretty_num_remove_trailing_zeroes(c(5000000000, 9876543210), dp = 2)
#'
#' @export
pretty_num_remove_zero <- function(x, dp = 2, ...) {
  # Apply pretty_num_large to format the numbers
  formatted_numbers <- pretty_num_large(x, dp = dp, ...)

  # Remove trailing zeroes after decimal point
  if (abs(as.numeric(x)) >= 1 || abs(as.numeric(x)) == 0) {
    formatted_numbers <- sub("\\.0+(?=\\s|$)", "", formatted_numbers, perl = TRUE)
  }

  formatted_numbers
}


#' Format Axes for Plotting
#'
#' This function formats the axes for a ggplot2 plot based on the provided
#' data. It sets the y-axis limits, breaks, and labels, as well as the
#' x-axis breaks, ensuring that the axes are appropriately scaled for
#' the given dataset.
#'
#' The function first calculates the number of years in the dataset using
#' `get_num_years` and then determines the pretty y-gridline breaks using
#' `pretty_y_gridlines`. It then creates a list containing the necessary
#' scale functions for both the y-axis and x-axis.
#'
#' @param data_long A data frame in long format, which contains the data
#'   used for plotting. The structure should include relevant time and
#'   numeric values.
#'
#' @return A list containing the ggplot2 scale functions for formatting
#'   the y-axis and x-axis. This can be directly added to a ggplot object.
#'
#' @examples
#' # Example usage:
#' axes <- format_axes(data_long)
#' ggplot(data_long) +
#'   axes +
#'   geom_line()
format_axes <- function(data_long, indicator_dps = 2) {
  # Get pretty Y-axis breaks
  y_breaks <- pretty_y_gridlines(data_long)

  # Number of breaks for X-axis
  num_years <- get_years(data_long)

  # Get X-axis year labels (these can be non-numeric such as 2019-20)
  year_labels <- get_years(data_long, "character")

  # Check if suffixes consistent
  if (!check_year_suffix_consistency(data_long)) {
    year_labels <- num_years
  }

  # Axes formatting
  list(
    ggplot2::scale_y_continuous(
      limits = range(y_breaks),
      expand = expansion(0, 0),
      breaks = pretty(y_breaks),
      labels = unlist(lapply(pretty(y_breaks), pretty_num_remove_zero, indicator_dps))
    ),
    ggplot2::scale_x_continuous(
      breaks = num_years,
      labels = year_labels,
      guide = guide_axis(check.overlap = TRUE)
    )
  )
}


#' Set Plot Colours for ggplot2
#'
#' This function applies custom colour scales to a ggplot2 plot based on the
#' specified colour type. It allows for manual adjustments to colour and
#' fill aesthetics, and supports highlighting a focus group if desired.
#'
#' The function accepts a `colour_type` parameter to determine whether to
#' apply colour or fill scales, and can adjust the aesthetics for a specific
#' focus group. It generates colour palettes using helper functions tailored
#' to the data provided.
#'
#' @param data_long A data frame in long format, which contains the data
#'   for plotting. The structure should include relevant categorical values
#'   for colours.
#' @param colour_type A character string indicating the type of colour scale
#'   to apply. Options include "colour", "fill", "focus", and "focus-fill".
#' @param focus_group An optional character string specifying the focus group
#'   for which to apply special colour and size adjustments.
#'
#' @return A ggplot2 scale function (or a list of scale functions)
#'   for setting colour or fill aesthetics. This can be directly added to
#'   a ggplot object.
#'
#' @examples
#' # Example usage:
#' colours <- set_plot_colours(data_long, "focus", "Barking and Dagenham")
#' ggplot(data_long) +
#'   colours +
#'   geom_line()
#'
set_plot_colours <- function(data_long,
                             colour_type = "colour",
                             focus_group = NULL) {
  if (colour_type == "colour") {
    ggplot2::scale_colour_manual(values = create_plot_colours(data_long, focus_group))
  } else if (colour_type == "fill") {
    ggplot2::scale_fill_manual(values = create_plot_colours(data_long, focus_group))
  } else if (colour_type == "focus") {
    list(
      ggplot2::scale_color_manual(values = create_focus_plot_colours(data_long, focus_group)),
      ggplot2::scale_size_manual(values = create_focus_plot_sizes(data_long, focus_group))
    )
  } else if (colour_type == "focus-fill") {
    ggplot2::scale_fill_manual(values = create_focus_plot_colours(data_long, focus_group))
  }
}


#' Set Plot Labels for ggplot2
#'
#' This function sets the labels for a ggplot2 plot, including the x-axis,
#' y-axis, and the main title. It retrieves the y-axis title and plot title
#' based on the provided filtered data.
#'
#' The function uses helper functions to determine appropriate titles for
#' the axes and the plot, ensuring that the labels are relevant to the data
#' being visualised.
#'
#' @param filtered_bds A data frame or object containing the filtered data
#'   used for plotting. This should include information necessary to derive
#'   the y-axis and plot titles.
#'
#' @return A `labs` object for ggplot2, containing the specified labels for
#'   the x-axis, y-axis, and title. This can be directly added to a ggplot
#'   object.
#'
#' @examples
#' # Example usage:
#' labs <- set_plot_labs(filtered_bds)
#' ggplot(data) +
#'   labs +
#'   geom_line()
#'
set_plot_labs <- function(filtered_bds) {
  x_title <- get_xaxis_title(filtered_bds)
  y_title <- get_yaxis_title(filtered_bds)
  plot_title <- get_plot_title(filtered_bds)

  ggplot2::labs(
    x = x_title,
    y = y_title,
    title = plot_title
  )
}


#' Create a Custom ggplot2 Theme
#'
#' This function defines a custom theme for ggplot2 plots, enhancing the
#' visual presentation of the charts. It builds upon the minimal theme and
#' applies various aesthetic adjustments for titles, axes, and gridlines.
#'
#' The customisations include centered plot titles, specific angles and
#' adjustments for the y-axis title, and the positioning of the legend. It
#' also modifies the appearance of gridlines for a cleaner look.
#'
#' @return A list of theme elements to be used with ggplot2. This can be
#'   added to a ggplot object to apply the custom styling.
#'
#' @examples
#' # Example usage:
#' ggplot(data) +
#'   geom_line() +
#'   custom_theme()
#'
custom_theme <- function(title_margin = 0) {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      # Keeps title within chart
      plot.title = ggtext::element_textbox(
        hjust = 0.5,
        width = unit(1, "npc"),
        halign = 0.5,
        margin = margin(b = unit(22.5 + 7.5 * title_margin, "lines"))
      ),
      axis.title.x = element_text(
        hjust = 0.5,
        margin = margin(t = 15, r = 0, b = 0, l = 0)
      ),
      axis.title.y = element_text(
        angle = 0,
        vjust = 0.5,
        margin = margin(t = 0, r = 10, b = 0, l = 0)
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid = element_line(colour = get_focus_back_colour()),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  )
}


#' Generate year text for tooltip
#'
#' This function creates a text string indicating the year based on the
#' specified years number and the provided data frame.
#'
#' @param data A data frame containing year information.
#' @param years_num A numeric value representing the year.
#'
#' @return A formatted string with the year information.
#' @export
generate_year_text <- function(data, years_num) {
  glue::glue(
    "Year: {data |>
      dplyr::filter(Years_num == years_num) |>
      get_years(type = 'character')}"
  )
}


#' Generate tooltip text when including measures
#'
#' This function generates a tooltip text string that includes the measures
#' along with the corresponding LA and region values.
#'
#' @param data A data frame containing the measures and values.
#' @param years_num A numeric value representing the year for filtering.
#' @param indicator_dp A numeric value for decimal places to format the values.
#'
#' @return A formatted string containing measures and corresponding values.
#' @export
tooltip_text_w_indicator <- function(data, years_num, indicator_dp, geog_colours) {
  measure_summary <- data |>
    dplyr::filter(Years_num == years_num) |>
    dplyr::arrange(dplyr::desc(values_num)) |>
    pretty_num_table(include_columns = "values_num", dp = indicator_dp) |>
    dplyr::group_by(Measure) |>
    dplyr::summarise(
      tooltip_text = paste0(
        "<span style='color:", geog_colours[`LA and Regions`], ";'>",
        `LA and Regions`, ": ", values_num, "</span>",
        collapse = "\n"
      ),
      .groups = "drop"
    )

  glue::glue_data(
    measure_summary,
    "{Measure}:\n  {tooltip_text}",
    .sep = "\n"
  ) |>
    paste(collapse = "\n\n")
}


#' Generate Tooltip Text with Conditional Formatting
#'
#' This function generates formatted tooltip text for a given dataset, including
#' optional styling for a specified geography (focus group), with special styling
#' for "England". The tooltip can be customized to display values and other details
#' for each geographical region in the dataset.
#'
#' @param data A data frame containing the dataset with values to be displayed
#'   in the tooltip.
#' @param years_num Numeric. The year to filter the data by.
#' @param indicator_dp Numeric. The number of decimal places for formatting the
#'   values.
#' @param highlight_geography Character or NULL. The geography to be highlighted
#'   in the tooltip. If NULL, no specific geography is highlighted.
#' @param focus_colour Character. The colour to use for highlighting the specified
#'   geography (highlight_geography). Defaults to a color retrieved by
#'   `get_selected_la_colour()`.
#'
#' @return A character string representing the formatted tooltip text for each
#'   region. Includes the value for each region, with conditional styling for
#'   "England" and the specified highlight geography.
#'
#' @details The function formats the tooltip for each region, and applies bold
#'   styling and colours for the specified `highlight_geography` and "England".
#'   The tooltip text includes the region name and the corresponding value, with
#'   specific styling applied based on the geography.
#'
#' @examples
#' # Generate tooltip text for a dataset with focus on "Barnet" and bold red for "England"
#' tooltip_text(
#'   data = my_data, years_num = 2022, indicator_dp = 2,
#'   highlight_geography = "Barnet", focus_colour = "red"
#' )
#'
#' # Generate tooltip text without any highlighted geography
#' tooltip_text(data = my_data, years_num = 2022, indicator_dp = 2)
#'
tooltip_text <- function(data, years_num, indicator_dp, focus_geog = NULL, geog_colours) {
  data_clean <- data |>
    dplyr::filter(Years_num == years_num) |>
    dplyr::arrange(dplyr::desc(values_num)) |>
    pretty_num_table(include_columns = "values_num", dp = indicator_dp)

  tooltip_lines <- apply(data_clean, 1, function(row) {
    geography <- row["LA and Regions"]
    value <- row["values_num"]

    text_colour <- if (!is.null(focus_geog)) {
      if (geography == focus_geog) get_focus_front_colour() else get_gov_secondary_text_colour()
    } else {
      geog_colours[geography]
    }

    weight <- if (text_colour %in% c(get_selected_la_colour(), get_focus_front_colour())) "font-weight: bold;" else ""

    paste0("<span style='color:", text_colour, "; ", weight, "'>", geography, ": ", value, "</span>")
  })

  paste(tooltip_lines, collapse = "\n")
}



#' Add an Interactive Vertical Line with Tooltips
#'
#' Creates an interactive vertical line at a specified `x` position in the plot,
#' with tooltip content that includes the year and optionally the measure value.
#' The line can highlight a specified focus group with a custom color.
#'
#' @param x Numeric. The x-coordinate for the vertical line, e.g., a specific year.
#' @param data Data frame. The dataset used for plotting, containing columns
#'   relevant for generating tooltip content.
#' @param indicator_dp Integer, default = 1. Number of decimal places to display
#'   in tooltip values.
#' @param focus_group Character, optional. Name of the group to highlight in the
#'   tooltip, if present in the data.
#' @param focus_colour Character, default = `get_selected_la_colour()`. The color
#'   to apply to `focus_group` if it appears in tooltips.
#' @param include_measure Logical, default = FALSE. If TRUE, adds the measure
#'   value to the tooltip text.
#'
#' @details Applies conditional styling to highlight `focus_group` using
#'   `focus_colour` if specified. The tooltip displays the year and group value,
#'   optionally including the measure name. If no matching data is found for `x`,
#'   the function gracefully handles missing information.
#'
#' @return A `geom_vline_interactive` object, with custom tooltips and
#'   interactive hover effects.
#' @export
#'
#' @examples
#' tooltip_vlines(
#'   x = 2021, data = my_data, indicator_dp = 1,
#'   focus_group = "England", focus_colour = "blue",
#'   include_measure = TRUE
#' )
#'
tooltip_vlines <- function(x,
                           data,
                           indicator_dp = 1,
                           focus_geog = NULL,
                           include_measure = FALSE) {
  year_text <- generate_year_text(data, x)
  geog_colours <- create_plot_colours(data)

  tooltip_content <- if (include_measure) {
    tooltip_text_w_indicator(data, x, indicator_dp, geog_colours)
  } else {
    tooltip_text(data, x, indicator_dp, focus_geog, geog_colours)
  }

  geom_vline_interactive(
    xintercept = x,
    data_id = x,
    tooltip = paste(year_text, tooltip_content, sep = "\n\n"),
    hover_nearest = TRUE,
    linetype = "dashed",
    size = 2.5,
    color = "transparent"
  )
}


#' Generate Custom Tooltip Text for Bar Chart with Conditional Formatting
#'
#' This function generates tooltip text for each row of data to display in
#' an interactive bar chart, with conditional color formatting for specific
#' geographic groups such as "England" or a specified `focus_group`.
#'
#' @param data Data frame. The dataset used for plotting, containing columns
#'   relevant for tooltip content such as `LA and Regions`, `values_num`, and
#'   `Years`.
#' @param indicator_dps Integer. Number of decimal places to display in
#'   the `values_num` column.
#' @param focus_group Character, optional. Name of a specific group to highlight
#'   in the tooltip if present.
#' @param text_colour Character, default = `get_selected_la_colour()`. Color to
#'   apply to `focus_group` if it appears in tooltips.
#' @param include_measure Logical, default = FALSE. If TRUE, includes the
#'   measure name in the tooltip if the `Measure` column exists in `data`.
#'
#' @details This function processes each row of data to create tooltip text.
#'   The tooltip text includes the year and values, with optional inclusion
#'   of the measure name. Conditional styling is applied to `focus_group`
#'   and the "England" group, using the provided color or `get_england_colour()`
#'   for "England". The `indicator_dps` parameter controls the decimal precision
#'   of `values_num` in the tooltip.
#'
#' @return A character vector of tooltip text for each row in `data`.
#' @export
#'
#' @examples
#' tooltip_bar(
#'   data = my_data, indicator_dps = 1, focus_group = "England",
#'   text_colour = "blue", include_measure = TRUE
#' )
#'
tooltip_bar <- function(data,
                        indicator_dps,
                        focus_geog = NULL,
                        include_measure = FALSE) {
  # Prepare data with formatted numbers
  data_clean <- data |>
    pretty_num_table(include_columns = "values_num", dp = indicator_dps)

  # Get plot colours for geogs
  geog_colours <- create_plot_colours(data)

  apply(data_clean, 1, function(row) {
    geography <- row["LA and Regions"]
    value <- row["values_num"]
    year <- row["Years"]

    # Tooltip content
    measure_text <- if (include_measure && "Measure" %in% names(row)) {
      paste0("Measure: ", row["Measure"], "\n")
    } else {
      ""
    }

    # Apply colour formatting (set focus plot colours)
    text_colour <- if (!is.null(focus_geog)) {
      if (geography == focus_geog) get_focus_front_colour() else "black"
    } else {
      geog_colours[geography]
    }

    weight <- ifelse(
      text_colour %in% c(get_selected_la_colour(), get_focus_front_colour()) & !include_measure,
      "bold",
      "normal"
    )

    paste0(
      measure_text,
      "Year: ", year, "\n\n",
      "<span style='color:", text_colour, "; font-weight: ", weight, ";'>",
      geography, ": ", value, "</span>"
    )
  })
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
#' customising tooltips and toolbars in `ggiraph` visualisations.
#' It is designed to provide a set of default settings that can be easily
#' extended or overridden.
#'
#' @param ... Additional options to be passed to `ggiraph` functions.
#' These options will be added to the default options provided by this function.
#'
#' @return A list of `ggiraph` options, including tooltip styling,
#' toolbar position, and customisation for PNG download functionality.
#'
#' @details
#' The default options generated by this function include:
#' - Tooltip customisation via `ggiraph::opts_tooltip()`,
#' with a custom CSS styling and full opacity.
#' - Toolbar customisation via `ggiraph::opts_toolbar()`,
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
      hidden = c("selection", "zoom", "misc")
    ),
    ...
  )
}


#' Reorder Local Authority (LA) Regions in a Dataset
#'
#' This function reorders the `LA and Regions` column of a given dataset based
#' on a specified factor order.
#' It uses `forcats::fct_relevel` to reorder the factor levels and then
#' arranges the dataset accordingly.
#'
#' @param chart_data A data frame that contains the `LA and Regions` column.
#' @param factor_order A vector specifying the desired order
#' of `LA and Regions`.
#' @param ... Additional arguments passed to `forcats::fct_relevel`,
#' such as `after = Inf`.
#'
#' @return A reordered data frame where the `LA and Regions` column is
#' arranged according to the specified order.
#'
#' @import dplyr
#' @import forcats
#'
#' @examples
#' chart_data <- data.frame(`LA and Regions` = c(
#'   "London", "East Midlands",
#'   "North West"
#' ))
#' factor_order <- c("London", "North West", "East Midlands")
#' reordered_data <- reorder_la_regions(chart_data, factor_order)
#' print(reordered_data)
#'
reorder_la_regions <- function(chart_data, factor_order = NULL, reverse = FALSE, ...) {
  chart_data |>
    dplyr::mutate(
      `LA and Regions` = forcats::fct_relevel(`LA and Regions`, factor_order, ...),
      `LA and Regions` = if (reverse) forcats::fct_rev(`LA and Regions`) else `LA and Regions`
    ) |>
    dplyr::arrange(`LA and Regions`)
}


#' Create a Named Vector for Color Mapping in Charts
#'
#' This function creates a named vector for color mapping based on a set
#' of chart groups and the type of chart (line or bar).
#' It uses the `afcolours::af_colours` palette to generate a categorical
#' color palette and assigns colors to each group.
#' Depending on the chart type, it returns the appropriate color scale function
#' for use in ggplot (either `scale_colour_manual` for line charts or
#' `scale_fill_manual` for bar charts).
#'
#' @param chart_groups A character vector of groups (e.g., `LA and Regions`)
#' that require color mapping.
#' @param type A string indicating the chart type. Use `"line"` for line charts
#' or `"bar"` for bar charts.
#'
#' @return A `ggplot2` scale function (`scale_colour_manual` for line charts or
#' `scale_fill_manual` for bar charts) that maps the specified groups
#' to corresponding colors.
#'
#' @import ggplot2
#' @import afcolours
#'
#' @examples
#' chart_groups <- c("London", "North East", "South East")
#' color_scale <- manual_colour_mapping(chart_groups, type = "line")
#' # Use the `color_scale` in a ggplot plot
#'
manual_colour_mapping <- function(chart_groups, type) {
  # Get the required number of colors from af_colours
  colour_values <- afcolours::af_colours(
    type = "categorical",
    n = 4
  )[seq_along(chart_groups)]

  # Create a named vector of colors corresponding to the groups
  colour_mapping <- setNames(colour_values, chart_groups)

  # Whether line or bar chart
  if (type == "line") {
    ggplot2::scale_colour_manual(values = colour_mapping)
  } else if (type == "bar") {
    ggplot2::scale_fill_manual(values = colour_mapping)
  }
}


#' Display a "No Data" Plot with Custom Message
#'
#' This function generates a placeholder plot displaying a custom message when
#' no data is available for plotting. The message is centered within a blank
#' plot to inform the user that data is missing, with a prompt to report any
#' discrepancies.
#'
#' @param label Character. A custom message to display on the plot. Defaults to
#'   "No plot due to no available data." Additional instructions are included
#'   in the message to prompt user feedback if they believe data should be
#'   present.
#'
#' @details The function creates an empty plot using `ggplot2`, removing any
#'   axis lines, gridlines, or other plot elements to focus solely on the
#'   message text. The plot includes padding for better readability and centers
#'   the text both horizontally and vertically.
#'
#' @return A ggplot object with the custom "no data" message displayed.
#' @export
#'
#' @examples
#' # Display a plot with the default "no data" message
#' display_no_data_plot()
#'
#' # Display a plot with a custom message
#' display_no_data_plot(label = "No data available for this indicator.")
#'
display_no_data_plot <- function(label = "No plot due to no available data.") {
  error_plot <- ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      # Position at the center of the plot
      label = paste0(
        label,
        "\n",
        "If you think this is incorrect, ",
        "please report so in the feedback form."
      ),
      size = 6,
      color = "red",
      hjust = 0.5,
      vjust = 0.5
    ) +
    theme_void() + # Remove axis, gridlines, etc.
    theme(plot.margin = margin(50, 50, 50, 50)) + # Add some padding to the text
    coord_cartesian(clip = "off")

  error_plot
}


#' Create Show Points for COVID-Affected Data
#'
#' This function processes data to identify key points to display on a plot
#' when handling COVID-affected data, such as the first and last non-missing
#' values before and after missing periods (2019–2021).
#'
#' @param data A data frame or tibble containing the dataset with columns
#'   `LA and Regions`, `Years_num`, and `values_num`. Optionally, it may
#'   include a `Measure` column for custom groupings.
#' @param covid_affected A logical vector indicating whether the data is
#'   affected by COVID (e.g., missing values in 2019–2021).
#'
#' @details
#' This function:
#' - Groups the data by `LA and Regions` and `Measure` (if present).
#' - Identifies the first and last missing (`NA`) values in the period
#'   2019–2021 due to COVID-19.
#' - Marks the last non-missing value before the first COVID-affected year
#'   and the first non-missing value after the last COVID-affected year.
#' - Identifies isolated points for better visualisation.
#' - Removes intermediate helper columns used for processing.
#'
#' @return A tibble with the same structure as the input, augmented with a
#'   `show_point` column, which is a logical flag indicating whether a
#'   specific row should be highlighted in the plot.
#'
#' @examples
#' # Example dataset
#' data <- tibble::tibble(
#'   `LA and Regions` = rep("Region A", 10),
#'   Years_num = 2015:2024,
#'   values_num = c(1:5, NA, NA, 8:10)
#' )
#' covid_affected <- rep(TRUE, 10)
#'
#' # Process data
#' result <- create_show_point(data, covid_affected)
#'
create_show_point <- function(data, covid_affected_data, selected_indicators) {
  # Check if all indicators affected by COVID
  all_covid_affected <- all(
    covid_affected_data |>
      pull_uniques("Measure") %in% selected_indicators
  )

  data |>
    dplyr::group_by(
      `LA and Regions`,
      # Groupby Measure if it exists (for create your own)
      !!!rlang::syms(if ("Measure" %in% colnames(data)) "Measure" else NULL)
    ) |>
    dplyr::arrange(`LA and Regions`, Years_num) |>
    dplyr::mutate(
      # Helper: Is the current value NA
      is_na = is.na(values_num),

      # First COVID affected year (First NA within 2019–2021)
      is_first_covid_na = (Years_num >= 2019 & Years_num <= 2021) &
        is_na & dplyr::lag(!is_na, default = FALSE),

      # Last COVID affected year (Last NA within 2019–2021)
      is_last_covid_na = (Years_num >= 2019 & Years_num <= 2021) &
        is_na & dplyr::lead(!is_na, default = FALSE),

      # Finds the last non-NA before first COVID (show point)
      is_prev_covid = dplyr::lead(is_first_covid_na, default = FALSE),
      # Finds the first non-NA after last COVID (show point)
      is_post_covid = dplyr::lag(is_last_covid_na, default = FALSE),

      # General NA show point conditions to show isolated points
      show_point = dplyr::if_else(
        # Isolated in middle of plot
        (dplyr::lag(is_na) & dplyr::lead(is_na)) |
          # Isolated at start of plot
          (dplyr::row_number() == 1 & dplyr::lead(is_na)) |
          # Isolated at end of plot
          (dplyr::row_number() == dplyr::n() & dplyr::lag(is_na)) |
          # Covid start and end points
          # (uses all for multiple indicators in create your own)
          (all_covid_affected & is_prev_covid) |
          (all_covid_affected & is_post_covid),
        TRUE,
        FALSE
      )
    ) |>
    dplyr::ungroup() |>
    # Clean up - remove uneeded cols
    dplyr::select(-dplyr::starts_with("is_"))
}


#' Calculate Data for COVID Plot (Break in Timeseries)
#'
#' This function identifies periods in the data affected by missing values
#' during the COVID-19 pandemic (2019–2021) and prepares the data required
#' to visualise these periods on a plot.
#'
#' @param data A data frame or tibble containing the dataset, including columns
#'   `Years_num` (years), `values_num` (values), and optionally `Measure`
#'   (for custom groupings in "Create Own" charts).
#' @param covid_affected A logical vector indicating whether the data is
#'   affected by COVID-19 (e.g., missing values in 2019–2021).
#' @param chart_type A string specifying the type of chart: either `"line"` or
#'   `"bar"`. This determines whether to apply an offset to align with
#'   non-missing points.
#'
#' @details
#' The function:
#' - Filters rows with missing (`NA`) values in `values_num` during 2019–2021.
#' - Calculates start and end years of the missing period and adjusts these
#'   with an offset for line charts.
#' - Groups by `Measure` if applicable, allowing for multiple indicators in
#'   custom charts.
#' - Checks whether the missing period is consistent across all indicators.
#' - Generates appropriate labels and returns a data structure for plotting.
#'
#' If `chart_type` is `"bar"` and `Measure` is present, it returns grouped data
#' to support `facet_wrap`. Otherwise, it returns shared COVID-affected period
#' data for the entire dataset.
#'
#' @return A tibble with columns:
#' - `start_year`: Start of the COVID-affected period.
#' - `end_year`: End of the COVID-affected period.
#' - `label_x`: Position for the label.
#' - `vertical_lines`: Years to draw vertical lines (if applicable).
#' - `label`: Text label explaining the missing data.
#'
#' Returns `NULL` if `covid_affected` is `FALSE`.
#'
#' @examples
#' # Example dataset
#' data <- tibble::tibble(
#'   Years_num = c(2018, 2019, 2020, 2021, 2022),
#'   values_num = c(10, NA, NA, 15, 20),
#'   Measure = rep("Example Measure", 5)
#' )
#' covid_affected <- rep(TRUE, 5)
#'
#' # Line chart example
#' covid_plot_data <- calculate_covid_plot(data, covid_affected, "line")
#'
calculate_covid_plot <- function(data, covid_affected_data, selected_indicators, chart_type) {
  # Check if measures affected by COVID
  covid_affected <- covid_affected_data |>
    dplyr::filter(Measure %in% selected_indicators)

  if (nrow(covid_affected) > 0) {
    # Group/ filter by `Measure` if it exists (for Create Own charts - multiple indicators)
    grouping_vars <- if ("Measure" %in% colnames(data)) "Measure" else NULL

    # Join covid data to find the NA years due to COVID
    na_rows <- data |>
      dplyr::inner_join(
        covid_affected |> dplyr::select(Measure, Years_num),
        by = c(grouping_vars, "Years_num")
      )

    # Whether to offset vline to last/next non-NA point (for line chart)
    yr_offset <- ifelse(chart_type == "line", 1, 0)

    # Find missing COVID period and calculate label position
    # (by indicator for Create Own)
    na_periods <- na_rows |>
      dplyr::group_by(!!!rlang::syms(grouping_vars)) |>
      dplyr::summarise(
        start_year = min(Years_num) - yr_offset,
        end_year = max(Years_num) + yr_offset,
        label_x = (min(Years_num) - yr_offset + max(Years_num) + yr_offset) / 2,
        label = "No data\ndue to COVID",
        .groups = "drop"
      )

    # Check if all indicators share the same period (for Create Own)
    shared_period <- na_periods |>
      dplyr::summarise(
        same_period = all(
          length(unique(start_year)) == 1,
          length(unique(end_year)) == 1
        ),
        start_year = min(start_year),
        end_year = max(end_year),
        label_x = (min(start_year) + max(end_year)) / 2
      )

    # Check if all indicators affected by COVID
    all_covid_affected <- all(
      covid_affected |>
        pull_uniques("Measure") %in% selected_indicators
    )

    # Set label based on whether the COVID period is the same across all indicators
    if (shared_period$same_period && all_covid_affected) {
      shared_period$label <- "No data\ndue to COVID"
    } else {
      shared_period$label <- "Some indicators have\nmissing data due to COVID"
    }

    # If Create Own bar chart then use grouped COVID period data (for facet_wrap)
    if (!is.null(grouping_vars) && chart_type == "bar") {
      na_periods
    } else if (!is.null(shared_period)) {
      # Otherwise, return the shared COVID period (or individual if only one indicator)
      tibble::tibble(
        xmin = shared_period$start_year,
        xmax = shared_period$end_year,
        label_x = shared_period$label_x,
        vertical_lines = c(shared_period$start_year, shared_period$end_year),
        label = shared_period$label
      )
    }
  } else {
    # Return nothing if not affected by COVID
    NULL
  }
}


#' Add COVID Plot Elements to a ggplot Object
#'
#' This function generates ggplot elements to visually indicate periods
#' affected by missing data during the COVID-19 pandemic. It includes options
#' for vertical lines, explanatory labels, and optionally a shaded box.
#'
#' @param covid_plot_data A data frame or tibble containing information about
#'   COVID-affected periods, with columns such as:
#'   - `start_year`, `end_year`: Start and end years of the COVID-affected
#'     period.
#'   - `vertical_lines`: Years to draw vertical lines (alternative format).
#'   - `label_x`: Horizontal position for the explanatory label.
#'   - `label`: Text to display in the label.
#'   - Optionally, `xmin` and `xmax` for shaded box boundaries.
#' @param include_shaded_box A logical value indicating whether to include a
#'   shaded rectangle to highlight the COVID-affected period
#'   (`FALSE` by default).
#'
#' @details
#' The function:
#' - Adds dashed vertical lines at `start_year` and `end_year` to mark the
#'   boundaries of COVID-affected periods.
#' - Optionally adds vertical lines using the `vertical_lines` column, if present.
#' - Includes an explanatory label positioned at `label_x` and the top of the
#'   plot, with customisable text from the `label` column.
#' - Optionally adds a shaded rectangle between `xmin` and `xmax` if
#'   `include_shaded_box` is `TRUE`.
#' - Ensures that plot elements outside the plot area are not clipped and adds
#'   padding around the plot.
#'
#' @return A list of ggplot2 elements representing the COVID-related annotations.
#'   Returns `NULL` if `covid_plot_data` is `NULL`.
#'
#' @examples
#' # Example dataset for COVID plot data
#' covid_plot_data <- tibble::tibble(
#'   start_year = 2019,
#'   end_year = 2021,
#'   label_x = 2020,
#'   label = "No data\ndue to COVID",
#'   xmin = 2019,
#'   xmax = 2021
#' )
#'
#' # Add COVID elements without a shaded box
#' covid_elements <- add_covid_elements(covid_plot_data, include_shaded_box = FALSE)
#'
#' # Add COVID elements with a shaded box
#' covid_elements_with_box <- add_covid_elements(covid_plot_data, include_shaded_box = TRUE)
#'
add_covid_elements <- function(covid_plot_data, include_shaded_box = FALSE) {
  if (!is.null(covid_plot_data)) {
    elements <- list()

    # Add vertical lines for COVID periods (mainly for Create Own Bar - facet_wrap)
    if ("start_year" %in% colnames(covid_plot_data) && "end_year" %in% colnames(covid_plot_data)) {
      elements <- append(elements, list(
        ggplot2::geom_vline(
          data = covid_plot_data,
          ggplot2::aes(xintercept = start_year),
          linetype = "dashed",
          color = "grey50",
          alpha = 0.5,
          linewidth = 0.3
        ),
        ggplot2::geom_vline(
          data = covid_plot_data,
          ggplot2::aes(xintercept = end_year),
          linetype = "dashed",
          color = "grey50",
          alpha = 0.5,
          linewidth = 0.3
        )
      ))
    }

    # Add vertical lines for COVID periods (for all other line and bar charts)
    if ("vertical_lines" %in% colnames(covid_plot_data)) {
      elements <- append(elements, list(
        ggplot2::geom_vline(
          data = covid_plot_data,
          ggplot2::aes(xintercept = vertical_lines),
          linetype = "dashed",
          color = "grey50",
          alpha = 0.5,
          linewidth = 0.3
        )
      ))
    }

    # Add a label to explain COVID impact
    elements <- append(
      elements,
      list(
        ggplot2::geom_text(
          data = covid_plot_data,
          ggplot2::aes(
            x = label_x, # Centered horizontally in the shaded region
            y = Inf, # Positioned at the top of the plot
            label = label
          ),
          vjust = 0.5, # Move label slightly higher
          color = "black",
          size = 4,
          fontface = "italic",
          inherit.aes = FALSE
        ),
        # Ensure the plot does not clip elements outside the plot area
        ggplot2::coord_cartesian(clip = "off"),
        # Add padding around the plot
        ggplot2::theme(
          plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
        )
      )
    )

    # Optionally add a shaded box to show COVID-affected periods
    if (include_shaded_box) {
      elements <- append(
        elements,
        ggplot2::geom_rect(
          data = covid_plot_data,
          ggplot2::aes(
            xmin = xmin,
            xmax = xmax,
            ymin = -Inf,
            ymax = Inf
          ),
          fill = "grey",
          alpha = 0.1,
          inherit.aes = FALSE
        )
      )
    }

    # Return COVID plotting elements
    return(elements)
  } else {
    return(NULL)
  }
}


#' Calculate padded range for a specified variable
#'
#' This function calculates the minimum and maximum values of a specified
#' variable in the data, and then adds padding to the range for use in plotting.
#' It is useful when adjusting axis limits for visual clarity in plots.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param var The variable for which the range is calculated. Should be
#'   specified in the form of an unquoted variable name (e.g., `var = x`).
#' @param padding A numeric value specifying the amount of padding to be added
#'   to the min and max values. Default is 0.2.
#'
#' @return A numeric vector of length 2, representing the padded range
#'   of the specified variable.
#' @examples
#' # Example usage:
#' thin_bar(my_data, var = my_var, padding = 0.5)
#' @export
thin_bar <- function(data, var, padding = 0.2) {
  # Pull the variable data
  var_data <- dplyr::pull(data, {{ var }})

  # Calculate min and max values
  x_min <- min(var_data, na.rm = TRUE)
  x_max <- max(var_data, na.rm = TRUE)

  # Return the range with padding as a two-element vector
  c(x_min - padding, x_max + padding)
}
