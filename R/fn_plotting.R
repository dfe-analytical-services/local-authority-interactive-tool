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
#' multiple unique values, a generic "Plain Years" label is used.
#'
#' @param data_full A data frame containing the `Year_Type` column, which will
#'   be used to determine the X-axis title.
#'
#' @return A character string representing the X-axis title. This can either
#'   be the value of `Year_Type` formatted with line breaks or the string
#'   "Plain Years" if there are multiple unique values.
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
  if (length(x_axis_title) == 1) {
    add_line_breaks(x_axis_title)
  } else {
    "Plain Years"
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
    plot_colours[focus_group] <- get_la_focus_colour()
    plot_groups <- setdiff(plot_groups, focus_group)
  }

  # Assign specific colour for "England" if present & remove from plot groups
  if ("England" %in% plot_groups) {
    plot_colours["England"] <- get_england_colour()
    plot_groups <- setdiff(plot_groups, "England")
  }

  # Assign colours for remaining groups
  remaining_colours <- get_clean_af_colours()[1:length(plot_groups)]
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
#'   visualization.
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
format_axes <- function(data_long) {
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
      labels = unlist(lapply(pretty(y_breaks), dfeR::pretty_num))
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
custom_theme <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      # Keeps title within chart
      plot.title = ggtext::element_textbox(
        hjust = 0.5,
        width = unit(0.9, "npc"),
        halign = 0.5,
        margin = margin(b = unit(10, "lines"))
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
      panel.grid = element_line(colour = "#D9D9D9"),
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
tooltip_text_w_indicator <- function(data, years_num, indicator_dp) {
  measure_summary <- data |>
    pretty_num_table(include_columns = "values_num", dp = indicator_dp) |>
    dplyr::filter(Years_num == years_num) |>
    dplyr::group_by(Measure) |>
    dplyr::summarise(
      tooltip_text = paste(
        paste0(`LA and Regions`, ": ", values_num),
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
#'   `get_la_focus_colour()`.
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
tooltip_text <- function(data,
                         years_num,
                         indicator_dp,
                         highlight_geography = NULL,
                         focus_colour) {
  data_clean <- data |>
    pretty_num_table(include_columns = "values_num", dp = indicator_dp) |>
    dplyr::filter(Years_num == years_num) |>
    dplyr::arrange(dplyr::desc(values_num))

  # Create formatted tooltip text
  tooltip_lines <- sapply(1:nrow(data_clean), function(i) {
    row <- data_clean[i, ]
    geography <- row$`LA and Regions`
    value <- row$values_num

    # Apply styling for highlighted geography
    if (!is.null(highlight_geography) &&
      geography == highlight_geography) {
      paste0(
        "<span style='color:",
        focus_colour,
        "; font-weight: bold;'>",
        geography,
        ": ",
        value,
        "</span>"
      )
      # Apply specific styling for "England" if present
    } else if (geography == "England") {
      paste0(
        "<span style='color:",
        get_england_colour(),
        "; font-weight: bold;'>",
        geography,
        ": ",
        value,
        "</span>"
      )
    } else {
      paste0(geography, ": ", value)
    }
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
#' @param focus_colour Character, default = `get_la_focus_colour()`. The color
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
                           focus_group = NULL,
                           focus_colour = get_la_focus_colour(),
                           include_measure = FALSE) {
  year_text <- generate_year_text(data, x)

  tooltip_content <- if (include_measure) {
    tooltip_text_w_indicator(data, x, indicator_dp)
  } else {
    tooltip_text(data, x, indicator_dp, focus_group, focus_colour)
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
#' @param text_colour Character, default = `get_la_focus_colour()`. Color to
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
                        focus_group = NULL,
                        text_colour = get_la_focus_colour(),
                        include_measure = FALSE) {
  # Prepare data with specified decimal points
  data_clean <- data |>
    pretty_num_table(include_columns = "values_num", dp = indicator_dps)

  # Generate tooltip text for each row of data
  sapply(1:nrow(data_clean), function(i) {
    row <- data_clean[i, ]
    geography <- row$`LA and Regions`
    value <- row$values_num
    year <- row$Years
    measure_text <- ""

    # Conditionally add Measure to the tooltip if include_measure is TRUE
    if (include_measure && "Measure" %in% names(row)) {
      measure_text <- paste0("Measure: ", row$Measure, "\n")
    }

    # Create formatted tooltip text with colour formatting for "England" and focus_group
    tooltip_text <- paste0(
      measure_text,
      "Year: ",
      year,
      "\n\n",
      ifelse(
        geography == "England",
        paste0(
          "<span style='color:",
          get_england_colour(),
          "; font-weight: bold;'>",
          geography,
          ": ",
          value,
          "</span>"
        ),
        ifelse(
          !is.null(focus_group) && geography == focus_group,
          paste0(
            "<span style='color:",
            text_colour,
            "; font-weight: bold;'>",
            geography,
            ": ",
            value,
            "</span>"
          ),
          paste0(geography, ": ", value)
        )
      )
    )

    tooltip_text
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
reorder_la_regions <- function(chart_data, factor_order, ...) {
  chart_data |>
    dplyr::mutate(
      `LA and Regions` = forcats::fct_relevel(`LA and Regions`, factor_order, ...)
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


#' Identify Data Points Surrounded by NAs
#'
#' Adds a `show_point` column to indicate if a data point in `values_num` is
#' surrounded by `NA` values or is the first/last point in a group where the
#' adjacent point is `NA`. Groups are based on `LA and Regions`.
#'
#' @param data A dataframe containing `values_num` and `LA and Regions` columns.
#'
#' @return A dataframe with an additional logical column `show_point`, set to
#' `TRUE` if the point meets conditions to be highlighted, `FALSE` otherwise.
#'
#' @details The `show_point` column is `TRUE` if the `values_num` entry is:
#' - Surrounded by `NA` values on both sides.
#' - The first or last non-NA value within its group with an adjacent `NA`.
#'
#' @examples
#' df <- create_show_point(data)
#'
#' @export
create_show_point <- function(data) {
  data |>
    dplyr::group_by(`LA and Regions`) |>
    dplyr::mutate(
      show_point = dplyr::if_else(
        (is.na(dplyr::lag(values_num)) & is.na(dplyr::lead(values_num))) |
          (dplyr::row_number() == 1 & is.na(dplyr::lead(values_num))) |
          (dplyr::row_number() == dplyr::n() & is.na(dplyr::lag(values_num))),
        TRUE,
        FALSE
      )
    ) |>
    dplyr::ungroup()
}
