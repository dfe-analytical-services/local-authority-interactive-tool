# -----------------------------------------------------------------------------
# This is the plotting.R file.
#
# This is where we've stored the functions for creating the plots in the app.
#
# It is up to you whether you put all plots in this script, move the plots to
# the helper_functions.R script or have a multiple scripts or even a folder of
# scripts that contain your custom plotting functions.
# -----------------------------------------------------------------------------

# Revenue balance time series line chart --------------------------------------
create_avg_rev_timeseries <- function(df, input_area) {
  ggplot2::ggplot(df, aes(
    x = year,
    y = average_revenue_balance,
    color = area_name,
    id = area_name
  )) +
    ggiraph::geom_line_interactive(size = 1) +
    ggiraph::geom_point_interactive(
      aes(
        tooltip = paste0(
          "<p><b>", area_name, ", ", year, "</b></p>",
          unlist(lapply(average_revenue_balance, dfeR::pretty_num, prefix = "£")), "</p>"
        )
      ),
      size = 0.5
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(
        angle = 0, vjust = 0.5,
        margin = margin(r = 12)
      ),
      axis.line = element_line(linewidth = 0.75),
      legend.position = "top"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 1, big = ",", prefix = "£")
    ) +
    ggplot2::xlab("Academic year end") +
    ggplot2::ylab(stringr::str_wrap("Average revenue balance", 16)) +
    ggplot2::scale_color_manual(
      "Area",
      breaks = unique(c("England", input_area)),
      values = gss_colour_pallette
    )
}

# Revenue balance bar chart ---------------------------------------------------
plot_avg_rev_benchmark <- function(df_revenue_balance, input_area) {
  ggplot2::ggplot(df_revenue_balance, aes(
    x = stringr::str_wrap(area_name, width = 12),
    y = average_revenue_balance,
    fill = area_name,
    id = area_name,
    tooltip = paste(
      "<p><b>", area_name, "</b></p>",
      unlist(lapply(average_revenue_balance, dfeR::pretty_num, prefix = "£")), "</p>"
    )
  )) +
    ggiraph::geom_col_interactive() +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        angle = 0, vjust = 0.5,
        margin = margin(r = 12)
      ),
      axis.line = element_line(linewidth = 0.75),
      legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 1, big = ",", prefix = "£")
    ) +
    ggplot2::xlab("Area") +
    ggplot2::ylab(stringr::str_wrap("Average revenue balance", 12)) +
    ggplot2::scale_fill_manual(
      "Area",
      breaks = unique(df_revenue_balance$area_name),
      values = gss_colour_pallette
    )
}
