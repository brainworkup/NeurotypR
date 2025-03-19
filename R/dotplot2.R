#' Create Dotplot for Neurocognitive Domains
#' This function generates a dotplot for neurocognitive and neurobehavioral
#' domains.
#' @param data The dataset or df containing the data for the dotplot.
#' @param x The column name in the data frame for the x-axis variable, typically
#'   the mean z-score for a cognitive domain.
#' @param y The column name in the data frame for the y-axis variable, typically
#'   the cognitive domain to plot.
#' @param linewidth The width of the line, Default: 0.5
#' @param fill The fill color for the points, Default: x-axis variable
#' @param shape The shape of the points, Default: 21
#' @param point_size The size of the points, Default: 6
#' @param line_color The color of the lines, Default: 'black'
#' @param colors A vector of colors for fill gradient, Default: NULL (uses
#'   pre-defined color palette)
#' @param theme The ggplot theme to be used, Default: 'fivethirtyeight'. Other
#'   options include 'minimal' and 'classic'
#' @param return_plot Whether to return the plot object, Default: TRUE
#' @param filename The filename to save the plot to, Default: NULL
#' @param ... Additional arguments to be passed to the function.
#' @return An object of class 'ggplot' representing the dotplot.
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot geom_segment aes geom_point scale_fill_gradientn theme element_rect ggsave
#' @importFrom ggthemes theme_fivethirtyeight
#' @importFrom ggtext element_markdown
#' @importFrom tibble tibble
#' @importFrom highcharter list_parse
#' @rdname dotplot2
#' @export
dotplot2 <- function(
    data,
    x,
    y,
    linewidth = 0.5,
    fill = x,
    shape = 21,
    point_size = 6,
    line_color = "black",
    colors = NULL,
    theme = "fivethirtyeight",
    return_plot = NULL,
    filename = NULL,
    ...) {
  # Define the color palette
  color_palette <- if (is.null(colors)) {
    c(
      "#7E1700",
      "#832504",
      "#883008",
      "#8E3B0B",
      "#92450F",
      "#984E14",
      "#9C5717",
      "#A05F1B",
      "#A4671E",
      "#A86F22",
      "#AD7826",
      "#B0802B",
      "#B58A30",
      "#B99336",
      "#BD9C3D",
      "#C2A647",
      "#C7B051",
      "#CBBA5D",
      "#CEC56C",
      "#D0CE7A",
      "#D2D78A",
      "#D1DE98",
      "#CFE4A6",
      "#CBE7B3",
      "#C4EABD",
      "#BCEAC6",
      "#B2E8CD",
      "#A7E6D2",
      "#9BE2D4",
      "#8EDDD7",
      "#80D6D7",
      "#73CED5",
      "#65C6D5",
      "#59BDD2",
      "#4FB5D0",
      "#45ABCB",
      "#3DA3C8",
      "#379BC5",
      "#3292C2",
      "#2E8ABF",
      "#2A81BA",
      "#2779B7",
      "#2471B4",
      "#2269B0",
      "#1F60AD",
      "#1B57A8",
      "#184EA4",
      "#1344A0",
      "#0C3B9C",
      "#023198"
    )
  } else {
    colors
  }

  # Make the plot
  plot_object <-
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(
        x = x,
        y = stats::reorder(y, x),
        xend = 0,
        yend = y
      ),
      color = line_color,
      linewidth = linewidth
    ) +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(
        x = x,
        y = stats::reorder(y, x),
        fill = x
      ),
      shape = shape,
      size = point_size,
      color = line_color
    ) +
    ggplot2::scale_fill_gradientn(colors = color_palette, guide = "none")

  # Apply theme
  plot_object <- plot_object +
    switch(theme,
      "fivethirtyeight" = ggthemes::theme_fivethirtyeight(),
      "minimal" = ggplot2::theme_minimal(),
      "classic" = ggplot2::theme_classic(),
      ggplot2::theme_minimal()
    )

  # Add margins and turn off clipping
  plot_object <- plot_object +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(color = "white"),
      # Add this line to increase the left margin
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 10)
    )

  # Add this after creating your plot object
  plot_object <- plot_object +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.1)))

  # Save the plot to a file if filename is provided
  if (!is.null(filename)) {
    # Determine file extension to save accordingly
    ext <- tools::file_ext(filename)

    if (ext == "pdf") {
      ggplot2::ggsave(
        filename = filename,
        plot = plot_object,
        device = "pdf",
        width = 10, # Try a wider width
        height = 6,
        dpi = 300
      )
    } else if (ext == "png") {
      ggplot2::ggsave(
        filename = filename,
        plot = plot_object,
        device = "png",
        width = 10, # Try a wider width
        height = 6,
        dpi = 300
      )
    } else if (ext == "svg") {
      ggplot2::ggsave(
        filename = filename,
        plot = plot_object,
        device = "svg"
      )
    } else {
      warning(
        "File extension not recognized.
              Supported extensions are 'pdf', 'png', and 'svg'."
      )
    }
  }

  # Return the plot if return_plot is TRUE
  if (return_plot) {
    return(plot_object)
  }
}
