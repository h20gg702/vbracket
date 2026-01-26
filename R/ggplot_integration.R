#' @importFrom stats setNames
#' @importFrom grDevices dev.size
#' @importFrom utils getS3method
NULL

#' Add legend with brackets to a ggplot object
#'
#' This function allows you to add a custom legend with brackets using
#' the + operator, similar to ggplot2 layers.
#'
#' @param labels Character vector of group names (in order)
#' @param colors Character vector of colors matching the groups
#' @param comparisons Data frame with columns: group1, group2, label
#' @param x Numeric. X position of legend (0-1 scale or with unit)
#' @param y Numeric. Y position of legend (0-1 scale or with unit)
#' @param width Numeric. Width of legend box (default NULL = auto)
#' @param height Numeric. Height of legend box (default NULL = auto)
#' @param unit Character. Unit for width and height: "npc" (0-1 scale), "in", "cm", "mm" (default "npc")
#' @param position Character. Preset position: "topleft", "topright", "bottomleft", "bottomright", or NULL for manual x/y
#' @param title Character. Legend title (optional)
#' @param text_size Numeric. Font size for labels (default 10)
#' @param text_family Character. Font family (default "sans")
#' @param text_face Character. Font face (default "plain")
#' @param title_size Numeric. Title font size (default 11)
#' @param title_face Character. Title font face (default "bold")
#' @param sig_size Numeric. Significance symbol size (default 11)
#' @param sig_face Character. Significance symbol face (default "plain")
#' @param output_width Numeric. Output figure width in inches (for accurate bracket positioning)
#' @param output_height Numeric. Output figure height in inches (for accurate bracket positioning)
#' @param bracket_margin Numeric. Custom horizontal spacing between legend text and brackets (default NULL = auto-adaptive)
#' @param legend_x Numeric. Custom X position for legend box (0-1 scale, overrides adaptive positioning)
#' @param legend_y Numeric. Custom Y position for legend box (0-1 scale, overrides adaptive positioning)
#' @param line_length Numeric. Manual override for legend symbol line length (default NULL = auto-scaled by text_size)
#' @param line_width Numeric. Manual override for legend symbol line width (default NULL = auto-scaled by text_size)
#' @param item_spacing Numeric. Manual override for vertical spacing between legend items (default NULL = auto-scaled by text_size)
#' @param bracket_layer_spacing Numeric. Manual override for horizontal spacing between bracket layers (default NULL = auto-calculated)
#'
#' @return A vbracket_legend object
#' @export
#' @examples
#' \donttest{
#' library(ggplot2)
#' data <- data.frame(x = 1:10, y = 1:10, group = rep(c("A", "B"), each = 5))
#' labels <- c("A", "B")
#' colors <- c("A" = "red", "B" = "blue")
#' comparisons <- add_bracket_comparisons(groups1 = "A", groups2 = "B", labels = "*")
#'
#' p <- ggplot(data, aes(x, y, color = group)) + geom_point() +
#'   legend_bracket(labels, colors, comparisons,
#'                  position = "topleft",
#'                  output_width = 6, output_height = 4)
#'
#' # Then use regular ggsave with same dimensions
#' ggsave(file.path(tempdir(), "plot.pdf"), p, width = 6, height = 4)
#' }
legend_bracket <- function(labels,
                          colors,
                          comparisons = NULL,
                          x = NULL,
                          y = NULL,
                          width = NULL,
                          height = NULL,
                          unit = "npc",
                          position = "topleft",
                          title = NULL,
                          text_size = 10,
                          text_family = "sans",
                          text_face = "plain",
                          title_size = 11,
                          title_face = "bold",
                          sig_size = 11,
                          sig_face = "plain",
                          output_width = NULL,
                          output_height = NULL,
                          bracket_margin = NULL,
                          legend_x = NULL,
                          legend_y = NULL,
                          line_length = NULL,
                          line_width = NULL,
                          item_spacing = NULL,
                          bracket_layer_spacing = NULL) {

  # Create object with all parameters
  structure(
    list(
      labels = labels,
      colors = colors,
      comparisons = comparisons,
      x = x,
      y = y,
      width = width,
      height = height,
      unit = unit,
      position = position,
      title = title,
      text_size = text_size,
      text_family = text_family,
      text_face = text_face,
      title_size = title_size,
      title_face = title_face,
      sig_size = sig_size,
      sig_face = sig_face,
      output_width = output_width,
      output_height = output_height,
      bracket_margin = bracket_margin,
      legend_x = legend_x,
      legend_y = legend_y,
      line_length = line_length,
      line_width = line_width,
      item_spacing = item_spacing,
      bracket_layer_spacing = bracket_layer_spacing
    ),
    class = "vbracket_legend"
  )
}


#' Add vbracket legend to ggplot
#'
#' @param object A vbracket_legend object
#' @param plot A ggplot object
#' @param ... Additional arguments (not used)
#' @return A ggplot object (classes \code{"gg"} and \code{"ggplot"}) with the vbracket legend incorporated.
#'   The plot's default legend is typically suppressed and a custom vbracket legend showing statistical comparison brackets is added.
#' @export
ggplot_add.vbracket_legend <- function(object, plot, ...) {

  # Remove original legend from plot
  plot <- plot + theme(legend.position = "none")

  # Inherit font family from ggplot theme if not explicitly specified
  if (is.null(object$text_family) || object$text_family == "sans") {
    # Try to get font family from plot theme
    if (!is.null(plot$theme$text$family) && plot$theme$text$family != "") {
      object$text_family <- plot$theme$text$family
    }
  }

  # Check if output dimensions are provided
  has_dimensions <- !is.null(object$output_width) && !is.null(object$output_height)

  if (has_dimensions) {
    # ========================================================================
    # PATH 1: Dimensions provided → Create actual ggplot layer
    # This allows regular ggsave() to work
    # ========================================================================

    output_width <- object$output_width
    output_height <- object$output_height

    # Calculate adaptive position based on output size
    if (!is.null(object$position) && is.null(object$x)) {
      n_items <- length(object$labels)

      if (is.null(object$width)) {
        object$width <- 0.25
      }

      # Adaptive X position for left-aligned legends (unless legend_x is provided)
      if (is.null(object$legend_x)) {
        aspect_ratio <- output_width / output_height
        if (output_width < 4) {
          x_left <- 0.20  # 20% from left for very small plots
        } else if (output_width < 5) {
          x_left <- 0.08  # 8% from left for 4x3 plots (closer to Y-axis)
        } else if (output_width < 7 || aspect_ratio < 1) {
          x_left <- 0.10  # 10% from left for narrow/tall plots
        } else {
          x_left <- 0.08  # 8% from left for 8x6+ plots (closer to Y-axis)
        }
      } else {
        x_left <- object$legend_x  # Use custom legend_x
      }

      # Default Y positions (unless legend_y is provided)
      if (is.null(object$legend_y)) {
        y_top <- 0.92  # Higher position
        y_bottom <- 0.12 + (n_items * 0.05)
      } else {
        y_top <- object$legend_y
        y_bottom <- object$legend_y
      }

      if (object$position == "topleft") {
        object$x <- x_left
        object$y <- y_top
      } else if (object$position == "topright") {
        object$x <- 0.98 - object$width
        object$y <- y_top
      } else if (object$position == "bottomleft") {
        object$x <- x_left
        object$y <- y_bottom
      } else if (object$position == "bottomright") {
        object$x <- 0.98 - object$width
        object$y <- y_bottom
      }
    }

    # Create a self-contained grob for annotation_custom
    legend_grob <- create_annotation_legend_grob(
      labels = object$labels,
      colors = object$colors,
      comparisons = object$comparisons,
      x = object$x,
      y = object$y,
      width = object$width,
      height = object$height,
      title = object$title,
      text_size = object$text_size,
      text_family = object$text_family,
      text_face = object$text_face,
      title_size = object$title_size,
      title_face = object$title_face,
      sig_size = object$sig_size,
      sig_face = object$sig_face,
      output_width = output_width,
      output_height = output_height,
      bracket_margin = object$bracket_margin,
      line_length = object$line_length,
      line_width = object$line_width,
      item_spacing = object$item_spacing,
      bracket_layer_spacing = object$bracket_layer_spacing
    )

    # Add legend as annotation (actual ggplot layer)
    plot <- plot +
      annotation_custom(
        grob = legend_grob,
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      )

  } else {
    # ========================================================================
    # PATH 2: No dimensions → Store as metadata
    # Requires ggsave_vbracket() to render
    # ========================================================================

    attr(plot, "vbracket_legend") <- object
  }

  # Return modified plot
  plot
}


#' Print a ggplot with vbracket overlay
#'
#' @param x ggplot object
#' @param ... Additional arguments
#' @keywords internal
print_vbracket_plot <- function(x, ...) {

  vb_legend <- attr(x, "vbracket_legend")

  if (is.null(vb_legend)) {
    # No vbracket legend, use default print
    print.default(x)
    return(invisible(x))
  }

  # Print the plot first
  print.default(x)

  # Navigate to layout viewport
  grid::seekViewport("layout")

  # Calculate position if using preset
  if (!is.null(vb_legend$position) && is.null(vb_legend$x)) {

    n_items <- length(vb_legend$labels)

    # Default width based on position
    if (is.null(vb_legend$width)) {
      vb_legend$width <- 0.25
    }

    # Get device size to adjust position for narrow plots
    dev_size <- dev.size("in")
    plot_width <- dev_size[1]
    plot_height <- dev_size[2]
    aspect_ratio <- plot_width / plot_height

    # Adaptive X position for left-aligned legends
    # Smaller plots need progressively more spacing from Y-axis
    if (plot_width < 4) {
      x_left <- 0.20  # 20% from left for very small plots (< 4in)
    } else if (plot_width < 5) {
      x_left <- 0.08  # 8% from left for small plots (< 5in)
    } else if (plot_width < 7 || aspect_ratio < 1) {
      x_left <- 0.10  # 10% from left for narrow plots (< 7in or tall)
    } else {
      x_left <- 0.08  # 8% from left for normal/wide plots
    }

    if (vb_legend$position == "topleft") {
      vb_legend$x <- x_left
      vb_legend$y <- 0.92  # Higher position
    } else if (vb_legend$position == "topright") {
      vb_legend$x <- 0.98 - vb_legend$width
      vb_legend$y <- 0.88
    } else if (vb_legend$position == "bottomleft") {
      vb_legend$x <- x_left
      vb_legend$y <- 0.12 + (n_items * 0.05)
    } else if (vb_legend$position == "bottomright") {
      vb_legend$x <- 0.98 - vb_legend$width
      vb_legend$y <- 0.12 + (n_items * 0.05)
    }
  }

  # Convert width/height to npc if specified in other units
  final_width <- vb_legend$width
  final_height <- vb_legend$height

  if (vb_legend$unit != "npc") {
    # Get device size in inches
    dev_size <- dev.size("in")

    # Convert width
    if (!is.null(vb_legend$width)) {
      if (vb_legend$unit == "in") {
        final_width <- vb_legend$width / dev_size[1]
      } else if (vb_legend$unit == "cm") {
        final_width <- (vb_legend$width / 2.54) / dev_size[1]
      } else if (vb_legend$unit == "mm") {
        final_width <- (vb_legend$width / 25.4) / dev_size[1]
      }
    }

    # Convert height
    if (!is.null(vb_legend$height)) {
      if (vb_legend$unit == "in") {
        final_height <- vb_legend$height / dev_size[2]
      } else if (vb_legend$unit == "cm") {
        final_height <- (vb_legend$height / 2.54) / dev_size[2]
      } else if (vb_legend$unit == "mm") {
        final_height <- (vb_legend$height / 25.4) / dev_size[2]
      }
    }
  }

  # Draw legend with brackets
  legend_grobs <- draw_legend_with_brackets(
    labels = vb_legend$labels,
    colors = vb_legend$colors,
    comparisons = vb_legend$comparisons,
    x = vb_legend$x,
    y = vb_legend$y,
    width = final_width,
    height = final_height,
    title = vb_legend$title,
    text_size = vb_legend$text_size,
    text_family = vb_legend$text_family,
    text_face = vb_legend$text_face,
    title_size = vb_legend$title_size,
    title_face = vb_legend$title_face,
    sig_size = vb_legend$sig_size,
    sig_face = vb_legend$sig_face,
    line_length = vb_legend$line_length,
    line_width = vb_legend$line_width,
    item_spacing = vb_legend$item_spacing,
    bracket_layer_spacing = vb_legend$bracket_layer_spacing
  )

  grid::grid.draw(legend_grobs)

  invisible(x)
}
