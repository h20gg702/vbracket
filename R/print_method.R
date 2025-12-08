#' Print method for ggplot objects with vbracket legend
#'
#' This overrides the default ggplot print method to add brackets automatically
#'
#' @param x A ggplot object
#' @param newpage Draw on new page (default TRUE)
#' @param vp Viewport to draw in
#' @param ... Additional arguments
#' @export
print.gg <- function(x, newpage = is.null(vp), vp = NULL, ...) {

  # Check if this plot has a vbracket legend attached
  vb_legend <- attr(x, "vbracket_legend")

  if (is.null(vb_legend)) {
    # No vbracket legend, use default ggplot2 print
    ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)
    return(invisible(x))
  }

  # First print the plot normally
  ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)

  # Now add the custom legend on top
  grid::seekViewport("layout")

  # Calculate position if using preset
  if (!is.null(vb_legend$position) && is.null(vb_legend$x)) {
    n_items <- length(vb_legend$labels)

    if (is.null(vb_legend$width)) {
      vb_legend$width <- 0.25
    }

    if (vb_legend$position == "topleft") {
      vb_legend$x <- 0.12
      vb_legend$y <- 0.88
    } else if (vb_legend$position == "topright") {
      vb_legend$x <- 0.98 - vb_legend$width
      vb_legend$y <- 0.88
    } else if (vb_legend$position == "bottomleft") {
      vb_legend$x <- 0.12
      vb_legend$y <- 0.12 + (n_items * 0.05)
    } else if (vb_legend$position == "bottomright") {
      vb_legend$x <- 0.98 - vb_legend$width
      vb_legend$y <- 0.12 + (n_items * 0.05)
    }
  }

  # Convert units if needed
  final_width <- vb_legend$width
  final_height <- vb_legend$height

  if (vb_legend$unit != "npc") {
    dev_size <- dev.size("in")

    if (!is.null(vb_legend$width)) {
      if (vb_legend$unit == "in") {
        final_width <- vb_legend$width / dev_size[1]
      } else if (vb_legend$unit == "cm") {
        final_width <- (vb_legend$width / 2.54) / dev_size[1]
      } else if (vb_legend$unit == "mm") {
        final_width <- (vb_legend$width / 25.4) / dev_size[1]
      }
    }

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

  # Draw the legend with brackets
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
    output_width = vb_legend$output_width,
    output_height = vb_legend$output_height
  )

  grid::grid.draw(legend_grobs)

  invisible(x)
}
