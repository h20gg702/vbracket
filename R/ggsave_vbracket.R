#' Save a ggplot with vbracket legend
#'
#' This function is a wrapper around ggsave() that properly handles vbracket legends.
#' Use this instead of ggsave() when your plot has a legend_bracket().
#'
#' @param filename File name to save plot to
#' @param plot Plot to save (must have legend_bracket)
#' @param device Device to use (default auto-detects from filename)
#' @param width Width in units
#' @param height Height in units
#' @param units Units for width and height ("in", "cm", "mm", "px")
#' @param dpi DPI for raster devices
#' @param ... Additional arguments passed to ggsave()
#'
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(data, aes(x, y, color = group)) +
#'   geom_point() +
#'   legend_bracket(labels, colors, comparisons)
#'
#' ggsave_vbracket("plot.pdf", p, width = 8, height = 6)
#' }
ggsave_vbracket <- function(filename, plot, device = NULL, width = NA, height = NA,
                           units = c("in", "cm", "mm", "px"), dpi = 300, ...) {

  # Check if plot has vbracket legend
  vb_legend <- attr(plot, "vbracket_legend")

  if (is.null(vb_legend)) {
    # No vbracket legend, use regular ggsave
    ggplot2::ggsave(filename, plot, device = device, width = width, height = height,
                   units = units, dpi = dpi, ...)
    return(invisible())
  }

  # Detect device from filename if not specified
  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
    if (device == "pdf") {
      device_fun <- grDevices::pdf
    } else if (device == "png") {
      device_fun <- grDevices::png
    } else if (device %in% c("jpg", "jpeg")) {
      device_fun <- grDevices::jpeg
    } else if (device == "svg") {
      device_fun <- grDevices::svg
    } else {
      stop("Unknown device: ", device)
    }
  } else {
    device_fun <- device
  }

  # Get units
  units <- match.arg(units)

  # Convert dimensions if needed
  if (units != "in") {
    if (units == "cm") {
      width <- width / 2.54
      height <- height / 2.54
    } else if (units == "mm") {
      width <- width / 25.4
      height <- height / 25.4
    } else if (units == "px") {
      width <- width / dpi
      height <- height / dpi
    }
  }

  # Open device
  if (device == "png" || device %in% c("jpg", "jpeg")) {
    device_fun(filename, width = width, height = height, units = "in", res = dpi)
  } else {
    device_fun(filename, width = width, height = height)
  }

  # Calculate position if using preset
  if (!is.null(vb_legend$position) && is.null(vb_legend$x)) {
    n_items <- length(vb_legend$labels)

    if (is.null(vb_legend$width)) {
      vb_legend$width <- 0.25
    }

    # Adaptive X position for left-aligned legends
    # Smaller plots need progressively more spacing from Y-axis
    aspect_ratio <- width / height
    if (width < 4) {
      x_left <- 0.30  # 30% from left for very small plots (< 4in)
    } else if (width < 5) {
      x_left <- 0.25  # 25% from left for small plots (< 5in)
    } else if (width < 7 || aspect_ratio < 1) {
      x_left <- 0.18  # 18% from left for narrow plots (< 7in or tall)
    } else {
      x_left <- 0.12  # 12% from left for normal/wide plots
    }

    if (vb_legend$position == "topleft") {
      vb_legend$x <- x_left
      vb_legend$y <- 0.88
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

  # Convert width/height to npc if needed
  final_width <- vb_legend$width
  final_height <- vb_legend$height

  if (vb_legend$unit != "npc") {
    if (!is.null(vb_legend$width)) {
      if (vb_legend$unit == "in") {
        final_width <- vb_legend$width / width
      } else if (vb_legend$unit == "cm") {
        final_width <- (vb_legend$width / 2.54) / width
      } else if (vb_legend$unit == "mm") {
        final_width <- (vb_legend$width / 25.4) / width
      }
    }

    if (!is.null(vb_legend$height)) {
      if (vb_legend$unit == "in") {
        final_height <- vb_legend$height / height
      } else if (vb_legend$unit == "cm") {
        final_height <- (vb_legend$height / 2.54) / height
      } else if (vb_legend$unit == "mm") {
        final_height <- (vb_legend$height / 25.4) / height
      }
    }
  }

  # Print plot
  print(plot + ggplot2::theme(legend.position = "none"))

  # Add vbracket legend
  grid::seekViewport("layout")

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
    output_width = width,
    output_height = height
  )

  grid::grid.draw(legend_grobs)

  # Close device
  grDevices::dev.off()

  invisible()
}
