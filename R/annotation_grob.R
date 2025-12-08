#' Create a self-contained vbracket legend grob for annotation_custom
#'
#' This creates a grob that can be used with annotation_custom() in ggplot2.
#' Unlike draw_legend_with_brackets(), this creates a single grob that handles
#' its own viewport setup internally.
#'
#' @keywords internal
create_annotation_legend_grob <- function(labels, colors, comparisons = NULL,
                                         x = 0.1, y = 0.9, width = 0.25, height = NULL,
                                         title = NULL,
                                         text_size = 10, text_family = "sans", text_face = "plain",
                                         title_size = 11, title_face = "bold",
                                         sig_size = 11, sig_face = "plain",
                                         output_width = 8, output_height = 6,
                                         bracket_margin = NULL) {

  # Store all parameters in the grob (use gTree not grob)
  gTree(
    labels = labels,
    colors = colors,
    comparisons = comparisons,
    x = x,
    y = y,
    width = width,
    height = height,
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
    cl = "vbracket_annotation_grob"
  )
}

#' @export
makeContent.vbracket_annotation_grob <- function(x) {
  # Draw legend directly using absolute coordinates (no viewports)
  # This works correctly with annotation_custom()

  grobs <- gList()

  # Calculate dimensions
  n_items <- length(x$labels)
  height <- x$height
  if (is.null(height)) {
    # Adaptive vertical spacing based on output width
    plot_width <- x$output_width
    if (!is.null(plot_width) && plot_width < 5) {
      # 4x3 plots need more vertical spacing between items
      height <- n_items * 0.08
    } else {
      # Standard spacing for larger plots
      height <- n_items * 0.055
    }
  }

  # Background rectangle (white, no border)
  bg_rect <- rectGrob(
    x = x$x, y = x$y,
    width = x$width, height = height,
    just = c("left", "top"),
    gp = gpar(fill = "white", col = NA)
  )
  grobs <- gList(grobs, bg_rect)

  # Title (if provided)
  y_offset <- 0
  if (!is.null(x$title)) {
    title_grob <- textGrob(
      x$title,
      x = x$x + x$width/2,
      y = x$y - 0.01,
      just = c("center", "top"),
      gp = gpar(fontsize = x$title_size, fontface = x$title_face, fontfamily = x$text_family)
    )
    grobs <- gList(grobs, title_grob)
    y_offset <- 0.03
  }

  # Calculate item positions
  item_height <- height / (n_items + 1)

  # Draw legend items (color line + text)
  for (i in seq_along(x$labels)) {
    item_y <- x$y - y_offset - (i * item_height)

    # Color line
    line_grob <- linesGrob(
      x = c(x$x + 0.01, x$x + 0.05),
      y = c(item_y, item_y),
      gp = gpar(col = x$colors[i], lwd = 3)
    )
    grobs <- gList(grobs, line_grob)

    # Text label
    text_grob <- textGrob(
      x$labels[i],
      x = x$x + 0.06,
      y = item_y,
      just = c("left", "center"),
      gp = gpar(fontsize = x$text_size, fontface = x$text_face, fontfamily = x$text_family)
    )
    grobs <- gList(grobs, text_grob)
  }

  # Draw brackets if comparisons provided
  if (!is.null(x$comparisons) && nrow(x$comparisons) > 0) {

    # Create position map
    item_positions <- setNames(
      x$y - y_offset - (seq_along(x$labels) * item_height),
      x$labels
    )

    # Calculate bracket X position (after longest label)
    # Use actual rendered text width instead of character count estimation
    max_text_width <- max(sapply(x$labels, function(label) {
      tg <- textGrob(label, gp = gpar(fontsize = x$text_size, fontfamily = x$text_family))
      convertWidth(grobWidth(tg), "npc", valueOnly = TRUE)
    }))
    text_width <- max_text_width

    # Determine bracket margin - use user-provided value or adaptive calculation
    if (!is.null(x$bracket_margin)) {
      bracket_margin <- x$bracket_margin
    } else {
      # Adaptive margin based on output width
      # Now that we use actual text width, we can use smaller margins
      plot_width <- x$output_width
      if (plot_width < 4) {
        bracket_margin <- 0.05  # Very small plots
      } else if (plot_width < 5) {
        bracket_margin <- 0.08  # 4x3 plots
      } else if (plot_width < 7) {
        bracket_margin <- 0.02  # Medium plots - minimal clearance with accurate text width
      } else {
        bracket_margin <- 0.02  # 8x6+ plots - minimal clearance with accurate text width
      }
    }

    bracket_x_base <- x$x + 0.06 + text_width + bracket_margin

    # Check for overlapping brackets
    bracket_layers <- rep(0, nrow(x$comparisons))
    for (i in seq_len(nrow(x$comparisons))) {
      if (i > 1) {
        y1_i <- item_positions[as.character(x$comparisons$group1[i])]
        y2_i <- item_positions[as.character(x$comparisons$group2[i])]
        min_y_i <- min(y1_i, y2_i)
        max_y_i <- max(y1_i, y2_i)

        for (j in seq_len(i - 1)) {
          y1_j <- item_positions[as.character(x$comparisons$group1[j])]
          y2_j <- item_positions[as.character(x$comparisons$group2[j])]
          min_y_j <- min(y1_j, y2_j)
          max_y_j <- max(y1_j, y2_j)

          if (!(max_y_i < min_y_j || min_y_i > max_y_j)) {
            bracket_layers[i] <- max(bracket_layers[i], bracket_layers[j] + 1)
          }
        }
      }
    }

    # Determine bracket layer spacing adaptively based on:
    # 1. Label type (asterisks vs text)
    # 2. Text size (sig_size)
    # 3. Figure size (output_width)

    all_asterisks <- all(grepl("^\\*+$", x$comparisons$label))

    # Base spacing
    if (all_asterisks) {
      base_spacing <- 0.06  # Narrow for asterisks
    } else {
      base_spacing <- 0.10  # Wider for text
    }

    # Adjust for figure size - smaller figures need relatively more spacing
    size_factor <- 1.0
    if (!is.null(x$output_width)) {
      if (x$output_width < 5) {
        size_factor <- 1.2  # 20% more spacing for small figures
      } else if (x$output_width > 8) {
        size_factor <- 0.9  # 10% less spacing for large figures
      }
    }

    # Adjust for text size - larger text needs more spacing
    text_factor <- 1.0
    if (!is.null(x$sig_size)) {
      # Normalize to size 12 as baseline
      text_factor <- x$sig_size / 12
    }

    # Calculate final spacing
    layer_spacing <- base_spacing * size_factor * text_factor

    # Draw each bracket
    for (i in seq_len(nrow(x$comparisons))) {
      group1 <- as.character(x$comparisons$group1[i])
      group2 <- as.character(x$comparisons$group2[i])
      label <- as.character(x$comparisons$label[i])

      y1 <- item_positions[group1]
      y2 <- item_positions[group2]

      bracket_x <- bracket_x_base + (bracket_layers[i] * layer_spacing)

      # Vertical line
      bracket_line <- linesGrob(
        x = c(bracket_x, bracket_x),
        y = c(y1, y2),
        gp = gpar(col = "black", lwd = 1)
      )
      grobs <- gList(grobs, bracket_line)

      # Top connector
      top_connector <- linesGrob(
        x = c(bracket_x - 0.015, bracket_x),
        y = c(y1, y1),
        gp = gpar(col = "black", lwd = 1)
      )
      grobs <- gList(grobs, top_connector)

      # Bottom connector
      bottom_connector <- linesGrob(
        x = c(bracket_x - 0.015, bracket_x),
        y = c(y2, y2),
        gp = gpar(col = "black", lwd = 1)
      )
      grobs <- gList(grobs, bottom_connector)

      # Significance symbol
      mid_y <- (y1 + y2) / 2
      # Add vertical offset for better visual centering
      # Asterisks need to be positioned lower than other text symbols
      if (grepl("^\\*+$", label)) {
        # Only asterisks - use negative offset to position lower
        sig_y <- mid_y - (x$sig_size / 72) * 0.050
      } else {
        # Other symbols (ns, p<0.05, etc.) - use standard offset (keep unchanged)
        sig_y <- mid_y + (x$sig_size / 72) * 0.003
      }
      sig_grob <- textGrob(
        label,
        x = bracket_x + 0.02,
        y = sig_y,
        just = c("left", "center"),
        gp = gpar(fontsize = x$sig_size, fontface = x$sig_face, fontfamily = x$text_family)
      )
      grobs <- gList(grobs, sig_grob)
    }
  }

  # Set the children
  setChildren(x, grobs)
}
