#' Create a custom legend with vertical brackets
#'
#' This function draws a completely custom legend outside of ggplot2's system,
#' with vertical brackets showing comparisons.
#'
#' @param labels Character vector of group names
#' @param colors Character vector of colors for each group
#' @param comparisons Data frame with columns: group1, group2, label
#' @param x Numeric. X position of legend (0-1, in npc units)
#' @param y Numeric. Y position of legend (0-1, in npc units)
#' @param width Numeric. Width of legend box
#' @param height Numeric. Height of legend box
#' @param title Character. Legend title (optional)
#' @param text_size Numeric. Font size for legend labels (default 10)
#' @param text_family Character. Font family (e.g., "sans", "serif", "mono", "Helvetica", "Times")
#' @param text_face Character. Font face: "plain", "bold", "italic", "bold.italic" (default "plain")
#' @param title_size Numeric. Font size for title (default 11)
#' @param title_face Character. Font face for title (default "bold")
#' @param sig_size Numeric. Font size for significance symbols (default 11)
#' @param sig_face Character. Font face for significance symbols (default "plain")
#' @param output_width Numeric. Output figure width in inches (optional, for METHOD 2)
#' @param output_height Numeric. Output figure height in inches (optional, for METHOD 2)
#'
#' @return A gTree object containing the custom legend
#' @export
#' @import grid
#' @examples
#' \donttest{
#' labels <- c("WT", "WT/Dox", "CH3+5")
#' colors <- c("green", "orange", "blue")
#' comps <- data.frame(group1 = "WT", group2 = "WT/Dox", label = "*")
#' legend_grob <- draw_legend_with_brackets(labels, colors, comps,
#'                                          text_family = "Times New Roman",
#'                                          text_size = 12)
#' grid.draw(legend_grob)
#' }
draw_legend_with_brackets <- function(labels,
                                     colors,
                                     comparisons = NULL,
                                     x = 0.1,
                                     y = 0.9,
                                     width = 0.2,
                                     height = NULL,
                                     title = NULL,
                                     text_size = 10,
                                     text_family = "sans",
                                     text_face = "plain",
                                     title_size = 11,
                                     title_face = "bold",
                                     sig_size = 11,
                                     sig_face = "plain",
                                     output_width = NULL,
                                     output_height = NULL) {

  n_items <- length(labels)

  # Calculate height if not specified
  if (is.null(height)) {
    # Auto-calculate based on number of items
    # Use generous spacing to avoid overlap
    min_item_spacing <- 0.055  # 5.5% per item minimum
    title_height <- if (!is.null(title)) 0.10 else 0
    height <- title_height + (n_items * min_item_spacing) + 0.05
  }

  # Create viewport for legend
  legend_vp <- viewport(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(width, "npc"),
    height = unit(height, "npc"),
    just = c("left", "top"),
    name = "legend_vp"
  )

  # Create grobs list
  grobs <- list()

  # Background rectangle
  bg_rect <- rectGrob(
    x = 0.5, y = 0.5,
    width = 1, height = 1,
    gp = gpar(fill = "white", col = NA),
    vp = legend_vp
  )
  grobs[[length(grobs) + 1]] <- bg_rect

  # Title (if provided)
  y_start <- 0.95
  if (!is.null(title)) {
    title_grob <- textGrob(
      label = title,
      x = 0.5, y = y_start,
      gp = gpar(fontsize = title_size, fontface = title_face, fontfamily = text_family),
      vp = legend_vp
    )
    grobs[[length(grobs) + 1]] <- title_grob
    y_start <- y_start - 0.15
  }

  # Calculate item positions with spacing to prevent overlap
  # Minimum spacing to ensure text and brackets don't overlap
  min_spacing <- 0.055  # Minimum 5.5% spacing between items

  available_height <- y_start - 0.05

  # Use even spacing, but enforce minimum
  ideal_spacing <- available_height / (n_items + 1)
  item_spacing <- max(min_spacing, ideal_spacing)

  # If items don't fit, warn user
  if (item_spacing * n_items > available_height) {
    warning("Legend height (", round(height, 3), ") may be too small for ", n_items,
            " items. Recommend height >= ", round((n_items * min_spacing) + 0.15, 2))
  }

  item_y_positions <- seq(y_start - item_spacing, y_start - (n_items * item_spacing), length.out = n_items)
  names(item_y_positions) <- labels

  # Calculate maximum text width to position brackets correctly
  max_text_width <- 0

  if (!is.null(output_width)) {
    # Accurate calculation using actual output size
    # Text width in inches (approximate: char width * font size)
    for (label in labels) {
      text_width_inches <- nchar(label) * (text_size / 72) * 0.6  # 0.6 = average char width factor
      max_text_width <- max(max_text_width, text_width_inches)
    }

    # Convert to npc units within legend viewport
    # Legend width in inches
    legend_width_inches <- width * output_width
    max_text_width_npc <- max_text_width / legend_width_inches

    # Fixed margin in inches
    margin_inches <- 0.15  # 0.15 inch margin
    margin_npc <- margin_inches / legend_width_inches

    # Base X position for text start
    text_x_start <- 0.3
    # Calculate bracket X position
    bracket_x_base <- text_x_start + max_text_width_npc + margin_npc

  } else {
    # Fallback: rough estimation without output size
    for (label in labels) {
      text_width_estimate <- nchar(label) * (text_size / 72) * 0.007
      max_text_width <- max(max_text_width, text_width_estimate)
    }
    text_x_start <- 0.3
    bracket_x_base <- text_x_start + max_text_width + 0.12
  }

  # Draw legend items
  for (i in seq_along(labels)) {
    y_pos <- item_y_positions[i]

    # Line symbol
    line_grob <- linesGrob(
      x = c(0.1, 0.25),
      y = c(y_pos, y_pos),
      gp = gpar(col = colors[i], lwd = 3),
      vp = legend_vp
    )
    grobs[[length(grobs) + 1]] <- line_grob

    # Point symbol
    point_grob <- pointsGrob(
      x = 0.175,
      y = y_pos,
      pch = 19,
      size = unit(0.3, "char"),
      gp = gpar(col = colors[i]),
      vp = legend_vp
    )
    grobs[[length(grobs) + 1]] <- point_grob

    # Label text
    text_grob <- textGrob(
      label = labels[i],
      x = 0.3,
      y = y_pos,
      just = "left",
      gp = gpar(fontsize = text_size, fontface = text_face, fontfamily = text_family),
      vp = legend_vp
    )
    grobs[[length(grobs) + 1]] <- text_grob
  }

  # Add brackets if comparisons provided
  if (!is.null(comparisons) && nrow(comparisons) > 0) {
    # Detect overlapping brackets and assign horizontal offsets
    bracket_layers <- rep(0, nrow(comparisons))

    for (i in seq_len(nrow(comparisons))) {
      group1_i <- as.character(comparisons$group1[i])
      group2_i <- as.character(comparisons$group2[i])

      if (!group1_i %in% names(item_y_positions) || !group2_i %in% names(item_y_positions)) {
        next
      }

      y1_i <- item_y_positions[group1_i]
      y2_i <- item_y_positions[group2_i]
      range_i <- c(min(y1_i, y2_i), max(y1_i, y2_i))

      # Check for overlap with previous brackets
      if (i > 1) {
        for (j in 1:(i-1)) {
          group1_j <- as.character(comparisons$group1[j])
          group2_j <- as.character(comparisons$group2[j])

          if (!group1_j %in% names(item_y_positions) || !group2_j %in% names(item_y_positions)) {
            next
          }

          y1_j <- item_y_positions[group1_j]
          y2_j <- item_y_positions[group2_j]
          range_j <- c(min(y1_j, y2_j), max(y1_j, y2_j))

          # Check if ranges overlap
          if (range_i[1] <= range_j[2] && range_i[2] >= range_j[1]) {
            # Overlaps - use next layer
            bracket_layers[i] <- max(bracket_layers[i], bracket_layers[j] + 1)
          }
        }
      }
    }

    for (i in seq_len(nrow(comparisons))) {
      group1 <- as.character(comparisons$group1[i])
      group2 <- as.character(comparisons$group2[i])
      sig_label <- as.character(comparisons$label[i])

      if (!group1 %in% names(item_y_positions) || !group2 %in% names(item_y_positions)) {
        next
      }

      y1 <- item_y_positions[group1]
      y2 <- item_y_positions[group2]

      # Ensure y1 > y2 (top to bottom)
      if (y1 < y2) {
        temp <- y1
        y1 <- y2
        y2 <- temp
      }

      # Offset bracket endpoints to avoid overlapping with text
      # Text is centered at y position, so offset brackets slightly above/below
      text_height_offset <- 0.015  # Small offset to clear text
      y1_bracket <- y1 + text_height_offset  # Top bracket slightly above text center
      y2_bracket <- y2 - text_height_offset  # Bottom bracket slightly below text center

      # Calculate bracket X position based on text width and layer
      bracket_x <- bracket_x_base + (bracket_layers[i] * 0.10)

      # Vertical line (use offset positions)
      vert_line <- linesGrob(
        x = c(bracket_x, bracket_x),
        y = c(y2_bracket, y1_bracket),
        gp = gpar(col = "black", lwd = 1.5),
        vp = legend_vp
      )
      grobs[[length(grobs) + 1]] <- vert_line

      # Top horizontal connector - pointing LEFT (toward text)
      top_horiz <- linesGrob(
        x = c(bracket_x - 0.05, bracket_x),
        y = c(y1_bracket, y1_bracket),
        gp = gpar(col = "black", lwd = 1.5),
        vp = legend_vp
      )
      grobs[[length(grobs) + 1]] <- top_horiz

      # Bottom horizontal connector - pointing LEFT (toward text)
      bottom_horiz <- linesGrob(
        x = c(bracket_x - 0.05, bracket_x),
        y = c(y2_bracket, y2_bracket),
        gp = gpar(col = "black", lwd = 1.5),
        vp = legend_vp
      )
      grobs[[length(grobs) + 1]] <- bottom_horiz

      # Significance label - to the RIGHT of bracket
      y_mid <- (y1 + y2) / 2
      sig_text <- textGrob(
        label = sig_label,
        x = bracket_x + 0.05,
        just = "left",
        y = y_mid,
        gp = gpar(fontsize = sig_size, fontface = sig_face, fontfamily = text_family),
        vp = legend_vp
      )
      grobs[[length(grobs) + 1]] <- sig_text
    }
  }

  # Combine all grobs
  do.call(gList, grobs)
}


#' Add custom legend with brackets to a ggplot
#'
#' Remove ggplot2's legend and add a custom legend with brackets
#'
#' @param plot A ggplot object
#' @param labels Character vector of group names (in order)
#' @param colors Character vector of colors matching the groups
#' @param comparisons Data frame with columns: group1, group2, label
#' @param legend_x X position (0-1)
#' @param legend_y Y position (0-1)
#' @param legend_width Width of legend
#' @param title Legend title
#'
#' @return A ggplot object (classes \code{"gg"} and \code{"ggplot"}) with the vbracket legend incorporated.
#'   The plot's default legend is typically suppressed and a custom vbracket legend showing statistical comparison brackets is added.
#' @export
#' @import ggplot2
#' @import grid
#' @examples
#' \donttest{
#' p <- ggplot(data, aes(x, y, color = group)) + geom_line()
#' comps <- add_bracket_comparisons(c("A", "B", "*"))
#' plot_with_custom_legend(p, c("A", "B"), c("red", "blue"), comps)
#' }
plot_with_custom_legend <- function(plot,
                                   labels,
                                   colors,
                                   comparisons = NULL,
                                   legend_x = 0.05,
                                   legend_y = 0.95,
                                   legend_width = 0.25,
                                   title = NULL) {

  # Remove original legend
  plot <- plot + theme(legend.position = "none")

  # Print the plot
  print(plot)

  # Add custom legend on top
  grid::seekViewport("layout")

  legend_grobs <- draw_legend_with_brackets(
    labels = labels,
    colors = colors,
    comparisons = comparisons,
    x = legend_x,
    y = legend_y,
    width = legend_width,
    title = title
  )

  grid::grid.draw(legend_grobs)

  invisible(plot)
}
