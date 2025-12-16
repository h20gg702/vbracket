# ============================================================================
# vbracket Package - Getting Started Guide
# ============================================================================
# This script demonstrates how to install and use the vbracket package
# for creating custom legends with statistical comparison brackets in ggplot2
# ============================================================================

# ----------------------------------------------------------------------------
# STEP 1: Installation
# ----------------------------------------------------------------------------

cat("\n")
cat("============================================================================\n")
cat("vbracket Package - Getting Started Guide\n")
cat("============================================================================\n\n")

cat("STEP 1: Installing vbracket package...\n")
cat("----------------------------------------\n")

# Install from source (assuming you have the package files)
# If the package is on GitHub, use:
# install.packages("devtools")
# devtools::install_github("yourusername/vbracket")

# For now, we'll source the individual files
source("R/helpers.R")
source("R/custom_legend_with_brackets.R")
source("R/annotation_grob.R")
source("R/ggplot_integration.R")
source("R/print_method.R")
source("R/ggsave_vbracket.R")

cat("✅ Package loaded successfully!\n\n")

# ----------------------------------------------------------------------------
# STEP 2: Load Required Libraries
# ----------------------------------------------------------------------------

cat("STEP 2: Loading required libraries...\n")
cat("----------------------------------------\n")

library(ggplot2)
library(grid)

cat("✅ Libraries loaded!\n\n")

# ----------------------------------------------------------------------------
# STEP 3: Prepare Sample Data
# ----------------------------------------------------------------------------

cat("STEP 3: Preparing sample data...\n")
cat("----------------------------------------\n")

# Create sample tumor growth data
set.seed(42)
days <- c(0, 5, 10, 15, 20, 25)
groups <- c("WT", "WT/Dox", "CH3+5", "CH3+5/Dox", "PUMA-KO", "PUMA-KO/Dox")
colors <- c("WT" = "#00AA00", "WT/Dox" = "#FF4400", "CH3+5" = "#00AAFF",
            "CH3+5/Dox" = "#0000CC", "PUMA-KO" = "#AA7700", "PUMA-KO/Dox" = "#AA00FF")

df_line <- do.call(rbind, lapply(groups, function(grp) {
  if (grp == "WT/Dox") {
    mean_values <- c(0, 50, 150, 200, 500, 950)
  } else if (grp == "WT") {
    mean_values <- c(0, 50, 100, 350, 900, 2100)
  } else if (grp == "CH3+5") {
    mean_values <- c(0, 45, 95, 380, 950, 1900)
  } else if (grp == "CH3+5/Dox") {
    mean_values <- c(0, 48, 105, 360, 920, 2000)
  } else if (grp == "PUMA-KO") {
    mean_values <- c(0, 52, 110, 370, 880, 2150)
  } else {
    mean_values <- c(0, 50, 100, 355, 910, 2200)
  }
  sd_values <- mean_values * 0.08 + 10
  data.frame(days = days, volume = mean_values, sd = sd_values, group = grp)
}))

cat("✅ Data prepared!\n")
cat("   - Groups:", paste(groups, collapse=", "), "\n")
cat("   - Time points:", paste(days, collapse=", "), "days\n\n")

# Create output directory
output_dir <- "getting_started_output"
dir.create(output_dir, showWarnings = FALSE)
cat("Output directory:", output_dir, "\n\n")

# ----------------------------------------------------------------------------
# EXAMPLE 1: Basic Usage - Two Comparisons
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 1: Basic Usage - Two Comparisons\n")
cat("============================================================================\n\n")

cat("Creating a plot with two statistical comparisons...\n\n")

# Define comparison between two groups
comps1 <- add_bracket_comparisons(
  groups1 = c("WT/Dox", "CH3+5/Dox"),    # First groups
  groups2 = c("CH3+5", "PUMA-KO"),        # Second groups
  labels = c("*", "*")                    # Significance labels
)

# Create ggplot
p1 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
  scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
  labs(title = "Example 1: Basic Usage",
       x = "Days",
       y = expression(paste("Tumor Volume (mm"^3, ")"))) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "none") +
  # Add vbracket legend with comparison
  legend_bracket(labels = groups,           # All group labels
                 colors = colors,            # Group colors
                 comparisons = comps1,       # Comparison brackets
                 position = "topleft",       # Legend position
                 output_width = 6,           # Output width (inches)
                 output_height = 4)          # Output height (inches)

# Save plot using regular ggsave
ggsave(file.path(output_dir, "example1_basic.png"), p1, width = 6, height = 4, dpi = 300)
cat("✅ Saved: example1_basic.png\n\n")

# ----------------------------------------------------------------------------
# EXAMPLE 2: Multiple Comparisons with Asterisks
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 2: Multiple Comparisons with Asterisks\n")
cat("============================================================================\n\n")

cat("Creating a plot with multiple significance levels...\n")
cat("  * = p<0.05, ** = p<0.01, *** = p<0.001\n\n")

# Define multiple comparisons with different significance levels
comps2 <- add_bracket_comparisons(
  groups1 = c("WT/Dox", "CH3+5/Dox", "PUMA-KO"),
  groups2 = c("CH3+5", "PUMA-KO", "PUMA-KO/Dox"),
  labels = c("*", "**", "***")
)

p2 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
  scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
  labs(title = "Example 2: Multiple Asterisks",
       x = "Days",
       y = expression(paste("Tumor Volume (mm"^3, ")"))) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "none") +
  legend_bracket(labels = groups, colors = colors, comparisons = comps2,
                 position = "topleft",
                 output_width = 6, output_height = 4)

ggsave(file.path(output_dir, "example2_asterisks.png"), p2, width = 6, height = 4, dpi = 300)
cat("✅ Saved: example2_asterisks.png\n\n")

# ----------------------------------------------------------------------------
# EXAMPLE 3: P-value Notation
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 3: P-value Notation\n")
cat("============================================================================\n\n")

cat("Using p-value text instead of asterisks...\n\n")

comps3 <- add_bracket_comparisons(
  groups1 = c("WT/Dox", "CH3+5"),
  groups2 = c("CH3+5", "PUMA-KO"),
  labels = c("p<0.001", "ns")  # ns = not significant
)

p3 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
  scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
  labs(title = "Example 3: P-value Notation",
       x = "Days",
       y = expression(paste("Tumor Volume (mm"^3, ")"))) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "none") +
  legend_bracket(labels = groups, colors = colors, comparisons = comps3,
                 position = "topleft",
                 output_width = 6, output_height = 4)

ggsave(file.path(output_dir, "example3_pvalues.png"), p3, width = 6, height = 4, dpi = 300)
cat("✅ Saved: example3_pvalues.png\n\n")

# ----------------------------------------------------------------------------
# EXAMPLE 4: Different Legend Positions
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 4: Different Legend Positions\n")
cat("============================================================================\n\n")

cat("Testing all four legend positions...\n\n")

positions <- c("topleft", "topright", "bottomleft", "bottomright")

for (pos in positions) {
  p4 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
    scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
    labs(title = sprintf("Position: %s", pos),
         x = "Days",
         y = expression(paste("Tumor Volume (mm"^3, ")"))) +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none") +
    legend_bracket(labels = groups, colors = colors, comparisons = comps1,
                   position = pos,  # Try different positions
                   output_width = 6, output_height = 4)

  filename <- sprintf("example4_position_%s.png", pos)
  ggsave(file.path(output_dir, filename), p4, width = 6, height = 4, dpi = 300)
  cat(sprintf("✅ Saved: %s\n", filename))
}
cat("\n")

# ----------------------------------------------------------------------------
# EXAMPLE 5: Custom Bracket Margin
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 5: Custom Bracket Margin\n")
cat("============================================================================\n\n")

cat("Adjusting the space between legend text and brackets...\n\n")

margins <- c(0.05, 0.15, 0.30)

for (margin in margins) {
  p5 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
    scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
    labs(title = sprintf("Bracket Margin: %.2f", margin),
         x = "Days",
         y = expression(paste("Tumor Volume (mm"^3, ")"))) +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none") +
    legend_bracket(labels = groups, colors = colors, comparisons = comps1,
                   position = "topleft",
                   bracket_margin = margin,  # Custom margin
                   output_width = 6, output_height = 4)

  filename <- sprintf("example5_margin_%.2f.png", margin)
  ggsave(file.path(output_dir, filename), p5, width = 6, height = 4, dpi = 300)
  cat(sprintf("✅ Saved: %s\n", filename))
}
cat("\n")

# ----------------------------------------------------------------------------
# EXAMPLE 6: Different Plot Sizes
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 6: Different Plot Sizes (Adaptive Positioning)\n")
cat("============================================================================\n\n")

cat("vbracket automatically adapts to different plot sizes...\n\n")

sizes <- list(
  list(w = 4, h = 3, name = "small_4x3"),
  list(w = 6, h = 4, name = "medium_6x4"),
  list(w = 8, h = 6, name = "large_8x6")
)

for (size_info in sizes) {
  p6 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
    scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
    labs(title = sprintf("Size: %dx%d inches", size_info$w, size_info$h),
         x = "Days",
         y = expression(paste("Tumor Volume (mm"^3, ")"))) +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none") +
    legend_bracket(labels = groups, colors = colors, comparisons = comps2,
                   position = "topleft",
                   output_width = size_info$w,   # Specify output size
                   output_height = size_info$h)

  filename <- sprintf("example6_%s.png", size_info$name)
  ggsave(file.path(output_dir, filename), p6,
         width = size_info$w, height = size_info$h, dpi = 300)
  cat(sprintf("✅ Saved: %s\n", filename))
}
cat("\n")

# ----------------------------------------------------------------------------
# EXAMPLE 7: Font Family Support
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 7: Font Family Support\n")
cat("============================================================================\n\n")

cat("vbracket automatically inherits fonts from your ggplot theme...\n\n")

fonts <- c("sans", "Times New Roman", "mono")

for (font in fonts) {
  p7 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
    scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
    labs(title = sprintf("Font: %s", font),
         x = "Days",
         y = expression(paste("Tumor Volume (mm"^3, ")"))) +
    theme_classic(base_size = 14, base_family = font) +  # Set font here
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none") +
    legend_bracket(labels = groups, colors = colors, comparisons = comps1,
                   position = "topleft",
                   output_width = 6, output_height = 4)

  filename <- sprintf("example7_font_%s.png", font)
  ggsave(file.path(output_dir, filename), p7, width = 6, height = 4, dpi = 300)
  cat(sprintf("✅ Saved: %s (auto-inherited %s font)\n", filename, font))
}
cat("\n")

# ----------------------------------------------------------------------------
# EXAMPLE 8: Custom Text Sizes
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("EXAMPLE 8: Custom Text Sizes\n")
cat("============================================================================\n\n")

cat("Customizing legend text size and significance symbol size...\n\n")

text_sizes <- list(
  list(text = 8, sig = 12, name = "small"),
  list(text = 10, sig = 14, name = "medium"),
  list(text = 12, sig = 18, name = "large")
)

for (size_info in text_sizes) {
  p8 <- ggplot(df_line, aes(x = days, y = volume, color = group, group = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = volume - sd, ymax = volume + sd), width = 1.2, linewidth = 0.8) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) +
    scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
    labs(title = sprintf("Text size: %d, Sig size: %d", size_info$text, size_info$sig),
         x = "Days",
         y = expression(paste("Tumor Volume (mm"^3, ")"))) +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none") +
    legend_bracket(labels = groups, colors = colors, comparisons = comps1,
                   position = "topleft",
                   text_size = size_info$text,   # Legend text size
                   sig_size = size_info$sig,     # Significance symbol size
                   output_width = 6, output_height = 4)

  filename <- sprintf("example8_textsize_%s.png", size_info$name)
  ggsave(file.path(output_dir, filename), p8, width = 6, height = 4, dpi = 300)
  cat(sprintf("✅ Saved: %s\n", filename))
}
cat("\n")

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------

cat("============================================================================\n")
cat("GETTING STARTED GUIDE COMPLETED!\n")
cat("============================================================================\n\n")

cat("All example plots saved to:", output_dir, "\n\n")

cat("Key Features Demonstrated:\n")
cat("  1. ✅ Basic usage with single comparison\n")
cat("  2. ✅ Multiple comparisons with asterisks (*, **, ***)\n")
cat("  3. ✅ P-value notation (p<0.001, ns)\n")
cat("  4. ✅ All legend positions (topleft, topright, bottomleft, bottomright)\n")
cat("  5. ✅ Custom bracket margin adjustment\n")
cat("  6. ✅ Adaptive positioning for different plot sizes\n")
cat("  7. ✅ Automatic font family inheritance\n")
cat("  8. ✅ Custom text sizes\n\n")

cat("Main Functions:\n")
cat("  - add_bracket_comparisons(): Define group comparisons\n")
cat("  - legend_bracket(): Add custom legend with brackets to ggplot\n\n")

cat("Key Parameters:\n")
cat("  - labels: Group names for legend\n")
cat("  - colors: Named vector of group colors\n")
cat("  - comparisons: Comparison data from add_bracket_comparisons()\n")
cat("  - position: Legend position ('topleft', 'topright', 'bottomleft', 'bottomright')\n")
cat("  - output_width/output_height: Output dimensions for proper bracket positioning\n")
cat("  - bracket_margin: Space between text and brackets (optional)\n")
cat("  - text_size: Legend text size (default: 9)\n")
cat("  - sig_size: Significance symbol size (default: 14)\n\n")

cat("Next Steps:\n")
cat("  1. Review the example plots in the output directory\n")
cat("  2. Try modifying the examples with your own data\n")
cat("  3. Experiment with different parameters\n")
cat("  4. Check demo_comprehensive.R for more advanced examples\n\n")

cat("============================================================================\n")
cat("Happy plotting with vbracket!\n")
cat("============================================================================\n")

