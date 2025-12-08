#' Create comparison specification for vbracket
#'
#' Helper function to create a properly formatted comparison data frame
#' for use with guide_legend_bracket().
#'
#' @param ... Named arguments or list of comparisons. Each comparison can be:
#'   \itemize{
#'     \item A character vector of length 3: c(group1, group2, label)
#'     \item A named list: list(from = "A", to = "B", label = "***")
#'   }
#' @param groups1 Character vector of first groups to compare.
#' @param groups2 Character vector of second groups to compare.
#' @param labels Character vector of significance labels.
#'
#' @return A data frame with columns: group1, group2, label
#'
#' @export
#' @examples
#' # Using individual vectors
#' add_bracket_comparisons(
#'   groups1 = c("A", "C"),
#'   groups2 = c("B", "D"),
#'   labels = c("***", "ns")
#' )
#'
#' # Using ... with vectors
#' add_bracket_comparisons(
#'   c("A", "B", "***"),
#'   c("C", "D", "ns")
#' )
add_bracket_comparisons <- function(...,
                                   groups1 = NULL,
                                   groups2 = NULL,
                                   labels = NULL) {

  dots <- list(...)

  # Method 1: groups1, groups2, labels specified
  if (!is.null(groups1) && !is.null(groups2) && !is.null(labels)) {
    if (length(groups1) != length(groups2) || length(groups1) != length(labels)) {
      stop("groups1, groups2, and labels must have the same length")
    }

    return(data.frame(
      group1 = as.character(groups1),
      group2 = as.character(groups2),
      label = as.character(labels),
      stringsAsFactors = FALSE
    ))
  }

  # Method 2: ... contains vectors of length 3
  if (length(dots) > 0) {
    # Check if all elements are vectors of length 3
    if (all(sapply(dots, function(x) is.vector(x) && length(x) == 3))) {
      comparisons <- do.call(rbind, lapply(dots, function(x) {
        data.frame(
          group1 = as.character(x[1]),
          group2 = as.character(x[2]),
          label = as.character(x[3]),
          stringsAsFactors = FALSE
        )
      }))
      return(comparisons)
    }

    # Check if all elements are named lists
    if (all(sapply(dots, is.list))) {
      comparisons <- do.call(rbind, lapply(dots, function(x) {
        if (!all(c("from", "to", "label") %in% names(x))) {
          stop("Each comparison list must have 'from', 'to', and 'label' elements")
        }
        data.frame(
          group1 = as.character(x$from),
          group2 = as.character(x$to),
          label = as.character(x$label),
          stringsAsFactors = FALSE
        )
      }))
      return(comparisons)
    }
  }

  stop("Invalid comparison specification. See ?add_bracket_comparisons for examples.")
}


#' Get standard significance symbols from p-values
#'
#' Convert p-values to standard significance symbols
#'
#' @param p_values Numeric vector of p-values
#' @param symbols Character vector of symbols for different significance levels.
#'   Default: c("***", "**", "*", "ns")
#' @param breaks Numeric vector of p-value thresholds.
#'   Default: c(0, 0.001, 0.01, 0.05, 1)
#'
#' @return Character vector of significance symbols
#'
#' @export
#' @examples
#' p_to_symbol(c(0.0001, 0.005, 0.03, 0.15))
#' # Returns: "***"  "**"   "*"    "ns"
p_to_symbol <- function(p_values,
                       symbols = c("***", "**", "*", "ns"),
                       breaks = c(0, 0.001, 0.01, 0.05, 1)) {

  if (length(symbols) != length(breaks) - 1) {
    stop("Length of symbols must be length of breaks - 1")
  }

  cut(p_values,
      breaks = breaks,
      labels = symbols,
      include.lowest = TRUE,
      right = FALSE)
}


#' Validate comparison data frame
#'
#' Check if comparison data frame has required columns and valid data
#'
#' @param comparisons Data frame to validate
#'
#' @return TRUE if valid, otherwise throws error
#' @keywords internal
validate_comparisons <- function(comparisons) {

  if (!is.data.frame(comparisons)) {
    stop("comparisons must be a data frame")
  }

  required_cols <- c("group1", "group2", "label")
  missing_cols <- setdiff(required_cols, names(comparisons))

  if (length(missing_cols) > 0) {
    stop(sprintf("comparisons missing required columns: %s",
                paste(missing_cols, collapse = ", ")))
  }

  if (nrow(comparisons) == 0) {
    warning("comparisons data frame is empty")
    return(TRUE)
  }

  # Check for NA values
  if (any(is.na(comparisons[required_cols]))) {
    stop("comparisons contains NA values in required columns")
  }

  TRUE
}
