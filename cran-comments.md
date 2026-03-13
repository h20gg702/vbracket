# CRAN Submission Comments — vbracket 1.3.0

## Summary of changes since last CRAN release (v1.1.0)

### v1.3.0
* Added `bracket_layer_spacing` parameter for manual control of horizontal spacing between bracket layers.

### v1.2.0
* Added `line_length`, `line_width`, `item_spacing` parameters for manual override of auto-scaled legend symbol dimensions.

## R CMD check results

Checked on: macOS Sequoia 15.7.4 (aarch64), R 4.4.2

Status: 0 ERRORs, 0 WARNINGs, 3 NOTEs

NOTEs:
1. "unable to verify current time" — network issue in check environment, not a package problem.
2. HTML validation warnings — from an outdated local version of tidy; not expected to appear on CRAN servers.
3. "Found the following files/directories: 'vbracket-manual.tex'" — leftover from PDF manual check (pdflatex not installed locally); no actual LaTeX errors in the Rd files.

## Downstream dependencies

None.
