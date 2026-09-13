# Parity tests: the real JS pattern generators (docs/js/patterns/*.js), run
# from R via the V8 package, must produce SVGs structurally and numerically
# equivalent (see helper-compare.R) to the pure-R oracle ports of the
# original shiny/R/*_module.R maths.
#
# Grid design: a full Cartesian product across every slider's min/mid/max is
# combinatorially huge and, since almost every parameter affects the maths
# independently (no cross-terms beyond the ones exercised explicitly below),
# mostly redundant. Instead each module tests a baseline, then varies one
# parameter at a time across its full slider range from that baseline (an OAT
# design), plus a handful of explicit combinations for the few places where
# parameters do genuinely interact (e.g. bulge sign x switch for square).
#
# NOTE: run from the repository root, e.g.
#   Rscript -e 'testthat::test_dir("tests/testthat")'
# testthat::test_dir() changes the working directory while running test
# files, so we locate the repo root (marked by savage-patterns.Rproj)
# relative to the original working directory rather than assuming cwd.

find_repo_root <- function() {
  candidates <- c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."))
  for (dir in candidates) {
    if (file.exists(file.path(dir, "savage-patterns.Rproj"))) {
      return(normalizePath(dir))
    }
  }
  stop("Could not locate repo root (savage-patterns.Rproj) from ", getwd())
}

js_dir <- file.path(find_repo_root(), "docs", "js")

new_pattern_ctx <- function() {
  ctx <- V8::v8()
  ctx$source(file.path(js_dir, "rcompat.js"))
  ctx$source(file.path(js_dir, "patterns", "line.js"))
  ctx$source(file.path(js_dir, "patterns", "ring.js"))
  ctx$source(file.path(js_dir, "patterns", "square.js"))
  ctx
}

ctx <- new_pattern_ctx()

colour_sets <- list(
  c("#ff0000", "#00ff00", "#0000ff"),
  c("#123456", "#abcdef", "#7f7f7f")
)

rows_from_baseline <- function(baseline, variations) {
  rows <- list(baseline)
  for (name in names(variations)) {
    for (value in variations[[name]]) {
      if (identical(value, baseline[[name]])) next
      row <- baseline
      row[[name]] <- value
      rows[[length(rows) + 1]] <- row
    }
  }
  rows
}

check_line <- function(row, i) {
  col <- colour_sets[[(i %% length(colour_sets)) + 1]]
  common <- list(speed = 30, stroke = 40, opacity = c(25, 75), colour1 = col[1], colour2 = col[2], colour3 = col[3])

  js_svg <- ctx$call("renderLineSVG", c(row, common))
  r_svg <- do.call(reference_line_svg, c(
    list(outer = row$outer, inner = row$inner, inner_size = row$innerSize,
         inner_offset = row$innerOffset, breath = row$breath, zoom = row$zoom),
    common
  ))
  expect_svg_equivalent(r_svg, js_svg, label = sprintf("line #%d: %s", i, paste(names(row), unlist(row), sep = "=", collapse = ",")))
}

test_that("line pattern SVG matches the R reference (OAT across the slider ranges)", {
  # inner = 2 (the slider minimum) is a known, documented exception - see
  # js/patterns/line.js - excluded here deliberately, not by oversight.
  baseline <- list(outer = 9, inner = 3, innerSize = 50, innerOffset = 50, breath = 20, zoom = FALSE)
  variations <- list(
    outer = c(4, 20), inner = c(3, 14), innerSize = c(10, 80),
    innerOffset = c(0, 80), breath = c(10, 50), zoom = c(TRUE, FALSE)
  )
  rows <- rows_from_baseline(baseline, variations)
  # a couple of explicit zoom x large-grid combinations
  rows <- c(rows, list(
    list(outer = 20, inner = 15, innerSize = 80, innerOffset = 80, breath = 50, zoom = TRUE),
    list(outer = 4, inner = 3, innerSize = 10, innerOffset = 0, breath = 10, zoom = TRUE)
  ))
  for (i in seq_along(rows)) check_line(rows[[i]], i)
})

check_ring <- function(row, i) {
  col <- colour_sets[[(i %% length(colour_sets)) + 1]]
  common <- list(speed = 30, stroke = 0.5, colour1 = col[1], colour2 = col[2], colour3 = col[3])

  js_svg <- ctx$call("renderRingSVG", c(row, common))
  r_svg <- do.call(reference_ring_svg, c(
    list(reps = row$reps, radius = row$radius, bulge = row$bulge, space = row$space, breath = row$breath),
    common
  ))
  expect_svg_equivalent(r_svg, js_svg, label = sprintf("ring #%d: %s", i, paste(names(row), unlist(row), sep = "=", collapse = ",")))
}

test_that("ring pattern SVG matches the R reference (OAT across the slider ranges)", {
  baseline <- list(reps = 25, radius = 30, bulge = 1, space = 10, breath = 20)
  variations <- list(
    reps = c(7, 45), radius = c(10, 50), bulge = c(0.1, 100),
    space = c(5, 30), breath = c(10, 30)
  )
  rows <- rows_from_baseline(baseline, variations)
  rows <- c(rows, list(
    list(reps = 7, radius = 10, bulge = 100, space = 5, breath = 10),
    list(reps = 45, radius = 50, bulge = 0.1, space = 30, breath = 30)
  ))
  for (i in seq_along(rows)) check_ring(rows[[i]], i)
})

check_square <- function(row, i) {
  col <- colour_sets[[(i %% length(colour_sets)) + 1]]
  js_svg <- ctx$call("renderSquareSVG", c(row, list(speed = 30, colourSpeed = 25, colour1 = col[1], colour2 = col[2], colour3 = col[3])))
  r_svg <- reference_square_svg(
    reps = row$reps, bulge = row$bulge, internal = row$internal,
    switch_on = row$switchOn, colour_dif = row$colourDif,
    speed = 30, colour_speed = 25, colour1 = col[1], colour2 = col[2], colour3 = col[3]
  )
  # tol wider than line/ring's default: square rounds every coordinate to
  # 3dp (see js/patterns/square.js and js/rcompat.js's roundR), and on rare
  # values that land within float noise of an exact .0005 tie, R's round()
  # and a straightforward IEEE-754 round can each legitimately land on
  # either side - a documented, provably tiny (<=0.001 on a 1000-unit
  # canvas) disagreement, not a transcription bug.
  expect_svg_equivalent(r_svg, js_svg, tol = 2e-3, label = sprintf("square #%d: %s", i, paste(names(row), sapply(row, paste, collapse = "/"), sep = "=", collapse = ",")))
}

test_that("square pattern SVG matches the R reference (OAT across the slider ranges)", {
  baseline <- list(reps = 19, bulge = 5, internal = c(10, 50), switchOn = FALSE, colourDif = 50)
  variations <- list(
    reps = list(7, 31),
    bulge = list(-10, -0.5, 0.5, 10),
    internal = list(c(10, 20), c(80, 90)),
    switchOn = list(TRUE),
    colourDif = list(5, 95)
  )
  rows <- rows_from_baseline(baseline, variations)
  # explicit bulge-sign x checkerboard interactions
  rows <- c(rows, list(
    list(reps = 7, bulge = -10, internal = c(10, 90), switchOn = TRUE, colourDif = 5),
    list(reps = 31, bulge = 10, internal = c(10, 90), switchOn = TRUE, colourDif = 95),
    list(reps = 7, bulge = 0, internal = c(10, 20), switchOn = TRUE, colourDif = 50)
  ))
  for (i in seq_along(rows)) check_square(rows[[i]], i)
})
