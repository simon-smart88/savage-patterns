# Structural + numeric-tolerant SVG comparison.
#
# R's and JS's default number-to-string formatting differ (R's ~15
# significant-digit `as.character` vs JS's shortest-round-trip
# `Number#toString`) even when the underlying doubles were computed
# identically, so a raw string diff of two independently-generated SVGs is
# the wrong tool. Instead we parse both with xml2 and walk the trees,
# requiring exact tag/attribute-name/structural equality, but comparing any
# embedded numbers (in attribute values like `viewBox`/`cx`, or in `<style>`
# text like `--stroke: 0.5;` or `animate` `values="a; b; a"`) with a small
# tolerance.

number_pattern <- "-?[0-9]+\\.?[0-9]*(?:[eE][-+]?[0-9]+)?"

tokens_and_skeleton <- function(s) {
  m <- gregexpr(number_pattern, s)
  nums <- suppressWarnings(as.numeric(regmatches(s, m)[[1]]))
  skeleton <- gsub(number_pattern, "#", s)
  # Whitespace (indentation/newlines) differs between the hand-formatted JS
  # template literals and the dense R sprintf() output but is not meaningful
  # in CSS/SVG text content, so it's stripped entirely rather than collapsed.
  skeleton <- gsub("\\s+", "", skeleton)
  list(numbers = nums, skeleton = skeleton)
}

compare_text_numeric <- function(a, b, tol, path) {
  ta <- tokens_and_skeleton(a)
  tb <- tokens_and_skeleton(b)
  if (!identical(ta$skeleton, tb$skeleton)) {
    return(sprintf("%s: text skeleton mismatch:\n  a=%s\n  b=%s", path, a, b))
  }
  if (length(ta$numbers) != length(tb$numbers)) {
    return(sprintf("%s: number count mismatch:\n  a=%s\n  b=%s", path, a, b))
  }
  if (length(ta$numbers) == 0) {
    return(character(0))
  }
  bad <- abs(ta$numbers - tb$numbers) > tol
  if (any(bad)) {
    return(sprintf(
      "%s: numeric mismatch at position(s) %s (a=%s, b=%s)",
      path, paste(which(bad), collapse = ","),
      paste(ta$numbers[bad], collapse = ","), paste(tb$numbers[bad], collapse = ",")
    ))
  }
  character(0)
}

compare_nodes <- function(a, b, tol, path = "") {
  diffs <- character(0)
  name_a <- xml2::xml_name(a)
  name_b <- xml2::xml_name(b)
  if (!identical(name_a, name_b)) {
    return(sprintf("%s: tag mismatch %s vs %s", path, name_a, name_b))
  }
  path <- paste0(path, "/", name_a)

  attrs_a <- xml2::xml_attrs(a)
  attrs_b <- xml2::xml_attrs(b)
  keys <- union(names(attrs_a), names(attrs_b))
  for (k in keys) {
    va <- attrs_a[[k]]
    vb <- attrs_b[[k]]
    if (is.null(va) || is.null(vb)) {
      diffs <- c(diffs, sprintf("%s@%s: present in only one side (a=%s, b=%s)", path, k, va, vb))
      next
    }
    diffs <- c(diffs, compare_text_numeric(va, vb, tol, paste0(path, "@", k)))
  }

  children_a <- xml2::xml_children(a)
  children_b <- xml2::xml_children(b)
  if (length(children_a) == 0 && length(children_b) == 0) {
    txt_a <- xml2::xml_text(a)
    txt_b <- xml2::xml_text(b)
    diffs <- c(diffs, compare_text_numeric(txt_a, txt_b, tol, paste0(path, "#text")))
    return(diffs)
  }

  if (length(children_a) != length(children_b)) {
    return(c(diffs, sprintf("%s: child count mismatch %d vs %d", path, length(children_a), length(children_b))))
  }

  for (i in seq_along(children_a)) {
    diffs <- c(diffs, compare_nodes(children_a[[i]], children_b[[i]], tol, path))
  }
  diffs
}

expect_svg_equivalent <- function(svg_a, svg_b, tol = 1e-6, label = "") {
  doc_a <- xml2::read_xml(svg_a)
  doc_b <- xml2::read_xml(svg_b)
  diffs <- compare_nodes(doc_a, doc_b, tol)
  testthat::expect_true(
    length(diffs) == 0,
    info = paste(c(sprintf("SVG mismatch (%s):", label), diffs), collapse = "\n")
  )
}
