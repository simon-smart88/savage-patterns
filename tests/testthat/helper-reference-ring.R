# Pure-R oracle for the ring pattern, transliterated directly from
# shiny/R/ring_module.R's svg_pattern reactive, including the `seq(low_half:1)`
# gotcha (seq() on a vector argument returns seq_along() of it) and the
# "unique values used as 1-indexed row lookups" used to build the
# animation-delay CSS. Base R only.

reference_ring_data <- function(reps, radius, bulge, space, breath, speed) {
  low_half <- (reps - 1) / 2
  high_half <- (reps + 1) / 2

  element_mat <- matrix(c(
    rep(seq(1:reps), reps),
    rep(seq(1:reps), each = reps),
    rep(c(0:low_half, (low_half - 1):0), reps) +
      c(rep(seq(1:high_half), each = reps), rev(rep(seq(low_half:1), each = reps)))
  ), nrow = reps^2, byrow = FALSE)

  breath_frac <- breath / 100
  radius_bulge <- (element_mat[, 3] / reps * 2) * (bulge / 10)

  cx <- radius + (element_mat[, 1] * space)
  cy <- radius + (element_mat[, 2] * space)
  r_start <- (radius * (1 - breath_frac)) + radius_bulge
  r_end <- (radius * (1 + breath_frac)) + radius_bulge

  centre <- radius + (element_mat[ceiling(reps / 2), 1] * space)
  edge <- centre - (radius + (element_mat[3, 1] * space))
  top_corner <- centre - edge
  bottom_corner <- (centre + edge) - top_corner

  elements_sub <- element_mat[unique(element_mat[, 3]), , drop = FALSE]
  delay_css <- paste(
    sprintf(
      ".circle_%s { animation-delay: %ss;}",
      elements_sub[, 1], -(speed / reps) * elements_sub[, 1]
    ),
    collapse = " "
  )

  list(cls = element_mat[, 3], cx = cx, cy = cy, r_start = r_start, r_end = r_end,
       top_corner = top_corner, bottom_corner = bottom_corner, delay_css = delay_css)
}

reference_ring_svg <- function(reps, radius, bulge, space, breath, speed, stroke,
                                colour1, colour2, colour3) {
  d <- reference_ring_data(reps, radius, bulge, space, breath, speed)

  vars <- c("--colour_1", "--colour_2", "--colour_3", "--colour_2", "--colour_1")
  colour_keyframes <- paste(
    sprintf("%s%% {stroke: var(%s)}", c(0, 25, 50, 75, 100), vars),
    collapse = " "
  )

  elements <- paste(
    sprintf(
      '<circle class="circle_%s" cx="%s" cy="%s"><animate attributeName="r" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate></circle>',
      d$cls, d$cx, d$cy, d$r_start, d$r_end, d$r_start, speed
    ),
    collapse = "\n"
  )

  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="%s %s %s %s" height="100%%" id="pattern"><style>:root{--colour_1: %s;--colour_2: %s;--colour_3: %s;--stroke: %s;}circle {animation: col 30s linear infinite; fill: none; stroke-width: var(--stroke)}@keyframes col { %s }</style><style>%s</style>%s</svg>',
    d$top_corner, d$top_corner, d$bottom_corner, d$bottom_corner,
    colour1, colour2, colour3, stroke,
    colour_keyframes, d$delay_css, elements
  )
}
