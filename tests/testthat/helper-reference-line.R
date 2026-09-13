# Pure-R oracle for the line pattern, transliterated directly from
# shiny/R/line_module.R's svg_pattern reactive (with `input$x` replaced by
# explicit arguments, and the final `element_mat[sample(nrow(element_mat)),]`
# shuffle omitted - see plan's "Porting the pattern maths" section for why).
# No shiny/glue dependency - base R only.

reference_line_data <- function(outer, inner, inner_size, inner_offset, breath, zoom) {
  outer_points <- outer
  inner_points <- inner

  inner_offset_calc <- (outer_points - inner_points) / 2
  outer_x <- c(seq(0, outer_points - 1), rep(c(0, outer_points - 1), outer_points - 2), seq(0, outer_points - 1))
  outer_y <- c(rep(0, outer_points), rep(seq(1, outer_points - 2), each = 2), rep(outer_points - 1, outer_points))
  inner_x <- c(seq(0, inner_points - 1), rep(c(0, inner_points - 1), inner_points - 2), seq(0, inner_points - 1)) + inner_offset_calc
  inner_y <- c(rep(0, inner_points), rep(seq(1, inner_points - 2), each = 2), rep(inner_points - 1, inner_points)) + inner_offset_calc

  old_inner_range <- diff(range(inner_y))
  new_inner_range <- outer_points * (inner_size / 100)
  inner_scale <- new_inner_range / old_inner_range
  inner_mean <- mean(inner_x)
  inner_x1 <- (inner_x - inner_mean) * inner_scale + inner_mean
  inner_y1 <- (inner_y - inner_mean) * inner_scale + inner_mean

  animation_factor <- 1 + (breath / 100)
  inner_x2 <- (inner_x1 - inner_mean) / animation_factor + inner_mean
  inner_y2 <- (inner_y1 - inner_mean) / animation_factor + inner_mean

  outer_x_set <- rep(outer_x, length(inner_x))
  outer_y_set <- rep(outer_y, length(inner_y))
  inner_x1_set <- rep(inner_x1, each = length(outer_x))
  inner_y1_set <- rep(inner_y1, each = length(outer_y))
  inner_x2_set <- rep(inner_x2, each = length(outer_x))
  inner_y2_set <- rep(inner_y2, each = length(outer_y))

  side_top <- outer_y_set == 0 & outer_x_set < outer_points - 1 & outer_x_set > 0
  side_bottom <- outer_y_set == outer_points - 1 & outer_x_set < outer_points - 1 & outer_x_set > 0
  side_left <- outer_x_set == 0
  side_right <- outer_x_set == outer_points - 1
  side_index <- (as.numeric(side_left) * 1) + (as.numeric(side_top) * 2) +
    (as.numeric(side_right) * 3) + (as.numeric(side_bottom) * 4)

  point_offset <- (inner_x[2] - inner_x[1]) * (inner_offset / 100)
  inner_y1_set[side_top] <- inner_y1_set[side_top] - point_offset
  inner_y1_set[side_bottom] <- inner_y1_set[side_bottom] + point_offset
  inner_x1_set[side_left] <- inner_x1_set[side_left] - point_offset
  inner_x1_set[side_right] <- inner_x1_set[side_right] + point_offset

  if (zoom) {
    top_corner <- min(inner_x1)
    bottom_corner <- max(inner_x1) - top_corner
  } else {
    top_corner <- 0
    bottom_corner <- outer_points - 1
  }

  list(
    x1 = outer_x_set, y1 = outer_y_set,
    x2 = inner_x1_set, y2 = inner_y1_set,
    x2_anim = inner_x2_set, y2_anim = inner_y2_set,
    side = side_index,
    top_corner = top_corner, bottom_corner = bottom_corner
  )
}

reference_line_svg <- function(outer, inner, inner_size, inner_offset, breath, zoom,
                                speed, stroke, opacity, colour1, colour2, colour3) {
  d <- reference_line_data(outer, inner, inner_size, inner_offset, breath, zoom)

  opacity_start <- opacity[1] / 100
  opacity_mid <- mean(opacity) / 100
  opacity_end <- opacity[2] / 100

  line_gradients <- paste0(
    '<stop offset="0%" stop-color="var(--colour_1)" stop-opacity="var(--opacity_start)"></stop>',
    '<stop offset="50%" stop-color="var(--colour_2)" stop-opacity="var(--opacity_mid)"></stop>',
    '<stop offset="100%" stop-color="var(--colour_3)" stop-opacity="var(--opacity_end)"></stop>'
  )

  elements <- paste0(
    sprintf(
      '<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="url(#gradient-opacity%d)"><animate attributeName="x2" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate><animate attributeName="y2" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate></line>',
      d$x1, d$x2, d$y1, d$y2, d$side,
      d$x2, d$x2_anim, d$x2, speed,
      d$y2, d$y2_anim, d$y2, speed
    ),
    collapse = "\n"
  )

  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="%s %s %s %s" height="100%%" id="pattern"><style>:root{--stroke: %s;--opacity_start: %s;--opacity_mid: %s;--opacity_end: %s;--colour_1: %s;--colour_2: %s;--colour_3: %s;}line { stroke-width: var(--stroke); }</style><defs><linearGradient id="gradient-opacity1" x1="0%%" y1="0%%" x2="100%%" y2="0%%">%s</linearGradient><linearGradient id="gradient-opacity2" x1="0%%" y1="0%%" x2="0%%" y2="100%%">%s</linearGradient><linearGradient id="gradient-opacity3" x1="100%%" y1="0%%" x2="0%%" y2="0%%">%s</linearGradient><linearGradient id="gradient-opacity4" x1="0%%" y1="100%%" x2="0%%" y2="0%%">%s</linearGradient></defs>%s</svg>',
    d$top_corner, d$top_corner, d$bottom_corner, d$bottom_corner,
    stroke / 100, opacity_start, opacity_mid, opacity_end, colour1, colour2, colour3,
    line_gradients, line_gradients, line_gradients, line_gradients,
    elements
  )
}
