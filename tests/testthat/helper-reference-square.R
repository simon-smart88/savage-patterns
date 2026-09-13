# Pure-R oracle for the square pattern, transliterated directly from
# shiny/R/square_module.R's svg_pattern reactive: same seq()-on-a-vector
# gotcha as ring, the same value-as-row-index lookup for the delay CSS (whose
# selector target doesn't match the class the rects actually use), and the
# checkerboard switch only relabelling the colour-animation class. Base R only.

reference_square_data <- function(reps, bulge, internal, switch_on, speed, colour_speed, colour_dif) {
  top_corner <- 0
  bottom_corner <- 1000

  low_half <- (reps - 1) / 2
  high_half <- (reps + 1) / 2

  col1 <- rep(seq(1:reps), reps) - 1
  col2 <- rep(seq(1:reps), each = reps) - 1
  col3 <- rep(c(1:(low_half + 1), (low_half):1), reps)
  col4 <- rep(c(0:low_half, (low_half - 1):0), reps) +
    c(rep(seq(1:high_half), each = reps), rev(rep(seq(low_half:1), each = reps)))
  col5 <- rep(c(low_half:0, (1:low_half)), reps) +
    c(rev(rep(1:high_half, each = reps)), rep(2:high_half, each = reps))
  col6 <- rep(c((high_half):1, (2:high_half)), reps)

  element_mat <- matrix(c(col1, col2, col3, col4, col5, col6), nrow = reps^2, byrow = FALSE)

  if (bulge >= 0) {
    width_bulge <- element_mat[, 3]^(bulge / 5)
  } else {
    pattern <- rep(c((high_half):1, (2:high_half)), reps)
    width_bulge <- pattern^(abs(bulge) / 5)
  }

  if (switch_on) {
    element_mat[(element_mat[, 4] %% 2 == 1), 4] <- element_mat[(element_mat[, 4] %% 2 == 1), 5]
  }

  total_width <- sum(width_bulge)
  width_fraction <- width_bulge / total_width
  width <- width_fraction * reps * 1000
  element_mat <- cbind(element_mat, width)

  x <- rep(c(0, cumsum(width[1:(reps - 1)])), reps)
  element_mat <- cbind(element_mat, x)

  height <- rep(width[1:reps], each = reps)
  element_mat <- cbind(element_mat, height)

  y <- rep(x[1:reps], each = reps)
  element_mat <- cbind(element_mat, y)

  n <- reps^2
  rects <- vector("list", n)
  for (i in seq_len(n)) {
    m <- element_mat[i, ]
    w <- round(m[7], 3) + 0.5
    rx <- round(m[8], 3)
    h <- round(m[9], 3) + 0.5
    ry <- round(m[10], 3)

    center_x <- rx + (w * 0.5)
    center_y <- ry + (h * 0.5)

    initial_width <- round(w * (1 - (internal[1] / 100)), 3)
    initial_height <- round(h * (1 - (internal[1] / 100)), 3)
    final_width <- round(w * (1 - (internal[2] / 100)), 3)
    final_height <- round(h * (1 - (internal[2] / 100)), 3)

    initial_x <- round(center_x - (initial_width / 2), 3)
    initial_y <- round(center_y - (initial_height / 2), 3)
    final_x <- round(center_x - (final_width / 2), 3)
    final_y <- round(center_y - (final_height / 2), 3)

    rects[[i]] <- list(
      cls = m[4], x = rx, y = ry, width = w, height = h,
      initial_x = initial_x, initial_y = initial_y, final_x = final_x, final_y = final_y,
      initial_width = initial_width, initial_height = initial_height,
      final_width = final_width, final_height = final_height
    )
  }

  elements_sub <- element_mat[unique(element_mat[, 4]), , drop = FALSE]
  delay_css <- paste(
    apply(elements_sub, 1, function(m) {
      sprintf(
        ".rect_%s_out {animation-delay: %ss;} .rect_%s_in {animation-delay: %ss;}",
        m[1] + 1, -(colour_speed / reps) * m[4],
        m[1] + 1, (-((colour_speed / reps) * m[4])) - (colour_speed * (colour_dif / 100))
      )
    }),
    collapse = " "
  )

  list(rects = rects, top_corner = top_corner, bottom_corner = bottom_corner, delay_css = delay_css)
}

reference_square_svg <- function(reps, bulge, internal, switch_on, speed, colour_speed, colour_dif,
                                  colour1, colour2, colour3) {
  d <- reference_square_data(reps, bulge, internal, switch_on, speed, colour_speed, colour_dif)

  vars <- c("--colour_1", "--colour_2", "--colour_3", "--colour_2", "--colour_1")
  colour_keyframes <- paste(
    sprintf("%s%% {fill: var(%s)}", c(0, 25, 50, 75, 100), vars),
    collapse = " "
  )

  elements <- paste(
    vapply(d$rects, function(r) {
      sprintf(
        '<g><rect class="rect_%s_out" x="%s" y="%s" width="%s" height="%s"></rect><rect class="rect_%s_in"><animate attributeName="x" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate><animate attributeName="y" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate><animate attributeName="width" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate><animate attributeName="height" values="%s; %s; %s" dur="%ss" repeatCount="indefinite"></animate></rect></g>',
        r$cls, r$x, r$y, r$width, r$height,
        r$cls,
        r$initial_x, r$final_x, r$initial_x, speed,
        r$initial_y, r$final_y, r$initial_y, speed,
        r$initial_width, r$final_width, r$initial_width, speed,
        r$initial_height, r$final_height, r$initial_height, speed
      )
    }, character(1)),
    collapse = "\n"
  )

  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="%s %s %s %s" height="100%%" id="pattern"><style>:root{--colour_1: %s;--colour_2: %s;--colour_3: %s;--colour_speed: %ss;}rect {animation: col var(--colour_speed) linear infinite; fill: none;}@keyframes col { %s }</style><style>%s</style>%s</svg>',
    d$top_corner, d$top_corner, d$bottom_corner, d$bottom_corner,
    colour1, colour2, colour3, colour_speed,
    colour_keyframes, d$delay_css, elements
  )
}
