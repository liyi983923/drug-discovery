# Drug discovery and development workflow
# Reproducible 16:9 figure built with R/grid.
#
# Edit the three sections below to change:
#   1) colours and font
#   2) stage labels and supporting text
#   3) stage positions and taper geometry

library(grid)
library(ragg)
library(svglite)
library(systemfonts)
library(showtext)
library(magick)

detect_script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  if (length(file_arg)) return(sub("^--file=", "", file_arg[1]))

  if (sys.nframe() > 0) {
    for (i in rev(seq_len(sys.nframe()))) {
      candidate <- sys.frame(i)$ofile
      if (!is.null(candidate)) return(candidate)
    }
  }

  "drug_discovery_pipeline.R"
}

script_path <- normalizePath(detect_script_path(), mustWork = FALSE)
script_dir <- dirname(script_path)

# -----------------------------------------------------------------------------
# 1. STYLE SETTINGS — easiest place to change colours and font
# -----------------------------------------------------------------------------

font_family <- "Roboto Condensed"
font_regular <- file.path(script_dir, "fonts", "RobotoCondensed-Regular.ttf")
font_bold <- file.path(script_dir, "fonts", "RobotoCondensed-Bold.ttf")

if (!file.exists(font_regular) || !file.exists(font_bold)) {
  stop(
    "Roboto Condensed font files are missing. Expected:\n  ",
    font_regular, "\n  ", font_bold
  )
}

# Register bundled local fonts so R never silently falls back to another family.
register_font(font_family, plain = font_regular, bold = font_bold)

colours <- c(
  charcoal = "#222222",
  wine     = "#982B2B",
  orange   = "#F47720",
  green    = "#459943",
  sage     = "#A8C7A0",
  warmgrey = "#E9E7E2",
  white    = "#FFFFFF"
)

line_width <- 1.7

# -----------------------------------------------------------------------------
# 2. TEXT SETTINGS — edit labels here without touching drawing code
# -----------------------------------------------------------------------------

stage_text <- list(
  list(
    number = "01",
    title = "TARGET DISCOVERY",
    details = c("Target identification", "Target validation", "Prioritization")
  ),
  list(
    number = "02",
    title = "MOLECULAR DESIGN",
    details = c("Hit finding", "Hit-to-lead", "Lead optimization", "Candidate selection")
  ),
  list(
    number = "03",
    title = c("PRECLINICAL", "DEVELOPMENT"),
    details = c("In vitro pharmacology", "ADME / PK", "In vivo efficacy",
                "Safety & toxicology", "IND-enabling")
  ),
  list(
    number = "04",
    title = c("CLINICAL", "DEVELOPMENT"),
    details = c("Phase I: safety", "Phase II: efficacy",
                "Phase III: confirmation", "Regulatory review")
  ),
  list(
    number = "05",
    title = "MARKETED MEDICINE",
    details = character(0)
  )
)

# Current evidence-based capability profile (editable).
# Scores reflect demonstrated ownership/participation, not future potential.
capability <- data.frame(
  stage_short = c("TARGET", "DESIGN", "PRECLINICAL", "CLINICAL", "MARKETED"),
  score = c(9, 7, 7, 7, 2),
  basis = c(
    "Owned target selection plus HTS platform design and statistics",
    "Strong hit finding; participated in H2L and lead optimization; medicinal chemistry is an interface gap",
    "IND involvement plus pharmacology, PK/PD and biomarker breadth; tox/CMC depth still developing",
    "Participated across Phase 1a, 1b and 2; owned PD, biomarker and omics components",
    "No direct evidence yet of commercial manufacturing, NDA or launch ownership"
  ),
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(capability) == length(stage_text),
  all(capability$score >= 1 & capability$score <= 10)
)

# -----------------------------------------------------------------------------
# 3. GEOMETRY — normalized coordinates, so the figure stays exactly 16:9
# -----------------------------------------------------------------------------

stages <- data.frame(
  left   = c(0.035, 0.210, 0.397, 0.563, 0.717),
  right  = c(0.205, 0.392, 0.558, 0.712, 0.852),
  top_l  = c(0.700, 0.660, 0.630, 0.610, 0.590),
  top_r  = c(0.660, 0.630, 0.610, 0.590, 0.575),
  bot_l  = c(0.300, 0.340, 0.370, 0.390, 0.410),
  bot_r  = c(0.340, 0.370, 0.390, 0.410, 0.425),
  fill   = unname(colours[c("wine", "orange", "green", "sage", "warmgrey")]),
  stringsAsFactors = FALSE
)

# Subtle gradients reproduce the reference image; solid colours above remain
# available for supporting bars and easy palette editing.
stage_fills <- list(
  linearGradient(c("#DC3A3D", "#A71C22"), x1 = 0, y1 = 1, x2 = 1, y2 = 0),
  linearGradient(c("#FF8523", "#D95811"), x1 = 0, y1 = 1, x2 = 1, y2 = 0),
  linearGradient(c("#60AC53", "#2E7F34"), x1 = 0, y1 = 1, x2 = 1, y2 = 0),
  linearGradient(c("#C1DBBB", "#83A97E"), x1 = 0, y1 = 1, x2 = 1, y2 = 0),
  linearGradient(c("#F5F4F1", "#D2CFC9"), x1 = 0, y1 = 1, x2 = 1, y2 = 0)
)

details_x <- c(0.055, 0.235, 0.415, 0.585)

# -----------------------------------------------------------------------------
# Drawing helpers
# -----------------------------------------------------------------------------

npc_x <- function(x) unit(x, "npc")
npc_y <- function(y) unit(y, "npc")

draw_text <- function(label, x, y, size, colour = colours["charcoal"],
                      bold = FALSE, hjust = 0.5, vjust = 0.5,
                      lineheight = 0.95) {
  grid.text(
    label,
    x = npc_x(x), y = npc_y(y),
    just = c(hjust, vjust),
    gp = gpar(
      fontfamily = font_family,
      fontface = if (bold) "bold" else "plain",
      fontsize = size,
      col = colour,
      lineheight = lineheight
    )
  )
}

draw_score_badge <- function(x, y, score, dark = FALSE) {
  pushViewport(viewport(
    x = npc_x(x), y = npc_y(y),
    width = npc_x(0.046), height = npc_y(0.031)
  ))

  badge_border <- if (dark) colours["charcoal"] else colours["white"]
  badge_fill <- if (dark) "#FFFFFFCC" else "#FFFFFF24"
  badge_text <- if (dark) colours["charcoal"] else colours["white"]

  grid.roundrect(
    x = 0.5, y = 0.5, width = 0.96, height = 0.90,
    r = unit(0.10, "snpc"),
    gp = gpar(fill = badge_fill, col = badge_border, lwd = 1.4)
  )
  grid.text(
    sprintf("%d/10", score),
    x = 0.5, y = 0.5,
    gp = gpar(
      fontfamily = font_family, fontface = "bold",
      fontsize = 10.5, col = badge_text
    )
  )
  popViewport()
}

draw_bracket <- function(x0, x1, y, label) {
  grid.lines(
    npc_x(c(x0, x0, x1, x1)), npc_y(c(y - 0.020, y, y, y - 0.020)),
    gp = gpar(col = colours["charcoal"], lwd = line_width)
  )
  draw_text(label, mean(c(x0, x1)), y + 0.027, 17, bold = TRUE)
}

draw_target_icon <- function(x, y, scale = 1) {
  for (r in c(0.038, 0.025, 0.012) * scale) {
    grid.circle(npc_x(x), npc_y(y), r = npc_x(r),
                gp = gpar(fill = NA, col = colours["white"], lwd = 2.6))
  }
  grid.lines(npc_x(c(x + 0.006, x + 0.035)),
             npc_y(c(y + 0.006, y + 0.041)),
             gp = gpar(col = colours["white"], lwd = 2.6, lineend = "round"))
  grid.lines(npc_x(c(x + 0.035, x + 0.035, x + 0.025)),
             npc_y(c(y + 0.041, y + 0.028, y + 0.041)),
             gp = gpar(col = colours["white"], lwd = 2.6, lineend = "round"))
}

draw_molecule_icon <- function(x, y, scale = 1) {
  rx <- 0.021 * scale
  ry <- 0.034 * scale
  theta <- seq(pi / 3, 2 * pi + pi / 3, length.out = 7)
  hx <- x + rx * cos(theta)
  hy <- y + ry * sin(theta)
  grid.lines(npc_x(hx), npc_y(hy),
             gp = gpar(col = colours["white"], lwd = 2.6))

  nodes <- rbind(
    c(x - 0.034 * scale, y + 0.039 * scale),
    c(x + 0.034 * scale, y + 0.040 * scale),
    c(x + 0.047 * scale, y - 0.010 * scale),
    c(x + 0.006 * scale, y - 0.052 * scale),
    c(x - 0.039 * scale, y - 0.022 * scale)
  )
  anchors <- rbind(
    c(x - 0.017 * scale, y + 0.021 * scale),
    c(x + 0.017 * scale, y + 0.021 * scale),
    c(x + 0.021 * scale, y - 0.007 * scale),
    c(x + 0.003 * scale, y - 0.031 * scale),
    c(x - 0.020 * scale, y - 0.012 * scale)
  )

  for (i in seq_len(nrow(nodes))) {
    grid.lines(npc_x(c(anchors[i, 1], nodes[i, 1])),
               npc_y(c(anchors[i, 2], nodes[i, 2])),
               gp = gpar(col = colours["white"], lwd = 2.4))
    grid.circle(npc_x(nodes[i, 1]), npc_y(nodes[i, 2]), r = npc_x(0.0063 * scale),
                gp = gpar(fill = stages$fill[2], col = colours["white"], lwd = 2.4))
  }
}

draw_flask_icon <- function(x, y, scale = 1) {
  xx <- x + c(-0.013, -0.013, -0.036, -0.041, 0.041, 0.036, 0.013, 0.013) * scale
  yy <- y + c(0.050, 0.014, -0.050, -0.061, -0.061, -0.050, 0.014, 0.050) * scale
  grid.lines(npc_x(xx), npc_y(yy),
             gp = gpar(col = colours["white"], lwd = 2.7, linejoin = "round"))
  grid.lines(npc_x(x + c(-0.016, 0.016) * scale),
             npc_y(c(y + 0.050 * scale, y + 0.050 * scale)),
             gp = gpar(col = colours["white"], lwd = 2.7))
  grid.lines(npc_x(x + c(-0.030, 0.030) * scale),
             npc_y(c(y - 0.026 * scale, y - 0.026 * scale)),
             gp = gpar(col = colours["white"], lwd = 2.2))

  # Subtle liquid meniscus, matching the reference flask icon.
  t <- seq(-1, 1, length.out = 30)
  grid.lines(
    npc_x(x + 0.029 * scale * t),
    npc_y(y - 0.030 * scale + 0.002 * scale * cos(pi * t)),
    gp = gpar(col = colours["white"], lwd = 1.8)
  )
}

draw_clinical_icon <- function(x, y, scale = 1) {
  # Medical cross
  cross_x <- x + c(-0.007, 0.007, 0.007, 0.020, 0.020, 0.007,
                   0.007, -0.007, -0.007, -0.020, -0.020, -0.007, -0.007) * scale
  cross_y <- y + c(0.052, 0.052, 0.039, 0.039, 0.026, 0.026,
                   0.013, 0.013, 0.026, 0.026, 0.039, 0.039, 0.052) * scale
  grid.lines(npc_x(cross_x), npc_y(cross_y),
             gp = gpar(col = colours["white"], lwd = 2.4, linejoin = "round"))

  centres <- x + c(-0.030, 0, 0.030) * scale
  radii <- c(0.009, 0.011, 0.009) * scale
  for (i in seq_along(centres)) {
    head_y <- y - 0.004 * scale
    grid.circle(npc_x(centres[i]), npc_y(head_y), r = npc_x(radii[i]),
                gp = gpar(fill = NA, col = colours["white"], lwd = 2.2))

    # Rounded shoulders instead of the previous straight baseline.
    theta <- seq(pi, 0, length.out = 30)
    shoulder_x <- centres[i] + 0.015 * scale * cos(theta)
    shoulder_y <- y - 0.030 * scale + 0.011 * scale * sin(theta)
    grid.lines(npc_x(shoulder_x), npc_y(shoulder_y),
               gp = gpar(col = colours["white"], lwd = 2.2, lineend = "round"))
  }
}

draw_pill_icon <- function(x, y, width = 0.034, height = 0.084,
                           colour = colours["charcoal"]) {
  pushViewport(viewport(x = npc_x(x), y = npc_y(y),
                        width = npc_x(width), height = npc_y(height),
                        angle = -45))
  # Construct a true two-piece capsule: white upper half, black lower half,
  # diagonal split after rotation, and a single clean outer outline.
  cx <- 0.5
  x_left <- 0.18
  x_right <- 0.82
  y_bottom <- 0.04
  y_top <- 0.96
  radius_x <- (x_right - x_left) / 2
  physical_aspect <- (width * 16) / (height * 9)
  radius_y <- radius_x * physical_aspect
  top_cy <- y_top - radius_y
  bottom_cy <- y_bottom + radius_y

  theta_top <- seq(pi, 0, length.out = 40)
  theta_bottom <- seq(0, -pi, length.out = 40)
  outline_x <- c(
    cx + radius_x * cos(theta_top),
    x_right,
    cx + radius_x * cos(theta_bottom),
    x_left
  )
  outline_y <- c(
    top_cy + radius_y * sin(theta_top),
    bottom_cy,
    bottom_cy + radius_y * sin(theta_bottom),
    top_cy
  )

  grid.polygon(unit(outline_x, "npc"), unit(outline_y, "npc"),
               gp = gpar(fill = colours["white"], col = NA))

  theta_black <- seq(pi, 2 * pi, length.out = 40)
  black_x <- c(x_left, cx + radius_x * cos(theta_black), x_right)
  black_y <- c(0.5, bottom_cy + radius_y * sin(theta_black), 0.5)
  grid.polygon(unit(black_x, "npc"), unit(black_y, "npc"),
               gp = gpar(fill = colour, col = NA))

  grid.lines(unit(c(x_left, x_right), "npc"), unit(c(0.5, 0.5), "npc"),
             gp = gpar(col = colour, lwd = 2.1))
  grid.polygon(unit(outline_x, "npc"), unit(outline_y, "npc"),
               gp = gpar(fill = NA, col = colour, lwd = 2.5, linejoin = "round"))
  popViewport()
}

draw_stage <- function(i, show_capability = TRUE) {
  s <- stages[i, ]
  if (i < 5) {
    grid.polygon(
      npc_x(c(s$left, s$right, s$right, s$left)),
      npc_y(c(s$top_l, s$top_r, s$bot_r, s$bot_l)),
      gp = gpar(fill = stage_fills[[i]], col = colours["white"], lwd = 3.2)
    )
  } else {
    # The final stage has the small arrow-like point visible in the reference.
    shoulder <- s$right - 0.015
    tip <- s$right
    grid.polygon(
      npc_x(c(s$left, shoulder, tip, shoulder, s$left)),
      npc_y(c(s$top_l, s$top_r, 0.505, s$bot_r, s$bot_l)),
      gp = gpar(fill = stage_fills[[i]], col = colours["white"], lwd = 3.2)
    )
  }

  cx <- mean(c(s$left, s$right))
  text_colour <- if (i == 5) colours["charcoal"] else colours["white"]

  number_y <- c(0.600, 0.580, 0.570, 0.550, 0.535)[i]
  title_y1 <- c(0.535, 0.525, 0.515, 0.505, 0.490)[i]
  title_y2 <- c(NA, NA, 0.480, 0.475, NA)[i]
  icon_y   <- c(0.425, 0.430, 0.430, 0.417, 0.425)[i]

  if (show_capability) {
    draw_text(stage_text[[i]]$number, cx - 0.022, number_y, 25.5,
              colour = text_colour, bold = TRUE)
    draw_score_badge(
      cx + 0.028, number_y,
      capability$score[i], dark = (i == 5)
    )
  } else {
    draw_text(stage_text[[i]]$number, cx, number_y, 25.5,
              colour = text_colour, bold = TRUE)
  }

  title <- stage_text[[i]]$title
  if (length(title) == 1) {
    title_size <- if (i == 5) 12.2 else 16.5
    draw_text(title, cx, title_y1, title_size,
              colour = text_colour, bold = TRUE)
  } else {
    draw_text(title[1], cx, title_y1, 16,
              colour = text_colour, bold = TRUE)
    draw_text(title[2], cx, title_y2, 16,
              colour = text_colour, bold = TRUE)
  }

  if (i == 1) draw_target_icon(cx, icon_y, 0.95)
  if (i == 2) draw_molecule_icon(cx, icon_y, 0.95)
  if (i == 3) draw_flask_icon(cx, icon_y, 0.58)
  if (i == 4) draw_clinical_icon(cx, icon_y, 0.60)
  if (i == 5) draw_pill_icon(cx, icon_y, width = 0.032, height = 0.076)
}

draw_gate <- function(x) {
  grid.circle(npc_x(x), npc_y(0.505), r = npc_x(0.020),
              gp = gpar(fill = colours["white"], col = colours["charcoal"], lwd = 2.4))
  grid.lines(npc_x(c(x, x)), npc_y(c(0.479, 0.531)),
             gp = gpar(col = colours["charcoal"], lwd = 2.0))
}

draw_details <- function(i, x) {
  labels <- stage_text[[i]]$details
  if (!length(labels)) return(invisible(NULL))

  top <- 0.268
  spacing <- 0.040
  bottom <- top - (length(labels) - 1) * spacing - 0.018
  grid.lines(npc_x(c(x, x)), npc_y(c(top + 0.012, bottom)),
             gp = gpar(col = stages$fill[i], lwd = 2.1))

  for (j in seq_along(labels)) {
    draw_text(labels[j], x + 0.014, top - (j - 1) * spacing,
              14.5, hjust = 0, vjust = 0.5)
  }
}

draw_capability_heatmap <- function() {
  heat_colours <- colorRampPalette(
    c("#F6EAEA", "#D98B8D", colours["wine"])
  )(10)

  cell_centres <- rowMeans(stages[, c("left", "right")])
  cell_widths <- (stages$right - stages$left) * 0.82
  cell_y <- 0.043
  cell_height <- 0.042

  draw_text(
    "CAPABILITY HEATMAP · CURRENT EVIDENCE PROFILE",
    0.055, 0.082, 11.5, bold = TRUE, hjust = 0
  )

  for (i in seq_len(nrow(capability))) {
    cell_colour <- heat_colours[capability$score[i]]
    label_colour <- if (capability$score[i] >= 6) {
      colours["white"]
    } else {
      colours["charcoal"]
    }

    grid.roundrect(
      x = npc_x(cell_centres[i]), y = npc_y(cell_y),
      width = npc_x(cell_widths[i]), height = npc_y(cell_height),
      r = unit(0.035, "snpc"),
      gp = gpar(fill = cell_colour, col = colours["white"], lwd = 1.4)
    )
    grid.text(
      paste0(capability$stage_short[i], "\n", capability$score[i], "/10"),
      x = npc_x(cell_centres[i]), y = npc_y(cell_y),
      gp = gpar(
        fontfamily = font_family, fontface = "bold",
        fontsize = 9.3, col = label_colour, lineheight = 0.88
      )
    )
  }

  # Compact 1–10 legend to make heat intensity interpretable.
  legend_left <- 0.870
  legend_right <- 0.965
  legend_width <- (legend_right - legend_left) / 10
  for (s in 1:10) {
    grid.rect(
      x = npc_x(legend_left + (s - 0.5) * legend_width),
      y = npc_y(cell_y),
      width = npc_x(legend_width), height = npc_y(0.018),
      gp = gpar(fill = heat_colours[s], col = NA)
    )
  }
  draw_text("1 LOW", legend_left, 0.067, 8.5, hjust = 0)
  draw_text("10 HIGH", legend_right, 0.067, 8.5, hjust = 1)
}

draw_figure <- function(show_capability = TRUE) {
  grid.newpage()

  # White canvas and editorial frame
  grid.rect(gp = gpar(fill = colours["white"], col = colours["charcoal"],
                      lwd = line_width))

  # Title and divider
  draw_text("DRUG DISCOVERY & DEVELOPMENT", 0.045, 0.900, 34,
            bold = TRUE, hjust = 0)
  grid.lines(npc_x(c(0.040, 0.790)), npc_y(c(0.842, 0.842)),
             gp = gpar(col = colours["charcoal"], lwd = line_width))

  # Stage group brackets
  draw_bracket(0.047, 0.390, 0.742, "DISCOVERY STAGE")
  draw_bracket(0.440, 0.750, 0.742, "DEVELOPMENT STAGE")

  # Main tapered pathway
  for (i in seq_len(nrow(stages))) {
    draw_stage(i, show_capability = show_capability)
  }
  for (x in stages$right[1:4]) draw_gate(x)

  # Arrow and marketed outcome
  grid.lines(npc_x(c(0.862, 0.897)), npc_y(c(0.505, 0.505)),
             arrow = arrow(type = "closed", length = unit(0.13, "inches")),
             gp = gpar(col = colours["charcoal"], fill = colours["charcoal"], lwd = 2.5))
  draw_pill_icon(0.930, 0.510, width = 0.043, height = 0.102)
  draw_text("1 MARKETED DRUG", 0.930, 0.420, 15.5, bold = TRUE)

  # Supporting details
  for (i in 1:4) draw_details(i, details_x[i])

  # Optional bottom-stage capability heatmap
  if (show_capability) draw_capability_heatmap()
}

draw_stage_capability_overlay <- function() {
  badge_x <- stages$right - 0.025
  badge_y <- c(0.630, 0.603, 0.582, 0.558, 0.548)

  for (i in seq_len(nrow(capability))) {
    draw_score_badge(
      badge_x[i], badge_y[i], capability$score[i], dark = (i == 5)
    )
  }
}

draw_stage_progress_bars <- function() {
  centres <- rowMeans(stages[, c("left", "right")])
  widths <- pmin((stages$right - stages$left) * 0.82, 0.138)
  filled_colours <- c(
    colours["wine"], colours["orange"], colours["green"],
    "#86AA80", colours["charcoal"]
  )
  empty_colour <- "#E8E5E1"
  label_y <- 0.053
  bar_y <- 0.027
  bar_height <- 0.016
  gap <- 0.0023

  draw_text(
    "STAGE CAPABILITY PROFILE (1–10)",
    0.055, 0.082, 10.5, bold = TRUE, hjust = 0
  )

  for (i in seq_len(nrow(capability))) {
    left <- centres[i] - widths[i] / 2
    cell_width <- (widths[i] - 9 * gap) / 10

    draw_text(
      capability$stage_short[i], left, label_y, 9.2,
      bold = TRUE, hjust = 0
    )
    draw_text(
      sprintf("%d/10", capability$score[i]), left + widths[i], label_y, 9.2,
      bold = TRUE, hjust = 1
    )

    for (j in 1:10) {
      cell_x <- left + (j - 0.5) * cell_width + (j - 1) * gap
      cell_colour <- if (j <= capability$score[i]) {
        filled_colours[i]
      } else {
        empty_colour
      }
      grid.roundrect(
        x = npc_x(cell_x), y = npc_y(bar_y),
        width = npc_x(cell_width), height = npc_y(bar_height),
        r = unit(0.06, "snpc"),
        gp = gpar(fill = cell_colour, col = NA)
      )
    }
  }
}

draw_hybrid_figure <- function(flow_raster, crop_box, show_capability = FALSE) {
  grid.newpage()

  # R-drawn surround: canvas, border, title, section brackets and details.
  grid.rect(gp = gpar(fill = colours["white"], col = colours["charcoal"],
                      lwd = line_width))
  draw_text("DRUG DISCOVERY & DEVELOPMENT", 0.045, 0.900, 34,
            bold = TRUE, hjust = 0)
  grid.lines(npc_x(c(0.040, 0.790)), npc_y(c(0.842, 0.842)),
             gp = gpar(col = colours["charcoal"], lwd = line_width))
  draw_bracket(0.047, 0.390, 0.742, "DISCOVERY STAGE")
  draw_bracket(0.440, 0.750, 0.742, "DEVELOPMENT STAGE")

  # The exact cropped process flow from the source PNG is inserted here.
  grid.raster(
    flow_raster,
    x = npc_x(crop_box$x + crop_box$width / 2),
    y = npc_y(1 - crop_box$y - crop_box$height / 2),
    width = npc_x(crop_box$width),
    height = npc_y(crop_box$height),
    interpolate = TRUE
  )

  for (i in 1:4) draw_details(i, details_x[i])

  if (show_capability) {
    draw_stage_capability_overlay()
    draw_stage_progress_bars()
  }
}

# -----------------------------------------------------------------------------
# Export — run this script to regenerate all three formats
# -----------------------------------------------------------------------------

output_dir <- script_dir

png_file <- file.path(output_dir, "drug_discovery_pipeline_capability_R_16x9.png")
png_file_600 <- file.path(output_dir, "drug_discovery_pipeline_capability_R_16x9_600dpi.png")
svg_file <- file.path(output_dir, "drug_discovery_pipeline_capability_R_16x9.svg")
pdf_file <- file.path(output_dir, "drug_discovery_pipeline_capability_R_16x9.pdf")

agg_png(png_file, width = 16, height = 9, units = "in", res = 300,
        background = "white", scaling = 1)
draw_figure()
dev.off()

agg_png(png_file_600, width = 16, height = 9, units = "in", res = 600,
        background = "white", scaling = 1)
draw_figure()
dev.off()

svglite(svg_file, width = 16, height = 9,
        bg = "white", system_fonts = list(sans = font_family))
draw_figure()
dev.off()

# Use showtext for PDF so the chosen condensed font is embedded as vector paths
# without requiring X11/Cairo on macOS.
font_add(font_family, regular = font_regular, bold = font_bold)

pdf(pdf_file, width = 16, height = 9, bg = "white", useDingbats = FALSE)
showtext_begin()
draw_figure()
showtext_end()
dev.off()

# Base figure matching the reference: no capability badges or heatmap.
# The PNG is strict UHD 4K (3840 x 2160); the SVG remains fully vector-based.
base_png_4k <- file.path(output_dir, "drug_discovery_pipeline_base_R_4K.png")
base_svg <- file.path(output_dir, "drug_discovery_pipeline_base_R_16x9.svg")

agg_png(base_png_4k, width = 3840, height = 2160, units = "px", res = 300,
        background = "white", scaling = 1)
draw_figure(show_capability = FALSE)
dev.off()

svglite(base_svg, width = 16, height = 9,
        bg = "white", system_fonts = list(sans = font_family))
draw_figure(show_capability = FALSE)
dev.off()

# Hybrid version requested by the user:
# 1) crop the process-flow band from the source PNG and save it separately;
# 2) redraw the surrounding content in R and insert the crop at its exact
#    original normalized coordinates.
reference_file <- file.path(output_dir, "drug_discovery_pipeline_reference.png")
flow_crop_file <- file.path(output_dir, "drug_discovery_pipeline_flowchart_crop_4K.png")
hybrid_png_4k <- file.path(output_dir, "drug_discovery_pipeline_hybrid_R_4K.png")
hybrid_svg <- file.path(output_dir, "drug_discovery_pipeline_hybrid_R_16x9.svg")
hybrid_capability_png_4k <- file.path(
  output_dir, "drug_discovery_pipeline_hybrid_capability_R_4K.png"
)
hybrid_capability_svg <- file.path(
  output_dir, "drug_discovery_pipeline_hybrid_capability_R_16x9.svg"
)

if (!file.exists(reference_file)) {
  stop("Reference PNG is missing: ", reference_file)
}

reference_image <- image_read(reference_file)
reference_4k <- image_resize(reference_image, "3840x2160!", filter = "Lanczos")

# Crop box measured on the 1680 x 945 reference image.
source_width <- 1680
source_height <- 945
crop_left <- 50
crop_top <- 277
crop_right <- 1665
crop_bottom <- 669

crop_box <- list(
  x = crop_left / source_width,
  y = crop_top / source_height,
  width = (crop_right - crop_left) / source_width,
  height = (crop_bottom - crop_top) / source_height
)

crop_geometry <- geometry_area(
  width = round(crop_box$width * 3840),
  height = round(crop_box$height * 2160),
  x_off = round(crop_box$x * 3840),
  y_off = round(crop_box$y * 2160)
)
flow_crop <- image_crop(reference_4k, crop_geometry, repage = TRUE)
image_write(flow_crop, flow_crop_file, format = "png", quality = 100)
flow_raster <- as.raster(flow_crop)

agg_png(hybrid_png_4k, width = 3840, height = 2160, units = "px", res = 300,
        background = "white", scaling = 1)
draw_hybrid_figure(flow_raster, crop_box)
dev.off()

svglite(hybrid_svg, width = 16, height = 9,
        bg = "white", system_fonts = list(sans = font_family))
draw_hybrid_figure(flow_raster, crop_box)
dev.off()

agg_png(hybrid_capability_png_4k, width = 3840, height = 2160,
        units = "px", res = 300, background = "white", scaling = 1)
draw_hybrid_figure(flow_raster, crop_box, show_capability = TRUE)
dev.off()

svglite(hybrid_capability_svg, width = 16, height = 9,
        bg = "white", system_fonts = list(sans = font_family))
draw_hybrid_figure(flow_raster, crop_box, show_capability = TRUE)
dev.off()

message("Created:")
message("  ", png_file)
message("  ", png_file_600)
message("  ", svg_file)
message("  ", pdf_file)
message("  ", base_png_4k)
message("  ", base_svg)
message("  ", flow_crop_file)
message("  ", hybrid_png_4k)
message("  ", hybrid_svg)
message("  ", hybrid_capability_png_4k)
message("  ", hybrid_capability_svg)
