# Drug discovery and development capability profile
# 药物发现与开发流程及个人阶段能力图
#
# 使用方法：在终端进入本脚本所在目录，然后运行：
#   Rscript drug_discovery_pipeline.R
#
# 脚本会在同一目录生成以下两个文件：
#   drug_discovery_pipeline_4K.png   (3840 x 2160)
#   drug_discovery_pipeline.svg      (16:9 SVG)
#
# 最常需要修改的内容集中在第 2 节：
#   1) stage_details：各阶段下方的工作内容
#   2) capability$score：五个阶段的能力评分（1–10）
#   3) colours：整张图的配色
#
# 说明：流程图主体来自 drug_discovery_pipeline_reference.png 的裁切区域；
# 标题、阶段括号、评分徽章、说明文字和底部能力条均由 R 重新绘制。

# -----------------------------------------------------------------------------
# 1. 依赖包与项目路径
# -----------------------------------------------------------------------------

required_packages <- c("grid", "ragg", "svglite", "systemfonts", "magick")
# requireNamespace() 只检查包是否存在，不会提前加载包或打印启动信息。
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install the required R packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(grid)
  library(ragg)
  library(svglite)
  library(systemfonts)
  library(magick)
})

# 获取当前 R 脚本的实际路径，确保从其他目录调用 Rscript 时仍能找到素材。
detect_script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)

  if (length(file_arg)) {
    return(sub("^--file=", "", file_arg[1]))
  }

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

reference_file <- file.path(
  script_dir, "drug_discovery_pipeline_reference.png"
)
font_regular <- file.path(
  script_dir, "fonts", "RobotoCondensed-Regular.ttf"
)
font_bold <- file.path(
  script_dir, "fonts", "RobotoCondensed-Bold.ttf"
)

required_files <- c(reference_file, font_regular, font_bold)
missing_files <- required_files[!file.exists(required_files)]

# 在绘图开始前检查素材，避免运行到一半才因缺少文件而失败。
if (length(missing_files)) {
  stop(
    "Required project files are missing:\n  ",
    paste(missing_files, collapse = "\n  ")
  )
}

font_family <- "Roboto Condensed"

# 若系统尚未安装 Roboto Condensed，则注册项目内附带的字体文件。
# 这样在不同电脑上运行时仍可保持版式一致。
if (!font_family %in% system_fonts()$family) {
  register_font(font_family, plain = font_regular, bold = font_bold)
}

# -----------------------------------------------------------------------------
# 2. 可编辑内容与视觉样式
# -----------------------------------------------------------------------------

colours <- c(
  charcoal = "#222222",
  wine = "#982B2B",
  orange = "#F47720",
  green = "#459943",
  sage = "#A8C7A0",
  white = "#FFFFFF"
)

# 各阶段下方的说明文字。第五阶段没有说明列，因此这里只定义前四个阶段。
stage_details <- list(
  c("Target identification", "Target validation", "Prioritization"),
  c("Hit finding", "Hit-to-lead", "Lead optimization", "Candidate selection"),
  c(
    "In vitro pharmacology", "ADME / PK", "In vivo efficacy",
    "Safety & toxicology", "IND-enabling"
  ),
  c(
    "Phase I: safety", "Phase II: efficacy",
    "Phase III: confirmation", "Regulatory review"
  )
)

# 五个阶段的能力评分与对应颜色。
# 修改 score 即可同时更新流程图上的评分徽章和底部进度条。
capability <- data.frame(
  stage = c("TARGET", "DESIGN", "PRECLINICAL", "CLINICAL", "MARKETED"),
  score = c(9, 7, 7, 7, 2),
  colour = unname(c(
    colours["wine"], colours["orange"], colours["green"],
    "#86AA80", colours["charcoal"]
  )),
  stringsAsFactors = FALSE
)

# 基础数据校验：必须恰好有五个阶段，且所有评分均在 1–10 之间。
stopifnot(
  nrow(capability) == 5,
  all(capability$score >= 1 & capability$score <= 10)
)

# 所有坐标使用 0–1 的标准化画布坐标，使 PNG 与 SVG 保持完全相同的布局。
details_x <- c(0.055, 0.235, 0.415, 0.585)
badge_x <- c(0.180, 0.367, 0.533, 0.687, 0.827)
badge_y <- c(0.630, 0.603, 0.582, 0.558, 0.548)
# 整体流程图向上移动的距离；调大则上移，调小则下移。
pipeline_y_shift <- 0.035

# -----------------------------------------------------------------------------
# 3. 裁切并准备流程图主体
# -----------------------------------------------------------------------------

source_width <- 1680
source_height <- 945

# 以下像素边界是在 1680 × 945 原始参考图上测量得到的裁切框。
# 如更换参考图，需要重新测量这四个值以及上面的原图尺寸。
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

# 先将参考图精确缩放至 4K，再按同比例坐标裁切，可减少放大后的锯齿。
reference_image <- image_read(reference_file)
reference_4k <- image_resize(
  reference_image, "3840x2160!", filter = "Lanczos"
)

crop_geometry <- geometry_area(
  width = round(crop_box$width * 3840),
  height = round(crop_box$height * 2160),
  x_off = round(crop_box$x * 3840),
  y_off = round(crop_box$y * 2160)
)

# 转成 grid.raster() 可直接使用的 raster 对象。
flow_raster <- as.raster(
  image_crop(reference_4k, crop_geometry, repage = TRUE)
)

# -----------------------------------------------------------------------------
# 4. 绘图辅助函数
# -----------------------------------------------------------------------------

# 将普通数值转换为 grid 的 normalized parent coordinates（npc）单位。
npc <- function(value) unit(value, "npc")

# 统一文字样式，避免在每个绘图步骤中重复设置字体与颜色。
draw_text <- function(label, x, y, size, bold = FALSE,
                      colour = colours["charcoal"],
                      hjust = 0.5, vjust = 0.5) {
  grid.text(
    label,
    x = npc(x), y = npc(y),
    just = c(hjust, vjust),
    gp = gpar(
      fontfamily = font_family,
      fontface = if (bold) "bold" else "plain",
      fontsize = size,
      col = colour,
      lineheight = 0.95
    )
  )
}

# 绘制 DISCOVERY / DEVELOPMENT 分组括号及标题。
draw_bracket <- function(x0, x1, y, label) {
  grid.lines(
    npc(c(x0, x0, x1, x1)),
    npc(c(y - 0.020, y, y, y - 0.020)),
    gp = gpar(col = colours["charcoal"], lwd = 1.7)
  )
  draw_text(label, mean(c(x0, x1)), y + 0.027, 17, bold = TRUE)
}

# 绘制每个阶段上的“x/10”评分徽章。
# 浅色阶段使用 dark = TRUE，以保证文字和边框有足够对比度。
draw_score_badge <- function(x, y, score, dark = FALSE) {
  pushViewport(viewport(
    x = npc(x), y = npc(y),
    width = npc(0.046), height = npc(0.031)
  ))

  border_colour <- if (dark) colours["charcoal"] else colours["white"]
  fill_colour <- if (dark) "#FFFFFFCC" else "#FFFFFF24"
  text_colour <- if (dark) colours["charcoal"] else colours["white"]

  grid.roundrect(
    x = 0.5, y = 0.5,
    width = 0.96, height = 0.90,
    r = unit(0.10, "snpc"),
    gp = gpar(fill = fill_colour, col = border_colour, lwd = 1.4)
  )
  grid.text(
    sprintf("%d/10", score),
    x = 0.5, y = 0.5,
    gp = gpar(
      fontfamily = font_family,
      fontface = "bold",
      fontsize = 10.5,
      col = text_colour
    )
  )

  popViewport()
}

# 绘制前四个阶段下方的彩色竖线与工作内容。
draw_stage_details <- function(index, x) {
  labels <- stage_details[[index]]
  top <- 0.275 + pipeline_y_shift
  spacing <- 0.0345
  font_size <- 13.2
  bottom <- top - (length(labels) - 1) * spacing - 0.018

  grid.lines(
    npc(c(x, x)), npc(c(top + 0.012, bottom)),
    gp = gpar(col = capability$colour[index], lwd = 2.1)
  )

  for (j in seq_along(labels)) {
    draw_text(
      labels[j],
      x + 0.014,
      top - (j - 1) * spacing,
      font_size,
      hjust = 0
    )
  }
}

# 绘制底部 STAGE CAPABILITY PROFILE 面板。
# 每个阶段由 10 个小格组成，填充格数等于该阶段的能力评分。
draw_capability_profile <- function() {
  centres <- seq(0.12, 0.86, length.out = 5)
  widths <- rep(0.145, 5)
  empty_colour <- "#E8E5E1"
  label_y <- 0.063
  bar_y <- 0.036
  bar_height <- 0.015
  gap <- 0.0023

  grid.roundrect(
    x = npc(0.500), y = npc(0.063),
    width = npc(0.910), height = npc(0.105),
    r = unit(0.020, "snpc"),
    gp = gpar(fill = "#FAF9F7", col = "#E6E2DD", lwd = 1.0)
  )

  draw_text(
    "STAGE CAPABILITY PROFILE (1–10)",
    0.055, 0.097, 10.8,
    bold = TRUE, hjust = 0
  )

  for (i in seq_len(nrow(capability))) {
    left <- centres[i] - widths[i] / 2
    cell_width <- (widths[i] - 9 * gap) / 10

    draw_text(
      capability$stage[i], left, label_y, 9.2,
      bold = TRUE, hjust = 0
    )
    draw_text(
      sprintf("%d/10", capability$score[i]),
      left + widths[i], label_y, 9.2,
      bold = TRUE, hjust = 1
    )

    for (j in 1:10) {
      cell_x <- left + (j - 0.5) * cell_width + (j - 1) * gap
      cell_colour <- if (j <= capability$score[i]) {
        capability$colour[i]
      } else {
        empty_colour
      }

      grid.roundrect(
        x = npc(cell_x), y = npc(bar_y),
        width = npc(cell_width), height = npc(bar_height),
        r = unit(0.06, "snpc"),
        gp = gpar(fill = cell_colour, col = NA)
      )
    }
  }
}

# -----------------------------------------------------------------------------
# 5. 组合最终图片
# -----------------------------------------------------------------------------

draw_figure <- function() {
  grid.newpage()

  # 无外边框的白色画布。
  grid.rect(gp = gpar(fill = colours["white"], col = NA))

  draw_text(
    "DRUG DISCOVERY & DEVELOPMENT",
    0.045, 0.900, 34,
    bold = TRUE, hjust = 0
  )
  grid.lines(
    npc(c(0.040, 0.790)), npc(c(0.842, 0.842)),
    gp = gpar(col = colours["charcoal"], lwd = 1.7)
  )

  draw_bracket(
    0.047, 0.390,
    0.742 + pipeline_y_shift,
    "DISCOVERY STAGE"
  )
  draw_bracket(
    0.440, 0.750,
    0.742 + pipeline_y_shift,
    "DEVELOPMENT STAGE"
  )

  # 将裁切后的流程图主体插回原始测量位置，并按设定值整体上移。
  grid.raster(
    flow_raster,
    x = npc(crop_box$x + crop_box$width / 2),
    y = npc(
      1 - crop_box$y - crop_box$height / 2 + pipeline_y_shift
    ),
    width = npc(crop_box$width),
    height = npc(crop_box$height),
    interpolate = TRUE
  )

  for (i in seq_len(nrow(capability))) {
    draw_score_badge(
      badge_x[i],
      badge_y[i] + pipeline_y_shift,
      capability$score[i],
      dark = (i == 5)
    )
  }

  for (i in seq_along(stage_details)) {
    draw_stage_details(i, details_x[i])
  }

  draw_capability_profile()
}

# -----------------------------------------------------------------------------
# 6. 导出 PNG 与 SVG
# -----------------------------------------------------------------------------

png_file <- file.path(script_dir, "drug_discovery_pipeline_4K.png")
svg_file <- file.path(script_dir, "drug_discovery_pipeline.svg")

# 4K PNG：严格输出 3840 × 2160 像素，白色背景，适合屏幕和演示文稿。
agg_png(
  png_file,
  width = 3840, height = 2160,
  units = "px", res = 300,
  background = "white", scaling = 1
)
draw_figure()
dev.off()

# SVG：16 × 9 英寸画布，文字和 R 绘制元素保持为矢量对象；
# 中间插入的流程图主体仍是嵌入式 raster 图像。
svglite(
  svg_file,
  width = 16, height = 9,
  bg = "white",
  system_fonts = list(sans = font_family)
)
draw_figure()
dev.off()

message("Created:")
message("  ", png_file)
message("  ", svg_file)
