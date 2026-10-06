#!/usr/bin/env Rscript
# FINAL 20260930 figure contract: Stage 2A selection -> signed importance -> independent
# held-out association -> Stage 3 prediction and risk. All graphics use R.
# Figure S13 uses the final integrated Stage 3 score for held-out clinical risk stratification.

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(forcats)
  library(ggplot2); library(patchwork); library(scales)
  library(ragg); library(cowplot)
})
options(stringsAsFactors = FALSE, scipen = 999)
root <- "/Users/yifan2/Desktop/日常工作/UKB_multimorbidity"
analysis <- file.path(root, "R_final_analysis")
source_dir <- file.path(analysis, "figure_source_data")
out_dir <- file.path(analysis, "figures")
main_dir <- file.path(out_dir, "main")
supp_dir <- file.path(out_dir, "supplementary")
log_dir <- file.path(analysis, "logs")
for (d in c(main_dir, supp_dir, log_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
read_src <- function(name) {
  path <- file.path(source_dir, name)
  if (!file.exists(path)) stop("Missing source: ", path)
  z <- as_tibble(fread(path, showProgress = FALSE))
  # 兼容旧 source 中的 display label；v8 正文统一使用 Biomarkers。
  for (nm in intersect(c("modality_label", "component", "omic_label"), names(z))) {
    z[[nm]] <- dplyr::recode(
      as.character(z[[nm]]),
      Blood = "Biomarkers",
      .default = as.character(z[[nm]])
    )
  }
  z
}

pattern_levels <- paste0("P", 1:8)
outcomes <- c("Overall", "Women", "Men", pattern_levels)
modalities <- c("Environmental/phenotypic", "Biomarkers", "Metabolomics", "Genetics")

pattern_col <- c(
  P1 = "#426B8A", P2 = "#6585A4", P3 = "#488A83", P4 = "#AD9255",
  P5 = "#B97764", P6 = "#A36F86", P7 = "#81739C", P8 = "#687C8B"
)
modality_col <- c(
  "Environmental/phenotypic" = "#4F7897",
  "Biomarkers" = "#B87376",
  "Metabolomics" = "#6C987C",
  "Genetics" = "#9B85AF"
)
sharedness_col <- c(
  "Shared" = "#4F7897",
  "Multi-pattern" = "#83AAA6",
  "Pattern-specific" = "#D3AE75"
)
combination_display_col <- c(
  "E + Biomarkers" = "#B45F5C",
  "E + Metabolomics" = "#4C9078",
  "E + Genetics" = "#745AA8",
  "Integrated" = "#D08752"
)
combination_col <- c(
  E = "#869199", "E+B" = "#B45F5C", "E+M" = "#4C9078", "E+G" = "#745AA8",
  "E+B+M" = "#D08752", "E+B+G" = "#557EB4",
  "E+M+G" = "#4F9CA6", "E+B+M+G" = "#5C477E"
)

theme_set(
  theme_classic(base_size = 8.5, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, colour = "#111111"),
      plot.subtitle = element_text(face = "bold", size = 7.5, colour = "#555D65"),
      plot.caption = element_text(face = "bold", size = 7.0, colour = "#444444"),
      plot.tag = element_text(face = "bold", size = 11.5, colour = "#111111"),
      strip.background = element_rect(fill = "#EFF2F3", colour = NA),
      strip.text = element_text(face = "bold", size = 8.0, colour = "#111111"),
      axis.title = element_text(face = "bold", size = 9.0, colour = "#111111"),
      axis.text = element_text(face = "bold", size = 8.0, colour = "#26313A"),
      axis.line = element_line(linewidth = .55, colour = "#222222"),
      axis.ticks = element_line(linewidth = .45, colour = "#222222"),
      legend.title = element_text(face = "bold", size = 8.1, colour = "#111111"),
      legend.text = element_text(face = "bold", size = 7.4, colour = "#222222"),
      legend.key = element_rect(fill = "white", colour = NA),
      plot.margin = margin(4, 5, 4, 4)
    )
)

tile_theme <- theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  legend.position = "bottom",
  legend.key.height = grid::unit(3.8, "mm"),
  legend.key.width = grid::unit(8, "mm")
)

sym_lim <- function(x) {
  y <- abs(x[is.finite(x)])
  if (!length(y)) return(1)
  max(as.numeric(quantile(y, .95)), .001)
}

diverging <- function(x, title) {
  scale_fill_gradient2(
    low = "#487CA2", mid = "#F5F5F4", high = "#B96968", midpoint = 0,
    limits = c(-sym_lim(x), sym_lim(x)), oob = squish,
    na.value = "#E9ECEE", name = title,
    guide = guide_colorbar(ticks = FALSE, frame.colour = NA)
  )
}

diverging_soft <- function(x, title) {
  scale_fill_gradient2(
    low = "#3F78A8", mid = "#D8DCE2", high = "#B85D5B", midpoint = 0,
    limits = c(-sym_lim(x), sym_lim(x)), oob = squish,
    na.value = "#E9ECEE", name = title,
    guide = guide_colorbar(ticks = FALSE, frame.colour = NA)
  )
}


# 每张图完成后记录可继续的位置；与 07 共用本次最终版 checkpoint。
checkpoint_path <- file.path(root, "UKB_final_20260930_checkpoint.md")
expected_figures <- c(paste0("Figure", 2:5), paste0("FigureS", 1:13))
record_figure_checkpoint <- function(id, complete = FALSE) {
  position <- match(id, expected_figures)
  next_step <- if (complete) {
    "检查导出的 Figure 2–5、Figure S1–S13，并用于稿件。"
  } else if (!is.na(position) && position < length(expected_figures)) {
    paste0("从 08 的 ", expected_figures[[position + 1L]], " 代码块继续。")
  } else {
    "运行 08 最后的 export/order QA 代码块。"
  }
  writeLines(c(
    "# UKB multimorbidity final output checkpoint",
    paste0("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Current stage: ", if (complete) "08 exports/order QA completed" else paste("08 exported", id)),
    "MAIN cohort: 309,378; source/UKB_diseases_data_with_patterns.csv",
    "Tables: S1–S3 and S10–S26 generated by 07; other tables prepared separately",
    "Figures: main Figure 2–5; supplementary Figure S1–S13; S9a/b = 5/10-year calibration",
    paste0("Next step: ", next_step)
  ), checkpoint_path, useBytes = TRUE)
}

export_rows <- list()
plots <- list()
save_figure <- function(id, plot, width_mm = 183, height_mm = 135, group = "main") {
  base <- file.path(if (group == "main") main_dir else supp_dir, id)

  # 若目录中残留旧版 SVG，先删除，确保本版只保留 PDF/TIFF/PNG。
  stale_svg <- paste0(base, ".svg")
  if (file.exists(stale_svg)) unlink(stale_svg)

  w <- width_mm / 25.4
  h <- height_mm / 25.4

  render <- function(open_device, path) {
    open_device(path)
    print(plot)
    grDevices::dev.off()
  }

  # 统一仅导出 PDF / TIFF / PNG；不再生成 SVG。
  render(
    function(path) grDevices::pdf(
      path, width = w, height = h, bg = "white", family = "Helvetica",
      useDingbats = FALSE
    ),
    paste0(base, ".pdf")
  )

  render(
    function(path) ragg::agg_tiff(
      path, width = width_mm, height = height_mm, units = "mm", res = 600,
      compression = "lzw", background = "white"
    ),
    paste0(base, ".tiff")
  )

  render(
    function(path) ragg::agg_png(
      path, width = width_mm, height = height_mm, units = "mm", res = 300,
      background = "white"
    ),
    paste0(base, ".png")
  )

  files <- paste0(base, c(".pdf", ".tiff", ".png"))

  export_rows[[id]] <<- tibble(
    figure = id,
    group = group,
    width_mm = width_mm,
    height_mm = height_mm,
    n_expected_files = length(files),
    all_files_exist = all(file.exists(files)),
    all_nonempty = all(file.info(files)$size > 0)
  )

  plots[[id]] <<- plot
  record_figure_checkpoint(id)
  cat("Exported ", id, " (PDF/TIFF/PNG)\n", sep = "")
}

# Supplementary figures: no internal plot titles/subtitles; axis text/titles are bold.
supp_theme <- theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  axis.title = element_text(face = "bold", size = 9.8, colour = "#111111"),
  axis.text = element_text(face = "bold", size = 8.7, colour = "#26313A"),
  strip.text = element_text(face = "bold", size = 8.8, colour = "#111111"),
  legend.title = element_text(face = "bold", size = 8.8, colour = "#111111"),
  legend.text = element_text(face = "bold", size = 8.1, colour = "#222222")
)

# =============================================================================
# Main Figure 2: multimorbidity burden, disease network and disease key####
# =============================================================================
n2 <- read_src("Figure2_nodes.csv")
b2 <- read_src("Figure2_burden.csv")
c2 <- read_src("Figure2_pattern_counts.csv")

# P3 在原始网络布局中心节点较密；仅用于绘图时围绕 P3 自身质心做更明显但仍温和的展开，
# 不改变任何 cluster membership、edge、prevalence 或统计结果。
n2_plot <- n2 %>%
  group_by(pattern_id) %>%
  mutate(
    .cx = mean(x, na.rm = TRUE),
    .cy = mean(y, na.rm = TRUE),
    x_plot = if_else(pattern_id == "P3", .cx + 1.78 * (x - .cx), x),
    y_plot = if_else(pattern_id == "P3", .cy + 1.78 * (y - .cy), y)
  ) %>%
  ungroup() %>%
  select(-.cx, -.cy)

e2 <- read_src("Figure2_edges.csv") %>%
  left_join(
    n2_plot %>% select(source = disease, x1 = x_plot, y1 = y_plot),
    by = "source"
  ) %>%
  left_join(
    n2_plot %>% select(target = disease, x2 = x_plot, y2 = y_plot),
    by = "target"
  )

p2a <- ggplot(
  b2,
  aes(
    disease_count_end_followup,
    percent
  )
) +
  geom_col(fill = "#6F89A6", width = .82) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, .05))
  ) +
  labs(
    x = "Number of chronic diseases by end of follow-up",
    y = "Participants (%)",
    title = "Multimorbidity burden",
    tag = "a"
  ) +
  theme(
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 6, 4, 8)
  )

p2b <- ggplot() +
  # 边线略加深，仍保持背景网络感；alpha 随相关强度变化。
  geom_segment(
    data = e2,
    aes(x = x1, y = y1, xend = x2, yend = y2, alpha = abs(correlation)),
    colour = "#7A909B", linewidth = .38, show.legend = FALSE
  ) +
  # 使用连续 size range 而不是 area 从接近 0 起步，保证低患病率节点也可读。
  geom_point(
    data = n2_plot,
    aes(x_plot, y_plot, fill = pattern_id, size = prevalence_percent),
    shape = 21, colour = "white", stroke = .72
  ) +
  geom_text(
    data = n2_plot,
    aes(x_plot, y_plot, label = sprintf("%02d", node_id)),
    colour = "white", fontface = "bold", size = 2.92
  ) +
  scale_fill_manual(
    values = pattern_col,
    breaks = pattern_levels,
    guide = guide_legend(
      title = "Pattern", nrow = 2, byrow = TRUE,
      keywidth = grid::unit(7.2, "mm"), keyheight = grid::unit(7.2, "mm"),
      override.aes = list(shape = 21, size = 6.9, colour = "white", stroke = .68)
    )
  ) +
  scale_size_continuous(
    name = "Prevalence (%)",
    range = c(5.2, 15.4),
    breaks = c(1, 5, 10),
    limits = c(0, max(10, max(n2_plot$prevalence_percent, na.rm = TRUE))),
    guide = guide_legend(
      title = "Prevalence (%)",
      nrow = 1, byrow = TRUE,
      keywidth = grid::unit(14.0, "mm"),
      keyheight = grid::unit(12.0, "mm"),
      override.aes = list(
        shape = 21,
        size = c(5.8, 10.2, 14.5),
        fill = rep("#D7E5EA", 3),
        colour = rep("#5F7F95", 3),
        alpha = rep(1, 3),
        stroke = rep(.75, 3)
      )
    )
  ) +
  guides(
    fill = guide_legend(order = 1, title = "Pattern", nrow = 2, byrow = TRUE,
                        override.aes = list(shape = 21, size = 6.9,
                                            colour = "white", stroke = .68)),
    size = guide_legend(order = 2, title = "Prevalence (%)", nrow = 1, byrow = TRUE,
                        override.aes = list(shape = 21, size = c(5.8, 10.2, 14.5),
                                            fill = rep("#D7E5EA", 3),
                                            colour = rep("#5F7F95", 3), alpha = rep(1, 3),
                                            stroke = rep(.75, 3)))
  ) +
  scale_alpha(range = c(.12, .40), guide = "none") +
  coord_equal(clip = "off") +
  theme_void(base_size = 8.5) +
  labs(title = NULL, tag = "b") +
  theme(
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.tag.position = c(.01, .995),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.title = element_text(face = "bold", size = 8.8),
    legend.text = element_text(face = "bold", size = 8.1),
    legend.spacing.x = grid::unit(3.8, "mm"),
    legend.spacing.y = grid::unit(1.4, "mm"),
    legend.key.width = grid::unit(8.0, "mm"),
    plot.margin = margin(7, 7, 4, 8)
  )

p2c <- ggplot(
  c2,
  aes(x = events, y = fct_rev(factor(pattern_id, levels = pattern_levels)), fill = pattern_id)
) +
  geom_col(width = .70) +
  scale_fill_manual(values = pattern_col, guide = "none") +
  scale_x_continuous(
    labels = label_number(big.mark = ","),
    expand = expansion(mult = c(0, .02))
  ) +
  labs(x = "Incident pattern events", y = NULL, title = "Follow-up event counts", tag = "c") +
  theme(
    plot.tag.position = c(.005, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(6, 6, 4, 8)
  )

# Disease key 作为整张 Figure 2 的说明区域，不再作为 panel d。
key2 <- n2_plot %>%
  arrange(node_id) %>%
  mutate(
    key_col = if_else(row_number() <= 18, 1L, 2L),
    key_row = if_else(row_number() <= 18, row_number(), row_number() - 18L),
    key_x = if_else(key_col == 1L, 1, 4.0),
    key_label = paste0(disease_display, " (", pattern_id, ")")
  )

p2key <- ggplot(key2) +
  geom_point(
    aes(key_x, -key_row, fill = pattern_id),
    shape = 21, size = 4.1, colour = "white", stroke = .50
  ) +
  geom_text(
    aes(key_x, -key_row, label = sprintf("%02d", node_id)),
    colour = "white", fontface = "bold", size = 1.95
  ) +
  geom_text(
    aes(key_x + .20, -key_row, label = key_label, colour = pattern_id),
    hjust = 0, fontface = "bold", size = 2.85
  ) +
  scale_fill_manual(values = pattern_col, guide = "none") +
  scale_colour_manual(values = pattern_col, guide = "none") +
  scale_x_continuous(limits = c(.65, 7.2)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  labs(title = "Disease key", tag = NULL) +
  theme(
    plot.title = element_text(face = "bold", size = 9.5),
    plot.tag = element_blank(),
    plot.margin = margin(2, 12, 2, 8)
  )

figure2_plot <- ((p2a | p2b) / p2c / p2key) +
  plot_layout(heights = c(1.22, .60, .98))
save_figure("Figure2", figure2_plot, 195, 240)

# =============================================================================
# Main Figure 3####
# Cross-pattern architecture + aligned signed importance + held-out association
# =============================================================================

a3 <- read_src("Figure3a_sharedness_architecture.csv")
b3 <- read_src("Figure3b_signed_importance.csv")
c3 <- read_src("Figure3c_heldout_associations.csv")
row3 <- read_src("Figure3_row_annotation.csv")

shared_order <- c("Shared", "Multi-pattern", "Pattern-specific")

# Figure 3a：coord_flip() 后反转 levels，最终从上到下固定为
# Environmental/phenotypic -> Biomarkers -> Metabolomics -> Genetics。
a3 <- a3 %>%
  mutate(
    modality_label = factor(modality_label, levels = rev(modalities)),
    sharedness = factor(sharedness, levels = shared_order)
  )

key_b <- paste(b3$modality, b3$model_variable, b3$outcome_label, sep = "||")
key_c <- paste(c3$modality, c3$model_variable, c3$outcome_label, sep = "||")
if (!identical(key_b, key_c) || !identical(b3$feature_order, c3$feature_order)) {
  stop("Figure 3b/3c keys or row order differ")
}

p3a <- ggplot(
  a3,
  aes(x = modality_label, y = proportion_within_modality, fill = sharedness)
) +
  geom_col(width = .68, colour = "white", linewidth = .22) +
  coord_flip() +
  scale_fill_manual(values = sharedness_col, drop = FALSE) +
  scale_y_continuous(
    labels = label_percent(), breaks = c(0, .25, .50, .75, 1),
    limits = c(0, 1), expand = expansion(mult = c(0, .005))
  ) +
  labs(
    x = NULL, y = "Share of selected features", fill = NULL,
    title = "Cross-pattern feature architecture", tag = "a"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 8.6, colour = "#26313A"),
    axis.text.y = element_text(face = "bold", size = 8.7, colour = "#26313A"),
    axis.title.x = element_text(face = "bold", size = 9.0, colour = "#26313A"),
    axis.title.y = element_blank(),
    plot.title = element_text(face = "bold", size = 10.8, hjust = 0),
    plot.tag.position = c(.005, .995),
    plot.tag = element_text(face = "bold", size = 13.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 7, 5, 7)
  )

row3 <- row3 %>%
  mutate(
    sharedness = factor(sharedness, levels = shared_order),
    modality_label = factor(modality_label, levels = modalities)
  ) %>%
  arrange(feature_order) %>%
  mutate(feature_key_plot = paste(modality, feature_order, display_name, sep = "||"))
feature_levels3 <- rev(row3$feature_key_plot)

prep3 <- function(z) {
  z %>%
    left_join(
      row3 %>% select(modality, feature_order, feature_key_plot),
      by = c("modality", "feature_order")
    ) %>%
    mutate(
      outcome_label = factor(outcome_label, levels = pattern_levels),
      feature_key_plot = factor(feature_key_plot, levels = feature_levels3),
      modality_label = factor(modality_label, levels = modalities),
      sharedness = factor(sharedness, levels = shared_order)
    )
}
b3 <- prep3(b3)
c3 <- prep3(c3)
row3 <- row3 %>%
  mutate(feature_key_plot = factor(feature_key_plot, levels = feature_levels3))

# 左侧只显示一套变量名；允许文字向左扩展，不再被 panel 裁切。
p3labels <- ggplot(
  row3, aes(x = 1, y = feature_key_plot, label = display_name)
) +
  geom_text(
    hjust = 1, fontface = "bold", size = 3.20,
    colour = "#26313A", lineheight = .96
  ) +
  scale_x_continuous(limits = c(0, 1.02), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(23, 2, 5, 18))

p3view <- ggplot(row3, aes(x = 1, y = feature_key_plot, fill = modality_label)) +
  geom_tile(width = .82, colour = "white", linewidth = .30) +
  scale_fill_manual(
    values = modality_col, limits = modalities, drop = FALSE,
    name = "Data domain",
    guide = guide_legend(
      direction = "horizontal", nrow = 1, byrow = TRUE,
      keywidth = grid::unit(5.5, "mm"), keyheight = grid::unit(3.8, "mm"),
      title.position = "top"
    )
  ) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(23, 1, 5, 1))

p3b <- ggplot(b3, aes(x = outcome_label, y = feature_key_plot)) +
  geom_tile(fill = "#E9ECEE", colour = "white", linewidth = .22) +
  geom_tile(
    data = filter(b3, selected_stage2a),
    aes(fill = plot_signed_importance), colour = "white", linewidth = .22
  ) +
  diverging(b3$plot_signed_importance, "Signed importance") +
  guides(
    fill = guide_colorbar(
      title = "Signed importance", title.position = "top", title.hjust = .5,
      barwidth = grid::unit(46, "mm"), barheight = grid::unit(4.2, "mm"),
      ticks = TRUE, frame.colour = "#4D4D4D"
    )
  ) +
  labs(x = NULL, y = NULL, title = "Signed importance", tag = "b") +
  tile_theme +
  theme(
    axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.text.x = element_text(
      angle = 0, hjust = .5, vjust = .5, face = "bold",
      size = 8.8, colour = "#26313A"
    ),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 10.8, hjust = .5),
    plot.tag.position = c(.005, .995),
    plot.tag = element_text(face = "bold", size = 13.5, hjust = 0, vjust = 1),
    legend.position = "none",
    plot.margin = margin(7, 4, 3, 2)
  )

p3c <- ggplot(c3, aes(x = outcome_label, y = feature_key_plot)) +
  geom_tile(fill = "#E9ECEE", colour = "white", linewidth = .22) +
  geom_tile(
    data = filter(c3, selected_stage2a & estimable %in% TRUE),
    aes(fill = plot_log2_HR), colour = "white", linewidth = .22
  ) +
  geom_text(
    data = filter(c3, selected_stage2a & estimable %in% TRUE & FDR < .05),
    label = "*", size = 2.8, fontface = "bold", colour = "black"
  ) +
  diverging(c3$plot_log2_HR, "log2(HR)") +
  guides(
    fill = guide_colorbar(
      title = "Held-out association: log2(HR)",
      title.position = "top", title.hjust = .5,
      barwidth = grid::unit(46, "mm"), barheight = grid::unit(4.2, "mm"),
      ticks = TRUE, frame.colour = "#4D4D4D"
    )
  ) +
  labs(x = NULL, y = NULL, title = "Held-out association", tag = "c") +
  tile_theme +
  theme(
    axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.text.x = element_text(
      angle = 0, hjust = .5, vjust = .5, face = "bold",
      size = 8.8, colour = "#26313A"
    ),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 10.8, hjust = .5),
    plot.tag.position = c(.005, .995),
    plot.tag = element_text(face = "bold", size = 13.5, hjust = 0, vjust = 1),
    legend.position = "none",
    plot.margin = margin(7, 4, 3, 4)
  )

p3type <- ggplot(row3, aes(x = 1, y = feature_key_plot, fill = sharedness)) +
  geom_tile(width = .82, colour = "white", linewidth = .30) +
  scale_fill_manual(
    values = sharedness_col, limits = shared_order, drop = FALSE,
    name = "Feature type",
    guide = guide_legend(
      direction = "horizontal", nrow = 1, byrow = TRUE,
      keywidth = grid::unit(5.5, "mm"), keyheight = grid::unit(3.8, "mm"),
      title.position = "top"
    )
  ) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(23, 2, 5, 1))

bottom3 <- (p3labels | p3view | p3b | p3c | p3type) +
  plot_layout(widths = c(2.15, .13, 2.70, 2.70, .13))

legend_domain <- cowplot::get_legend(
  p3view +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8, colour = "#26313A"),
      legend.text = element_text(face = "bold", size = 8.2, colour = "#26313A"),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    guides(
      fill = guide_legend(
        title = "Data domain", direction = "horizontal", nrow = 1, byrow = TRUE,
        keywidth = grid::unit(5.8, "mm"), keyheight = grid::unit(4.2, "mm"),
        title.position = "top", title.hjust = 0
      )
    )
)
legend_type <- cowplot::get_legend(
  p3type +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8, colour = "#26313A"),
      legend.text = element_text(face = "bold", size = 8.2, colour = "#26313A"),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    guides(
      fill = guide_legend(
        title = "Feature type", direction = "horizontal", nrow = 1, byrow = TRUE,
        keywidth = grid::unit(5.8, "mm"), keyheight = grid::unit(4.2, "mm"),
        title.position = "top", title.hjust = 0
      )
    )
)
legend_imp <- cowplot::get_legend(
  p3b +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8, colour = "#26313A"),
      legend.text = element_text(face = "bold", size = 8.0, colour = "#26313A"),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    guides(
      fill = guide_colorbar(
        title = "Signed importance", title.position = "top", title.hjust = .5,
        barwidth = grid::unit(48, "mm"), barheight = grid::unit(4.4, "mm"),
        ticks = TRUE, frame.colour = "#4D4D4D"
      )
    )
)
legend_hr <- cowplot::get_legend(
  p3c +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8, colour = "#26313A"),
      legend.text = element_text(face = "bold", size = 8.0, colour = "#26313A"),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    guides(
      fill = guide_colorbar(
        title = "Held-out association: log2(HR)", title.position = "top", title.hjust = .5,
        barwidth = grid::unit(48, "mm"), barheight = grid::unit(4.4, "mm"),
        ticks = TRUE, frame.colour = "#4D4D4D"
      )
    )
)
legend_row1 <- cowplot::plot_grid(
  legend_domain, legend_type, nrow = 1, rel_widths = c(1.55, 1.00), align = "h"
)
legend_row2 <- cowplot::plot_grid(
  legend_imp, legend_hr, nrow = 1, rel_widths = c(1, 1), align = "h"
)

figure3_plot <- (
  p3a /
    bottom3 /
    wrap_elements(full = legend_row1) /
    wrap_elements(full = legend_row2)
) +
  plot_layout(heights = c(.61, 4.20, .34, .38))

save_figure("Figure3", figure3_plot, 245, 305)

# =============================================================================
# Main Figure 4: incremental prediction, final discrimination and Wald architecture####
# =============================================================================
a4 <- read_src("Figure4a_incremental_prediction.csv")
b4 <- read_src("Figure4b_final_discrimination.csv")
c4 <- read_src("Figure4c_modality_wald_contribution.csv")

# Panel a: lollipop plot; four combinations are vertically separated within each outcome.
a4_long <- bind_rows(
  a4 %>% transmute(
    outcome_label, outcome_order, combination_display,
    metric = "C-index", gain_pp = delta_c_index_pp
  ),
  a4 %>% transmute(
    outcome_label, outcome_order, combination_display,
    metric = "Mean time-dependent AUC", gain_pp = delta_mean_time_dependent_auc_pp
  )
) %>%
  mutate(
    combination_display = factor(
      combination_display,
      levels = c("E + Biomarkers", "E + Metabolomics", "E + Genetics", "Integrated")
    ),
    metric = factor(metric, levels = c("C-index", "Mean time-dependent AUC")),
    outcome_index = match(outcome_label, outcomes),
    y_base = (length(outcomes) - outcome_index) * 1.35 + 1,
    combo_index = as.integer(combination_display),
    y_plot = y_base + c(-.33, -.11, .11, .33)[combo_index]
  )

p4a <- ggplot(a4_long, aes(x = gain_pp, y = y_plot, colour = combination_display)) +
  geom_vline(xintercept = 0, linewidth = .45, colour = "#777F86") +
  geom_vline(
    xintercept = c(4, 8, 12), linetype = 2,
    linewidth = .30, colour = "#CFD4D8"
  ) +
  geom_segment(aes(x = 0, xend = gain_pp, yend = y_plot), linewidth = .60) +
  geom_point(size = 2.05) +
  facet_wrap(~ metric, nrow = 1) +
  scale_y_continuous(
    breaks = (rev(seq_along(outcomes)) - 1) * 1.35 + 1,
    labels = outcomes,
    expand = expansion(add = .65)
  ) +
  scale_x_continuous(
    labels = function(x) paste0(number(x, accuracy = .1), " pp"),
    breaks = pretty_breaks(n = 5)
  ) +
  scale_colour_manual(values = combination_display_col, drop = FALSE) +
  labs(
    x = "Gain versus environmental/phenotypic model",
    y = NULL, colour = NULL,
    title = "Incremental predictive gain", tag = "a"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 5, 4, 7)
  )

# Panel b: final C-index + mean time-dependent AUC; no numeric labels to avoid crowding.
b4_long <- bind_rows(
  b4 %>% transmute(outcome_label, outcome_order, metric = "C-index", value = c_index),
  b4 %>% transmute(
    outcome_label, outcome_order, metric = "Mean time-dependent AUC",
    value = mean_time_dependent_auc
  )
) %>%
  mutate(
    outcome_label = fct_rev(factor(outcome_label, levels = outcomes)),
    metric = factor(metric, levels = c("C-index", "Mean time-dependent AUC"))
  )

p4b <- ggplot(b4_long, aes(x = value, y = outcome_label, shape = metric)) +
  geom_vline(
    xintercept = seq(.55, .95, by = .05), linetype = 2,
    linewidth = .30, colour = "#D1D5D8"
  ) +
  geom_point(size = 2.25, colour = "#67499B") +
  scale_shape_manual(values = c("C-index" = 16, "Mean time-dependent AUC" = 17)) +
  coord_cartesian(
    xlim = c(
      max(.52, min(b4_long$value, na.rm = TRUE) - .035),
      min(.98, max(b4_long$value, na.rm = TRUE) + .035)
    )
  ) +
  labs(
    x = "Discrimination", y = NULL, shape = NULL,
    title = "Final discrimination", tag = "b"
  ) +
  theme(
    legend.position = "bottom",
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 7, 4, 5)
  )

# Panel c: statistical definition unchanged -- relative Wald chi-square share of
# the four Stage 3 modality-specific partial scores in the held-out Cox model.
p4c <- ggplot(
  c4 %>%
    mutate(
      outcome_label = fct_rev(factor(outcome_label, levels = outcomes)),
      component = factor(component, levels = modalities)
    ),
  aes(x = relative_wald_share, y = outcome_label, fill = component)
) +
  geom_col(width = .72) +
  scale_x_continuous(labels = label_percent(), breaks = seq(0, 1, .25)) +
  scale_fill_manual(values = modality_col, drop = FALSE) +
  labs(
    x = "Relative Wald chi-square contribution (%)", y = NULL, fill = NULL,
    title = "Prognostic information by modality", tag = "c"
  ) +
  theme(
    legend.position = "bottom",
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 7, 4, 7)
  )

figure4_plot <- (p4a | p4b) / p4c +
  plot_layout(widths = c(1.35, .82), heights = c(1.18, 1.00))
save_figure("Figure4", figure4_plot, 205, 210)

# =============================================================================
# Main Figure 5: Stage 2A signed-importance domain scores and joint risk profiles####
# =============================================================================
a5 <- read_src("Figure5a_risk_oriented_domain_score_associations.csv")
b5 <- read_src("Figure5b_higher_risk_joint_contrasts.csv")
c5 <- read_src("Figure5c_environment_combined_omics.csv")
omics_ref5 <- read_src("Figure5_combined_omics_reference.csv")

figure5_adjusted_flag <- if ("association_adjusted" %in% names(a5)) {
  vals <- unique(na.omit(as.logical(a5$association_adjusted)))
  if (length(vals) > 1L) stop("Figure 5 source mixes adjusted and unadjusted models.")
  if (length(vals)) vals[[1]] else FALSE
} else FALSE
figure5_hr_prefix <- if (isTRUE(figure5_adjusted_flag)) "Adjusted HR" else "HR"

if (
  nrow(omics_ref5) != 11L ||
  any(abs(omics_ref5$biomarker_weight - 1 / 3) > 1e-12) ||
  any(abs(omics_ref5$metabolomics_weight - 1 / 3) > 1e-12) ||
  any(abs(omics_ref5$genetics_weight - 1 / 3) > 1e-12)
) {
  stop(
    "Figure 5c combined-omics contract failed: expected 11 outcomes and fixed ",
    "equal weights of 1/3 for B/M/G."
  )
}

# Panel a: continuous score associations for Overall, Women, Men and P1-P8.
p5a_dat <- a5 %>%
  mutate(
    outcome_label = fct_rev(factor(outcome_label, levels = outcomes)),
    component = factor(component, levels = modalities)
  )

p5a <- ggplot(p5a_dat, aes(x = HR, y = outcome_label, colour = component)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "#9BA3A9", linewidth = .40) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .12,
    position = position_dodge(width = .58), na.rm = TRUE
  ) +
  geom_point(position = position_dodge(width = .58), size = 1.70, na.rm = TRUE) +
  scale_x_log10() +
  scale_colour_manual(values = modality_col, drop = FALSE) +
  labs(
    x = NULL, y = NULL, colour = NULL,
    title = "Domain risk-score associations", tag = "a"
  ) +
  theme(
    legend.position = "bottom",
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 6, 5, 7)
  )

# Panel b: Higher/Higher vs Lower/Lower for E × each individual omic.
b5 <- b5 %>%
  mutate(
    logHR = log(HR),
    label = if_else(is.finite(HR), sprintf("%.2f", HR), "x"),
    omic_label = factor(
      omic_label, levels = c("Biomarkers", "Metabolomics", "Genetics")
    ),
    outcome_label = fct_rev(factor(outcome_label, levels = outcomes))
  )

p5b <- ggplot(b5, aes(omic_label, outcome_label)) +
  geom_tile(aes(fill = logHR), colour = NA) +
  geom_text(aes(label = label), size = 2.15, fontface = "bold") +
  diverging_soft(b5$logHR, "log(HR)") +
  labs(
    x = NULL, y = NULL,
    title = "Higher-risk environment + individual omic", tag = "b"
  ) +
  tile_theme +
  theme(
    axis.text.x = element_text(face = "bold", angle = 18, hjust = 1),
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 7, 5, 5)
  )

# Panel c: 2×2 Environmental × equal-weighted combined-omics profiles.
joint_levels5 <- c(
  "Lower risk / Lower risk",
  "Lower risk / Higher risk",
  "Higher risk / Lower risk",
  "Higher risk / Higher risk"
)
joint_labels5 <- c(
  "Lower E / Lower omics",
  "Lower E / Higher omics",
  "Higher E / Lower omics",
  "Higher E / Higher omics"
)

c5_plot <- c5 %>%
  mutate(
    joint_group = factor(joint_group, levels = joint_levels5, labels = joint_labels5),
    outcome_label = fct_rev(factor(outcome_label, levels = outcomes))
  )

p5c <- ggplot(c5_plot, aes(x = HR, y = outcome_label, colour = joint_group)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "#9BA3A9", linewidth = .40) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high),
    orientation = "y", width = .10,
    position = position_dodge(width = .64), na.rm = TRUE
  ) +
  geom_point(
    position = position_dodge(width = .64), size = 1.70, na.rm = TRUE
  ) +
  scale_x_log10() +
  scale_colour_manual(
    values = c(
      "Lower E / Lower omics" = "#8D969D",
      "Lower E / Higher omics" = "#6B8EAD",
      "Higher E / Lower omics" = "#B4866E",
      "Higher E / Higher omics" = "#8A4F62"
    ),
    drop = FALSE
  ) +
  labs(
    x = paste0(figure5_hr_prefix, " versus Lower E / Lower combined omics"),
    y = NULL, colour = NULL,
    title = "Environmental + equal-weighted combined-omics joint profiles", tag = "c"
  ) +
  theme(
    legend.position = "bottom",
    plot.tag.position = c(.01, .995),
    plot.tag = element_text(face = "bold", size = 12.5, hjust = 0, vjust = 1),
    plot.margin = margin(7, 7, 5, 7)
  )

# 正文 Figure 5 不再添加底部长 footnote；评分定义保留在 source/table/caption inventory 中。
figure5_plot <- (p5a | p5b) / p5c +
  plot_layout(widths = c(1.20, .88), heights = c(1.08, 1.18))
save_figure("Figure5", figure5_plot, 205, 235)

# =============================================================================
# Supplementary Figures S1–S13####
# Final order follows 260927 Supplementary Figure(1).docx:
#   1) no internal plot titles/subtitles;
#   2) all visible axis text/titles are bold;
#   3) modality order is always Environmental/phenotypic -> Biomarkers ->
#      Metabolomics -> Genetics;
#   4) outcome order is Overall -> Women -> Men -> P1 ... P8;
#   5) no numeric rank prefixes are added to feature labels.
# =============================================================================

# -----------------------------------------------------------------------------
# Figure S1: Stage 1 signed importance, top 50 per modality####
# -----------------------------------------------------------------------------
s1 <- read_src("FigureS1_stage1_signed_top50.csv") %>%
  mutate(
    outcome_label = factor(outcome_label, levels = outcomes),
    modality_label = factor(modality_label, levels = modalities),
    feature_key = paste(modality, model_variable, sep = "||")
  )
s1_level_df <- s1 %>%
  distinct(modality, modality_label, modality_order, feature_key, display_name, feature_rank_modality) %>%
  arrange(modality_order, desc(feature_rank_modality))
s1_levels <- unique(s1_level_df$feature_key)
s1_labels <- setNames(s1_level_df$display_name, s1_level_df$feature_key)
s1 <- s1 %>% mutate(feature = factor(feature_key, levels = s1_levels))
p <- ggplot(s1, aes(outcome_label, feature)) +
  geom_tile(aes(fill = signed_importance), colour = "white") +
  facet_grid(modality_label ~ ., scales = "free_y", space = "free_y") +
  diverging(s1$signed_importance, "Signed importance") +
  scale_y_discrete(labels = function(x) unname(s1_labels[x])) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  tile_theme + supp_theme +
  theme(
    axis.text.y = element_text(face = "bold", size = 4.4),
    axis.text.x = element_text(face = "bold", size = 7.2, angle = 45, hjust = 1)
  )
save_figure("FigureS1", p, 195, 285, "supplementary")

# -----------------------------------------------------------------------------
# Figure S2: Stage 2A internal-CV 1-SE selection####
# -----------------------------------------------------------------------------
s2 <- read_src("FigureS2_stage2A_internal_CV_1SE.csv") %>%
  mutate(
    outcome_label = factor(outcome_label, levels = outcomes),
    modality_label = factor(modality_label, levels = modalities)
  )
p <- ggplot(
  s2,
  aes(n_features, mean_auc, colour = modality_label,
      group = interaction(dataset, modality))
) +
  geom_line(linewidth = .45, alpha = .85) +
  geom_point(data = filter(s2, selected_point), size = 1.7) +
  facet_wrap(~ outcome_label, ncol = 4) +
  scale_colour_manual(values = modality_col, limits = modalities, drop = FALSE) +
  labs(x = "Number of selected features", y = "Internal CV mean AUC",
       title = NULL, subtitle = NULL, colour = NULL) +
  supp_theme +
  theme(legend.position = "bottom")
save_figure("FigureS2", p, 190, 165, "supplementary")

# -----------------------------------------------------------------------------
# Figure S3: Stage 2A-selected features and signed importance####
# Order: Environmental/phenotypic -> Biomarkers -> Metabolomics -> Genetics
# No numeric prefix in labels.
# -----------------------------------------------------------------------------
s3 <- read_src("FigureS3_stage2A_selected_signed_importance.csv") %>%
  mutate(
    outcome_label = factor(outcome_label, levels = outcomes),
    modality_label = factor(modality_label, levels = modalities),
    feature_key = paste(modality, model_variable, sep = "||")
  )
s3_level_df <- s3 %>%
  distinct(modality, modality_label, modality_order, feature_key, display_name, feature_rank_modality) %>%
  arrange(modality_order, desc(feature_rank_modality))
s3_levels <- unique(s3_level_df$feature_key)
s3_labels <- setNames(s3_level_df$display_name, s3_level_df$feature_key)
s3 <- s3 %>% mutate(feature = factor(feature_key, levels = s3_levels))
p <- ggplot(s3, aes(outcome_label, feature)) +
  geom_tile(fill = "#E9ECEE", colour = "white") +
  geom_tile(
    data = filter(s3, selected_stage2a),
    aes(fill = plot_signed_importance), colour = "white"
  ) +
  facet_grid(modality_label ~ ., scales = "free_y", space = "free_y") +
  diverging(s3$plot_signed_importance, "Signed importance") +
  scale_y_discrete(labels = function(x) unname(s3_labels[x])) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  tile_theme + supp_theme +
  theme(
    axis.text.y = element_text(face = "bold", size = 5.0),
    axis.text.x = element_text(face = "bold", size = 8.0, angle = 0, hjust = .5)
  )
save_figure("FigureS3", p, 200, 240, "supplementary")

# -----------------------------------------------------------------------------
# Figure S4: held-out feature associations for Overall / Women / Men####
# Wider export and clearer common HR scale.
# -----------------------------------------------------------------------------
s4 <- read_src("FigureS4_overall_women_men_stage2B_associations.csv") %>%
  mutate(
    outcome_label = factor(outcome_label, levels = c("Overall", "Women", "Men")),
    modality_label = factor(modality_label, levels = modalities),
    feature_key = paste(outcome_label, modality, model_variable, sep = "||")
  )
s4_level_df <- s4 %>%
  distinct(outcome_label, feature_key, display_name, abs_signed_importance, selected_rank) %>%
  arrange(outcome_label, abs_signed_importance, desc(selected_rank))
s4_levels <- unique(s4_level_df$feature_key)
s4_labels <- setNames(s4_level_df$display_name, s4_level_df$feature_key)
s4 <- s4 %>% mutate(feature = factor(feature_key, levels = s4_levels))

s4_hr_range <- range(c(s4$CI_low, s4$CI_high), na.rm = TRUE)
s4_break_candidates <- c(.8, .9, 1.0, 1.1, 1.2, 1.5, 2, 3, 5, 10)
s4_breaks <- s4_break_candidates[
  s4_break_candidates >= max(.01, s4_hr_range[[1]] * .95) &
    s4_break_candidates <= s4_hr_range[[2]] * 1.05
]
if (length(s4_breaks) < 4L) {
  s4_breaks <- scales::log_breaks(n = 6)(s4_hr_range)
}

p <- ggplot(s4, aes(HR, feature, colour = modality_label)) +
  geom_vline(
    xintercept = s4_breaks[s4_breaks != 1], linetype = 3,
    colour = "#D1D7DB", linewidth = .32
  ) +
  geom_vline(xintercept = 1, linetype = 2, colour = "#87939B", linewidth = .58) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high), orientation = "y",
    width = .10, na.rm = TRUE
  ) +
  geom_point(size = 1.85, na.rm = TRUE) +
  facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
  scale_x_log10(
    breaks = s4_breaks,
    labels = label_number(accuracy = .1),
    expand = expansion(mult = c(.04, .08))
  ) +
  scale_y_discrete(labels = function(x) unname(s4_labels[x])) +
  scale_colour_manual(values = modality_col, limits = modalities, drop = FALSE) +
  labs(x = "Adjusted held-out HR (95% CI)", y = NULL,
       title = NULL, subtitle = NULL, colour = NULL) +
  supp_theme +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold", size = 5.8),
    axis.text.x = element_text(face = "bold", size = 8.3),
    panel.spacing.x = grid::unit(11, "mm"),
    plot.margin = margin(6, 8, 6, 8)
  )
save_figure("FigureS4", p, 300, 215, "supplementary")

# -----------------------------------------------------------------------------
# Figure S5: complete P1-P8 Stage 2A feature membership.####
# ALL unique features selected in at least one P1-P8 pattern are retained.
# Left strip = data domain; right strip = Shared/Multi-pattern/Pattern-specific.
# -----------------------------------------------------------------------------
s5 <- read_src("FigureS5_exact_figure3_membership.csv") %>%
  filter(outcome_label %in% pattern_levels) %>%
  mutate(
    outcome_label = factor(outcome_label, levels = pattern_levels),
    modality_label = factor(modality_label, levels = modalities),
    sharedness = factor(
      sharedness,
      levels = c("Shared", "Multi-pattern", "Pattern-specific")
    ),
    feature_key = paste(modality, model_variable, sep = "||")
  )

if (!"feature_order_all" %in% names(s5)) {
  stop(
    "Figure S5 requires the all-feature v8 source with feature_order_all. ",
    "Re-run 07 Section 8 before running Figure S5."
  )
}

s5_anno <- s5 %>%
  distinct(
    feature_order_all, feature_key, display_name,
    modality_label, sharedness, n_selected_patterns
  ) %>%
  arrange(feature_order_all)

s5_levels <- rev(s5_anno$feature_key)
s5_anno <- s5_anno %>%
  mutate(feature = factor(feature_key, levels = s5_levels))
s5 <- s5 %>%
  mutate(feature = factor(feature_key, levels = s5_levels))

# 左侧全部 feature 名称；字号根据总行数自动缩小，但不隐藏任何 feature。
s5_n_features <- nrow(s5_anno)
s5_label_size <- dplyr::case_when(
  s5_n_features <= 120 ~ 2.55,
  s5_n_features <= 220 ~ 2.10,
  s5_n_features <= 320 ~ 1.78,
  TRUE ~ 1.55
)
s5_height_mm <- max(300, min(620, 1.45 * s5_n_features))

pS5_lab <- ggplot(s5_anno, aes(x = 1, y = feature, label = display_name)) +
  geom_text(
    hjust = 1, fontface = "bold", size = s5_label_size,
    colour = "#26313A", lineheight = .92
  ) +
  scale_x_continuous(limits = c(0, 1.01), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(5, 2, 8, 20))

pS5_domain <- ggplot(s5_anno, aes(x = 1, y = feature, fill = modality_label)) +
  geom_tile(width = .88, colour = "white", linewidth = .12) +
  scale_fill_manual(values = modality_col, limits = modalities, drop = FALSE,
                    name = "Data domain") +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(5, 1, 8, 1))

pS5_main <- ggplot(s5, aes(outcome_label, feature)) +
  geom_tile(aes(fill = selected_stage2a), colour = "white", linewidth = .12) +
  scale_fill_manual(
    values = c("FALSE" = "#E6E9EC", "TRUE" = "#557F9F"),
    name = "Selected", drop = FALSE
  ) +
  scale_x_discrete(position = "top", drop = FALSE) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  tile_theme + supp_theme +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8.8, angle = 0),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 3, 8, 3)
  )

pS5_type <- ggplot(s5_anno, aes(x = 1, y = feature, fill = sharedness)) +
  geom_tile(width = .88, colour = "white", linewidth = .12) +
  scale_fill_manual(values = sharedness_col,
                    limits = c("Shared", "Multi-pattern", "Pattern-specific"),
                    drop = FALSE, name = "Feature type") +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(5, 2, 8, 1))

legend_s6_domain <- cowplot::get_legend(
  pS5_domain +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8),
      legend.text = element_text(face = "bold", size = 8.0)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                               keywidth = grid::unit(5.2, "mm"),
                               keyheight = grid::unit(3.8, "mm")))
)
legend_s6_type <- cowplot::get_legend(
  pS5_type +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8),
      legend.text = element_text(face = "bold", size = 8.0)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                               keywidth = grid::unit(5.2, "mm"),
                               keyheight = grid::unit(3.8, "mm")))
)
legend_s6_selected <- cowplot::get_legend(
  pS5_main +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8.8),
      legend.text = element_text(face = "bold", size = 8.0)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                               keywidth = grid::unit(5.2, "mm"),
                               keyheight = grid::unit(3.8, "mm")))
)

s5_legend_row1 <- cowplot::plot_grid(
  legend_s6_domain, legend_s6_type,
  nrow = 1, rel_widths = c(1.18, 1.00), align = "h"
)
s5_legend_row2 <- cowplot::plot_grid(
  NULL, legend_s6_selected, NULL,
  nrow = 1, rel_widths = c(.60, .85, .60)
)
s5_legend_block <- cowplot::plot_grid(
  s5_legend_row1, s5_legend_row2,
  ncol = 1, rel_heights = c(1, .90), align = "v"
)

s5_body <- pS5_lab | pS5_domain | pS5_main | pS5_type
s5_body <- s5_body +
  plot_layout(widths = c(3.00, .12, 1.60, .12))

p <- s5_body / wrap_elements(full = s5_legend_block) +
  plot_layout(heights = c(1, .060))

save_figure("FigureS5", p, 250, s5_height_mm, "supplementary")

# -----------------------------------------------------------------------------
# Figure S6: single-modality held-out prediction####
# Overall -> Women -> Men -> P1 ... P8 from top to bottom.
# -----------------------------------------------------------------------------
s6 <- read_src("FigureS6_single_modality_heldout_prediction.csv") %>%
  filter(combination %in% c("E", "B", "M", "G")) %>%
  mutate(
    outcome_plot = factor(outcome_label, levels = rev(outcomes)),
    combination = factor(combination, levels = c("E", "B", "M", "G"))
  )
s6_auc_min <- suppressWarnings(min(s6$auc_10y[is.finite(s6$auc_10y)], na.rm = TRUE))
s6_auc_floor <- max(0.5, floor((s6_auc_min - 0.03) / 0.05) * 0.05)
s6_auc_breaks <- seq(s6_auc_floor, 1.0, by = 0.05)

p <- ggplot(s6, aes(outcome_plot, auc_10y, colour = combination)) +
  geom_point(position = position_dodge(width = .55), size = 2.0) +
  geom_hline(yintercept = s6_auc_breaks, linetype = 3,
             colour = "#D3D8DC", linewidth = .30) +
  scale_y_continuous(
    limits = c(s6_auc_floor, 1),
    breaks = s6_auc_breaks,
    labels = label_number(accuracy = 0.01),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_flip() +
  scale_colour_manual(
    values = c(E = "#4F7897", B = "#B87376", M = "#6C987C", G = "#9B85AF"),
    limits = c("E", "B", "M", "G"), drop = FALSE,
    labels = modalities
  ) +
  labs(x = NULL, y = "10-year held-out AUC", colour = "Modality",
       title = NULL, subtitle = NULL) +
  supp_theme +
  theme(legend.position = "bottom")
save_figure("FigureS6", p, 165, 145, "supplementary")

# -----------------------------------------------------------------------------
# Figure S7: incremental multimodal prediction####
# Combination order fixed as:
# E -> E+B -> E+M -> E+G -> E+B+M -> E+B+G -> E+M+G -> E+B+M+G
# Overall remains at the top; dashed guides retained.
# -----------------------------------------------------------------------------
s7_combination_levels <- c(
  "E",
  "E+B",
  "E+M",
  "E+G",
  "E+B+M",
  "E+B+G",
  "E+M+G",
  "E+B+M+G"
)

s7 <- read_src("FigureS7_incremental_multimodal_prediction.csv") %>%
  filter(contains_environment %in% TRUE)

# QA：S7 只允许上述 8 种 E-based combinations。
s7_unexpected <- setdiff(
  unique(as.character(na.omit(s7$combination))),
  s7_combination_levels
)
if (length(s7_unexpected)) {
  stop("Figure S7 contains unexpected combinations: ", paste(s7_unexpected, collapse = ", "))
}

s7 <- s7 %>%
  mutate(
    outcome_plot = factor(outcome_label, levels = rev(outcomes)),
    combination = factor(combination, levels = s7_combination_levels)
  ) %>%
  arrange(outcome_plot, combination)

s7_range <- range(
  s7$delta_auc_10y_vs_E[is.finite(s7$delta_auc_10y_vs_E)],
  na.rm = TRUE
)
s7_breaks <- pretty(
  c(min(-.01, s7_range[[1]]), max(.01, s7_range[[2]])),
  n = 7
)

p <- ggplot(
  s7,
  aes(outcome_plot, delta_auc_10y_vs_E, colour = combination)
) +
  geom_hline(
    yintercept = s7_breaks,
    linetype = 3,
    colour = "#D0D6DA",
    linewidth = .30
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "#8E9AA2",
    linewidth = .55
  ) +
  geom_point(
    position = position_dodge(width = .65),
    size = 1.5
  ) +
  coord_flip() +
  scale_colour_manual(
    values = combination_col[s7_combination_levels],
    limits = s7_combination_levels,
    breaks = s7_combination_levels,
    drop = FALSE
  ) +
  scale_y_continuous(breaks = s7_breaks) +
  labs(
    x = NULL,
    y = "10-year AUC gain over environment",
    colour = "Combination",
    title = NULL,
    subtitle = NULL
  ) +
  supp_theme +
  theme(legend.position = "bottom") +
  guides(
    colour = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

save_figure("FigureS7", p, 195, 155, "supplementary")

# -----------------------------------------------------------------------------
# Figure S8: held-out time-dependent ROC####
# 在每个 panel 内标注 5-year / 10-year AUC
# -----------------------------------------------------------------------------

roc <- fread(
  file.path(source_dir, "FigureS8_time_dependent_ROC.csv"),
  select = c(
    "outcome_label",
    "horizon_years",
    "false_positive_rate",
    "sensitivity",
    "auc"
  ),
  showProgress = FALSE
)

roc <- roc[
  is.finite(false_positive_rate) &
    is.finite(sensitivity)
]




roc_auc <- unique(
  roc[
    is.finite(auc),
    .(
      outcome_label,
      horizon_years,
      auc
    )
  ]
)

roc_auc[, outcome_label := factor(
  outcome_label,
  levels = outcomes
)]

roc_auc[, horizon_factor := factor(
  horizon_years,
  levels = c(5, 10),
  labels = c("5-year", "10-year")
)]

roc_auc[, auc_label := sprintf(
  "%g-y AUC = %.3f",
  horizon_years,
  auc
)]

roc_auc[, label_x := 0.96]

roc_auc[, label_y := fifelse(
  horizon_years == 5,
  0.18,
  0.09
)]


roc[, fpr_bin := round(
  false_positive_rate,
  3
)]

roc_plot <- roc[
  ,
  .(
    sensitivity = max(
      sensitivity,
      na.rm = TRUE
    )
  ),
  by = .(
    outcome_label,
    horizon_years,
    fpr_bin
  )
]

roc_plot[, outcome_label := factor(
  outcome_label,
  levels = outcomes
)]

roc_plot[, horizon_factor := factor(
  horizon_years,
  levels = c(5, 10),
  labels = c("5-year", "10-year")
)]

rm(roc)
gc(FALSE)



p <- ggplot(
  roc_plot,
  aes(
    x = fpr_bin,
    y = sensitivity,
    colour = horizon_factor
  )
) +
  
  # Reference line
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    colour = "#BEC4C8",
    linewidth = 0.45
  ) +
  
  # ROC curves
  geom_line(
    linewidth = 0.65
  ) +
  
  # AUC values
  geom_text(
    data = roc_auc,
    aes(
      x = label_x,
      y = label_y,
      label = auc_label,
      colour = horizon_factor
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0,
    size = 2.55,
    fontface = "bold",
    show.legend = FALSE
  )  +
  
  facet_wrap(
    ~ outcome_label,
    ncol = 4
  ) +
  
  coord_equal(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  
  scale_x_continuous(
    breaks = seq(
      0,
      1,
      by = 0.25
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.25
    )
  ) +
  
  scale_colour_manual(
    values = c(
      "5-year" = "#4F7897",
      "10-year" = "#B87376"
    ),
    drop = FALSE
  ) +
  
  labs(
    x = "False positive rate",
    y = "Sensitivity",
    colour = "Follow-year",
    title = NULL,
    subtitle = NULL
  ) +
  
  supp_theme +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    ),
    legend.text = element_text(
      face = "bold"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      face = "bold"
    )
  )


save_figure(
  "FigureS8",
  p,
  220,
  175,
  "supplementary"
)
# -----------------------------------------------------------------------------
# Figure S9 (a/b): standard calibration curves.####
# Saved Stage 3 calibration groups are connected in ascending predicted-risk order;
# point size is fixed (not proportional to group n) to avoid clipping/occlusion.
# -----------------------------------------------------------------------------
cal_plot <- function(z, horizon) {
  z <- z %>%
    mutate(outcome_label = factor(outcome_label, levels = outcomes)) %>%
    arrange(outcome_label, mean_predicted_risk)

  ggplot(
    z,
    aes(mean_predicted_risk, observed_km_risk, group = outcome_label)
  ) +
    geom_abline(
      slope = 1, intercept = 0, linetype = 2,
      colour = "#98A6AF", linewidth = .48
    ) +
    geom_line(
      colour = "#4F7897", linewidth = .72, alpha = .92
    ) +
    geom_point(
      colour = "#4F7897", fill = "white", shape = 21,
      size = 2.15, stroke = .70
    ) +
    facet_wrap(~ outcome_label, ncol = 4, scales = "free") +
    scale_x_continuous(expand = expansion(mult = c(.04, .09))) +
    scale_y_continuous(expand = expansion(mult = c(.04, .09))) +
    labs(
      x = paste0(horizon, "-year mean predicted risk"),
      y = paste0(horizon, "-year observed KM risk"),
      title = NULL, subtitle = NULL
    ) +
    supp_theme +
    theme(
      panel.grid.major = element_line(
        linetype = 3, colour = "#D9DEE2", linewidth = .28
      ),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      panel.spacing = grid::unit(4.8, "mm")
    )
}

# 最终 Figure S9：a = 5 年校准；b = 10 年校准，合并为同一张补充图。
pS9a <- cal_plot(read_src("FigureS9_5year_calibration.csv"), 5) +
  labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 12))
pS9b <- cal_plot(read_src("FigureS9_10year_calibration.csv"), 10) +
  labs(tag = "b") + theme(plot.tag = element_text(face = "bold", size = 12))
figureS9_plot <- pS9a / pS9b + plot_layout(heights = c(1, 1))
save_figure("FigureS9", figureS9_plot, 220, 360, "supplementary")

# -----------------------------------------------------------------------------
# Figure S10: leading final integrated predictors; no numeric prefix.####
# Legend only shows modalities that actually appear in the plotted top 15.
# -----------------------------------------------------------------------------
s10 <- read_src("FigureS10_final_integrated_feature_importance.csv") %>%
  mutate(
    outcome_label = factor(outcome_label, levels = outcomes),
    modality_label = factor(modality_label, levels = modalities),
    feature_key = paste(outcome_label, model_variable, sep = "||"),
    short_name = if_else(
      nchar(display_name) > 38,
      paste0(substr(display_name, 1, 35), "..."),
      display_name
    )
  )
s10_level_df <- s10 %>%
  distinct(outcome_label, feature_key, short_name, final_importance_rank) %>%
  arrange(outcome_label, desc(final_importance_rank))
s10_levels <- unique(s10_level_df$feature_key)
s10_labels <- setNames(s10_level_df$short_name, s10_level_df$feature_key)
s10 <- s10 %>% mutate(feature = factor(feature_key, levels = s10_levels))
s10_xmax <- max(s10$final_predictor_importance, na.rm = TRUE)
s10_guides <- pretty(c(0, s10_xmax), n = 6)
s10_present_modalities <- modalities[
  modalities %in% unique(as.character(s10$modality_label))
]

p <- ggplot(s10, aes(final_predictor_importance, feature, fill = modality_label)) +
  geom_vline(
    xintercept = s10_guides[s10_guides > 0], linetype = 3,
    colour = "#D7DBDE", linewidth = .30
  ) +
  geom_col(width = .75) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = modality_col[s10_present_modalities],
    limits = s10_present_modalities,
    drop = TRUE
  ) +
  scale_y_discrete(labels = function(x) unname(s10_labels[x])) +
  scale_x_continuous(breaks = s10_guides) +
  labs(x = "Final integrated feature importance magnitude", y = NULL,
       title = NULL, subtitle = NULL, fill = "Modality") +
  supp_theme +
  theme(
    axis.text.y = element_text(face = "bold", size = 5.4),
    legend.position = "bottom"
  )
save_figure("FigureS10", p, 205, 330, "supplementary")

# -----------------------------------------------------------------------------
# Figure S11: Environmental/phenotypic subdomain contributions only.####
# v8 source excludes B/M/G NA subdomain rows and renormalizes E subdomains to 100%.
# -----------------------------------------------------------------------------
s11 <- read_src("FigureS11_environmental_subdomain_wald.csv") %>%
  mutate(outcome_plot = factor(outcome_label, levels = rev(outcomes)))
subdomain_names <- unique(as.character(s11$subdomain))
subdomain_names <- c(sort(setdiff(subdomain_names, "Other / unclassified")),
                     intersect("Other / unclassified", subdomain_names))
subdomain_col <- setNames(
  grDevices::colorRampPalette(c("#426B8A", "#8FB3B0", "#C0B797", "#9A87A2"))(
    max(1, length(subdomain_names))
  ),
  subdomain_names
)
p <- ggplot(
  s11,
  aes(outcome_plot, relative_wald_share, fill = factor(subdomain, levels = subdomain_names))
) +
  geom_hline(
    yintercept = c(.25, .50, .75), linetype = 3,
    colour = "#FFFFFF", linewidth = .45
  ) +
  geom_col(width = .80) +
  coord_flip() +
  scale_y_continuous(labels = label_percent(), breaks = c(0, .25, .5, .75, 1)) +
  scale_fill_manual(values = subdomain_col, drop = FALSE) +
  labs(x = NULL, y = "Relative Wald share within environmental/phenotypic score",
       fill = "Subdomain", title = NULL, subtitle = NULL) +
  supp_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 7.2),
    legend.text = element_text(face = "bold", size = 6.5),
    legend.key.height = grid::unit(3.2, "mm"),
    legend.key.width = grid::unit(4.2, "mm")
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))
save_figure("FigureS11", p, 205, 175, "supplementary")

# -----------------------------------------------------------------------------
# Figure S12: full 2x2 environment x individual omic joint profiles.####
# Biomarkers, Metabolomics and Genetics are separated into three facet blocks.
# -----------------------------------------------------------------------------
s12 <- read_src("FigureS12_environment_individual_omics_joint_risk.csv") %>%
  mutate(
    logHR = log(HR),
    label = if_else(is.finite(HR), sprintf("%.2f", HR), "x"),
    omic_label = factor(
      omic_label,
      levels = c("Biomarkers", "Metabolomics", "Genetics")
    ),
    joint_group = factor(
      joint_group,
      levels = c(
        "Lower risk / Lower risk",
        "Lower risk / Higher risk",
        "Higher risk / Lower risk",
        "Higher risk / Higher risk"
      ),
      labels = c("L/L", "L/H", "H/L", "H/H")
    ),
    outcome_label = fct_rev(factor(outcome_label, levels = outcomes))
  )

p <- ggplot(s12, aes(joint_group, outcome_label)) +
  geom_tile(aes(fill = logHR), colour = "white", linewidth = .40) +
  geom_text(aes(label = label), size = 2.15, fontface = "bold") +
  facet_grid(
    . ~ omic_label,
    scales = "free_x",
    space = "free_x"
  ) +
  diverging_soft(s12$logHR, "log(HR)") +
  labs(x = "Environmental / omic risk group", y = NULL,
       title = NULL, subtitle = NULL) +
  tile_theme + supp_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 8.4),
    axis.text.y = element_text(face = "bold", size = 8.4),
    strip.text = element_text(face = "bold", size = 9.2),
    panel.spacing.x = grid::unit(6.5, "mm"),
    legend.position = "bottom"
  )
save_figure("FigureS12", p, 215, 185, "supplementary")

# -----------------------------------------------------------------------------
# Figure S13: final integrated score clinical stratification####
# a = held-out KM by development-defined tertile group
# b = held-out High versus Low Cox association
# -----------------------------------------------------------------------------

s13_km <- read_src("FigureS13_integrated_score_KM.csv") %>%
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = outcomes
    ),
    risk_group = factor(
      risk_group,
      levels = c(
        "Low",
        "Intermediate",
        "High"
      )
    )
  )

s13_p <- s13_km %>%
  distinct(
    outcome_label,
    logrank_p
  ) %>%
  mutate(
    p_label = case_when(
      is.na(logrank_p) ~ NA_character_,
      logrank_p < 0.001 ~ "Log-rank P < 0.001",
      TRUE ~ paste0(
        "Log-rank P = ",
        formatC(
          logrank_p,
          format = "f",
          digits = 3
        )
      )
    )
  )

risk_group_col <- c(
  Low = "#8B969D",
  Intermediate = "#4F7897",
  High = "#5C477E"
)

p13a <- ggplot(
  s13_km,
  aes(
    x = time_years,
    y = survival,
    colour = risk_group
  )
) +
  
  geom_step(
    linewidth = .60
  ) +
  
  geom_text(
    data = s13_p,
    aes(
      x = 1.25,
      y = 0.82,
      label = p_label
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 2.45,
    fontface = "bold",
    colour = "#26313A",
    na.rm = TRUE
  ) +
  
  facet_wrap(
    ~ outcome_label,
    ncol = 4,
    scales = "free_y"
  ) +
  
  scale_colour_manual(
    values = risk_group_col,
    drop = FALSE
  ) +
  
  scale_x_continuous(
    breaks = c(0, 5, 10, 15)
  ) +
  
  # IMPORTANT:
  # keep clipping ON so KM estimates beyond 15 years
  # are not drawn outside the displayed follow-up window
  coord_cartesian(
    xlim = c(0, 15),
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Follow-up time (years)",
    y = "Event-free survival",
    colour = "Risk group",
    title = NULL,
    subtitle = NULL,
    tag = "a"
  ) +
  
  supp_theme +
  
  theme(
    legend.position = "bottom",
    
    strip.text = element_text(
      face = "bold",
      size = 9.0,
      colour = "#111111"
    ),
    
    plot.tag = element_text(
      face = "bold",
      size = 11.5
    ),
    
    panel.spacing.x = grid::unit(
      4.5,
      "mm"
    ),
    
    panel.spacing.y = grid::unit(
      3.5,
      "mm"
    )
  )
p13b <- ggplot(s13_hr, aes(HR, outcome_plot)) +
  geom_vline(
    xintercept = c(1, 2, 5, 10, 20), linetype = 3,
    colour = "#D2D7DA", linewidth = .30
  ) +
  geom_vline(xintercept = 1, linetype = 2, colour = "#89949C", linewidth = .55) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .15) +
  geom_point(size = 1.9, colour = "#5C477E") +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50)) +
  labs(x = "Hazard ratio: High vs Low integrated risk (95% CI)", y = NULL,
       title = NULL, subtitle = NULL, tag = "b") +
  supp_theme +
  theme(plot.tag = element_text(face = "bold", size = 11.5))
figureS13_plot <- p13a / p13b + plot_layout(heights = c(2.3, 1.0))
save_figure("FigureS13", figureS13_plot, 195, 230, "supplementary")


# =============================================================================
# Final export/order QA
# =============================================================================
qa <- bind_rows(export_rows) %>% mutate(passed = all_files_exist & all_nonempty)
fwrite(as.data.table(qa), file.path(log_dir, "figure_export_QA.csv"), bom = TRUE)
if (!all(qa$passed)) {
  stop("Some figure exports failed QA; inspect figure_export_QA.csv")
}
figure_order_qa <- tibble(
  check = c("main Figure 2-5 and supplementary S1-S13 only", "17 figures exported"),
  passed = c(identical(qa$figure, expected_figures), nrow(qa) == 17L)
)
fwrite(as.data.table(figure_order_qa),
       file.path(log_dir, "figure_order_QA.csv"), bom = TRUE)
if (!all(figure_order_qa$passed)) {
  stop("Figure order QA failed; inspect figure_order_QA.csv. Run the missing figure blocks first.")
}

# 全部新编号输出成功后，移除已取消的旧 Figure S14 导出文件。
legacy_s14 <- file.path(supp_dir, paste0("FigureS14", c(".pdf", ".tiff", ".png", ".svg")))
if (any(file.exists(legacy_s14))) unlink(legacy_s14[file.exists(legacy_s14)])
record_figure_checkpoint("FigureS13", complete = TRUE)
cat("Final exports completed: Figure 2–5 and Figure S1–S13 (PDF/TIFF/PNG); S9 contains calibration panels a/b.\n")
