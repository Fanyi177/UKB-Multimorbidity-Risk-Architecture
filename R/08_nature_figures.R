#!/usr/bin/env Rscript
# FINAL 20261025 figure contract: Stage 2A selection -> signed importance -> independent
# held-out association -> Stage 3 prediction and risk. All graphics use R.
# Main Figure 5d/e uses the integrated Stage 3B CoxEM Weighted_beta score;
# duplicate supplementary plots are deliberately NOT exported.

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
  if (!file.exists(path)) stop("Missing figure source: ", path,
                                "; run 07 Section 0–11 first.")
  z <- as_tibble(fread(path, showProgress = FALSE))
  # In all figures use the compact labels E/P/M/G ONLY. Full formal names
  # belong in the figure legends/captions, not in panel strips or axes.
  # Do not modify source CSV values, Stage 2/3 Python keys or model codes.
  label_map <- c(
    "Environmental/phenotypic" = "E",
    "Environmental and phenotypic factors" = "E",
    "Environmental and clinical characteristics" = "E",
    "Environment (E)" = "E",
    "Environment" = "E",
    "Blood" = "P", "Biomarkers" = "P", "Phenotype (P)" = "P",
    "Phenotypic biomarkers" = "P", "Phenotype" = "P",
    "Metabolomics" = "M", "Metabolomics (M)" = "M",
    "Metabolomic biomarkers" = "M",
    "Genetics" = "G", "Genetics (G)" = "G",
    "Genetic factors" = "G"
  )
  for (nm in intersect(c("modality_label", "component", "omic_label"), names(z))) {
    old <- as.character(z[[nm]])
    is_old <- !is.na(old) & old %in% names(label_map)
    old[is_old] <- unname(label_map[old[is_old]])
    z[[nm]] <- old
  }
  z
}

pattern_levels <- paste0("P", 1:8)
outcomes <- c("Overall", pattern_levels)
# Shared plotting convention: in horizontal charts, Overall is at the TOP,
# followed by P1–P8. In ggplot discrete y axes, the last level is at the top;
# the same convention applies to the original x factor after coord_flip().
outcomes_y_levels <- rev(outcomes)
modalities <- c("E", "P", "M", "G")
# Define the abbreviations in figure captions (not in axes or panel labels):
# E: environmental and clinical characteristics;
# P: phenotypic biomarkers (routine blood and urine markers);
# M: metabolomic biomarkers; G: genetic factors.
# Keep the Python/Stage 3B raw combination code B on disk; manuscript plot only = P.
combo_display <- function(x) gsub("B", "P", as.character(x), fixed = TRUE)
short_domain_labels <- modalities


pattern_col <- c(
  P1 = "#426B8A", P2 = "#6585A4", P3 = "#488A83", P4 = "#AD9255",
  P5 = "#B97764", P6 = "#A36F86", P7 = "#81739C", P8 = "#687C8B"
)
modality_col <- c(E="#4F7897", P="#B87376", M="#6C987C", G="#9B85AF")
sharedness_col <- c(
  "Shared" = "#39434D",
  "Multi-pattern" = "#83AAA6",
  "Pattern-specific" = "#D3AE75"
)
combination_display_col <- c(
  "E+P" = "#B45F5C",
  "E+M" = "#4C9078",
  "E+G" = "#745AA8",
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

# Figure 5b only: a colour discontinuity at log(HR)=0 (HR=1).
# Strong blue for HR<1, neutral for HR=1, and distinctly RED from any HR>1.
# The very narrow transitions immediately adjacent to zero avoid making
# mildly increased risks look grey when a few groups have much larger HRs.
# This affects only colour encoding, NOT estimates, confidence intervals or p-values.
figure5b_hr_scale <- function(log_hr) {
  log_hr <- as.numeric(log_hr)
  log_hr <- log_hr[is.finite(log_hr)]
  if (!length(log_hr)) stop("Figure 5b: no finite HR available for colour scale")
  neg_span <- max(0.015, -min(0, log_hr))
  pos_span <- max(0.015,  max(0, log_hr))
  lim <- c(-neg_span, pos_span)
  eps <- min(1e-6, neg_span / 1000, pos_span / 1000)
  knots <- scales::rescale(c(-neg_span, -eps, 0, eps, pos_span), from=lim)
  # Keep the light-red threshold clearly red (not light neutral grey).
  # HR=1 remains neutral, with an abrupt visual change on either side.
  scale_fill_gradientn(
    colours = c("#4F7897", "#A8C5D7", "#E5E8EA", "#E5A09F", "#A3485D"),
    values = knots, limits = lim, oob = scales::squish,
    na.value = "#E9ECEE", name = "HR",
    breaks = sort(unique(c(0, pretty(lim, n=5)))),
    labels = function(x) sprintf("%.2f", exp(x)),
    guide = guide_colorbar(ticks = TRUE, frame.colour = "#6C7780")
  )
}

# Figure 3b/3c 专用连续色阶。零值采用柔和蓝灰色，避免底部色度尺中央发白；
# 不修改 S1/S3 等补充图既有的配色方案。
diverging_figure3 <- function(x, title) {
  scale_fill_gradient2(
    low = "#487CA2", mid = "#CBD4D8", high = "#B96968", midpoint = 0,
    limits = c(-sym_lim(x), sym_lim(x)), oob = squish,
    na.value = "#E9ECEE", name = title,
    guide = guide_colorbar(ticks = FALSE, frame.colour = NA)
  )
}


# 每张图完成后记录可继续的位置；与 07 共用本次最终版 checkpoint。
checkpoint_path <- file.path(root, "UKB_final_20261025_checkpoint.md")
expected_figures <- c(
  paste0("Figure", 2:5), paste0("FigureS", 1:4),
  paste0("FigureS5", letters[1:4]), "FigureS6", "FigureS7"
)
record_figure_checkpoint <- function(id, complete = FALSE) {
  position <- match(id, expected_figures)
  next_step <- if (complete) {
    "检查导出的 Figure 2–5、Figure S1–S4、S5a–d、S6–S7，并用于稿件。"
  } else if (!is.na(position) && position < length(expected_figures)) {
    paste0("从 08 的 ", expected_figures[[position + 1L]], " 代码块继续。")
  } else {
    "运行 08 最后的 export/order QA 代码块。"
  }
  writeLines(c(
    "# UKB multimorbidity final output checkpoint",
    paste0("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Current stage: ", if (complete) "08 exports/order QA completed" else paste("08 exported", id)),
    "MAIN cohort: dynamic N from source/UKB_diseases_data_with_patterns.csv",
    "Tables: S1–S3 and S10–S26 generated by 07; other tables prepared separately",
    "Figures: main Figure 2–5; supplementary S1–S4, S5a–d, S6 calibration, S7 predictors",
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

# Figure 3a: compact display E -> P -> M -> G, top to bottom;
# caption defines the four abbreviations once.
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
  diverging_figure3(b3$plot_signed_importance, "Signed importance") +
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
    label = "*", size = 3.4, fontface = "bold", colour = "#1F252B"
  ) +
  diverging_figure3(c3$plot_log2_HR, "log2(HR)") +
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
  plot_layout(widths = c(2.05, .12, 2.85, 2.85, .13))

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
  legend_domain, legend_type, nrow = 1, rel_widths = c(1.20, 1.00), align = "h"
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

# Row-count-driven figure height ensures additional shared/specific features remain readable.
figure3_height_mm <- max(350, min(540, 150 + nrow(row3) * 6.7))
save_figure("Figure3", figure3_plot, 255, figure3_height_mm)

# =============================================================================
# Main Figure 4: held-out discrimination, incremental gain, ROC and risk architecture####
# =============================================================================
# Six panels (3x2):
# a Single E/P/M/G 10-y AUC (old S6)
# b All 8 E-based combinations, including the E reference (old S7)
# c Final integrated C-index and mean time-dependent AUC (old Figure 4b)
# d 5-y and 10-y time-dependent ROC (old S8)
# e Relative Wald share of the four final-model partial scores (old Figure 4c)
# f Within-environmental-domain Wald composition (old S11)
# Backend combinations still use B. Display labels are E/P/M/G.
# =============================================================================

# a) Stand-alone domains: 9 outcomes x 4 domains, with fixed ordered offsets.
s6_4 <- read_src("FigureS6_single_modality_heldout_prediction.csv") %>%
  filter(combination %in% c("E", "B", "M", "G"), outcome_label %in% outcomes) %>%
  mutate(domain_code = factor(combo_display(combination), levels = c("E", "P", "M", "G")),
         outcome_plot = factor(outcome_label, levels = outcomes_y_levels))
if (!nrow(s6_4)) stop("Figure4a: no single-domain performance rows")
p4a <- ggplot(s6_4, aes(x = auc_10y, y = outcome_plot, colour = domain_code)) +
  geom_vline(xintercept = seq(.5, 1, .1), colour = "#E1E5E8", linetype = 3, linewidth = .32) +
  geom_point(aes(shape = domain_code), position = position_dodge(width = .60), size = 2.05) +
  scale_colour_manual(values = c(E="#4F7897", P="#B87376", M="#6C987C", G="#9B85AF"),
                      breaks = c("E","P","M","G"), name = NULL) +
  scale_shape_manual(values = c(E=16,P=17,M=15,G=18), name = NULL) +
  scale_x_continuous(breaks = pretty_breaks(6), labels = label_number(accuracy=.01),
                     limits = c(max(0.45, min(s6_4$auc_10y,na.rm=TRUE)-.045),
                                min(1, max(s6_4$auc_10y,na.rm=TRUE)+.045))) +
  labs(x="10-year held-out AUC",y=NULL,title="Single-domain prediction",tag="a") +
  guides(colour = guide_legend(nrow=1, byrow=TRUE,
           override.aes=list(shape=c(16,17,15,18),size=2.8)), shape="none") +
  theme(legend.position="bottom",plot.tag.position=c(.01,.99),
        legend.text=element_text(face="bold",size=7.4))

# b) E-based combinations; offset labels are mapped to ONE vertical slot per
# combination. Both shape and high-contrast colour encode group membership.
s7_codes <- c("E", "E+B", "E+M", "E+G", "E+B+M", "E+B+G", "E+M+G", "E+B+M+G")
s7_4 <- read_src("FigureS7_incremental_multimodal_prediction.csv") %>%
  filter(contains_environment %in% TRUE, combination %in% s7_codes,
         outcome_label %in% outcomes) %>%
  mutate(combination_display = factor(combo_display(combination), levels=combo_display(s7_codes)),
         combo_index = as.integer(combination_display),
         outcome_index = match(outcome_label, rev(outcomes)),
         y_position = outcome_index * 2.55 + (combo_index-4.5)*.245,
         gain_pp = 100 * delta_auc_10y_vs_E)
if (!nrow(s7_4)) stop("Figure4b: no E-based combinations")
s7_color <- c(E="#7B8790", "E+P"="#B44E65", "E+M"="#3E8E76",
              "E+G"="#7962A9", "E+P+M"="#CC8541", "E+P+G"="#3D7EAC",
              "E+M+G"="#3297A1", "E+P+M+G"="#4A3D80")
# 只使用三种常见形状：圆、方、三角。颜色区分8种具体组合。
s7_shape <- c(E=16, "E+P"=16, "E+M"=16, "E+G"=16,
              "E+P+M"=15, "E+P+G"=15, "E+M+G"=15, "E+P+M+G"=17)
p4b <- ggplot(s7_4, aes(x=gain_pp,y=y_position,colour=combination_display)) +
  geom_vline(xintercept=0, linewidth=.65,colour="#68747D") +
  geom_vline(xintercept=seq(
    floor(min(0,s7_4$gain_pp,na.rm=TRUE)/2)*2,
    ceiling(max(0,s7_4$gain_pp,na.rm=TRUE)/2)*2,by=2),
    linetype="dashed", colour="#C9D3DA",linewidth=.33) +
  geom_segment(aes(x=0,xend=gain_pp,yend=y_position),linewidth=.52,alpha=.90) +
  geom_point(aes(shape=combination_display),size=2.1,stroke=.55) +
  scale_colour_manual(values=s7_color,breaks=combo_display(s7_codes),name=NULL) +
  scale_shape_manual(values=s7_shape,breaks=combo_display(s7_codes),name=NULL) +
  scale_y_continuous(breaks=seq_along(outcomes)*2.55,
                     labels=rev(outcomes),expand=expansion(add=1.20)) +
  scale_x_continuous(labels=function(x) paste0(number(x,accuracy=1)," pp"),
                     breaks=seq(floor(min(0,s7_4$gain_pp,na.rm=TRUE)/2)*2,
                                ceiling(max(0,s7_4$gain_pp,na.rm=TRUE)/2)*2,by=2)) +
  labs(x="10-year AUC gain vs E",y=NULL,title="Incremental domain combinations",tag="b") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE,
         override.aes=list(shape=unname(s7_shape[combo_display(s7_codes)]),size=2.8)),
         shape="none") +
  theme(legend.position="bottom",legend.text=element_text(face="bold",size=7.0),
        plot.tag.position=c(.01,.99),axis.text.y=element_text(face="bold",size=8.0))

# c) Original final-model discrimination with exactly the two original metrics.
b4 <- read_src("Figure4b_final_discrimination.csv") %>% filter(outcome_label %in% outcomes)
b4_long <- bind_rows(
  b4 %>% transmute(outcome_label,metric="C-index",value=c_index),
  b4 %>% transmute(outcome_label,metric="Mean time-dependent AUC",value=mean_time_dependent_auc)
) %>% mutate(outcome_plot=factor(outcome_label,levels=outcomes_y_levels),
             metric=factor(metric,levels=c("C-index","Mean time-dependent AUC")))
p4c <- ggplot(b4_long,aes(x=value,y=outcome_plot,colour=metric,shape=metric)) +
  geom_vline(xintercept=c(.6,.7,.8,.9),linetype=2,colour="#C5CFD5",linewidth=.42) +
  geom_point(size=2.15,position=position_dodge(width=.45)) +
  scale_colour_manual(values=c("C-index"="#4F7897","Mean time-dependent AUC"="#A56984"),name=NULL) +
  scale_shape_manual(values=c("C-index"=16,"Mean time-dependent AUC"=17),name=NULL) +
  scale_x_continuous(breaks=seq(.5,1,.1),labels=label_number(accuracy=.01),
    limits=c(max(.50,min(b4_long$value,na.rm=TRUE)-.04),
             min(1,max(b4_long$value,na.rm=TRUE)+.04))) +
  labs(x="Held-out discrimination",y=NULL,title="Final integrated model",tag="c") +
  theme(legend.position="bottom",plot.tag.position=c(.01,.99))

# d) Read the same ROC source as supplementary S8; cache binned curves and AUC.
roc_raw4 <- fread(file.path(source_dir,"FigureS8_time_dependent_ROC.csv"),
  select=c("outcome_label","horizon_years","false_positive_rate","sensitivity","auc"),
  showProgress=FALSE)
roc_raw4 <- roc_raw4[is.finite(false_positive_rate) & is.finite(sensitivity) &
                       outcome_label %in% outcomes]
roc_auc4 <- unique(roc_raw4[is.finite(auc),
  .(outcome_label,horizon_years,auc)])
roc_raw4[,fpr_bin:=round(false_positive_rate,3)]
roc_curve4 <- roc_raw4[,.(sensitivity=max(sensitivity,na.rm=TRUE)),
 by=.(outcome_label,horizon_years,fpr_bin)]
rm(roc_raw4); gc(FALSE)
roc_curve4[,outcome_label:=factor(outcome_label,levels=outcomes)]
roc_auc4[,outcome_label:=factor(outcome_label,levels=outcomes)]
roc_curve4[,horizon_factor:=factor(horizon_years,levels=c(5,10),labels=c("5-year","10-year"))]
roc_auc4[,horizon_factor:=factor(horizon_years,levels=c(5,10),labels=c("5-year","10-year"))]
roc_auc4[,auc_label:=sprintf("%dy: %.3f",as.integer(horizon_years),auc)]
roc_auc4[,label_y:=fifelse(horizon_years==5,.18,.09)]
p4d <- ggplot(roc_curve4,aes(fpr_bin,sensitivity,colour=horizon_factor)) +
  geom_abline(slope=1,intercept=0,linetype=2,colour="#C2CBD1",linewidth=.35) +
  geom_line(linewidth=.59) +
  geom_text(data=roc_auc4,aes(x=.96,y=label_y,label=auc_label,colour=horizon_factor),
            inherit.aes=FALSE,hjust=1,size=2.18,fontface="bold",show.legend=FALSE) +
  facet_wrap(~outcome_label,ncol=3) +
  coord_equal(xlim=c(0,1),ylim=c(0,1)) +
  scale_x_continuous(breaks=c(0,.5,1)) + scale_y_continuous(breaks=c(0,.5,1)) +
  scale_colour_manual(values=c("5-year"="#4F7897","10-year"="#B87376"),name=NULL) +
  labs(x="False positive rate",y="Sensitivity",title="Time-dependent ROC",tag="d") +
  theme(legend.position="bottom",strip.text=element_text(face="bold",size=7.3),
        axis.text=element_text(size=6.7),axis.title=element_text(size=7.8),
        panel.spacing=grid::unit(2.5,"mm"),plot.tag.position=c(.01,.99))

# e) Figure 4e: 100% stacked horizontal bars, four final-model domain shares.
# Because coord_flip() reverses the visible vertical order, use outcomes_y_levels
# so the TOP-TO-BOTTOM order matches panels a–d: Overall, P1, ..., P8.
# IMPORTANT: the source column relative_wald_share already sums to 1 per outcome.
# geom_col(position="fill") makes the 100% stacked geometry explicit;
# coord_flip() creates HORIZONTAL bars. The raw source itself is never changed.
c4 <- read_src("Figure4c_modality_wald_contribution.csv") %>%
  filter(outcome_label %in% outcomes) %>%
  mutate(
    outcome_plot = factor(outcome_label, levels = outcomes_y_levels),
    component = factor(component_code, levels = modalities)
  )
figure4e_draw_qa <- c4 %>%
  group_by(outcome_label) %>%
  summarise(
    n_domains = n_distinct(component[!is.na(component)]),
    n_rows = n(),
    total = sum(relative_wald_share, na.rm = TRUE),
    all_nonnegative = all(is.finite(relative_wald_share) & relative_wald_share >= 0),
    .groups = "drop"
  )
if (nrow(figure4e_draw_qa) != length(outcomes) ||
    any(figure4e_draw_qa$n_domains != 4L) ||
    any(figure4e_draw_qa$n_rows != 4L) ||
    any(!figure4e_draw_qa$all_nonnegative) ||
    any(abs(figure4e_draw_qa$total - 1) > 1e-8)) {
  stop("Figure 4e source QA failed: each outcome must have four nonnegative shares summing to 100%.")
}
p4e <- ggplot(c4, aes(x=outcome_plot, y=relative_wald_share, fill=component)) +
  geom_col(position="fill", width=.78, colour="white", linewidth=.20) +
  coord_flip() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.25),
                     labels=label_percent(accuracy=1), expand=c(0,0)) +
  scale_x_discrete(drop=FALSE, expand=expansion(add=.52)) +
  scale_fill_manual(values=modality_col, breaks=modalities,
                    labels=c("E","P","M","G"), drop=FALSE, name=NULL) +
  labs(x=NULL, y="Relative Wald chi-square share",
       title="Prognostic contribution by domain", tag="e") +
  # Figure 4e 图例严格单排，不再人为拆成两行。
  guides(fill=guide_legend(nrow=1,byrow=TRUE,direction="horizontal",
                           keywidth=grid::unit(5.2,"mm"))) +
  theme(legend.position="bottom", legend.direction="horizontal",
        plot.tag.position=c(.01,.99),
        legend.text=element_text(face="bold",size=7.0),
        axis.text.y=element_text(face="bold",size=8.0),
        panel.grid.major.x=element_line(colour="#EBEFF1",linewidth=.25))

# f) Figure 4f: subdomain shares WITHIN E (NOT the whole model).
# Apply the identical top-to-bottom outcome ordering as Figure 4e.
# Equal principle: raw composition is checked, then fully stacked to exactly 100%.
s11_4 <- read_src("FigureS11_environmental_subdomain_wald.csv") %>%
  filter(outcome_label %in% outcomes) %>%
  mutate(outcome_plot=factor(outcome_label,levels=outcomes_y_levels))
subdomains4 <- unique(as.character(s11_4$subdomain))
subdomains4 <- c(sort(setdiff(subdomains4,"Other / unclassified")),
                 intersect("Other / unclassified",subdomains4))
sub_colors4 <- setNames(grDevices::hcl.colors(length(subdomains4),palette="Dynamic"),subdomains4)
s11_4 <- s11_4 %>% mutate(subdomain=factor(subdomain,levels=subdomains4))
figure4f_draw_qa <- s11_4 %>% group_by(outcome_label) %>%
  summarise(
    total=sum(relative_wald_share,na.rm=TRUE),
    all_nonnegative=all(is.finite(relative_wald_share) & relative_wald_share>=0),
    .groups="drop"
  )
if (nrow(figure4f_draw_qa) != length(outcomes) ||
    any(!figure4f_draw_qa$all_nonnegative) ||
    any(abs(figure4f_draw_qa$total-1)>1e-8)) {
  stop("Figure 4f source QA failed: E subdomain shares must sum to 100% for every outcome.")
}
p4f <- ggplot(s11_4,aes(x=outcome_plot,y=relative_wald_share,fill=subdomain)) +
  geom_col(position="fill",width=.78,colour="white",linewidth=.17) +
  coord_flip() +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.25),
                     labels=label_percent(accuracy=1),expand=c(0,0)) +
  scale_x_discrete(drop=FALSE,expand=expansion(add=.52)) +
  scale_fill_manual(values=sub_colors4,drop=FALSE,name=NULL) +
  labs(x=NULL,y="Relative Wald share within E",
       title="Within-E subdomain architecture",tag="f") +
  guides(fill=guide_legend(ncol=2,byrow=TRUE)) +
  theme(legend.position="bottom",plot.tag.position=c(.01,.99),
        legend.text=element_text(face="bold",size=6.5),
        legend.key.size=grid::unit(3.3,"mm"),
        axis.text.y=element_text(face="bold",size=8.0),
        panel.grid.major.x=element_line(colour="#EBEFF1",linewidth=.25))

# Persist the exact source percentages for independent QA; row-level sums must be 100%.
fwrite(as.data.table(figure4e_draw_qa),
       file.path(log_dir,"Figure4e_100pct_stacking_QA.csv"),bom=TRUE)
fwrite(as.data.table(figure4f_draw_qa),
       file.path(log_dir,"Figure4f_100pct_stacking_QA.csv"),bom=TRUE)

# Figure 4 不再在右下角添加 E/P/M/G 缩写解释；缩写定义交由图注说明。
figure4_plot <- ((p4a | p4b) / (p4c | p4d) / (p4e | p4f)) +
  plot_layout(heights=c(1.30,1.46,1.05),widths=c(.90,1.55))
save_figure("Figure4",figure4_plot,325,420)

# =============================================================================
# Main Figure 5: environmental/biological score associations, joint profiles,
# and integrated clinical stratification (source formerly Figure S13)####
# =============================================================================
# a Original continuous domain-score Cox associations (nine outcomes)
# b Full 2x2 E x P/M/G (formerly Figure S12)
# c Original combined-omics (equal thirds) full 2x2 contrasts
# d Integrated score held-out KM curves (formerly Figure S13a)
# e Integrated score Intermediate/High vs Low adjusted only as source specifies
# =============================================================================
a5 <- read_src("Figure5a_risk_oriented_domain_score_associations.csv")
b5 <- read_src("FigureS12_environment_individual_omics_joint_risk.csv")
c5 <- read_src("Figure5c_environment_combined_omics.csv")
omics_ref5 <- read_src("Figure5_combined_omics_reference.csv")
s13_km5 <- read_src("FigureS13_integrated_score_KM.csv")
s13_hr5 <- read_src("FigureS13_integrated_score_HR.csv")
if (nrow(omics_ref5) != length(outcomes) ||
    any(abs(omics_ref5$biomarker_weight-1/3)>1e-12) ||
    any(abs(omics_ref5$metabolomics_weight-1/3)>1e-12) ||
    any(abs(omics_ref5$genetics_weight-1/3)>1e-12)) {
  stop("Figure5 combined-omics equal 1/3 weights or 9 outcomes not satisfied")
}
figure5_adjusted_flag <- if ("association_adjusted" %in% names(a5)) {
  val <- unique(na.omit(as.logical(a5$association_adjusted)))
  if (length(val)>1) stop("Figure5 sources mix adjusted and unadjusted association models")
  if (length(val)) val[[1]] else FALSE
} else FALSE
figure5_hr_prefix <- if (isTRUE(figure5_adjusted_flag)) "Adjusted HR" else "HR"

# a) Risk-oriented continuous score per 1 development-defined SD.
a5 <- a5 %>% filter(outcome_label %in% outcomes) %>%
  mutate(outcome_plot=factor(outcome_label,levels=outcomes_y_levels),
         component=factor(component,levels=modalities))
p5a <- ggplot(a5,aes(HR,outcome_plot,colour=component,shape=component)) +
  geom_vline(xintercept=1,linetype=2,colour="#A3ABB0",linewidth=.5) +
  geom_errorbar(aes(xmin=CI_low,xmax=CI_high),orientation="y",width=.13,
                position=position_dodge(width=.60),na.rm=TRUE) +
  geom_point(position=position_dodge(width=.60),size=2.1,na.rm=TRUE) +
  scale_shape_manual(values=setNames(c(16,17,15,18),modalities),name=NULL) +
  scale_x_log10() + scale_colour_manual(values=modality_col,breaks=modalities,labels=c("E","P","M","G"),drop=FALSE,name=NULL) +
  labs(x=paste0(figure5_hr_prefix," per 1 SD"),y=NULL,
       title="Continuous domain risk scores",tag="a") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE,
         override.aes=list(shape=c(16,17,15,18))),shape="none") +
  theme(legend.position="bottom",plot.tag.position=c(.01,.99),
        legend.text=element_text(face="bold",size=7.0))

# b) All FOUR combinations (including reference 1.00) x each biological domain.
joint_levels5 <- c("Lower risk / Lower risk","Lower risk / Higher risk",
                   "Higher risk / Lower risk","Higher risk / Higher risk")
joint_labs5 <- c("L/L","L/H","H/L","H/H")
b5 <- b5 %>% filter(outcome_label %in% outcomes) %>%
  mutate(logHR=if_else(is.finite(HR) & HR>0,log(HR),NA_real_),
         label=if_else(is.finite(HR),sprintf("%.2f",HR),"NA"),
         omic_label=factor(omic_label,levels=modalities[-1]),
         joint_group=factor(joint_group,levels=joint_levels5,labels=joint_labs5),
         outcome_plot=factor(outcome_label,levels=outcomes_y_levels))
p5b <- ggplot(b5,aes(joint_group,outcome_plot)) +
  geom_tile(aes(fill=logHR),colour="white",linewidth=.4) +
  geom_text(aes(label=label),size=2.1,fontface="bold") +
  facet_grid(~omic_label) +
  figure5b_hr_scale(b5$logHR) +
  labs(x="Environmental / biological risk group",y=NULL,
       title="E × individual biological domains",tag="b") +
  theme(axis.text.x=element_text(face="bold",size=7.5),
        strip.text=element_text(face="bold",size=7.3),legend.position="bottom",
        plot.tag.position=c(.01,.99),panel.spacing.x=grid::unit(3.5,"mm")) +
  tile_theme

# c) Original environment x equal-weighted P/M/G combined scores.
c5 <- c5 %>% filter(outcome_label %in% outcomes) %>%
  mutate(joint_group=factor(joint_group,levels=joint_levels5,
                            labels=c("Lower E / Lower omics","Lower E / Higher omics",
                                     "Higher E / Lower omics","Higher E / Higher omics")),
         outcome_plot=factor(outcome_label,levels=outcomes_y_levels))
joint_color5 <- c("Lower E / Lower omics"="#8D969D",
                  "Lower E / Higher omics"="#6B8EAD",
                  "Higher E / Lower omics"="#B4866E",
                  "Higher E / Higher omics"="#8A4F62")
p5c <- ggplot(c5,aes(HR,outcome_plot,colour=joint_group,shape=joint_group)) +
  geom_vline(xintercept=1,linetype=2,colour="#9BA3A9",linewidth=.40) +
  geom_errorbar(aes(xmin=CI_low,xmax=CI_high),orientation="y",width=.10,
    position=position_dodge(width=.65),na.rm=TRUE) +
  geom_point(position=position_dodge(width=.65),size=2.0,na.rm=TRUE) +
  scale_shape_manual(values=setNames(c(16,17,15,18),names(joint_color5)),name=NULL) +
  scale_x_log10() + scale_colour_manual(values=joint_color5,name=NULL,drop=FALSE) +
  labs(x=paste0(figure5_hr_prefix," vs Lower E / Lower combined omics"),y=NULL,
       title="E × combined omics",tag="c") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE,
         override.aes=list(shape=c(16,17,15,18))),shape="none") +
  theme(legend.position="bottom",plot.tag.position=c(.01,.99),
        legend.text=element_text(face="bold",size=7.0))

# d/e) Both are the SAME Stage 3B CoxEM Weighted_beta, equal-domain-weighted
# integrated score used to define Figure 5 a-c. These are *not* Stage 3 predictions.
# 与全文 E/P/M/G 的低饱和配色统一：Low灰蓝、Intermediate柔和金棕、High砖玫瑰。
# 同时用线型、线宽、点形区分，避免单靠颜色或相近的罕见结局生存曲线。
risk_colors5 <- c(Low="#597D98", Intermediate="#B09A70", High="#B46F77")
risk_linetypes5 <- c(Low="dotted", Intermediate="longdash", High="solid")
s13_km5 <- s13_km5 %>% filter(outcome_label %in% outcomes) %>%
  mutate(outcome_label=factor(outcome_label,levels=outcomes),
         risk_group=factor(risk_group,levels=c("Low","Intermediate","High")))
s13_km_p5 <- s13_km5 %>%
  distinct(outcome_label,logrank_p) %>%
  mutate(p_label=case_when(
    !is.finite(logrank_p) ~ "Log-rank P = NA",
    logrank_p < .001 ~ "Log-rank P < 0.001",
    TRUE ~ sprintf("Log-rank P = %.3f",logrank_p)))
# Do not force y = 0..1; outcome-specific y-axis zoom clarifies small
# differences while preserving the underlying survival estimates.
p5d <- ggplot(s13_km5,aes(time_years,survival,colour=risk_group,
                            linetype=risk_group)) +
  geom_step(linewidth=1.04,alpha=.98) +
  geom_label(data=s13_km_p5,
    aes(x=.35,y=Inf,label=p_label),inherit.aes=FALSE,
    hjust=0,vjust=1.2,label.size=0,size=2.25,fontface="bold",
    fill="white",colour="#26313A",label.padding=grid::unit(.8,"mm"),
    show.legend=FALSE) +
  facet_wrap(~outcome_label,ncol=3,scales="free_y") +
  coord_cartesian(xlim=c(0,15)) +
  scale_y_continuous(labels=label_number(accuracy=.01),
    expand=expansion(mult=c(.06,.16))) +
  scale_x_continuous(breaks=c(0,5,10,15)) +
  scale_colour_manual(values=risk_colors5,name="Risk group") +
  scale_linetype_manual(values=risk_linetypes5,name="Risk group") +
  guides(colour=guide_legend(nrow=1,override.aes=list(
           linetype=unname(risk_linetypes5),linewidth=1.4)),linetype="none") +
  labs(x="Follow-up time (years)",y="Event-free survival",
       title="Epidemiologic risk-score strata: survival",tag="d") +
  theme(legend.position="bottom",strip.text=element_text(face="bold",size=7.9),
        axis.text=element_text(size=6.8),axis.title=element_text(size=8.0),
        panel.spacing=grid::unit(3.5,"mm"),plot.tag.position=c(.01,.99))
s13_hr5 <- s13_hr5 %>% filter(outcome_label %in% outcomes,
                                risk_group %in% c("Intermediate","High")) %>%
  mutate(outcome_plot=factor(outcome_label,levels=outcomes_y_levels),
         risk_group=factor(risk_group,levels=c("Intermediate","High")))
p5e <- ggplot(s13_hr5,aes(HR,outcome_plot,colour=risk_group,shape=risk_group)) +
  geom_vline(xintercept=1,linetype=2,colour="#97A3AD",linewidth=.45) +
  geom_errorbar(aes(xmin=CI_low,xmax=CI_high),orientation="y",width=.13,
    position=position_dodge(width=.55),na.rm=TRUE) +
  geom_point(position=position_dodge(width=.55),size=2.3,na.rm=TRUE) +
  scale_shape_manual(values=c(Intermediate=15,High=17),name=NULL) +
  scale_x_log10() + scale_colour_manual(values=risk_colors5[c("Intermediate","High")],name=NULL) +
  labs(x="Hazard ratio vs Low integrated risk (95% CI)",y=NULL,
       title="Epidemiologic risk-score strata: associations",tag="e") +
  guides(colour=guide_legend(override.aes=list(shape=c(15,17),size=3.0)),shape="none") +
  theme(legend.position="bottom",plot.tag.position=c(.01,.99))

figure5_plot <- ((p5a | p5b) / (p5c | p5d) / p5e) +
  plot_layout(widths=c(1.10,1.24),heights=c(1.00,1.40,.83))
save_figure("Figure5",figure5_plot,315,400)

# =============================================================================
# Supplementary Figures S1–S4, S5a–d, S6–S7####
# Trimmed inventory: old S6–S8/S11–S13 moved to main figures; S9→S6, S10→S7.
#   1) no internal plot titles/subtitles;
#   2) all visible axis text/titles are bold;
#   3) domain display is E -> P -> M -> G (definitions in captions);
#   4) outcome order is Overall -> P1 ... P8;
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
# Order: E -> P -> M -> G (full names belong in caption)
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
# Figure S4: Overall only, 40 selected-feature Stage 2B held-out associations####
# 07 chooses E12 / P10 / M10 / G8 by development Stage 1 importance.
# Full nine-outcome HR information remains in Table S20, not in this figure.
# -----------------------------------------------------------------------------
s4 <- read_src("FigureS4_nine_outcomes_stage2B_associations.csv") %>%
  filter(outcome_label == "Overall") %>%
  arrange(desc(abs_signed_importance),selected_rank,modality_label)
if (!nrow(s4) || n_distinct(s4$outcome_label)!=1L) {
  stop("Figure S4 must contain Overall only; rerun 07 Section 5.")
}
# Reject stale 20-feature figure sources before drawing. Check per-domain quota.
s4_target_counts <- c(E=12L, P=10L, M=10L, G=8L)
s4_actual_counts <- table(factor(as.character(s4$modality_label), levels=modalities))
if (nrow(s4) != sum(s4_target_counts) ||
    !identical(as.integer(s4_actual_counts), as.integer(s4_target_counts))) {
  stop("Figure S4 expected 40 features (E12/P10/M10/G8), got ",
       paste(names(s4_actual_counts), as.integer(s4_actual_counts), collapse="; "),
       ". Run modified 07 Section 5 before 08.")
}
s4 <- s4 %>%
  mutate(
    modality_label=factor(modality_label,levels=modalities),
    feature=factor(paste(modality,model_variable,sep="||"),
      levels=rev(unique(paste(modality,model_variable,sep="||"))))
  )
s4_names <- setNames(s4$display_name,as.character(s4$feature))
s4_hr_range <- range(c(s4$CI_low,s4$CI_high),finite=TRUE)
if (!all(is.finite(s4_hr_range)) || s4_hr_range[1] <= 0) {
  stop("Figure S4 HR/CI range invalid: check Stage2B Cox source.")
}
s4_breaks <- scales::log_breaks(n=6)(s4_hr_range)
p <- ggplot(s4,aes(x=HR,y=feature,colour=modality_label)) +
  geom_vline(xintercept=1,linetype=2,colour="#84939E",linewidth=.58) +
  geom_errorbar(aes(xmin=CI_low,xmax=CI_high),orientation="y",width=.16,
                linewidth=.56,na.rm=TRUE) +
  geom_point(size=2.25,na.rm=TRUE) +
  scale_x_log10(breaks=s4_breaks,labels=label_number(accuracy=.01),
                expand=expansion(mult=c(.04,.08))) +
  scale_y_discrete(labels=function(x) unname(s4_names[x])) +
  scale_colour_manual(values=modality_col,limits=modalities,drop=FALSE,name=NULL) +
  labs(x="Adjusted held-out HR (95% CI)",y=NULL,title=NULL,subtitle=NULL) +
  supp_theme +
  theme(legend.position="bottom",
        axis.text.y=element_text(face="bold",size=7.0),
        axis.text.x=element_text(face="bold",size=8.2),
        plot.margin=margin(7,8,7,9))
# Taller portrait figure prevents 40 labels and confidence intervals from crowding.
s4_height_mm <- max(225, min(310, 70 + 4.4*nrow(s4)))
save_figure("FigureS4",p,250,s4_height_mm,"supplementary")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Figure S5a–d: complete P1–P8 Stage 2A selection matrix, split by domain####
# a = E; b = P (Python key B); c = M; d = G.
# Preserve ALL selected predictors in each domain (Table S21 uses same source).
# Left: predictor names; middle: P1–P8 membership; right: Shared / Multi-pattern / Specific.
# -----------------------------------------------------------------------------
s5 <- read_src("FigureS5_exact_figure3_membership.csv") %>%
  filter(outcome_label %in% pattern_levels) %>%
  mutate(
    outcome_label=factor(outcome_label,levels=pattern_levels),
    modality_label=factor(modality_label,levels=modalities),
    sharedness=factor(sharedness,levels=c("Shared","Multi-pattern","Pattern-specific")),
    feature_key=paste(modality,model_variable,sep="||")
  )
if (!"feature_order_all" %in% names(s5)) {
  stop("Figure S5a–d requires feature_order_all; run 07 Section 8.")
}
s5_expected_domains <- c(E="E", P="P", M="M", G="G")
if (n_distinct(s5$modality_label) != 4L) stop("Figure S5a–d lacks one or more domains")
s5_export_qa <- list()
for (idx in seq_along(s5_expected_domains)) {
  domain <- unname(s5_expected_domains[[idx]])
  panel_id <- paste0("FigureS5",letters[[idx]])
  one <- s5 %>% filter(as.character(modality_label)==domain)
  ann <- one %>%
    distinct(feature_order_all,feature_key,display_name,sharedness) %>%
    arrange(feature_order_all)
  if (!nrow(ann) || nrow(one) != 8L*nrow(ann)) {
    stop(panel_id,": incomplete membership matrix. Check source and Table S21.")
  }
  levels_here <- rev(ann$feature_key)
  one <- one %>% mutate(feature=factor(feature_key,levels=levels_here))
  ann <- ann %>% mutate(feature=factor(feature_key,levels=levels_here))
  nfeat <- nrow(ann)
  # Higher resolution/sufficient page height, maintaining reasonable label size.
  fig_height <- max(170,min(580,55 + 2.85*nfeat))
  fig_width <- if (domain=="M") 300 else 265
  label_size <- if (nfeat>150) 2.20 else if (nfeat>90) 2.55 else 2.85

  pnames <- ggplot(ann,aes(x=1,y=feature,label=display_name)) +
    geom_text(hjust=1,fontface="bold",size=label_size,
              colour="#26313A",lineheight=.95) +
    scale_x_continuous(limits=c(0,1.01),expand=c(0,0)) +
    coord_cartesian(clip="off") + theme_void() +
    labs(title=paste0(letters[[idx]], ".  ",domain)) +
    theme(plot.title=element_text(face="bold",size=10.0,colour="#26313A"),
          plot.margin=margin(8,2,3,15))
  pmatrix <- ggplot(one,aes(x=outcome_label,y=feature)) +
    geom_tile(aes(fill=selected_stage2a),colour="white",linewidth=.12) +
    scale_fill_manual(values=c("FALSE"="#E6E9EC","TRUE"="#557F9F"),
                      limits=c("FALSE","TRUE"),drop=FALSE,name="Selected") +
    scale_x_discrete(position="top",drop=FALSE) +
    labs(x=NULL,y=NULL) +
    tile_theme + supp_theme +
    theme(axis.text.y=element_blank(),axis.text.x=element_text(size=9.2,face="bold"),
          axis.ticks.x=element_blank(),legend.position="bottom",
          plot.margin=margin(8,3,3,3))
  ptype <- ggplot(ann,aes(x=1,y=feature,fill=sharedness)) +
    geom_tile(width=.90,colour="white",linewidth=.12) +
    scale_fill_manual(values=sharedness_col,
      limits=c("Shared","Multi-pattern","Pattern-specific"),
      drop=FALSE,name="Feature type") +
    theme_void() + theme(legend.position="none",plot.margin=margin(8,4,3,2))
  # Pull a small common Selected legend and a three-class feature-type legend.
  selected_legend <- cowplot::get_legend(
    pmatrix + theme(legend.position="bottom") +
      guides(fill=guide_legend(direction="horizontal",nrow=1,
        keywidth=grid::unit(5,"mm"),keyheight=grid::unit(3.8,"mm"))))
  type_legend <- cowplot::get_legend(
    ptype + theme(legend.position="bottom") +
      guides(fill=guide_legend(direction="horizontal",nrow=1,
        keywidth=grid::unit(5,"mm"),keyheight=grid::unit(3.8,"mm"))))
  body <- (pnames | (pmatrix+theme(legend.position="none")) | ptype) +
    plot_layout(widths=c(3.0,1.8,.15))
  legends <- cowplot::plot_grid(selected_legend,type_legend,
                               nrow=1,rel_widths=c(.80,1.35),align="h")
  pp <- body / wrap_elements(full=legends) + plot_layout(heights=c(1,.085))
  save_figure(panel_id,pp,fig_width,fig_height,"supplementary")
  s5_export_qa[[panel_id]] <- tibble(
    figure=panel_id,domain=domain,n_unique_features=nfeat,
    n_rows=nrow(one),expected_rows=8L*nfeat,
    passed=nrow(one)==8L*nfeat
  )
}
s5_qa <- bind_rows(s5_export_qa)
fwrite(as.data.table(s5_qa),file.path(log_dir,"FigureS5a_d_domain_split_QA.csv"),bom=TRUE)
if (!all(s5_qa$passed)) stop("Figure S5a–d membership QA failed")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Former Figure S6–S8 plots were removed because they are included in main Figure 4a/b/d.
# Their source files keep their historical names; main Figure 4 reads them directly.

# -----------------------------------------------------------------------------
# Figure S6 (a/b): 5-year and 10-year calibration (former S9)####
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

# 最终 Figure S6：a = 5 年校准；b = 10 年校准，合并为同一张补充图。
pS6a <- cal_plot(read_src("FigureS9_5year_calibration.csv"), 5) +
  labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 12))
pS6b <- cal_plot(read_src("FigureS9_10year_calibration.csv"), 10) +
  labs(tag = "b") + theme(plot.tag = element_text(face = "bold", size = 12))
figureS6_plot <- pS6a / pS6b + plot_layout(heights = c(1, 1))
save_figure("FigureS6", figureS6_plot, 220, 360, "supplementary")

# -----------------------------------------------------------------------------
# Figure S7: leading final integrated predictors (former S10).####
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
save_figure("FigureS7", p, 205, 330, "supplementary")

# -----------------------------------------------------------------------------
# Former S11 = main Figure 4f; former S12 = main Figure 5b; former S13 = main Figure 5d/e.
# Duplicate supplementary exports are deliberately removed. Sources stay in 07.

# =============================================================================
# Final export/order QA
# =============================================================================
qa <- bind_rows(export_rows) %>% mutate(passed = all_files_exist & all_nonempty)
fwrite(as.data.table(qa), file.path(log_dir, "figure_export_QA.csv"), bom = TRUE)
if (!all(qa$passed)) {
  stop("Some figure exports failed QA; inspect figure_export_QA.csv")
}
figure_order_qa <- tibble(
  check=c("main Figure2-5 + S1-S4, S5a-d, S6-S7 in exact order",
          "14 figure groups exported"),
  passed=c(identical(qa$figure,expected_figures),nrow(qa)==14L)
)
fwrite(as.data.table(figure_order_qa),
       file.path(log_dir,"figure_order_QA.csv"),bom=TRUE)
if (!all(figure_order_qa$passed)) {
  stop("Figure order QA failed; inspect figure_order_QA.csv. Rerun all 08 figure blocks.")
}
# Only after ALL replacement figures pass QA, remove old redundant names.
# S6/S7 were overwritten above with calibration and final predictors, respectively.
retired_ids <- c("FigureS5",paste0("FigureS",8:14))
retired_paths <- unlist(lapply(retired_ids,function(id)
  file.path(supp_dir,paste0(id,c(".pdf",".tiff",".png",".svg")))))
if (any(file.exists(retired_paths))) unlink(retired_paths[file.exists(retired_paths)])
fwrite(as.data.table(tibble(
  supplementary_label=c(paste0("FigureS",1:4),paste0("FigureS5",letters[1:4]),
                        "FigureS6","FigureS7"),
  content=c("Stage 1 importance","Stage 2A CV","Stage 2A selected importance",
            "Overall associations","E selected features","P selected features",
            "M selected features","G selected features","5y/10y calibration",
            "Final integrated predictor importance")
)),file.path(log_dir,"supplementary_figure_inventory.csv"),bom=TRUE)
record_figure_checkpoint("FigureS7",complete=TRUE)
cat("Final exports: Figure 2–5, S1–S4, S5a–d, S6 (calibration), S7 (predictors). Redundant S6–S8/S11–S13 contents removed.\n")
