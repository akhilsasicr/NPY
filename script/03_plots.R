# ============================================================
# NPY CUA — ALL PLOTS (primary model)
# ============================================================
# Reads the tables/RDS 02_model.R saves and produces every plot —
# 02_model.R itself makes no plots.
#
# WHAT'S IN HERE
#   1. Primary CE-plane (combined + incremental) — the core PSA
#      scatter plots.
#   2. Faceted versions of both — same data, one panel per strategy,
#      since several strategies sit close together in cost-QALY space
#      and overlap in the combined view.
#   3. Primary CEAC (each strategy vs No NPY).
#   4. CEAC vs Current NPY — 01_helpers.R's make_ceac_plot() only ever
#      compares each strategy to No NPY; make_ceac_plot_vs_current()
#      answers "is Ideal actually worth it over Current" directly for
#      Realistic Impr. and Ideal Impr.
#   5. A TRUE multi-strategy CEAC — probability each of the FOUR
#      strategies (No NPY included) has the single highest NMB, via
#      make_multi_ceac_plot(). This is the only CEAC where No NPY
#      appears as its own curve.
#   6. PSA convergence diagnostic plot (check_psa_convergence()).
#   7. Tornado chart (OWSA).
#   8. NPY coverage scale-up plot.
#
# INPUT: <out_root>/rds/model_env.rds, <out_root>/psa/PSA_raw_results.xlsx,
#        <out_root>/psa/OWSA_NMB_results.xlsx,
#        <out_root>/tables/coverage_scaleup_data.xlsx — all written by
#        02_model.R (out_root is output/).
# OUTPUT: every .png in <out_root>/plots/, plus a few small derived
#         .xlsx tables (CEAC curve data) in <out_root>/tables/.
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(scales)
library(here)

root <- here::here()
source(file.path(root, "script", "00_config.R"))
source(file.path(root, "script", "01_helpers.R"))

# A single output/ folder is used for every run (no more pct20/source
# mode split).
out_root <- file.path(root, "output")
rds_path <- file.path(out_root, "rds", "model_env.rds")
if (!file.exists(rds_path)) stop(rds_path, " not found — run 02_model.R first.")
env <- readRDS(rds_path)
invisible(list2env(env, envir = environment()))

psa_file <- file.path(out_root, "psa", "PSA_raw_results.xlsx")
if (!file.exists(psa_file)) stop(psa_file, " not found — run 02_model.R first.")
psa_raw <- read_excel(psa_file)

wtp_range <- seq(0, ceiling(wtp * 2), by = 1)

cat("Loaded PSA draws (n=", nrow(psa_raw), ") and paths from model_env.rds\n", sep = "")

# ============================================================
# SECTION 1 — PRIMARY CE-PLANE (combined)
# ============================================================

# Uses nrow(psa_raw), not the config's n_psa: psa_raw is the actual
# loaded PSA file, which is the authoritative row count. Using n_psa
# instead would silently misalign this factor if the loaded file's
# row count differs from whatever 00_config.R currently says (e.g.
# stale output, or config edited after the PSA was run).
psa_ce_df <- data.frame(
  Cost     = c(psa_raw$cost_nonpy, psa_raw$cost_soc, psa_raw$cost_ri, psa_raw$cost_ii),
  Effect   = c(psa_raw$qaly_nonpy, psa_raw$qaly_soc, psa_raw$qaly_ri, psa_raw$qaly_ii),
  Strategy = factor(rep(c("No NPY", "Current NPY", "Realistic Impr.", "Ideal Impr."), each = nrow(psa_raw)),
                    levels = c("No NPY", "Current NPY", "Realistic Impr.", "Ideal Impr."))
)
centroids_ce <- psa_ce_df %>%
  group_by(Strategy) %>%
  summarise(Cost_mean = mean(Cost), Effect_mean = mean(Effect),
            Cost_lo = quantile(Cost, 0.025), Cost_hi = quantile(Cost, 0.975),
            Effect_lo = quantile(Effect, 0.025), Effect_hi = quantile(Effect, 0.975),
            .groups = "drop")

ce_psa_plot <- ggplot(psa_ce_df, aes(x = Effect, y = Cost, colour = Strategy)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_errorbar(data = centroids_ce, aes(x = Effect_mean, ymin = Cost_lo, ymax = Cost_hi),
                width = 0, linewidth = 0.7, linetype = "dotted", inherit.aes = FALSE, colour = "black") +
  geom_segment(data = centroids_ce, aes(x = Effect_lo, xend = Effect_hi, y = Cost_mean, yend = Cost_mean),
               linewidth = 0.7, linetype = "dotted", inherit.aes = FALSE, colour = "black") +
  geom_point(data = centroids_ce, aes(x = Effect_mean, y = Cost_mean, fill = Strategy),
             size = 2.2, shape = 23, colour = "black", stroke = 0.6, inherit.aes = FALSE) +
  scale_colour_manual(values = c("No NPY" = "#E41A1C", "Current NPY" = "#377EB8",
                                 "Realistic Impr." = "#4DAF4A", "Ideal Impr." = "#984EA3")) +
  scale_fill_manual(values = c("No NPY" = "#E41A1C", "Current NPY" = "#377EB8",
                                "Realistic Impr." = "#4DAF4A", "Ideal Impr." = "#984EA3")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)), fill = "none") +
  labs(title = paste0("PSA Cost-Effectiveness Plane (n=", n_psa, ")"),
       subtitle = paste0("Absolute cost vs. absolute QALYs, all four strategies\n",
                          "Centroids and 95% CrIs | Year-1 horizon"),
       x = "Effectiveness (QALYs)", y = "Cost (USD)", colour = "Strategy",
       caption = paste0("Diamond = PSA mean. Dotted whiskers = 95% credible\n",
                         "interval (vertical: cost; horizontal: effectiveness).")) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8.5, colour = "grey30"))
ggsave(file.path(out_plots, "psa_ce_plane.png"), ce_psa_plot, width = 8, height = 6, dpi = 150)
cat("OK - CE plane saved\n")

# ── Incremental CE Plane (vs No NPY) ──────────────────────────
# The more conventional of the two CE planes for a manuscript: shows
# INCREMENTAL cost vs INCREMENTAL QALY against the No NPY comparator
# (which sits at the origin), with the WTP threshold as a diagonal
# line — so a strategy's position relative to that line directly
# shows whether it is cost-effective. The plot above (total cost vs
# total QALY) is descriptive only and has no threshold line.
psa_incr <- data.frame(
  d_qaly   = c(psa_raw$d_qaly_soc, psa_raw$d_qaly_ri, psa_raw$d_qaly_ii),
  d_cost   = c(psa_raw$d_cost_soc, psa_raw$d_cost_ri, psa_raw$d_cost_ii),
  Strategy = rep(c("Current NPY", "Realistic Impr.", "Ideal Impr."), each = nrow(psa_raw))
) %>% mutate(Strategy = factor(Strategy,
                               levels = c("Current NPY", "Realistic Impr.", "Ideal Impr.")))

centroids_incr <- psa_incr %>%
  group_by(Strategy) %>%
  summarise(d_qaly = mean(d_qaly), d_cost = mean(d_cost), .groups = "drop")

ce_psa_incr_plot <- ggplot(psa_incr, aes(x = d_qaly, y = d_cost, colour = Strategy)) +
  geom_point(alpha = 0.15, size = 0.6) +
  suppressWarnings(stat_ellipse(level = 0.95, linewidth = 0.8, linetype = "dotted", na.rm = TRUE)) +
  geom_point(data = centroids_incr, size = 2.2, shape = 18, colour = "black") +
  geom_point(data = centroids_incr, size = 1.5, shape = 18) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_abline(slope = wtp, intercept = 0, linetype = "dotted",
              colour = "red", linewidth = 0.8) +
  annotate("point", x = 0, y = 0, size = 3, shape = 4, colour = "black") +
  annotate("text", x = 0.005, y = min(psa_incr$d_cost, na.rm = TRUE) * 0.85,
           label = "No NPY (origin)", size = 3, hjust = 0) +
  annotate("text",
           x = quantile(psa_incr$d_qaly, 0.98, na.rm = TRUE) * 0.4,
           y = quantile(psa_incr$d_cost, 0.85, na.rm = TRUE),
           label = paste0("WTP = ", wtp_label, "/QALY"),
           colour = "red", size = 3, hjust = 0) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(title    = "Incremental Cost-Effectiveness Plane (vs No NPY)",
       subtitle = paste0("PSA scatter with 95% ellipses | n=", n_psa, " | Year-1 horizon"),
       x = "Incremental QALY vs No NPY",
       y = "Incremental Cost (USD) vs No NPY",
       colour = "Strategy",
       caption = paste0("Diamond = PSA mean. X = No NPY (reference, always at origin).\n",
                         "Dotted ellipse = 95% credible region.")) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8.5, colour = "grey30"))
ggsave(file.path(out_plots, "psa_ce_plane_incremental.png"),
       ce_psa_incr_plot, width = 8, height = 6, dpi = 150)
cat("OK - Incremental CE plane saved\n")

# ============================================================
# SECTION 2 — FACETED CE-PLANE PLOTS
# ============================================================
# Same data as Section 1, one panel per strategy, identical axis
# scales — additional to the combined plots above, not a replacement.

ce_psa_plot_facet <- ce_psa_plot +
  facet_wrap(~Strategy, nrow = 1) +
  guides(colour = "none", fill = "none") +
  labs(title = paste0("PSA Cost-Effectiveness Plane (n=", n_psa, ") — by strategy"),
       subtitle = "Same data as psa_ce_plane.png, one panel per strategy") +
  theme(legend.position = "none")
ggsave(file.path(out_plots, "psa_ce_plane_facet.png"), ce_psa_plot_facet, width = 12, height = 4, dpi = 150)
cat("OK - CE plane (faceted) saved\n")

# Note: the combined incremental plot above annotates "No NPY
# (origin)" and "WTP = $..." at fixed coordinates, which only makes
# sense in a single panel. Those two text labels are dropped here
# (they would either overlap or misplace across facets); the origin
# point, WTP line, and per-panel ellipse are enough context once each
# strategy has its own panel.
ce_psa_incr_plot_facet <- ggplot(psa_incr, aes(x = d_qaly, y = d_cost, colour = Strategy)) +
  geom_point(alpha = 0.15, size = 0.6) +
  suppressWarnings(stat_ellipse(level = 0.95, linewidth = 0.8, linetype = "dotted", na.rm = TRUE)) +
  geom_point(data = centroids_incr, size = 2.2, shape = 18, colour = "black") +
  geom_point(data = centroids_incr, size = 1.5, shape = 18) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_abline(slope = wtp, intercept = 0, linetype = "dotted", colour = "red", linewidth = 0.8) +
  annotate("point", x = 0, y = 0, size = 3, shape = 4, colour = "black") +
  scale_colour_manual(values = c("Current NPY" = "#377EB8",
                                 "Realistic Impr." = "#4DAF4A", "Ideal Impr." = "#984EA3")) +
  facet_wrap(~Strategy, nrow = 1) +
  guides(colour = "none") +
  labs(title    = "Incremental CE Plane (vs No NPY) — by strategy",
       subtitle = paste0("Dotted line = WTP ", wtp_label, "/QALY | X = No NPY (origin) | n=", n_psa),
       x = "Incremental QALY vs No NPY", y = "Incremental Cost (USD) vs No NPY",
       caption = "Diamond = PSA mean. Dotted ellipse = 95% credible region.") +
  theme_bw(base_size = 12) + theme(plot.caption = element_text(hjust = 0, size = 8.5, colour = "grey30"))
ggsave(file.path(out_plots, "psa_ce_plane_incremental_facet.png"),
       ce_psa_incr_plot_facet, width = 12, height = 4, dpi = 150)
cat("OK - Incremental CE plane (faceted) saved\n")

# ============================================================
# SECTION 3 — PRIMARY CEAC (each strategy vs No NPY)
# ============================================================

make_ceac_plot(psa_raw, wtp, wtp_range, "Primary model",
               png_path  = file.path(out_plots, "ceac.png"),
               xlsx_path = file.path(out_tables, "ceac_results.xlsx"))
cat("OK - CEAC saved\n")

# ============================================================
# SECTION 4 — CEAC vs CURRENT NPY (Realistic/Ideal only)
# ============================================================

make_ceac_plot_vs_current(
  psa_raw, wtp, wtp_range, "Primary model",
  png_path  = file.path(out_plots, "ceac_vs_current.png"),
  xlsx_path = file.path(out_tables, "ceac_vs_current_results.xlsx")
)
cat("OK - CEAC vs Current NPY saved\n")

# ============================================================
# SECTION 5 — TRUE MULTI-STRATEGY CEAC (all four strategies)
# ============================================================

make_multi_ceac_plot(
  psa_raw, wtp_range, "Primary model",
  png_path  = file.path(out_plots, "multi_strategy_ceac.png"),
  xlsx_path = file.path(out_tables, "multi_strategy_ceac_results.xlsx")
)
cat("OK - Multi-strategy CEAC saved\n")

# ============================================================
# SECTION 6 — PSA CONVERGENCE DIAGNOSTIC
# ============================================================
# Checks whether n_psa iterations were enough for the reported NMB
# means to be numerically stable (see check_psa_convergence() in
# 01_helpers.R for the full explanation of what/why/how). psa_raw
# does not have nmb_soc/nmb_ri/nmb_ii columns as read back from Excel
# with those exact names guaranteed — recomputed here from the raw
# cost/QALY columns to be safe.
psa_raw <- psa_raw %>%
  mutate(nmb_soc = wtp * (qaly_soc - qaly_nonpy) - (cost_soc - cost_nonpy),
         nmb_ri  = wtp * (qaly_ri  - qaly_nonpy) - (cost_ri  - cost_nonpy),
         nmb_ii  = wtp * (qaly_ii  - qaly_nonpy) - (cost_ii  - cost_nonpy))
check_psa_convergence(
  psa_raw,
  nmb_cols        = c("nmb_soc", "nmb_ri", "nmb_ii"),
  strategy_labels = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
  title_suffix    = "Primary model",
  png_path        = file.path(out_plots, "psa_convergence.png"),
  xlsx_path       = file.path(out_psa, "PSA_convergence_summary.xlsx")
)
cat("OK - PSA convergence diagnostic saved\n")

# ============================================================
# SECTION 7 — TORNADO CHART (OWSA)
# ============================================================
owsa_file <- file.path(out_psa, "OWSA_NMB_results.xlsx")
if (!file.exists(owsa_file)) stop(owsa_file, " not found — run 02_model.R first.")
owsa_results <- read_excel(owsa_file)
nmb_base_ri  <- wtp * (br$qaly_ri - br$qaly_nonpy) - (br$cost_ri - br$cost_nonpy)

tornado_data <- owsa_results %>% slice_head(n = 15) %>%
  mutate(Parameter = factor(Parameter, levels = rev(Parameter)))

tornado_plot <- ggplot(tornado_data) +
  geom_segment(aes(x = NMB_low, xend = NMB_high, y = Parameter, yend = Parameter),
               colour = "#2E75B6", linewidth = 5, alpha = 0.75) +
  geom_vline(xintercept = nmb_base_ri, linetype = "dashed", colour = "red") +
  labs(title = "Tornado Chart - OWSA",
       subtitle = paste0("NMB: Realistic Impr. vs No NPY | WTP=", wtp_label, "/QALY"),
       x = "Incremental NMB (USD)", y = NULL) +
  theme_bw(base_size = 11)
ggsave(file.path(out_plots, "tornado_NMB.png"), tornado_plot, width = 9, height = 6, dpi = 150)
cat("OK - Tornado saved\n")

# ============================================================
# SECTION 8 — NPY COVERAGE SCALE-UP PLOT
# ============================================================
coverage_file <- file.path(out_tables, "coverage_scaleup_data.xlsx")
if (!file.exists(coverage_file)) stop(coverage_file, " not found — run 02_model.R first.")
coverage_results <- read_excel(coverage_file)

p_current   <- as.numeric(base[["p_soc_timely"]])
p_ri_val    <- as.numeric(base[["p_ri_timely"]])
nmb_current <- coverage_results$nmb[which.min(abs(coverage_results$p_timely - p_current))]
nmb_ri_val  <- coverage_results$nmb[which.min(abs(coverage_results$p_timely - p_ri_val))]
nmb_ii_val  <- coverage_results$nmb[which.min(abs(coverage_results$p_timely - 1.0))]
y_min <- floor(min(coverage_results$nmb)   * 0.98)
y_max <- ceiling(max(coverage_results$nmb) * 1.02)

coverage_plot <- ggplot(coverage_results, aes(x = p_timely, y = nmb)) +
  geom_line(linewidth = 1, colour = "#2E75B6") +
  geom_vline(xintercept = p_ri_val, linetype = "dotted", colour = "orange") +
  geom_vline(xintercept = 1.0,      linetype = "dotted", colour = "red") +
  annotate("point", x = p_current, y = nmb_current, colour = "darkgreen", size = 3) +
  annotate("point", x = p_ri_val,  y = nmb_ri_val,  colour = "orange",    size = 3) +
  annotate("point", x = 1.0,       y = nmb_ii_val,  colour = "red",       size = 3) +
  annotate("text", x = p_current, y = nmb_current - (y_max - y_min) * 0.05,
           label = paste0("Current\n($", round(nmb_current), ", ", round(p_current*100,1), "%)"),
           colour = "darkgreen", size = 3, hjust = -0.1) +
  annotate("text", x = p_ri_val,  y = nmb_ri_val - (y_max - y_min) * 0.05,
           label = paste0("Realistic Impr.\n($", round(nmb_ri_val), ", ", round(p_ri_val*100,1), "%)"),
           colour = "orange", size = 3, hjust = -0.1) +
  annotate("text", x = 1.0, y = nmb_ii_val - (y_max - y_min) * 0.05,
           label = paste0("Ideal\n($", round(nmb_ii_val), ", 100%)"),
           colour = "red", size = 3, hjust = 1.1) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(ceiling(p_current * 10) / 10, 1, by = 0.1)) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = pretty(c(y_min, y_max), n = 8)) +
  labs(title = "NPY Scale-Up Plot",
       subtitle = paste0("NMB vs No NPY as timely receipt increases | WTP=", wtp_label, "/QALY"),
       x = "Proportion receiving NPY within 30 days",
       y = "Net Monetary Benefit (USD) vs No NPY") +
  theme_bw(base_size = 12)
ggsave(file.path(out_plots, "coverage_scaleup.png"), coverage_plot, width = 9, height = 6, dpi = 150)
cat("OK - Coverage scale-up plot saved\n")

# ============================================================
# SECTION 9 — PAIRED PSA NOISE TEST (Ideal vs Realistic Impr.)
# ============================================================
# Ideal and Realistic Impr. NMB estimates sit close together (KS's
# manuscript comment: "NMB estimates were tightly clustered"), and
# geometrically Realistic Impr. sits on the line between Current NPY
# and Ideal Impr. in cost-QALY space (extended dominance — see
# REVIEW_RESPONSE.md item 9), so a small gap between the two is
# expected, not a red flag on its own.
#
# This checks it directly rather than relying on that geometric
# argument alone: since psa_raw is PAIRED (each row is one PSA draw,
# with every strategy's cost/QALY computed from that SAME draw), the
# within-iteration difference nmb_ii - nmb_ri isolates the actual gap
# between the two strategies from the shared parameter-uncertainty
# noise both inherit from the same draw. Comparing the two strategies'
# separate marginal NMB distributions (e.g. two overlapping histograms)
# would NOT do this — much of their spread is common to both and
# cancels out only when subtracted within-iteration, not across two
# independently-summarised distributions.
diff_ii_ri <- psa_raw$nmb_ii - psa_raw$nmb_ri
diff_mean  <- mean(diff_ii_ri)
diff_se    <- sd(diff_ii_ri) / sqrt(length(diff_ii_ri))
diff_ci    <- quantile(diff_ii_ri, c(0.025, 0.975))
pct_positive <- mean(diff_ii_ri > 0) * 100

paired_noise_summary <- data.frame(
  Comparison       = "Ideal Impr. minus Realistic Impr. (NMB, paired within PSA iteration)",
  n_psa            = length(diff_ii_ri),
  Mean_diff        = diff_mean,
  SE               = diff_se,
  CrI_2.5pct       = diff_ci[[1]],
  CrI_97.5pct      = diff_ci[[2]],
  CrI_excludes_zero = (diff_ci[[1]] > 0) || (diff_ci[[2]] < 0),
  Pct_iterations_favouring_Ideal = pct_positive
)
write_xlsx(paired_noise_summary, file.path(out_tables, "paired_psa_noise_test_ii_vs_ri.xlsx"))

noise_lines <- data.frame(
  xintercept = c(0, diff_mean, diff_ci[[1]], diff_ci[[2]]),
  Line       = factor(c("Zero", "Mean difference", "95% CrI bound", "95% CrI bound"),
                      levels = c("Zero", "Mean difference", "95% CrI bound"))
)
# Zero and Mean difference are styled to stay visually distinct even
# when their x-positions sit only a few dollars apart (Zero:
# thin/grey/dashed vs Mean: thicker/red/solid) — mirrors the actual
# finding (mean is close to zero) rather than exaggerating the gap,
# which would misrepresent the data.
paired_noise_plot <- ggplot(data.frame(diff = diff_ii_ri), aes(x = diff)) +
  geom_histogram(bins = 60, fill = "#2E75B6", colour = NA, alpha = 0.8) +
  geom_vline(data = noise_lines, aes(xintercept = xintercept, colour = Line,
                                      linetype = Line, linewidth = Line)) +
  scale_colour_manual(values = c("Zero" = "grey30", "Mean difference" = "red", "95% CrI bound" = "red")) +
  scale_linetype_manual(values = c("Zero" = "dashed", "Mean difference" = "solid", "95% CrI bound" = "22")) +
  scale_linewidth_manual(values = c("Zero" = 0.5, "Mean difference" = 1.0, "95% CrI bound" = 0.7)) +
  labs(title = "Paired PSA Noise Test — Ideal Impr. vs Realistic Impr.",
       subtitle = paste0("Within-iteration NMB difference (n=", n_psa, ")\n",
                          "Mean = $", round(diff_mean, 2), " | 95% CrI [$",
                          round(diff_ci[[1]], 2), ", $", round(diff_ci[[2]], 2), "]"),
       x = "NMB(Ideal Impr.) - NMB(Realistic Impr.), per PSA iteration (USD)",
       y = "PSA iterations", colour = "Reference line", linetype = "Reference line",
       linewidth = "Reference line") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", plot.margin = margin(t = 5.5, r = 5.5, b = 22, l = 5.5))

# Label the mean-difference line with its actual dollar value, placed
# below the x-axis so it stays legible even when the line sits right
# next to the Zero line (their x-positions can be only a few dollars
# apart, per the finding itself) — only drawn when the mean is
# actually non-zero (in practice always true for a continuous PSA
# mean, but guarded explicitly rather than assumed).
if (diff_mean != 0) {
  paired_noise_plot <- paired_noise_plot +
    annotate("text", x = diff_mean, y = -Inf, vjust = 2.4, hjust = 0.5,
             label = paste0("$", round(diff_mean, 2)),
             colour = "red", size = 3.2, fontface = "bold") +
    coord_cartesian(clip = "off")
}

ggsave(file.path(out_plots, "paired_psa_noise_test_ii_vs_ri.png"), paired_noise_plot, width = 8, height = 6, dpi = 150)

cat("OK - Paired PSA noise test saved (Ideal vs Realistic Impr.): mean diff = $",
    round(diff_mean, 2), ", 95% CrI excludes zero = ", paired_noise_summary$CrI_excludes_zero, "\n", sep = "")

cat(sprintf("\n03_plots.R COMPLETE. All plots for the primary model are now in %s.\n", out_plots))
