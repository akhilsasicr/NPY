# ============================================================
# TB Decision Analytic Model — NPY
# SCENARIO C: Premature-death QALY loss
# ============================================================
#
# PRE-REQUISITE: run 02_model.R first. It saves <out_root>/rds/model_env.rds
# (a single output/ folder for every run).
# Does not depend on 04_scenarios.R — runs independently, but sits
# right after it in run order (this is script 05, 04_scenarios.R is
# 04) since it's the same kind of scenario, just materially bigger.
#
# WHAT THIS DOES: a 1-year decision-tree horizon discards most of a
# health intervention's mortality-related value, since averted deaths
# carry decades of uncounted future QALYs a 1-year tree can't score.
# This script adds a discounted YLL-style future-QALY term on top of
# the unchanged 1-year tree, via compute_qalys_lifetime()
# (01_helpers.R) — only the QALY payoff at each terminal node changes.
#
# Kept separate from Scenarios A, B, D (04_scenarios.R) as a
# materially bigger piece of work: sourced life table,
# literature-derived mortality adjustment, two robustness sweeps —
# and kept as a scenario rather than promoted to primary (every
# manuscript figure is currently year-1; changing that is a decision
# for the PI, not this script).
#
# WHAT TO EXPECT: absolute QALYs become large (~20/person rather than
# ~0.86), since every survivor now carries their remaining discounted
# lifetime — expected, not a bug. The number that matters is the
# INCREMENTAL QALY between strategies, driven almost entirely by
# deaths averted.
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(gtools)
library(dampack)
library(scales)
library(here)

root <- here::here()
source(file.path(root, "script", "00_config.R"))
source(file.path(root, "script", "01_helpers.R"))

# ============================================================
# SECTION 0 — LOAD MODEL ENVIRONMENT (data only — functions
# come from 01_helpers.R, sourced above, not from the RDS)
# ============================================================
# A single output/ folder is used for every run (no more pct20/source
# mode split).
out_root <- file.path(root, "output")
rds_path <- file.path(out_root, "rds", "model_env.rds")
if (!file.exists(rds_path))
  stop(rds_path, " not found. Please run 02_model.R first.")

env <- readRDS(rds_path)
list2env(env, envir = environment())
rm(env)

cat("OK - model environment loaded\n")
cat(sprintf("  WTP=$%g | n_psa=%d\n", wtp, n_psa))

# ── After-treatment utilities (config.R Group C) ──────────────
# compute_qalys_lifetime() needs uv_*_after (post-treatment utility)
# to multiply by the life-expectancy factor. These are not in Excel
# (see 00_config.R) so they are injected here via the shared
# inject_after_tx_utilities() helper (01_helpers.R) — the two scripts
# do not share state, so each calls it independently on its own base.
inj <- inject_after_tx_utilities(base, ci_lower, ci_upper)
base <- inj$base; ci_lower <- inj$ci_lower; ci_upper <- inj$ci_upper
cat("OK - After-treatment utilities injected for Scenario C\n")

# beta_params_active plus the after-treatment utilities, so the PSA
# sampler also samples uv_*_after (needed because compute_qalys_lifetime
# uses them) — same set Scenario B uses, defined independently here
# since this script does not source 04_scenarios.R.
beta_params_3yr <- union(beta_params_active, names(after_tx_utilities))

# ── Output folder ──────────────────────────────────────────────
out_scen <- file.path(out_root, "scenarios")
out_C    <- file.path(out_scen, "C_premature_death_qaly")
dir.create(file.path(out_C, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_C, "plots"),  recursive = TRUE, showWarnings = FALSE)
cat("OK - Scenario C output folder ready\n")

wtp_range <- seq(0, ceiling(wtp * 2), by = 1)

cat("\n\n============================================================\nSCENARIO C: Premature-death QALY loss\n============================================================\n")

# ── Remaining life expectancy from India's SRS life table ────
# Computed once here (rather than inside every PSA iteration, which
# would re-read the file 5,000 times) and passed explicitly into
# compute_qalys_lifetime().
lt_C      <- load_lifetable()
le_undisc <- discounted_life_expectancy(cohort_mean_age, post_tb_smr, 0, lt_C)
le_C      <- discounted_life_expectancy(cohort_mean_age, post_tb_smr,
                                         discount_rate_qaly, lt_C)

cat(sprintf("Life table: %s\n", basename(lifetable_path)))
cat(sprintf("Cohort mean age %.1f | SMR %.2f | discount %.1f%%\n",
            cohort_mean_age, post_tb_smr, 100 * discount_rate_qaly))
cat(sprintf("Remaining life expectancy: %.1f yrs undiscounted -> %.2f discounted\n",
            le_undisc, le_C))

# ── Method comparison: crude vs life-table-derived remaining life ──
# WHY THIS TABLE EXISTS
#   There is more than one way to estimate "QALYs lost due to
#   premature death," and the simplest reading is a NAIVE calculation:
#   (India life expectancy at birth) minus (cohort mean age, 38),
#   turned into a plain annuity, with no life table and no
#   TB-specific excess-mortality adjustment.
#
#   SOURCING THE CRUDE FIGURE: someone doing a
#   quick, crude calculation would not build a life table from
#   scratch — they would look up the published headline number. That
#   number is 70.6 years, from "SRS Based Abridged Life Tables
#   2020-24," Office of the Registrar General & Census Commissioner,
#   India, published May 2026 — the same, more recent official
#   document used to build the refined life table itself
#   (input/SRS_lifetable_India_2020-24.csv; see 00_config.R Group F
#   for the full source note and the earlier 2017-21 edition this
#   superseded, which gave 69.8). Because the crude figure and the
#   refined life table now come from the SAME publication, this
#   comparison isolates exactly one thing: what does building a full
#   age-specific life table (versus a single headline subtraction)
#   change, holding the underlying data source fixed.
#
#   This table reports BOTH methods side by side, deliberately, so
#   neither is hidden: the crude version (SRS headline figure, no
#   life table, no SMR), and the refined version this script actually
#   uses (raw SRS life table + Selvaraju's Indian post-TB SMR of
#   2.3). Showing both lets a reader see exactly how much the
#   refinement changes the answer, rather than presenting only the
#   more favourable or more rigorous number without comparison.
cat("\n-- Method comparison: crude estimate vs life-table + SMR --\n")

# SRS Based Abridged Life Tables 2020-24 (Registrar General of India,
# published May 2026): life expectancy at birth 70.6 years.
srs_headline_life_expectancy <- 70.6
crude_years <- srs_headline_life_expectancy - cohort_mean_age  # naive subtraction
crude_af    <- (1 - (1 + discount_rate_qaly)^(-crude_years)) / discount_rate_qaly
letable_nosmr  <- discounted_life_expectancy(cohort_mean_age, 1.0, discount_rate_qaly, lt_C)
letable_smr    <- le_C   # already computed above, SMR = post_tb_smr = 2.3

method_compare <- data.frame(
  Method = c("Crude (70.6 - age, no life table, no SMR)",
             "Life table, no excess mortality (SMR 1.0)",
             "Life table + Indian post-TB SMR 2.3 (USED)"),
  Discounted_years = round(c(crude_af, letable_nosmr, letable_smr), 2)
)
print(method_compare, row.names = FALSE)

for (nm in method_compare$Method) {
  af_m  <- method_compare$Discounted_years[method_compare$Method == nm]
  r_m   <- run_model(base, compute_qalys_lifetime(base, le_factor = af_m),
                      or_che_outcome, split_mode = "marginal", cap_env = NULL)
  i_m   <- (r_m$cost_soc - r_m$cost_nonpy) / (r_m$qaly_soc - r_m$qaly_nonpy)
  cat(sprintf("  %-45s %6.2f yrs -> ICER (Current NPY) $%.2f\n", nm, af_m, i_m))
}
write_xlsx(method_compare, file.path(out_C, "tables", "method_comparison.xlsx"))

# model_C(): the base-case call (p = base, no p$post_tb_smr field)
# uses the fixed le_C computed once above, at the deterministic SMR
# 2.3. PSA calls (p from sample_psa_params(), which now sets
# p$post_tb_smr on every draw — see 01_helpers.R step 8) instead
# recompute the discounted life expectancy from that iteration's
# OWN sampled SMR, so PSA output reflects genuine uncertainty in the
# SMR itself, not just in every other parameter at a fixed SMR.
model_C <- function(p) {
  if (is.null(p$post_tb_smr)) {
    le_i <- le_C
  } else {
    le_i <- discounted_life_expectancy(cohort_mean_age, p$post_tb_smr, discount_rate_qaly, lt_C)
  }
  run_model(p, compute_qalys_lifetime(p, le_factor = le_i),
            or_che_outcome, split_mode = "marginal", cap_env = NULL)
}

br_C    <- model_C(base)
costs_C <- c(br_C$cost_nonpy, br_C$cost_soc, br_C$cost_ri, br_C$cost_ii)
qalys_C <- c(br_C$qaly_nonpy, br_C$qaly_soc, br_C$qaly_ri, br_C$qaly_ii)
cats_C  <- c(br_C$cat_nonpy,  br_C$cat_soc,  br_C$cat_ri,  br_C$cat_ii)
print_icer("Scenario C: Premature-death QALY loss", strats, costs_C, qalys_C, cats_C, wtp,
           icer_xlsx_path     = file.path(out_C, "tables", "icer_table.xlsx"),
           frontier_xlsx_path = file.path(out_C, "tables", "efficiency_frontier.xlsx"))

# ── The headline comparison: ICER against BOTH thresholds ────
# wtp and wtp_opportunity_cost both come from 00_config.R ($2,536
# GDP-per-capita; $487 Ochalek-lineage opportunity-cost).
icer_vs_nonpy <- function(b) (b$cost_soc - b$cost_nonpy) / (b$qaly_soc - b$qaly_nonpy)
icer_ri_vs_nonpy <- function(b) (b$cost_ri - b$cost_nonpy) / (b$qaly_ri - b$qaly_nonpy)

cat("\n-- ICER vs No NPY: primary (year-1) vs Scenario C (lifetime) --\n")
cat(sprintf("  Current NPY : primary $%10.2f  ->  Scenario C $%10.2f\n",
            icer_vs_nonpy(br), icer_vs_nonpy(br_C)))
cat(sprintf("  Realistic   : primary $%10.2f  ->  Scenario C $%10.2f\n",
            icer_ri_vs_nonpy(br), icer_ri_vs_nonpy(br_C)))
cat(sprintf("  Thresholds  : GDP-per-capita $%d | opportunity-cost $%d\n",
            wtp, wtp_opportunity_cost))
cat(sprintf("  Scenario C Current NPY ICER is %s the opportunity-cost threshold\n",
            ifelse(icer_vs_nonpy(br_C) < wtp_opportunity_cost, "BELOW", "ABOVE")))

cat(sprintf("\n  Incremental QALY (Current vs No NPY): primary %.6f -> Scenario C %.6f (%.1fx)\n",
            br$qaly_soc - br$qaly_nonpy, br_C$qaly_soc - br_C$qaly_nonpy,
            (br_C$qaly_soc - br_C$qaly_nonpy) / (br$qaly_soc - br$qaly_nonpy)))

# ── Robustness sweep 1: post-TB excess mortality (SMR) ───────
# TB survivors do not live as long as the general population, so
# assuming a normal lifespan for survivors would inflate the QALY
# gain from averting a death. This sweep tests that directly. Each
# row re-derives remaining life expectancy from the SAME SRS
# life table with age-specific death probabilities multiplied by the
# SMR, so these are real actuarial figures, not assumed haircuts.
cat("\n-- Robustness 1: ICER vs post-TB excess mortality (SMR) --\n")
cat(sprintf("%6s %12s %12s %14s %14s %10s\n",
            "SMR", "LE_undisc", "LE_disc", "ICER_SoC", "ICER_RI", "vs $487"))
smr_rows <- do.call(rbind, lapply(post_tb_smr_range, function(s) {
  le_u <- discounted_life_expectancy(cohort_mean_age, s, 0, lt_C)
  le_d <- discounted_life_expectancy(cohort_mean_age, s, discount_rate_qaly, lt_C)
  r_s  <- run_model(base, compute_qalys_lifetime(base, le_factor = le_d),
                    or_che_outcome, split_mode = "marginal", cap_env = NULL)
  i_soc <- (r_s$cost_soc - r_s$cost_nonpy) / (r_s$qaly_soc - r_s$qaly_nonpy)
  i_ri  <- (r_s$cost_ri  - r_s$cost_nonpy) / (r_s$qaly_ri  - r_s$qaly_nonpy)
  cat(sprintf("%6.2f %12.1f %12.2f %14.2f %14.2f %10s\n", s, le_u, le_d, i_soc, i_ri,
              ifelse(i_soc < wtp_opportunity_cost, "clears", "fails")))
  data.frame(SMR = s, LE_undiscounted = le_u, LE_discounted = le_d,
             ICER_SoC = i_soc, ICER_RI = i_ri,
             Clears_487 = i_soc < wtp_opportunity_cost)
}))
write_xlsx(smr_rows, file.path(out_C, "tables", "smr_sensitivity.xlsx"))

# ── Robustness sweep 2: raw credited years ───────────────────
# A deliberately blunter version of the same question: how few
# future years would we have to credit per averted death before the
# conclusion flips? Here le_factor is a plain annuity factor over N
# certain years, not a life-table figure — the low end (2-5 years)
# is far more pessimistic than any real mortality assumption
# could justify.
cat("\n-- Robustness 2: ICER vs raw credited years (annuity, not life table) --\n")
cat(sprintf("%8s %14s %14s %10s\n", "Years", "ICER_SoC", "ICER_RI", "vs $487"))
le_rows <- do.call(rbind, lapply(life_expectancy_remaining_range, function(n) {
  af_n <- (1 - (1 + discount_rate_qaly)^(-n)) / discount_rate_qaly
  r_n  <- run_model(base, compute_qalys_lifetime(base, le_factor = af_n),
                    or_che_outcome, split_mode = "marginal", cap_env = NULL)
  i_soc <- (r_n$cost_soc - r_n$cost_nonpy) / (r_n$qaly_soc - r_n$qaly_nonpy)
  i_ri  <- (r_n$cost_ri  - r_n$cost_nonpy) / (r_n$qaly_ri  - r_n$qaly_nonpy)
  cat(sprintf("%8.0f %14.2f %14.2f %10s\n", n, i_soc, i_ri,
              ifelse(i_soc < wtp_opportunity_cost, "clears", "fails")))
  data.frame(Years_remaining = n, Annuity_factor = af_n,
             ICER_SoC = i_soc, ICER_RI = i_ri,
             Clears_487 = i_soc < wtp_opportunity_cost)
}))
write_xlsx(le_rows, file.path(out_C, "tables", "life_expectancy_sensitivity.xlsx"))

# ── Robustness sweep 3: age distribution ──────────────────────
# compute_qalys_lifetime() evaluates remaining life expectancy at a
# single age — cohort_mean_age (38) — not integrated across the real
# age distribution (mean 38.23, median 36, SD 17.94, right-skewed).
# Remaining life expectancy is a non-linear function of age, so this
# is an approximation, not an error, but its direction and size were
# previously unknown. This sweep re-derives life expectancy (at the
# base-case SMR) at several ages spanning that distribution, to show
# how much the ICER actually moves — the same kind of check as
# Robustness sweep 1, applied to age instead of SMR. Ages are
# approximated as mean +/- {0, 0.5, 1} SD (clipped to 0-100), plus the
# median directly, since only summary statistics (not the full
# distribution) are available.
age_points <- sort(unique(c(
  cohort_median_age,
  round(pmax(0, pmin(100, cohort_mean_age + c(-1, -0.5, 0, 0.5, 1) * cohort_age_sd)))
)))
cat("\n-- Robustness 3: ICER vs cohort age (base-case SMR = post_tb_smr) --\n")
cat(sprintf("%6s %12s %12s %14s %14s %10s\n",
            "Age", "LE_undisc", "LE_disc", "ICER_SoC", "ICER_RI", "vs $487"))
age_rows <- do.call(rbind, lapply(age_points, function(a) {
  le_u <- discounted_life_expectancy(a, post_tb_smr, 0, lt_C)
  le_d <- discounted_life_expectancy(a, post_tb_smr, discount_rate_qaly, lt_C)
  r_a  <- run_model(base, compute_qalys_lifetime(base, le_factor = le_d),
                    or_che_outcome, split_mode = "marginal", cap_env = NULL)
  i_soc <- (r_a$cost_soc - r_a$cost_nonpy) / (r_a$qaly_soc - r_a$qaly_nonpy)
  i_ri  <- (r_a$cost_ri  - r_a$cost_nonpy) / (r_a$qaly_ri  - r_a$qaly_nonpy)
  cat(sprintf("%6.0f %12.1f %12.2f %14.2f %14.2f %10s\n", a, le_u, le_d, i_soc, i_ri,
              ifelse(i_soc < wtp_opportunity_cost, "clears", "fails")))
  data.frame(Age = a, LE_undiscounted = le_u, LE_discounted = le_d,
             ICER_SoC = i_soc, ICER_RI = i_ri,
             Clears_487 = i_soc < wtp_opportunity_cost)
}))
write_xlsx(age_rows, file.path(out_C, "tables", "age_sensitivity.xlsx"))
cat("(Approximates the real age distribution from mean/median/SD only\n")
cat(" -- not a full weighted-average across actual per-patient ages.)\n")

# ── Scenario C PSA ───────────────────────────────────────────
# Uses the same expanded Beta set as Scenario B, because the
# post-treatment utilities (uv_*_after) are sampled here too.
cat(sprintf("\n=== Scenario C PSA (n=%d) ===\n", n_psa))
set.seed(psa_seed)
diag_C <- new.env(); diag_C$beta_phi_fail <- 0L; diag_C$dir_phi_warn <- 0L; diag_C$clamp_count <- 0L
psa_params_C <- sample_psa_params(n_psa, base, beta_params_3yr, cost_params,
                                   dirichlet_groups, dirichlet_group_names,
                                   ci_lower, ci_upper, cat_params, cat_ci_lower, cat_ci_upper,
                                   nocost_nr_params, diag_C)

# run_scenario_psa() now lives in 01_helpers.R, shared with
# 04_scenarios.R — previously duplicated identically in both files.
psa_C <- run_scenario_psa(n_psa, psa_params_C, model_C, label = "Scenario C")
psa_C$sim <- seq_len(n_psa)

# report_pce() now lives in 01_helpers.R, shared with 04_scenarios.R —
# previously duplicated identically in both files.
report_pce(psa_C, "PrematureDeathQALY")

# P(CE) at the opportunity-cost threshold specifically — the more
# conservative of the two WTP thresholds, so this is the harder test.
pce_at <- function(psa_df, w) round(100 * mean((w * (psa_df$qaly_soc - psa_df$qaly_nonpy) -
                                                   (psa_df$cost_soc - psa_df$cost_nonpy)) > 0), 1)
cat(sprintf("\n  P(CE) Current NPY at GDP threshold      $%4d : %.1f%%\n", wtp, pce_at(psa_C, wtp)))
cat(sprintf("  P(CE) Current NPY at opportunity cost   $%4d : %.1f%%\n",
            wtp_opportunity_cost, pce_at(psa_C, wtp_opportunity_cost)))

write_xlsx(psa_C, file.path(out_C, "tables", "psa_raw.xlsx"))
saveRDS(psa_C, file = file.path(out_rds, "psa_raw_C.rds"))
make_ceac_plot(psa_C, wtp, wtp_range, "Scenario C: Premature-death QALY loss",
               png_path  = file.path(out_C, "plots", "ceac.png"),
               xlsx_path = file.path(out_C, "tables", "ceac_data.xlsx"))

psa_C <- psa_C %>%
  mutate(nmb_soc = wtp * (qaly_soc - qaly_nonpy) - (cost_soc - cost_nonpy),
         nmb_ri  = wtp * (qaly_ri  - qaly_nonpy) - (cost_ri  - cost_nonpy),
         nmb_ii  = wtp * (qaly_ii  - qaly_nonpy) - (cost_ii  - cost_nonpy))
check_psa_convergence(psa_C, nmb_cols = c("nmb_soc", "nmb_ri", "nmb_ii"),
                       strategy_labels = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
                       title_suffix = "Scenario C",
                       png_path  = file.path(out_C, "plots", "psa_convergence.png"),
                       xlsx_path = file.path(out_C, "tables", "psa_convergence_summary.xlsx"))
cat("OK - Scenario C complete\n")

cat("\n========================================================\n")
cat("SCENARIO C COMPLETE.\n")
cat(sprintf("  All outputs saved to: %s\n", out_C))
cat("========================================================\n")
