# ============================================================
# TB Decision Analytic Model — NPY Scenarios
# ============================================================
#
# PRE-REQUISITE: run 02_model.R first. It saves <out_root>/rds/model_env.rds
# (a single output/ folder for every run).
#
#
#
# FOUR SCENARIOS (this file):
# ─────────────────────────────────────────────────────────────
# SCENARIO A: Temporal Mismatch Correction (Rs.1000 costs / Rs.500 data)
#   The base case applies Rs.1000 current costs to Rs.500-era historical
#   effectiveness data. This scenario models the expected effectiveness
#   gain from the larger transfer, holding coverage and costs constant.
#   Dimension 1 — CHE protection: effectiveness (1-RR) raised by
#     scenario_c_effectiveness_gain (config.R; 10%).
#   Dimension 2 — Treatment outcomes: unfavourable outcomes reduced
#     by scenario_c_outcome_improve (config.R; 10%) for receipt groups.
#   Both dimensions combined, applied to a transformed copy of the
#   parameter list (p_a) before calling the SAME shared strat_ev()/
#   nonpy_ev() used everywhere else.
#
# SCENARIO B: Cost-perspective split (health-system and patient)
#   Same tree, same probabilities/utilities — only the cost
#   parameters are swapped. Each total cost = NPY transfer (100%
#   health-system outlay, exact per the outcome rule — see
#   compute_npy_transfer_table(), 01_helpers.R) + a flat health-system
#   or patient "care" figure per TB type, from real dollar figures in
#   input/model_input_parameters.xlsx (cost_healthsystem_DSTB,
#   cost_patient_DSTB, cost_healthsystem_DRTB, cost_patient_DRTB). NO
#   GUESSED SPLIT IS USED — if any of the four are missing, Scenario B
#   is SKIPPED entirely (not run on a placeholder) and a clear message
#   is printed explaining why. Two perspectives (health-system,
#   patient), one scenario, one output folder — they are the same
#   decomposition viewed from two sides, not independent scenarios.
#
# Scenario C:
# NOTE: Scenario C (the premature-death QALY loss / lifetime
# extension) lives in its own script,
# 05_scenario_C_premature_death_qaly.R — it is a materially bigger
# piece of work (sourced life table, literature-derived mortality
# adjustment, two robustness sweeps) than a one-line scenario variant,
# and keeping it separate makes that visible. Run it independently,
# any time after 02_model.R; it does not depend on this script.
#
#
# SCENARIO D: DR-TB utility as a ratio off DS-TB
#   Primary model imports DR-TB utility directly from a Thailand study
#   (Kittikraisak et al. 2012); this scenario instead derives it as
#   India's own DS-TB utility x a ratio taken from that same study
#   (which reports both DS-TB and DR-TB) — see dr_ds_utility_ratio,
#   00_config.R Group D, for the full citation and caveats.
#
# ── STRUCTURAL NOTE ─────────────────────────
#   No NPY is evaluated via nonpy_ev() in every scenario, which goes
#   directly to DS-TB / DR-TB (no receipt group) and uses
#   nocost_nr_* costs — matching the corrected TreeAge tree.
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
cat(sprintf("  CHE: anchor=%.7f | rr_npy=%.2f | or_drtb=%.2f | or_outcome=%.1f\n",
            p_nr_dstb_cat_src, rr_npy_che, or_drtb_che, or_che_outcome))

# ── Output folders ────────────────────────────────────────────
out_scen <- file.path(out_root, "scenarios")
for (d in c("A_temporal_mismatch", "B_cost_perspective", "D_dr_utility_ratio")) {
  dir.create(file.path(out_scen, d, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_scen, d, "plots"),  recursive = TRUE, showWarnings = FALSE)
}
cat("OK - Scenario output folders ready\n")

wtp_range <- seq(0, ceiling(wtp * 2), by = 1)

# run_scenario_psa() (run all 4 strategies + PSA for a scenario in one
# call, given a function that builds ONE parameter draw's results) now
# lives in 01_helpers.R, shared with
# 05_scenario_C_premature_death_qaly.R — previously duplicated
# identically in both files.

# report_pce() now lives in 01_helpers.R, shared with
# 05_scenario_C_premature_death_qaly.R — previously duplicated
# identically in both files.

# ============================================================
# SCENARIO A — TEMPORAL MISMATCH CORRECTION (Rs.500 -> Rs.1000)
# ============================================================

# Diagnostic: quantify overlap between Dimension 1 (CHE) and
# Dimension 2 (outcomes) before running the combined scenario, to
# confirm applying both together does not double-count the CHE
# pathway's contribution.
cat("\n\n============================================================\nDIAGNOSTIC: Overlap check for Scenario A (Realistic Impr.)\n============================================================\n")

calc_unfavourable_ri <- function(b) {
  che_split <- function(p_unsuc, p_die, p_cat) {
    p0 <- p_unsuc + p_die
    p1 <- min((or_che_outcome * p0) / ((1 - p0) + (or_che_outcome * p0)), 0.999)
    su <- if (p0 > 0) p_unsuc / p0 else 0.5
    sd <- if (p0 > 0) p_die   / p0 else 0.5
    (1 - p_cat) * (su * p0 + sd * p0) + p_cat * (su * p1 + sd * p1)
  }
  unfav_ds <- as.numeric(b$p_ri_timely)  * che_split(b$p_timely_dstb_unsuc, b$p_timely_dstb_die, b$p_timely_dstb_cat) +
    as.numeric(b$p_ri_delayed) * che_split(b$p_del_dstb_unsuc, b$p_del_dstb_die, b$p_del_dstb_cat) +
    as.numeric(b$p_ri_nr)      * che_split(b$p_nr_dstb_unsuc,  b$p_nr_dstb_die,  b$p_nr_dstb_cat)
  unfav_dr <- as.numeric(b$p_ri_timely)  * che_split(b$p_timely_drtb_unsuc, b$p_timely_drtb_die, b$p_timely_drtb_cat) +
    as.numeric(b$p_ri_delayed) * che_split(b$p_del_drtb_unsuc, b$p_del_drtb_die, b$p_del_drtb_cat) +
    as.numeric(b$p_ri_nr)      * che_split(b$p_nr_drtb_unsuc,  b$p_nr_drtb_die,  b$p_nr_drtb_cat)
  list(unfav_ds = unfav_ds, unfav_dr = unfav_dr)
}

# New RR for Dimension 1 (10% more effective CHE protection)
rr_npy_A <- 1 - (1 - rr_npy_che) * scenario_c_effectiveness_gain

receipt_outcome_groups <- list(
  list(suc = "p_timely_dstb_suc", unsuc = "p_timely_dstb_unsuc", die = "p_timely_dstb_die"),
  list(suc = "p_timely_drtb_suc", unsuc = "p_timely_drtb_unsuc", die = "p_timely_drtb_die"),
  list(suc = "p_del_dstb_suc",    unsuc = "p_del_dstb_unsuc",    die = "p_del_dstb_die"),
  list(suc = "p_del_drtb_suc",    unsuc = "p_del_drtb_unsuc",    die = "p_del_drtb_die")
)

# apply_scenario_a(): given ANY parameter list p (base case or a PSA
# draw), returns a transformed copy with Dimension 1 (CHE) and
# Dimension 2 (outcomes) both applied to the timely/delayed receipt
# groups. Non-receipt fields are left untouched (those patients
# never received NPY, so the Rs.1000-era effect doesn't apply to them).
apply_scenario_a <- function(p) {
  p_a <- p
  p_a$p_timely_dstb_cat <- rr_npy_A * as.numeric(p$p_nr_dstb_cat)
  p_a$p_del_dstb_cat    <- rr_npy_A * as.numeric(p$p_nr_dstb_cat)
  p_a$p_timely_drtb_cat <- rr_npy_A * as.numeric(p$p_nr_drtb_cat)
  p_a$p_del_drtb_cat    <- rr_npy_A * as.numeric(p$p_nr_drtb_cat)
  for (grp in receipt_outcome_groups) {
    u_new <- as.numeric(p_a[[grp$unsuc]]) * scenario_c_outcome_improve
    d_new <- as.numeric(p_a[[grp$die]])   * scenario_c_outcome_improve
    p_a[[grp$suc]]   <- 1 - u_new - d_new
    p_a[[grp$unsuc]] <- u_new
    p_a[[grp$die]]   <- d_new
  }
  p_a
}

base_D2 <- base
for (grp in receipt_outcome_groups) {
  u_new <- base_D2[[grp$unsuc]] * scenario_c_outcome_improve
  d_new <- base_D2[[grp$die]]   * scenario_c_outcome_improve
  base_D2[[grp$suc]]   <- 1 - u_new - d_new
  base_D2[[grp$unsuc]] <- u_new
  base_D2[[grp$die]]   <- d_new
}
base_A <- apply_scenario_a(base)

u0 <- calc_unfavourable_ri(base)
u2 <- calc_unfavourable_ri(base_D2)
u3 <- calc_unfavourable_ri(base_A)
cat(sprintf("%-14s %-15s %-15s\n", "Config", "Unfav DS-TB", "Unfav DR-TB"))
cat(sprintf("%-14s %-15.6f %-15.6f\n", "Base",     u0$unfav_ds, u0$unfav_dr))
cat(sprintf("%-14s %-15.6f %-15.6f\n", "D2-only",  u2$unfav_ds, u2$unfav_dr))
cat(sprintf("%-14s %-15.6f %-15.6f\n", "Combined", u3$unfav_ds, u3$unfav_dr))
cat(sprintf("Delta unfav DS-TB (Combined vs D2-only): %.4f pp - ", (u3$unfav_ds - u2$unfav_ds) * 100))
if (abs(u3$unfav_ds - u2$unfav_ds) * 100 < 0.2) cat("NEGLIGIBLE (OK)\n") else cat("WARNING - overlap > 0.2pp\n")

cat("\n\n============================================================\nSCENARIO A: Temporal mismatch correction (Rs.500 -> Rs.1000)\n============================================================\n")
out_A <- file.path(out_scen, "A_temporal_mismatch")

model_A <- function(p) run_model(apply_scenario_a(p), compute_qalys(p), or_che_outcome, split_mode = "marginal", cap_env = NULL)

br_A    <- model_A(base)
costs_A <- c(br_A$cost_nonpy, br_A$cost_soc, br_A$cost_ri, br_A$cost_ii)
qalys_A <- c(br_A$qaly_nonpy, br_A$qaly_soc, br_A$qaly_ri, br_A$qaly_ii)
cats_A  <- c(br_A$cat_nonpy,  br_A$cat_soc,  br_A$cat_ri,  br_A$cat_ii)
print_icer("Scenario A: Temporal mismatch correction", strats, costs_A, qalys_A, cats_A, wtp,
           icer_xlsx_path     = file.path(out_A, "tables", "icer_table.xlsx"),
           frontier_xlsx_path = file.path(out_A, "tables", "efficiency_frontier.xlsx"))

nmb_ri_base <- wtp * (br$qaly_ri  - br$qaly_nonpy)  - (br$cost_ri  - br$cost_nonpy)
nmb_ri_A    <- wtp * (br_A$qaly_ri - br_A$qaly_nonpy) - (br_A$cost_ri - br_A$cost_nonpy)
cat(sprintf("\nRI Primary NMB: $%.2f  ->  Scenario A NMB: $%.2f\n", nmb_ri_base, nmb_ri_A))

che_compare <- data.frame(
  Strategy = rep(strats, 2), Scenario = rep(c("Primary", "Scenario A"), each = 4),
  P_CHE    = c(cats_v, cats_A)
) %>% mutate(Strategy = factor(Strategy, levels = strats))

che_plot <- ggplot(che_compare %>% filter(Strategy != "No NPY"), aes(x = Strategy, y = P_CHE, fill = Scenario)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Primary" = "#377EB8", "Scenario A" = "#4DAF4A")) +
  labs(title = "CHE Probability: Primary vs Scenario A",
       subtitle = paste0("Temporal mismatch correction: +10% CHE protection,\n",
                          "-10% unfavourable outcomes"),
       x = NULL, y = "Probability of catastrophic health expenditure", fill = NULL) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(out_A, "plots", "che_comparison.png"), che_plot, width = 8, height = 5, dpi = 150)
write_xlsx(che_compare, file.path(out_A, "tables", "che_comparison.xlsx"))

cat(sprintf("\n=== Scenario A PSA (n=%d) ===\n", n_psa))
set.seed(psa_seed)
diag_A <- new.env(); diag_A$beta_phi_fail <- 0L; diag_A$dir_phi_warn <- 0L; diag_A$clamp_count <- 0L
psa_params_A <- sample_psa_params(n_psa, base, beta_params_active, cost_params,
                                   dirichlet_groups, dirichlet_group_names,
                                   ci_lower, ci_upper, cat_params, cat_ci_lower, cat_ci_upper,
                                   nocost_nr_params, diag_A)
psa_A <- run_scenario_psa(n_psa, psa_params_A, model_A, label = "Scenario A")
psa_A$sim <- seq_len(n_psa)
report_pce(psa_A, "Scen A")

write_xlsx(psa_A, file.path(out_A, "tables", "psa_raw.xlsx"))
saveRDS(psa_A, file = file.path(out_rds, "psa_raw_A.rds"))
make_ceac_plot(psa_A, wtp, wtp_range, "Scenario A: Temporal mismatch",
               png_path  = file.path(out_A, "plots", "ceac.png"),
               xlsx_path = file.path(out_A, "tables", "ceac_data.xlsx"))

psa_A <- psa_A %>%
  mutate(nmb_soc = wtp * (qaly_soc - qaly_nonpy) - (cost_soc - cost_nonpy),
         nmb_ri  = wtp * (qaly_ri  - qaly_nonpy) - (cost_ri  - cost_nonpy),
         nmb_ii  = wtp * (qaly_ii  - qaly_nonpy) - (cost_ii  - cost_nonpy))
check_psa_convergence(psa_A, nmb_cols = c("nmb_soc", "nmb_ri", "nmb_ii"),
                       strategy_labels = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
                       title_suffix = "Scenario A",
                       png_path  = file.path(out_A, "plots", "psa_convergence.png"),
                       xlsx_path = file.path(out_A, "tables", "psa_convergence_summary.xlsx"))
cat("OK - Scenario A complete\n")

# ============================================================
# SCENARIO D — DR-TB UTILITY AS A RATIO OFF DS-TB (proportional-utility)
# ============================================================
# Primary model imports Thailand's DR-TB utility value directly (0.51,
# Kittikraisak et al. 2012) alongside India-sourced DS-TB utility
# (0.93/0.91) — mixing an absolute number from one country with a
# directly-measured value from another, and identical success/failure
# DR-TB values (0.51/0.51) despite the DS-TB values differing.
#
# This scenario instead derives DR-TB utility as INDIA'S OWN DS-TB
# value, scaled by a ratio taken from Kittikraisak (who report DS-TB
# AND DR-TB utility in the same Thai cohort/instrument):
#   dr_ds_utility_ratio = 0.51 / 0.69 = 0.7391  (00_config.R, Group D)
# So the absolute DR-TB number stays India-anchored; only the SHAPE of
# the DS-TB-to-DR-TB relationship is borrowed from Thailand — the
# reverse of what the primary model currently does. Applied per PSA
# draw, so DR-TB inherits DS-TB's own sampled (India) uncertainty,
# scaled — no separate DR-TB uncertainty is invented.
#
# Scope: DURING-treatment utilities only (uv_drtb_suc_during /
# uv_drtb_fail_during), matching the primary (year-1) model. The
# after-treatment DR-TB values used only by Scenario C
# (after_tx_utilities, 00_config.R) are untouched here — extending
# this fix there would need its own decision about whether the same
# ratio should apply post-treatment, out of scope for this pass.
cat("\n\n============================================================\nSCENARIO D: DR-TB utility as a ratio off DS-TB\n============================================================\n")
out_D <- file.path(out_scen, "D_dr_utility_ratio")

apply_scenario_d <- function(p) {
  p_d <- p
  p_d$uv_drtb_suc_during  <- as.numeric(p$uv_dstb_during)      * dr_ds_utility_ratio
  p_d$uv_drtb_fail_during <- as.numeric(p$uv_dstb_fail_during) * dr_ds_utility_ratio
  p_d
}
model_D <- function(p) {
  p_d <- apply_scenario_d(p)
  run_model(p_d, compute_qalys(p_d), or_che_outcome, split_mode = "marginal", cap_env = NULL)
}

br_D    <- model_D(base)
costs_D <- c(br_D$cost_nonpy, br_D$cost_soc, br_D$cost_ri, br_D$cost_ii)
qalys_D <- c(br_D$qaly_nonpy, br_D$qaly_soc, br_D$qaly_ri, br_D$qaly_ii)
cats_D  <- c(br_D$cat_nonpy,  br_D$cat_soc,  br_D$cat_ri,  br_D$cat_ii)
print_icer("Scenario D: DR-TB utility ratio", strats, costs_D, qalys_D, cats_D, wtp,
           icer_xlsx_path     = file.path(out_D, "tables", "icer_table.xlsx"),
           frontier_xlsx_path = file.path(out_D, "tables", "efficiency_frontier.xlsx"))

cat(sprintf("\nDR-TB during-treatment utility - Primary: suc=%.4f fail=%.4f  ->  Scenario D: suc=%.4f fail=%.4f\n",
            as.numeric(base[["uv_drtb_suc_during"]]), as.numeric(base[["uv_drtb_fail_during"]]),
            as.numeric(base[["uv_dstb_during"]]) * dr_ds_utility_ratio,
            as.numeric(base[["uv_dstb_fail_during"]]) * dr_ds_utility_ratio))

nmb_ri_base_D <- wtp * (br$qaly_ri  - br$qaly_nonpy)  - (br$cost_ri  - br$cost_nonpy)
nmb_ri_D      <- wtp * (br_D$qaly_ri - br_D$qaly_nonpy) - (br_D$cost_ri - br_D$cost_nonpy)
cat(sprintf("RI Primary NMB: $%.2f  ->  Scenario D NMB: $%.2f\n", nmb_ri_base_D, nmb_ri_D))

cat(sprintf("\n=== Scenario D PSA (n=%d) ===\n", n_psa))
set.seed(psa_seed)
diag_D <- new.env(); diag_D$beta_phi_fail <- 0L; diag_D$dir_phi_warn <- 0L; diag_D$clamp_count <- 0L
psa_params_D <- sample_psa_params(n_psa, base, beta_params_active, cost_params,
                                   dirichlet_groups, dirichlet_group_names,
                                   ci_lower, ci_upper, cat_params, cat_ci_lower, cat_ci_upper,
                                   nocost_nr_params, diag_D)
psa_D <- run_scenario_psa(n_psa, psa_params_D, model_D, label = "Scenario D")
psa_D$sim <- seq_len(n_psa)
report_pce(psa_D, "Scen D")

write_xlsx(psa_D, file.path(out_D, "tables", "psa_raw.xlsx"))
saveRDS(psa_D, file = file.path(out_rds, "psa_raw_D.rds"))
make_ceac_plot(psa_D, wtp, wtp_range, "Scenario D: DR-TB utility ratio",
               png_path  = file.path(out_D, "plots", "ceac.png"),
               xlsx_path = file.path(out_D, "tables", "ceac_data.xlsx"))

psa_D <- psa_D %>%
  mutate(nmb_soc = wtp * (qaly_soc - qaly_nonpy) - (cost_soc - cost_nonpy),
         nmb_ri  = wtp * (qaly_ri  - qaly_nonpy) - (cost_ri  - cost_nonpy),
         nmb_ii  = wtp * (qaly_ii  - qaly_nonpy) - (cost_ii  - cost_nonpy))
check_psa_convergence(psa_D, nmb_cols = c("nmb_soc", "nmb_ri", "nmb_ii"),
                       strategy_labels = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
                       title_suffix = "Scenario D",
                       png_path  = file.path(out_D, "plots", "psa_convergence.png"),
                       xlsx_path = file.path(out_D, "tables", "psa_convergence_summary.xlsx"))
cat("OK - Scenario D complete\n")

# ============================================================
# SCENARIO B — COST PERSPECTIVES (HEALTH-SYSTEM AND PATIENT)
# ============================================================
# The primary model uses total societal costs (health system +
# patient out-of-pocket combined). This scenario re-runs the SAME
# decision tree using only one cost component at a time, from two
# perspectives — letting the PI assess cost-effectiveness from a
# narrow health-system-budget view or the patient-household view.
# Merged into ONE scenario (24 Aug 2026, previously two: D and E) —
# they are the same cost decomposition viewed from two sides, not two
# independent scenarios; they share one output folder with two
# sub-labelled result sets ("hs" and "pt") rather than each getting
# its own top-level scenario folder.
#
# DATA SOURCE: four flat parameters in input/model_input_parameters.xlsx,
# "Parameter" sheet (added by the PI directly, under a "Scenario"
# section header):
#   cost_healthsystem_DSTB, cost_patient_DSTB,
#   cost_healthsystem_DRTB, cost_patient_DRTB
# Each is ONE dollar figure per TB type — the non-NPY-transfer
# ("care") portion of cost borne by that side, not broken down by
# receipt group or treatment outcome (Chatterjee/Muniyandi's source
# papers cost by service/pathway, not by outcome, so a single
# per-TB-type figure is the realistic level of detail). Patient cost
# was derived by the PI as a formula in Excel (Timely-success total -
# NPY transfer - health-system cost), then converted to a plain value
# once cached; both parameters carry no CI, so PSA sampling falls back
# to +/-20% automatically (same rule as every other parameter with a
# blank CI, sample_psa_params(), 01_helpers.R).
#
# T (total, already in cost_*/nocost_*) = S (health-system care) + N
# (NPY transfer, credited entirely to the health-system side) + P
# (patient care). S and P are FLAT across receipt group and outcome;
# N varies by outcome (death cells receive a reduced, death-adjusted
# transfer — compute_npy_transfer_table(), 01_helpers.R) and by
# receipt group (non-receipt/No-NPY cells receive none at all).
cat("\n── Loading NPY-transfer table + flat health-system/patient cost split ──\n")
transfer_usd <- compute_npy_transfer_table(root, cost_param_names, nocost_param_names, death_adjusted = TRUE)

# tb_type_of(): DS-TB or DR-TB from a cost_*/nocost_* parameter name —
# same pattern used in compute_npy_transfer_table() (01_helpers.R).
tb_type_of <- function(nm) if (grepl("dstb", tolower(nm))) "DSTB" else "DRTB"

hs_param_names <- c(DSTB = "cost_healthsystem_DSTB", DRTB = "cost_healthsystem_DRTB")
pt_param_names <- c(DSTB = "cost_patient_DSTB",       DRTB = "cost_patient_DRTB")

all_cell_names <- c(cost_param_names, nocost_param_names)
cell_tb        <- vapply(all_cell_names, tb_type_of, character(1))

# Available whenever all four flat parameters exist in the Excel and
# have a numeric base value — no placeholder default, same
# no-guessing principle as before, just a simpler check (four scalars
# instead of a per-cell resolution).
required_de_params <- c(hs_param_names, pt_param_names)
scenario_b_available <- all(vapply(required_de_params, function(nm) {
  isTRUE(!is.na(suppressWarnings(as.numeric(base[[nm]]))))
}, logical(1)))

if (scenario_b_available) {
  out_B <- file.path(out_scen, "B_cost_perspective")

  cat("  Health-system / patient cost (flat, per TB type):\n")
  for (tb in c("DSTB", "DRTB")) {
    cat(sprintf("    %-4s -> health-system $%.2f | patient $%.2f\n",
                tb, as.numeric(base[[hs_param_names[[tb]]]]), as.numeric(base[[pt_param_names[[tb]]]])))
  }

  cat("\n── Verification (one example cell per TB type) ──\n")
  example_names <- c(grep("dstb", all_cell_names, ignore.case = TRUE, value = TRUE)[1],
                      grep("drtb", all_cell_names, ignore.case = TRUE, value = TRUE)[1])
  cat(sprintf("%-24s %9s %9s %9s %9s %9s\n",
              "Parameter", "Total", "Transfer", "HS", "Patient", "HS+Pt+Tr"))
  cat(strrep("-", 75), "\n")
  for (nm in example_names) {
    total_val <- if (nm %in% cost_param_names) as.numeric(base[[nm]]) else nocost_nr_params[[nm]]$base
    tr_val    <- transfer_usd[[nm]]
    tb        <- cell_tb[[nm]]
    hs_val    <- as.numeric(base[[hs_param_names[[tb]]]])
    pt_val    <- as.numeric(base[[pt_param_names[[tb]]]])
    cat(sprintf("%-24s %9.2f %9.2f %9.2f %9.2f %9.2f\n",
                nm, total_val, tr_val, hs_val, pt_val, hs_val + pt_val + tr_val))
  }
  cat("(HS+Pt+Tr matches Total exactly only for the specific Timely-success\n")
  cat(" cell the split was derived from; other cells' totals differ because\n")
  cat(" HS/patient are flat across receipt group and outcome while Total and\n")
  cat(" Transfer are not — an intentional simplification, not an error.)\n")

  # f_hs_share: health-system fraction of the flat NPY-receiver
  # health-system/patient split, per TB type (derived from
  # cost_healthsystem_*/cost_patient_*, e.g. 209.40/(209.40+318.86)
  # = 39.6% for DS-TB). Used ONLY for the No-NPY counterfactual cells
  # (nocost_param_names) below -- see the fix note in make_persp_base()
  # for why.
  f_hs_share <- setNames(vapply(c("DSTB", "DRTB"), function(tb) {
    hs <- as.numeric(base[[hs_param_names[[tb]]]])
    pt <- as.numeric(base[[pt_param_names[[tb]]]])
    hs / (hs + pt)
  }, numeric(1)), c("DSTB", "DRTB"))

  # make_persp_base(): replaces the 18 NPY-active cost parameters and
  # the 6 No-NPY counterfactual cost parameters with their
  # perspective-specific value, but via TWO DIFFERENT mechanisms,
  # because they come from two independently-sourced datasets that
  # were never calibrated against each other:
  #
  #   NPY-active cells (cost_param_names): health-system (_hs) = real
  #   (death-adjusted) NPY transfer + the flat health-system cost for
  #   that cell's TB type; patient (_pt) = the flat patient cost for
  #   that TB type. Both flat figures are measured on NPY-receiving
  #   patients specifically (see cost_healthsystem_*/cost_patient_*,
  #   input Excel), so this is a real, directly-sourced split.
  #
  #   No-NPY counterfactual cells (nocost_param_names): FIXED 25 Aug
  #   2026 -- these previously went through the SAME flat-figure
  #   substitution as the NPY-active cells above, silently discarding
  #   the real, independently-sourced No-NPY cost data
  #   (nocost_nr_params, 00_config.R Group B, from
  #   Treeage/NoNPY cost.xlsx) and replacing it with a flat figure
  #   measured on a completely different population (NPY recipients).
  #   This broke additivity: health-system + patient no longer summed
  #   to the real societal No-NPY total (a $49.56, ~10% gap on the
  #   No-NPY row specifically, where there is no transfer to explain
  #   the mismatch). Fixed by instead taking the REAL No-NPY total for
  #   that cell and splitting IT proportionally, using the NPY-
  #   receiver health-system/patient ratio (f_hs_share) as the best
  #   available estimate of the split -- since the No-NPY population's
  #   own health-system/patient cost ratio was never independently
  #   measured. This is an ASSUMPTION (the same ratio measured on NPY
  #   receivers is assumed to apply to the No-NPY counterfactual too),
  #   not measured data, and should be described as such wherever this
  #   scenario's methods are reported -- but it guarantees exact
  #   additivity (health-system + patient = societal total) and avoids
  #   the alternative failure mode of leaving No-NPY costs unsplit
  #   (which would compare a split NPY-active cost against an unsplit,
  #   full-societal No-NPY cost, understating incremental cost by the
  #   entire patient share).
  make_persp_base <- function(base_params, suffix) {
    p <- base_params
    for (nm in cost_param_names) {
      tb <- cell_tb[[nm]]
      p[[nm]] <- if (suffix == "_hs") {
        transfer_usd[[nm]] + as.numeric(base_params[[hs_param_names[[tb]]]])
      } else {
        as.numeric(base_params[[pt_param_names[[tb]]]])
      }
    }
    for (nm in nocost_param_names) {
      tb <- cell_tb[[nm]]
      real_total <- as.numeric(base_params[[nm]])
      p[[nm]] <- if (suffix == "_hs") {
        real_total * f_hs_share[[tb]]
      } else {
        real_total * (1 - f_hs_share[[tb]])
      }
    }
    p
  }

  # sample_persp_psa(): standard PSA sampler (cost_* drawn from its
  # usual Gamma distribution, same as the primary model; the four flat
  # cost_healthsystem_*/cost_patient_* parameters are drawn the same
  # way sample_psa_params() draws every other Gamma cost — they are
  # NOT in cost_param_names, so the primary model/other scenarios never
  # waste a draw sampling them, but they ARE present in `base` and
  # sampled here directly per iteration since this scenario specifically
  # needs their uncertainty reflected).
  sample_persp_psa <- function(n, suffix, diag_env) {
    params_list <- sample_psa_params(n, base, beta_params_active, cost_params,
                                      dirichlet_groups, dirichlet_group_names,
                                      ci_lower, ci_upper, cat_params, cat_ci_lower, cat_ci_upper,
                                      nocost_nr_params, diag_env)
    de_param_names <- unname(c(hs_param_names, pt_param_names))
    de_draws <- lapply(de_param_names, function(nm) {
      mu <- as.numeric(base[[nm]])
      lo <- as.numeric(ci_lower[[nm]]); hi <- as.numeric(ci_upper[[nm]])
      if (is.na(lo) || is.na(hi)) { lo <- mu * 0.8; hi <- mu * 1.2 }
      rgamma_ci(n, mu, lo, hi)
    })
    names(de_draws) <- de_param_names
    for (i in seq_len(n)) {
      for (nm in cost_param_names) {
        tb <- cell_tb[[nm]]
        params_list[[i]][[nm]] <- if (suffix == "_hs") {
          transfer_usd[[nm]] + de_draws[[hs_param_names[[tb]]]][i]
        } else {
          de_draws[[pt_param_names[[tb]]]][i]
        }
      }
      # Same fix as make_persp_base(): split each iteration's own
      # SAMPLED No-NPY draw (already in params_list[[i]][[nm]], from
      # sample_psa_params()'s nocost_nr_params sampling) proportionally,
      # using that same iteration's sampled health-system/patient
      # ratio -- rather than discarding the real sampled No-NPY value
      # and substituting the flat NPY-receiver draw outright.
      for (nm in nocost_param_names) {
        tb <- cell_tb[[nm]]
        real_total_i <- params_list[[i]][[nm]]
        hs_i <- de_draws[[hs_param_names[[tb]]]][i]
        pt_i <- de_draws[[pt_param_names[[tb]]]][i]
        f_hs_i <- hs_i / (hs_i + pt_i)
        params_list[[i]][[nm]] <- if (suffix == "_hs") {
          real_total_i * f_hs_i
        } else {
          real_total_i * (1 - f_hs_i)
        }
      }
    }
    params_list
  }

  model_primary <- function(p) run_model(p, compute_qalys(p), or_che_outcome, split_mode = "marginal", cap_env = NULL)

  # ── Health-system perspective ──────────────────────────────
  cat("\n\n============================================================\nSCENARIO B: Cost perspectives — health-system\n============================================================\n")
  base_hs <- make_persp_base(base, "_hs")
  br_B_hs <- model_primary(base_hs)
  costs_B_hs <- c(br_B_hs$cost_nonpy, br_B_hs$cost_soc, br_B_hs$cost_ri, br_B_hs$cost_ii)
  qalys_B_hs <- c(br_B_hs$qaly_nonpy, br_B_hs$qaly_soc, br_B_hs$qaly_ri, br_B_hs$qaly_ii)
  cats_B_hs  <- c(br_B_hs$cat_nonpy,  br_B_hs$cat_soc,  br_B_hs$cat_ri,  br_B_hs$cat_ii)
  print_icer("Scenario B: Health-system perspective", strats, costs_B_hs, qalys_B_hs, cats_B_hs, wtp,
             icer_xlsx_path     = file.path(out_B, "tables", "health_system_icer_table.xlsx"),
             frontier_xlsx_path = file.path(out_B, "tables", "health_system_efficiency_frontier.xlsx"))

  cat("\n── Perspective comparison (Realistic Impr. vs No NPY) ──\n")
  cat(sprintf("  Total (societal) cost - No NPY: $%.2f  RI: $%.2f\n", br$cost_nonpy, br$cost_ri))
  cat(sprintf("  Health sys cost  - No NPY: $%.2f  RI: $%.2f  (HS fraction: %.0f%%/%.0f%%)\n",
              br_B_hs$cost_nonpy, br_B_hs$cost_ri, 100 * br_B_hs$cost_nonpy / br$cost_nonpy, 100 * br_B_hs$cost_ri / br$cost_ri))

  cat(sprintf("\n=== Scenario B (health-system) PSA (n=%d) ===\n", n_psa))
  set.seed(psa_seed)
  diag_B_hs <- new.env(); diag_B_hs$beta_phi_fail <- 0L; diag_B_hs$dir_phi_warn <- 0L; diag_B_hs$clamp_count <- 0L
  psa_params_B_hs <- sample_persp_psa(n_psa, "_hs", diag_B_hs)
  psa_B_hs <- run_scenario_psa(n_psa, psa_params_B_hs, model_primary, label = "Scenario B (health-system)")
  psa_B_hs$sim <- seq_len(n_psa)
  report_pce(psa_B_hs, "HS")

  write_xlsx(psa_B_hs, file.path(out_B, "tables", "health_system_psa_raw.xlsx"))
  saveRDS(psa_B_hs, file = file.path(out_rds, "psa_raw_B_hs.rds"))
  make_ceac_plot(psa_B_hs, wtp, wtp_range, "Scenario B: Health-system perspective",
                 png_path  = file.path(out_B, "plots", "health_system_ceac.png"),
                 xlsx_path = file.path(out_B, "tables", "health_system_ceac_data.xlsx"))

  psa_B_hs <- psa_B_hs %>%
    mutate(nmb_soc = wtp * (qaly_soc - qaly_nonpy) - (cost_soc - cost_nonpy),
           nmb_ri  = wtp * (qaly_ri  - qaly_nonpy) - (cost_ri  - cost_nonpy),
           nmb_ii  = wtp * (qaly_ii  - qaly_nonpy) - (cost_ii  - cost_nonpy))
  check_psa_convergence(psa_B_hs, nmb_cols = c("nmb_soc", "nmb_ri", "nmb_ii"),
                         strategy_labels = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
                         title_suffix = "Scenario B (health-system)",
                         png_path  = file.path(out_B, "plots", "health_system_psa_convergence.png"),
                         xlsx_path = file.path(out_B, "tables", "health_system_psa_convergence_summary.xlsx"))
  cat("OK - Scenario B (health-system) complete\n")

  # ── Patient perspective ────────────────────────────────────
  cat("\n\n============================================================\nSCENARIO B: Cost perspectives — patient\n============================================================\n")
  base_pt <- make_persp_base(base, "_pt")
  br_B_pt <- model_primary(base_pt)
  costs_B_pt <- c(br_B_pt$cost_nonpy, br_B_pt$cost_soc, br_B_pt$cost_ri, br_B_pt$cost_ii)
  qalys_B_pt <- c(br_B_pt$qaly_nonpy, br_B_pt$qaly_soc, br_B_pt$qaly_ri, br_B_pt$qaly_ii)
  cats_B_pt  <- c(br_B_pt$cat_nonpy,  br_B_pt$cat_soc,  br_B_pt$cat_ri,  br_B_pt$cat_ii)
  print_icer("Scenario B: Patient perspective", strats, costs_B_pt, qalys_B_pt, cats_B_pt, wtp,
             icer_xlsx_path     = file.path(out_B, "tables", "patient_icer_table.xlsx"),
             frontier_xlsx_path = file.path(out_B, "tables", "patient_efficiency_frontier.xlsx"))

  cat("\n── Perspective comparison (Realistic Impr. vs No NPY) ──\n")
  cat(sprintf("  Total (societal) cost - No NPY: $%.2f  RI: $%.2f\n", br$cost_nonpy, br$cost_ri))
  cat(sprintf("  Patient cost    - No NPY: $%.2f  RI: $%.2f  (Pt fraction: %.0f%%/%.0f%%)\n",
              br_B_pt$cost_nonpy, br_B_pt$cost_ri, 100 * br_B_pt$cost_nonpy / br$cost_nonpy, 100 * br_B_pt$cost_ri / br$cost_ri))

  cat(sprintf("\n=== Scenario B (patient) PSA (n=%d) ===\n", n_psa))
  set.seed(psa_seed)
  diag_B_pt <- new.env(); diag_B_pt$beta_phi_fail <- 0L; diag_B_pt$dir_phi_warn <- 0L; diag_B_pt$clamp_count <- 0L
  psa_params_B_pt <- sample_persp_psa(n_psa, "_pt", diag_B_pt)
  psa_B_pt <- run_scenario_psa(n_psa, psa_params_B_pt, model_primary, label = "Scenario B (patient)")
  psa_B_pt$sim <- seq_len(n_psa)
  report_pce(psa_B_pt, "Pt")

  write_xlsx(psa_B_pt, file.path(out_B, "tables", "patient_psa_raw.xlsx"))
  saveRDS(psa_B_pt, file = file.path(out_rds, "psa_raw_B_pt.rds"))
  make_ceac_plot(psa_B_pt, wtp, wtp_range, "Scenario B: Patient perspective",
                 png_path  = file.path(out_B, "plots", "patient_ceac.png"),
                 xlsx_path = file.path(out_B, "tables", "patient_ceac_data.xlsx"))

  psa_B_pt <- psa_B_pt %>%
    mutate(nmb_soc = wtp * (qaly_soc - qaly_nonpy) - (cost_soc - cost_nonpy),
           nmb_ri  = wtp * (qaly_ri  - qaly_nonpy) - (cost_ri  - cost_nonpy),
           nmb_ii  = wtp * (qaly_ii  - qaly_nonpy) - (cost_ii  - cost_nonpy))
  check_psa_convergence(psa_B_pt, nmb_cols = c("nmb_soc", "nmb_ri", "nmb_ii"),
                         strategy_labels = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
                         title_suffix = "Scenario B (patient)",
                         png_path  = file.path(out_B, "plots", "patient_psa_convergence.png"),
                         xlsx_path = file.path(out_B, "tables", "patient_psa_convergence_summary.xlsx"))
  cat("OK - Scenario B (patient) complete\n")

  # Combined summary for the scenario (both perspectives, one table)
  persp_summary <- data.frame(
    Perspective = c("Health-system", "Patient"),
    Cost_RI     = round(c(br_B_hs$cost_ri, br_B_pt$cost_ri), 4),
    QALY_RI     = round(c(br_B_hs$qaly_ri, br_B_pt$qaly_ri), 6),
    ICER_vs_NoNPY = round(c(
      (br_B_hs$cost_ri - br_B_hs$cost_nonpy) / (br_B_hs$qaly_ri - br_B_hs$qaly_nonpy),
      (br_B_pt$cost_ri - br_B_pt$cost_nonpy) / (br_B_pt$qaly_ri - br_B_pt$qaly_nonpy)
    ), 2),
    P_CE_PSA_pct = c(
      round(100 * mean((wtp * (psa_B_hs$qaly_ri - psa_B_hs$qaly_nonpy) - (psa_B_hs$cost_ri - psa_B_hs$cost_nonpy)) > 0), 1),
      round(100 * mean((wtp * (psa_B_pt$qaly_ri - psa_B_pt$qaly_nonpy) - (psa_B_pt$cost_ri - psa_B_pt$cost_nonpy)) > 0), 1)
    )
  )
  write_xlsx(persp_summary, file.path(out_B, "tables", "perspective_summary.xlsx"))
  cat("\nOK - Scenario B combined perspective summary saved\n")
} else {
  cat("\n*** SCENARIO B SKIPPED ***\n")
  cat("Reason: input/model_input_parameters.xlsx is missing one or more of:\n")
  cat("  cost_healthsystem_DSTB, cost_patient_DSTB,\n")
  cat("  cost_healthsystem_DRTB, cost_patient_DRTB\n")
  cat("No placeholder/guessed split is used. Add these rows with real figures\n")
  cat("and re-run this script to enable Scenario B.\n\n")
}

# ============================================================
# SUMMARY COMPARISON TABLE
# ============================================================
cat("\n\n============================================================\nSUMMARY: Primary vs Scenarios A/B/D\n(Realistic Improvement strategy vs No NPY)\n============================================================\n")
# NOTE: Scenario C (premature-death QALY loss) is intentionally NOT
# in this table — it lives in its own script,
# 05_scenario_C_premature_death_qaly.R (see that file's header for
# why). Run it separately for its own summary output under
# <out_root>/scenarios/C_premature_death_qaly/.

nmb_ri_fn <- function(b) wtp * (b$qaly_ri - b$qaly_nonpy) - (b$cost_ri - b$cost_nonpy)

psa_primary_file <- file.path(out_root, "psa", "PSA_raw_results.xlsx")
psa_primary      <- readxl::read_excel(psa_primary_file)
p_ce_primary     <- round(100 * mean((wtp * (psa_primary$qaly_ri - psa_primary$qaly_nonpy) -
                                         (psa_primary$cost_ri - psa_primary$cost_nonpy)) > 0), 1)
pce_ri <- function(psa_df) round(100 * mean((wtp * (psa_df$qaly_ri - psa_df$qaly_nonpy) -
                                                (psa_df$cost_ri - psa_df$cost_nonpy)) > 0), 1)

if (scenario_b_available) {
  summary_tbl <- data.frame(
    Scenario       = c("Primary (societal)", "A: Temporal mismatch",
                       "B: Cost perspective (health-system)", "B: Cost perspective (patient)",
                       "D: DR-TB utility ratio"),
    QALY_RI        = round(c(br$qaly_ri, br_A$qaly_ri, br_B_hs$qaly_ri, br_B_pt$qaly_ri, br_D$qaly_ri), 6),
    Cost_RI        = round(c(br$cost_ri, br_A$cost_ri, br_B_hs$cost_ri, br_B_pt$cost_ri, br_D$cost_ri), 4),
    NMB_RI         = round(c(nmb_ri_fn(br), nmb_ri_fn(br_A), nmb_ri_fn(br_B_hs), nmb_ri_fn(br_B_pt), nmb_ri_fn(br_D)), 2),
    P_CHE_RI       = round(c(br$cat_ri, br_A$cat_ri, br_B_hs$cat_ri, br_B_pt$cat_ri, br_D$cat_ri), 6),
    CHE_averted_RI = round(c(br$cat_nonpy - br$cat_ri, br_A$cat_nonpy - br_A$cat_ri,
                              br_B_hs$cat_nonpy - br_B_hs$cat_ri, br_B_pt$cat_nonpy - br_B_pt$cat_ri,
                              br_D$cat_nonpy - br_D$cat_ri), 6),
    P_CE_PSA_pct   = c(p_ce_primary, pce_ri(psa_A), pce_ri(psa_B_hs), pce_ri(psa_B_pt), pce_ri(psa_D))
  )
} else {
  # B skipped — summary table covers A/D/Primary only, with B's rows
  # explicitly marked as skipped rather than omitted silently.
  summary_tbl <- data.frame(
    Scenario       = c("Primary (societal)", "A: Temporal mismatch",
                       "B: Cost perspective (health-system)", "B: Cost perspective (patient)",
                       "D: DR-TB utility ratio"),
    QALY_RI        = c(round(c(br$qaly_ri, br_A$qaly_ri), 6), NA, NA, round(br_D$qaly_ri, 6)),
    Cost_RI        = c(round(c(br$cost_ri, br_A$cost_ri), 4), NA, NA, round(br_D$cost_ri, 4)),
    NMB_RI         = c(round(c(nmb_ri_fn(br), nmb_ri_fn(br_A)), 2), NA, NA, round(nmb_ri_fn(br_D), 2)),
    P_CHE_RI       = c(round(c(br$cat_ri, br_A$cat_ri), 6), NA, NA, round(br_D$cat_ri, 6)),
    CHE_averted_RI = c(round(c(br$cat_nonpy - br$cat_ri, br_A$cat_nonpy - br_A$cat_ri), 6), NA, NA,
                       round(br_D$cat_nonpy - br_D$cat_ri, 6)),
    P_CE_PSA_pct   = c(p_ce_primary, pce_ri(psa_A), NA, NA, pce_ri(psa_D)),
    Note           = c("", "", "SKIPPED - no PI cost split yet", "SKIPPED - no PI cost split yet", "")
  )
}
print(summary_tbl, row.names = FALSE)
write_xlsx(summary_tbl, file.path(out_scen, "scenario_summary.xlsx"))
cat("OK - Summary comparison table saved\n")

cat("\n========================================================\nSCENARIO SCRIPT COMPLETE\n========================================================\n")
cat("Scenario A (Temporal mismatch): OK - Base case + PSA + CEAC\n")
if (scenario_b_available) {
  cat("Scenario B (Cost perspective):  OK - Base case + PSA + CEAC [real cost split, both perspectives]\n")
} else {
  cat("Scenario B (Cost perspective):  SKIPPED - no real cost split yet (see message above)\n")
}
cat("Scenario D (DR-TB utility ratio):OK - Base case + PSA + CEAC\n")
cat("Scenario C (Premature-death QALY): see 05_scenario_C_premature_death_qaly.R (run separately)\n")
cat(sprintf("All outputs saved to: %s\n", out_scen))
if (!scenario_b_available) {
  cat("NOTE: fill in cost_healthsystem_DSTB/DRTB and cost_patient_DSTB/DRTB\n")
  cat("      in input/model_input_parameters.xlsx with real dollar figures,\n")
  cat("      then re-run this script to enable Scenario B.\n")
}
cat("========================================================\n")
