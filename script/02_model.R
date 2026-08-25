# ============================================================
# TB Decision Analytic Model — Coverage of NPY
# PRIMARY / BASE MODEL
# ============================================================
#
# This R script replicates the cost-utility analysis (CUA) of the
# Nikshay Poshan Yojana (NPY) — India's conditional cash transfer
# for TB patients — originally developed in TreeAge Pro.
#
# HOW TO RUN THIS PROJECT:
#   1. Run THIS script (02_model.R) first. It reads the Excel input,
#      runs the base case + PSA + OWSA, saves every result table, and
#      writes output/rds/model_env.rds for the scripts below (see
#      Section 0). This script produces NO plots — all plotting (CE-plane, CEAC, PSA
#      convergence, tornado, coverage scale-up) lives in 03_plots.R,
#      which reads this script's saved tables/RDS rather than
#      recomputing anything.
#   2. Then run 03_plots.R for the plots, 04_scenarios.R for
#      Scenarios A, B, D, and 05_scenario_C_premature_death_qaly.R
#      for Scenario C.
#   All scripts source 00_config.R (every toggle/assumption) and
#   01_helpers.R (every shared function) automatically — you do not
#   need to run those files yourself.
#
#
# DECISION TREE STRUCTURE
#   Strategy
#     -> Receipt group (timely / delayed / non-receipt)
#           -> TB type (DS-TB / DR-TB)
#                 -> CHE (catastrophic health expenditure: yes / no)
#                       -> Treatment outcome (success / failure / death)
#   NOTE: No NPY goes directly to TB type (no receipt group level),
#         matching the corrected TreeAge model NPY_9.6.26.
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(gtools)
library(dampack)
library(here)

root <- here::here()
source(file.path(root, "script", "00_config.R"))
source(file.path(root, "script", "01_helpers.R"))

# ============================================================
# SECTION 0 — FOLDER PATHS
# ============================================================
# A single output/ folder for every run — there is no longer a
# pct20/source mode split (see WORKLOG.md, 19 Aug 2026 entry): every
# parameter now uses its own real Excel CI, with the three receipt-
# group probabilities (p_soc_timely/p_soc_del/p_soc_nr) widened to
# +/-20% directly in the Excel rather than via a separate global mode.
out_root <- file.path(root, "output")
dirs <- c("input", file.path(out_root, "tables"), file.path(out_root, "plots"),
          file.path(out_root, "psa"), file.path(out_root, "rds"), "script")
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("OK - Folder structure ready -> %s\n", out_root))

input_xlsx <- file.path(root, "input", "model_input_parameters.xlsx")
out_tables <- file.path(out_root, "tables")
out_plots  <- file.path(out_root, "plots")
out_psa    <- file.path(out_root, "psa")
# <out_root>/rds/ — every .rds file in this project (model_env.rds,
# evppi_inputs.rds, and each scenario's raw PSA draws) lives here, so
# there is one place to look for saved-state files, separate from the
# human-readable .xlsx tables in <out_root>/tables/ and
# <out_root>/psa/. RDS is R's native binary format: much faster to
# read/write than Excel and preserves exact numeric precision, which
# matters for anything that needs to reload large PSA draws (e.g. a
# future "regenerate plots only" script) without re-running the PSA
# sampling loop itself.
out_rds    <- file.path(out_root, "rds")

# ============================================================
# SECTION 1 — DERIVED CHE PREVALENCES (from config.R Group A)
# ============================================================
# All CHE prevalence rates are derived from four constants set in
# 00_config.R (the anchor rate, and three relative-risk / odds-ratio
# multipliers). They are computed here, once, so the numbers below
# can be printed and sanity-checked before the PSA runs.

p_nr_dstb_cat <- p_nr_dstb_cat_src
p_nr_drtb_cat <- (or_drtb_che * p_nr_dstb_cat_src) /
  ((1 - p_nr_dstb_cat_src) + (or_drtb_che * p_nr_dstb_cat_src))

p_timely_dstb_cat <- rr_npy_che * p_nr_dstb_cat
p_del_dstb_cat    <- rr_npy_che * p_nr_dstb_cat
p_timely_drtb_cat <- rr_npy_che * p_nr_drtb_cat
p_del_drtb_cat    <- rr_npy_che * p_nr_drtb_cat

cat_params <- list(
  p_timely_dstb_cat = p_timely_dstb_cat,
  p_del_dstb_cat    = p_del_dstb_cat,
  p_nr_dstb_cat     = p_nr_dstb_cat,
  p_timely_drtb_cat = p_timely_drtb_cat,
  p_del_drtb_cat    = p_del_drtb_cat,
  p_nr_drtb_cat     = p_nr_drtb_cat
)

cat("\n=== Derived CHE prevalences ===\n")
cat(sprintf("  Anchor (NR DS-TB)  : %.6f\n", p_nr_dstb_cat))
cat(sprintf("  NR DR-TB           : %.6f\n", p_nr_drtb_cat))
cat(sprintf("  Timely / Del DS-TB : %.6f\n", p_timely_dstb_cat))
cat(sprintf("  Timely / Del DR-TB : %.6f\n", p_timely_drtb_cat))

# Always +/-20% — unlike Excel-read parameters, these CHE prevalences
# are DERIVED (from p_nr_dstb_cat_src, rr_npy_che, or_drtb_che), so
# there is no independent real-data CI for them at all. A permanent
# limitation of this project's CI-fitting approach, not a bug: real
# CI data is used everywhere it exists, and this is a case where it
# doesn't.
cat_ci_lower <- lapply(cat_params, function(v) v * 0.8)
cat_ci_upper <- lapply(cat_params, function(v) v * 1.2)

cat("\n=== No NPY cost constants (from config.R Group B) ===\n")
for (nm in names(nocost_nr_params)) {
  v <- nocost_nr_params[[nm]]
  cat(sprintf("  %-22s: %.2f  [%.2f - %.2f]\n", nm, v$base, v$lo, v$hi))
}

# ============================================================
# SECTION 2 — READ PARAMETERS FROM EXCEL
# ============================================================
skip_labels <- c(
  "Probabilities", "Utilities", "Costs", "Model settings",
  "Receipt probabilities", "TB type probabilities",
  "DS-TB treatment outcomes", "DR-TB treatment outcomes",
  "Conditional death / survival",
  "Year 1 - utility during treatment",
  "Year 2 - utility after treatment",
  "Year 1 — utility during treatment",
  "Year 2 — utility after treatment",
  "Death",
  "Model settings for QALY calculation",
  "Timely receipt costs", "Delayed receipt costs", "Non-receipt costs"
)

param_df <- read_excel(input_xlsx, sheet = "Parameter") %>%
  filter(!is.na(Parameter), !(Parameter %in% skip_labels)) %>%
  filter(!is.na(base_value))   # drop section-header rows that have no numeric value

base_all <- to_list(param_df, "Parameter", "base_value", allow_na = FALSE)
ci_lower <- to_list(param_df, "Parameter", "ci_lower",   allow_na = TRUE)
ci_upper <- to_list(param_df, "Parameter", "ci_upper",   allow_na = TRUE)

dirichlet_groups <- param_df %>%
  filter(distribution == "dirichlet", !is.na(sum_check_group)) %>%
  select(parameter = Parameter, sum_check_group)

beta_params           <- param_df$Parameter[param_df$distribution == "beta"]

# cost_params: the cost parameters the PRIMARY model actually reads.
# The Excel also contains 48 extra gamma-typed
# rows — the `_hs` (health system) and `_pt` (patient) cost splits
# used by Scenario B's two perspectives. The primary model never reads
# those, but they were being swept up here, which meant: 48 wasted
# Gamma draws per PSA iteration, 96 pointless OWSA model runs (all
# returning NMB_range = 0 and cluttering the OWSA table), and 48
# needless GAM fits in EVPPI. Results were unaffected (a parameter the
# model never reads cannot change the answer) but the waste is real.
# Scenario B injects its `_hs`/`_pt` values explicitly via
# make_persp_base() / sample_persp_psa() in 04_scenarios.R, so
# excluding them here does not affect that scenario.
cost_params           <- grep("_(hs|pt)$", param_df$Parameter[param_df$distribution == "gamma"],
                              value = TRUE, invert = TRUE)
dirichlet_group_names <- unique(dirichlet_groups$sum_check_group)
dirichlet_params      <- unique(dirichlet_groups$parameter)

beta_params_active <- setdiff(beta_params, c(dirichlet_params,
                                             "uv_dstb_suc_after", "uv_dstb_fail_after",
                                             "uv_drtb_suc_after", "uv_drtb_fail_after",
                                             "p_dstb_suc_die", "p_dstb_unsuc_die",
                                             "p_drtb_suc_die", "p_drtb_unsuc_die"))

cohort_size <- as.numeric(base_all$cohort_size)
# n_psa comes from 00_config.R (not the Excel) — see that file for why
# and for the convergence testing behind the current value.

cat(sprintf("\nOK - Parameters loaded | PSA n=%d | WTP=$%g\n",
            n_psa, wtp))

# ============================================================
# SECTION 3 — PARAMETER VALIDATION
# ============================================================
validate_params(base_all)

# ============================================================
# SECTION 4 — BASE CASE
# ============================================================
stopifnot(abs(as.numeric(base_all$p_ii_timely)  - 1) < 1e-6)
stopifnot(abs(as.numeric(base_all$p_ii_delayed) - 0) < 1e-6)
stopifnot(abs(as.numeric(base_all$p_ii_nr)      - 0) < 1e-6)

base <- base_all
base$p_ii_timely  <- 1
base$p_ii_delayed <- 0
base$p_ii_nr      <- 0
base <- lapply(base, function(x) suppressWarnings(as.numeric(x)))

for (nm in names(cat_params))       base[[nm]] <- cat_params[[nm]]
for (nm in names(nocost_nr_params)) base[[nm]] <- nocost_nr_params[[nm]]$base

# Re-validate after modification: base_all was validated above, but
# base has since been rebuilt (forced p_ii_*, re-coerced to numeric via
# lapply, cat_params/nocost_nr_params merged in) — this catches any
# corruption introduced by that process rather than assuming the
# original validation still holds for the modified object.
validate_params(base)

qv_base    <- compute_qalys(base)
cap_env_bc <- new_cap_env()
br <- run_model(base, qv_base, or_che_outcome, split_mode = "marginal", cap_env = cap_env_bc)

cat("\n=== BASE CASE RESULTS ===\n")
cat(sprintf("No NPY:            Cost=$%.4f  QALY=%.6f  Surv=%.6f  Deaths=%.6f  P(CHE)=%.6f\n",
            br$cost_nonpy,  br$qaly_nonpy,  br$surv_nonpy,  br$death_nonpy,  br$cat_nonpy))
cat(sprintf("Current NPY:       Cost=$%.4f  QALY=%.6f  Surv=%.6f  Deaths=%.6f  P(CHE)=%.6f\n",
            br$cost_soc,    br$qaly_soc,    br$surv_soc,    br$death_soc,    br$cat_soc))
cat(sprintf("Realistic Impr.:   Cost=$%.4f  QALY=%.6f  Surv=%.6f  Deaths=%.6f  P(CHE)=%.6f\n",
            br$cost_ri,     br$qaly_ri,     br$surv_ri,     br$death_ri,     br$cat_ri))
cat(sprintf("Ideal improvement: Cost=$%.4f  QALY=%.6f  Surv=%.6f  Deaths=%.6f  P(CHE)=%.6f\n",
            br$cost_ii,     br$qaly_ii,     br$surv_ii,     br$death_ii,     br$cat_ii))

cat("\n=== CHE DIRECTION CHECK ===\n")
cat(sprintf("No NPY (%.4f) -> SoC (%.4f) -> RI (%.4f) -> II (%.4f)\n",
            br$cat_nonpy, br$cat_soc, br$cat_ri, br$cat_ii))
# Checks the FULL ordering chain — No NPY should have the highest CHE
# probability, decreasing through Current NPY, Realistic Impr., and
# Ideal Impr. Each link is checked and named individually so a
# failure says WHICH link broke, not just that something did.
che_chain <- c(
  "No NPY > Current NPY" = br$cat_nonpy > br$cat_soc,
  "Current NPY >= RI"    = br$cat_soc  >= br$cat_ri,
  "RI >= Ideal"          = br$cat_ri   >= br$cat_ii
)
if (all(che_chain)) {
  cat("OK - Direction correct: No NPY > Current NPY >= RI >= II\n")
} else {
  cat("WARNING - CHE direction check FAILED on:",
      paste(names(che_chain)[!che_chain], collapse = "; "), "\n")
  cat("          Review CHE parameters in 00_config.R\n")
}

# ============================================================
# SECTION 5 — RESULTS TABLES
# ============================================================
strats   <- c("No NPY", "Current NPY", "Realistic Impr.", "Ideal Impr.")
costs_v  <- c(br$cost_nonpy,  br$cost_soc,  br$cost_ri,  br$cost_ii)
qalys_v  <- c(br$qaly_nonpy,  br$qaly_soc,  br$qaly_ri,  br$qaly_ii)
survs_v  <- c(br$surv_nonpy,  br$surv_soc,  br$surv_ri,  br$surv_ii)
deaths_v <- c(br$death_nonpy, br$death_soc, br$death_ri, br$death_ii)
cats_v   <- c(br$cat_nonpy,   br$cat_soc,   br$cat_ri,   br$cat_ii)

print_icer("TABLE 1: Base case ICERs vs No NPY", strats, costs_v, qalys_v, cats_v, wtp,
           icer_xlsx_path     = file.path(out_tables, "icer_table.xlsx"),
           frontier_xlsx_path = file.path(out_tables, "efficiency_frontier.xlsx"))
# See the fuller results table further down this section (with
# deaths/CHE averted, dominance status) actually used for the manuscript.

seq_table <- as.data.frame(dampack::calculate_icers(
  cost = costs_v, effect = qalys_v, strategies = strats
)) %>%
  rename(QALY = Effect, Seq_Incr_Cost = Inc_Cost,
         Seq_Incr_QALY = Inc_Effect, Seq_ICER = ICER) %>%
  mutate(Dominance = dplyr::recode(Status,
                                   "ND" = "On frontier", "D"  = "Dominated",
                                   "ED" = "Extendedly dominated", .default = Status))

results <- data.frame(
  Strategy   = strats,
  Cost       = costs_v,
  QALY       = qalys_v,
  Survival   = survs_v,
  Prob_Death = deaths_v,
  Prob_CHE   = cats_v
) %>%
  mutate(
    Incr_Cost          = Cost  - Cost[Strategy  == "No NPY"],
    Incr_QALY          = QALY  - QALY[Strategy  == "No NPY"],
    ICER_vs_NoNPY      = ifelse(Incr_QALY != 0, round(Incr_Cost / Incr_QALY, 2), NA),
    NMB                = round(wtp * Incr_QALY - Incr_Cost, 2),
    Deaths_Averted_pp  = round(Prob_Death[Strategy == "No NPY"] - Prob_Death, 6),
    Deaths_Averted_pop = round(Deaths_Averted_pp * cohort_size, 1),
    CHE_Averted_pp     = round(Prob_CHE[Strategy  == "No NPY"] - Prob_CHE, 6),
    CHE_Averted_pop    = round(CHE_Averted_pp * cohort_size, 1)
  ) %>%
  left_join(seq_table %>% select(Strategy, Seq_ICER, Dominance), by = "Strategy")

cat("\n=== TABLE: Full base-case results ===\n")
print(results %>% select(Strategy, Cost, QALY, Incr_Cost, Incr_QALY,
                         ICER_vs_NoNPY, NMB, Deaths_Averted_pop,
                         CHE_Averted_pop, Dominance),
      row.names = FALSE)
write_xlsx(results, file.path(out_tables, "base_case_results.xlsx"))
cat("OK - Base case table saved\n")

# ============================================================
# SECTION 6 — PSA DIAGNOSTICS ENVIRONMENT
# ============================================================
# One shared diagnostics environment for the whole PSA run:
#   beta_phi_fail : Beta draws where precision <= 0 (fell back to
#                   the point estimate)
#   dir_phi_warn  : Dirichlet groups where precision <= 0 for a
#                   member parameter (fell back to phi=1 for that
#                   parameter's contribution)
#   clamp_count   : PSA iterations where the RI-timely clamp fired
psa_diag <- new.env()
psa_diag$beta_phi_fail <- 0L
psa_diag$dir_phi_warn  <- 0L
psa_diag$clamp_count   <- 0L

# ============================================================
# SECTION 7 — RUN PSA
# ============================================================
set.seed(psa_seed)
cat(sprintf("\n=== Running PSA (n = %d) ===\n", n_psa))

cap_env_psa <- new_cap_env()

psa_params <- sample_psa_params(
  n_psa, base, beta_params_active, cost_params,
  dirichlet_groups, dirichlet_group_names,
  ci_lower, ci_upper, cat_params, cat_ci_lower, cat_ci_upper,
  nocost_nr_params, psa_diag
)

# At large n_psa this loop can run for minutes with zero output
# otherwise. Prints ~10 progress ticks (iteration count, %, elapsed
# seconds) plus a final elapsed-time summary.
psa_t0 <- Sys.time()
psa_progress_step <- max(1, floor(n_psa / 10))
cat(sprintf("  [Primary model] starting %d PSA iterations...\n", n_psa))
psa_results <- lapply(seq_along(psa_params), function(i) {
  if (i %% psa_progress_step == 0 || i == n_psa) {
    elapsed <- as.numeric(difftime(Sys.time(), psa_t0, units = "secs"))
    cat(sprintf("  [Primary model] %d / %d (%.0f%%) - %.0fs elapsed\n", i, n_psa, 100 * i / n_psa, elapsed))
  }
  p  <- psa_params[[i]]
  qv <- compute_qalys(p)
  r  <- run_model(p, qv, or_che_outcome, split_mode = "marginal", cap_env = cap_env_psa)
  data.frame(
    cost_nonpy = r$cost_nonpy, qaly_nonpy = r$qaly_nonpy, cat_nonpy = r$cat_nonpy, death_nonpy = r$death_nonpy,
    cost_soc   = r$cost_soc,   qaly_soc   = r$qaly_soc,   cat_soc   = r$cat_soc,   death_soc   = r$death_soc,
    cost_ri    = r$cost_ri,    qaly_ri    = r$qaly_ri,    cat_ri    = r$cat_ri,    death_ri    = r$death_ri,
    cost_ii    = r$cost_ii,    qaly_ii    = r$qaly_ii,    cat_ii    = r$cat_ii,    death_ii    = r$death_ii
  )
})
psa_raw <- do.call(rbind, psa_results)
psa_elapsed_total <- as.numeric(difftime(Sys.time(), psa_t0, units = "secs"))
cat(sprintf("  [Primary model] complete: %d iterations in %.1fs (%.0f/sec)\n",
            n_psa, psa_elapsed_total, n_psa / max(psa_elapsed_total, 0.001)))
psa_raw$sim <- seq_len(n_psa)

psa_raw <- psa_raw %>%
  mutate(
    d_cost_soc = cost_soc  - cost_nonpy, d_qaly_soc = qaly_soc  - qaly_nonpy,
    d_cost_ri  = cost_ri   - cost_nonpy, d_qaly_ri  = qaly_ri   - qaly_nonpy,
    d_cost_ii  = cost_ii   - cost_nonpy, d_qaly_ii  = qaly_ii   - qaly_nonpy,
    d_cat_soc  = cat_nonpy - cat_soc,    d_cat_ri   = cat_nonpy - cat_ri,    d_cat_ii   = cat_nonpy - cat_ii,
    d_death_soc = death_nonpy - death_soc, d_death_ri = death_nonpy - death_ri, d_death_ii = death_nonpy - death_ii,
    nmb_soc    = wtp * d_qaly_soc - d_cost_soc,
    nmb_ri     = wtp * d_qaly_ri  - d_cost_ri,
    nmb_ii     = wtp * d_qaly_ii  - d_cost_ii,
    # ABSOLUTE NMB per strategy (wtp*QALY - Cost), NOT incremental vs
    # No NPY — this is what the manuscript's Table 4 actually reports
    # (e.g. "No NPY $1,578 (1,390, 1,708), Current NPY $1,594 (1,379,
    # 1,740)"). Everything above this line (nmb_soc/ri/ii, d_cost_*,
    # d_qaly_*) is INCREMENTAL vs No NPY — a different, also-needed
    # quantity, but not the one Table 4 uses. Both are now computed;
    # do not conflate the two NMB definitions when reading downstream
    # tables.
    nmb_abs_nonpy = wtp * qaly_nonpy - cost_nonpy,
    nmb_abs_soc   = wtp * qaly_soc   - cost_soc,
    nmb_abs_ri    = wtp * qaly_ri    - cost_ri,
    nmb_abs_ii    = wtp * qaly_ii    - cost_ii
  )

if (mean(psa_raw$d_qaly_ii, na.rm = TRUE) < 0)
  stop("Ideal Improvement mean dQALY vs No NPY is negative - review parameter ordering.")

cat("PSA complete.\n")
cat(sprintf("NMB SoC (mean): $%.2f  SD=$%.2f\n", mean(psa_raw$nmb_soc), sd(psa_raw$nmb_soc)))
cat(sprintf("NMB RI  (mean): $%.2f  SD=$%.2f\n", mean(psa_raw$nmb_ri),  sd(psa_raw$nmb_ri)))
cat(sprintf("NMB II  (mean): $%.2f  SD=$%.2f\n", mean(psa_raw$nmb_ii),  sd(psa_raw$nmb_ii)))
cat(sprintf("Prob SoC cost-effective vs No NPY at WTP=$%.0f: %.1f%%\n", wtp, 100 * mean(psa_raw$nmb_soc > 0)))
cat(sprintf("Prob RI  cost-effective vs No NPY at WTP=$%.0f: %.1f%%\n", wtp, 100 * mean(psa_raw$nmb_ri  > 0)))
cat(sprintf("Prob II  cost-effective vs No NPY at WTP=$%.0f: %.1f%%\n", wtp, 100 * mean(psa_raw$nmb_ii  > 0)))
cat(sprintf("RI clamp activated: %d / %d (%.1f%%)\n",
            psa_diag$clamp_count, n_psa, 100 * psa_diag$clamp_count / n_psa))
cat(sprintf("rbeta_ci phi<=0 fallbacks: %d  |  Dirichlet phi<=0 warnings: %d\n",
            psa_diag$beta_phi_fail, psa_diag$dir_phi_warn))
cat("Utility ordering: truncated Beta for DS-TB failure; shared draw for DR-TB (no swaps needed)\n")
cat(sprintf("CHE-branch unfavourable cap (>0.999) triggered: %d times\n", cap_env_psa$count))

write_xlsx(
  psa_raw %>% select(sim,
                     cost_nonpy, qaly_nonpy, cat_nonpy, death_nonpy,
                     cost_soc,   qaly_soc,   cat_soc,   death_soc,
                     cost_ri,    qaly_ri,    cat_ri,    death_ri,
                     cost_ii,    qaly_ii,    cat_ii,    death_ii,
                     d_cost_soc, d_qaly_soc, d_cat_soc, d_death_soc, nmb_soc,
                     d_cost_ri,  d_qaly_ri,  d_cat_ri,  d_death_ri,  nmb_ri,
                     d_cost_ii,  d_qaly_ii,  d_cat_ii,  d_death_ii,  nmb_ii),
  file.path(out_psa, "PSA_raw_results.xlsx")
)
cat("OK - PSA raw results saved\n")

# Same draws, saved as RDS too (<out_root>/rds/psa_raw_primary.rds) —
# lets 03_plots.R or any future "regenerate plots only" script reload
# the full PSA output fast, without re-running the sampling loop or
# re-parsing the (much larger, slower) .xlsx export.
saveRDS(psa_raw, file = file.path(out_rds, "psa_raw_primary.rds"))
cat(sprintf("OK - PSA raw results also saved as RDS (%s)\n",
            file.path(out_rds, "psa_raw_primary.rds")))

# ============================================================
# SECTION 8 — dampack PSA OBJECT INPUTS (for EVPI/EVPPI)
# ============================================================
# costs_df/effects_df feed EVPI/EVPPI (Section 9, below) via the
# saved evppi_inputs.rds. All PLOTTING of the PSA output — CE-plane,
# incremental CE-plane, CEAC, PSA convergence diagnostic — now lives
# in 03_plots.R, not here: this script computes and saves data only
# (see that script's header for why the split).
costs_df <- data.frame(
  `No NPY` = psa_raw$cost_nonpy, `Current NPY` = psa_raw$cost_soc,
  `Realistic Impr.` = psa_raw$cost_ri, `Ideal Impr.` = psa_raw$cost_ii,
  check.names = FALSE
)
effects_df <- data.frame(
  `No NPY` = psa_raw$qaly_nonpy, `Current NPY` = psa_raw$qaly_soc,
  `Realistic Impr.` = psa_raw$qaly_ri, `Ideal Impr.` = psa_raw$qaly_ii,
  check.names = FALSE
)

wtp_range <- seq(0, ceiling(wtp * 2), by = 1)

# ── Save inputs for EVPI/EVPPI (computed in 05_value_of_information.R) ──
# EVPI and EVPPI are a distinct analytical question from "did the
# primary PSA converge" — they ask which parameter is worth
# researching further, not what the answer is — so that code now
# lives in its own script, 05_value_of_information.R, which can be
# run any time after this script (same pattern as Scenario C in
# 06_scenario_F_premature_death_qaly.R). That script needs the PSA
# parameter DRAWS themselves (not just the resulting costs/QALYs
# already saved to PSA_raw_results.xlsx above), so those draws are
# built and saved here, once, rather than re-run inside 05.
#
# sampled_param_names / params_df: one column per sampled parameter,
# one row per PSA iteration — the piece that captures the INPUT
# draws, not just the model OUTPUTS that resulted from them, so that
# 05_value_of_information.R's calc_evppi() calls can regress outcomes
# onto individual parameter draws.
#
# See 00_config.R (n_psa_evppi) for why only a SUBSAMPLE (the first
# n_psa_evppi rows of the already-drawn psa_params/psa_raw — not a
# fresh independent sample) is saved: EVPPI's GAM metamodel doesn't
# need the full n_psa sample, and a subsample keeps EVPPI's
# 64-parameter loop fast even when n_psa is raised for PSA precision
# elsewhere. costs_df/effects_df themselves (the FULL n_psa versions,
# above) stay in THIS script because the CE-plane plot above still
# needs them at full size — only the EVPPI-sized subsample is saved.
sampled_param_names <- unique(c(beta_params_active, cost_params,
                                dirichlet_params, names(cat_params),
                                names(nocost_nr_params)))
n_evppi_use  <- min(n_psa_evppi, n_psa)
evppi_idx    <- seq_len(n_evppi_use)

params_df_evppi <- as.data.frame(do.call(rbind, lapply(psa_params[evppi_idx], function(p) {
  vapply(sampled_param_names, function(nm) as.numeric(p[[nm]]), numeric(1))
})))

evppi_inputs <- list(
  costs_df    = costs_df[evppi_idx, , drop = FALSE],
  effects_df  = effects_df[evppi_idx, , drop = FALSE],
  params_df   = params_df_evppi,
  wtp_range   = wtp_range,
  n_psa_evppi_actual = n_evppi_use
)
saveRDS(evppi_inputs, file = file.path(out_rds, "evppi_inputs.rds"))
cat(sprintf("OK - EVPI/EVPPI inputs saved: %d of %d PSA draws -> %s\n",
            n_evppi_use, n_psa, file.path(out_rds, "evppi_inputs.rds")))
cat("     (Run 06_value_of_information.R next for EVPI/EVPPI outputs.)\n")

# ============================================================
# SECTION 9 — OWSA + TORNADO
# ============================================================
cat("\n=== Running OWSA ===\n")
nmb_base_ri <- wtp * (br$qaly_ri - br$qaly_nonpy) - (br$cost_ri - br$cost_nonpy)

prob_groups <- list(
  c("p_soc_timely",  "p_soc_del",    "p_soc_nr"),
  c("p_ri_timely",   "p_ri_delayed", "p_ri_nr"),
  c("p_timely_dstb", "p_timely_drtb"),
  c("p_del_dstb",    "p_del_drtb"),
  c("p_nr_dstb",     "p_nr_drtb"),
  c("p_timely_dstb_suc", "p_timely_dstb_unsuc", "p_timely_dstb_die"),
  c("p_del_dstb_suc",    "p_del_dstb_unsuc",    "p_del_dstb_die"),
  c("p_nr_dstb_suc",     "p_nr_dstb_unsuc",     "p_nr_dstb_die"),
  c("p_timely_drtb_suc", "p_timely_drtb_unsuc", "p_timely_drtb_die"),
  c("p_del_drtb_suc",    "p_del_drtb_unsuc",    "p_del_drtb_die"),
  c("p_nr_drtb_suc",     "p_nr_drtb_unsuc",     "p_nr_drtb_die")
)

# Same _hs/_pt exclusion as cost_params above (the primary model never
# reads those 48 rows) — previously missing here, so those parameters
# silently re-entered OWSA as zero-effect noise despite the comment
# above claiming they'd been eliminated from "pointless OWSA runs".
owsa_params <- param_df$Parameter[param_df$distribution %in% c("beta", "dirichlet", "gamma")]
owsa_params <- grep("_(hs|pt)$", owsa_params, value = TRUE, invert = TRUE)
owsa_cat_params    <- names(cat_params)
owsa_nocost_params <- names(nocost_nr_params)
owsa_all           <- c(owsa_params, owsa_cat_params, owsa_nocost_params)

owsa_results <- do.call(rbind, lapply(owsa_all, function(nm) {
  if (nm %in% names(cat_params)) {
    lo <- cat_ci_lower[[nm]]; hi <- cat_ci_upper[[nm]]
  } else if (nm %in% names(nocost_nr_params)) {
    lo <- nocost_nr_params[[nm]]$lo; hi <- nocost_nr_params[[nm]]$hi
  } else {
    lo <- as.numeric(ci_lower[[nm]]); hi <- as.numeric(ci_upper[[nm]])
  }
  if (is.na(lo) || is.na(hi)) return(NULL)
  run_nmb <- function(val) {
    if (nm %in% c(names(cat_params), names(nocost_nr_params))) {
      p <- base; p[[nm]] <- val
    } else {
      p <- normalize_group(base, base, nm, val, prob_groups)
    }
    if (is.null(p)) return(NA_real_)
    r <- run_model(p, compute_qalys(p), or_che_outcome, "marginal", NULL)
    wtp * (r$qaly_ri - r$qaly_nonpy) - (r$cost_ri - r$cost_nonpy)
  }
  nmb_lo <- tryCatch(run_nmb(lo), error = function(e) NA_real_)
  nmb_hi <- tryCatch(run_nmb(hi), error = function(e) NA_real_)
  if (is.na(nmb_lo) || is.na(nmb_hi)) return(NULL)
  data.frame(
    Parameter  = nm,
    Base_value = if (nm %in% names(nocost_nr_params)) nocost_nr_params[[nm]]$base
                 else if (nm %in% names(cat_params)) cat_params[[nm]]
                 else as.numeric(base[[nm]]),
    Low_value  = lo, High_value = hi,
    NMB_base   = nmb_base_ri, NMB_low = nmb_lo, NMB_high = nmb_hi,
    NMB_range  = abs(nmb_hi - nmb_lo)
  )
})) %>% arrange(desc(NMB_range))

owsa_mult_results <- do.call(rbind, lapply(names(owsa_multipliers), function(nm) {
  vals <- owsa_multipliers[[nm]]
  run_with_mult <- function(rr = rr_npy_che, dr = or_drtb_che) {
    p <- base
    p_nr_drtb_temp <- (dr * p_nr_dstb_cat_src) / ((1 - p_nr_dstb_cat_src) + (dr * p_nr_dstb_cat_src))
    p$p_timely_dstb_cat <- rr * p_nr_dstb_cat_src
    p$p_del_dstb_cat    <- rr * p_nr_dstb_cat_src
    p$p_nr_dstb_cat     <- p_nr_dstb_cat_src
    p$p_timely_drtb_cat <- rr * p_nr_drtb_temp
    p$p_del_drtb_cat    <- rr * p_nr_drtb_temp
    p$p_nr_drtb_cat     <- p_nr_drtb_temp
    r <- run_model(p, compute_qalys(p), or_che_outcome, "marginal", NULL)
    wtp * (r$qaly_ri - r$qaly_nonpy) - (r$cost_ri - r$cost_nonpy)
  }
  nmb_lo <- if (nm == "rr_npy_che") run_with_mult(rr = vals$lo) else run_with_mult(dr = vals$lo)
  nmb_hi <- if (nm == "rr_npy_che") run_with_mult(rr = vals$hi) else run_with_mult(dr = vals$hi)
  data.frame(Parameter = nm, Base_value = vals$base, Low_value = vals$lo, High_value = vals$hi,
             NMB_base = nmb_base_ri, NMB_low = nmb_lo, NMB_high = nmb_hi, NMB_range = abs(nmb_hi - nmb_lo))
}))

owsa_results <- bind_rows(owsa_results, owsa_mult_results) %>% arrange(desc(NMB_range))
write_xlsx(owsa_results, file.path(out_psa, "OWSA_NMB_results.xlsx"))
cat("OK - OWSA results saved (tornado chart built in 03_plots.R)\n")

# ============================================================
# SECTION 10 — COVERAGE SCALE-UP PLOT
# ============================================================
cat("\n=== Coverage scale-up plot ===\n")
p_ri_delayed_base <- as.numeric(base[["p_ri_delayed"]])
p_ri_nr_base      <- as.numeric(base[["p_ri_nr"]])
remaining_base    <- p_ri_delayed_base + p_ri_nr_base
coverage_seq      <- seq(as.numeric(base[["p_soc_timely"]]), 1, by = 0.01)

coverage_results <- do.call(rbind, lapply(coverage_seq, function(pt) {
  p <- base
  p[["p_ri_timely"]] <- pt
  remaining <- 1 - pt
  if (remaining_base > 0) {
    p[["p_ri_delayed"]] <- remaining * (p_ri_delayed_base / remaining_base)
    p[["p_ri_nr"]]      <- remaining * (p_ri_nr_base      / remaining_base)
  } else {
    p[["p_ri_delayed"]] <- 0; p[["p_ri_nr"]] <- remaining
  }
  r   <- run_model(p, compute_qalys(p), or_che_outcome, "marginal", NULL)
  nmb <- wtp * (r$qaly_ri - r$qaly_nonpy) - (r$cost_ri - r$cost_nonpy)
  data.frame(p_timely = pt, cost_ri = r$cost_ri, qaly_ri = r$qaly_ri, nmb = nmb, che = r$cat_ri)
}))

write_xlsx(coverage_results, file.path(out_tables, "coverage_scaleup_data.xlsx"))
cat("OK - Coverage scale-up data saved (plot built in 03_plots.R)\n")

# ============================================================
# SECTION 11 — CHE AND DEATHS SUMMARY TABLES
# ============================================================
# Deaths get the same PSA-level (mean/SD) summary treatment as CHE,
# so both outcomes are reported in parallel.
che_summary <- data.frame(
  Strategy        = strats,
  Prob_CHE        = round(cats_v, 6),
  CHE_Averted_pp  = round(br$cat_nonpy - cats_v, 6),
  CHE_Averted_pop = round((br$cat_nonpy - cats_v) * cohort_size, 1),
  PSA_mean_dCHE   = c(NA, round(mean(psa_raw$d_cat_soc), 6), round(mean(psa_raw$d_cat_ri), 6), round(mean(psa_raw$d_cat_ii), 6)),
  PSA_sd_dCHE     = c(NA, round(sd(psa_raw$d_cat_soc), 6),   round(sd(psa_raw$d_cat_ri), 6),   round(sd(psa_raw$d_cat_ii), 6))
)
cat("\n=== CHE Summary ===\n"); print(che_summary, row.names = FALSE)
write_xlsx(che_summary, file.path(out_tables, "CHE_summary.xlsx"))

deaths_summary <- data.frame(
  Strategy          = strats,
  Prob_Death        = round(deaths_v, 6),
  Deaths_Averted_pp  = round(br$death_nonpy - deaths_v, 6),
  Deaths_Averted_pop = round((br$death_nonpy - deaths_v) * cohort_size, 1),
  PSA_mean_dDeaths   = c(NA, round(mean(psa_raw$d_death_soc), 6), round(mean(psa_raw$d_death_ri), 6), round(mean(psa_raw$d_death_ii), 6)),
  PSA_sd_dDeaths     = c(NA, round(sd(psa_raw$d_death_soc), 6),   round(sd(psa_raw$d_death_ri), 6),   round(sd(psa_raw$d_death_ii), 6))
)
cat("\n=== Deaths Summary ===\n"); print(deaths_summary, row.names = FALSE)
write_xlsx(deaths_summary, file.path(out_tables, "deaths_summary.xlsx"))
cat("OK - CHE and deaths summaries saved\n")

# ============================================================
# SECTION 12 — SAVE ENVIRONMENT FOR SCENARIOS SCRIPT
# ============================================================
# Only DATA is saved here, not functions. 04_scenarios.R sources
# 00_config.R and 01_helpers.R directly for its logic, so the exact
# same function code always runs in both scripts, avoiding the risk
# of the scenarios script silently running an outdated copy of a
# function if 02_model.R were edited but not re-run first.
rds_path <- file.path(out_rds, "model_env.rds")
saveRDS(
  list(
    cat_params = cat_params, cat_ci_lower = cat_ci_lower, cat_ci_upper = cat_ci_upper,
    p_nr_dstb_cat = p_nr_dstb_cat, p_nr_drtb_cat = p_nr_drtb_cat,
    base = base, base_all = base_all, br = br,
    strats = strats, costs_v = costs_v, qalys_v = qalys_v, cats_v = cats_v, deaths_v = deaths_v,
    n_psa = n_psa, cohort_size = cohort_size,
    ci_lower = ci_lower, ci_upper = ci_upper,
    beta_params_active = beta_params_active, cost_params = cost_params,
    dirichlet_groups = dirichlet_groups, dirichlet_group_names = dirichlet_group_names,
    prob_groups = prob_groups,
    root = root, out_root = out_root, out_tables = out_tables, out_plots = out_plots, out_psa = out_psa, out_rds = out_rds
  ),
  file = rds_path
)
cat(sprintf("OK - environment saved to: %s\n", rds_path))

# Fingerprint of the input Excel at the moment this pipeline run used
# it, so a later document (e.g. a runnable Methods/Results markdown)
# can detect if the Excel has since been edited without a fresh
# pipeline run — the class of staleness bug that produced the
# $94.93/$94.31 mismatch earlier (13 Aug 2026, see WORKLOG.md).
writeLines(
  as.character(tools::md5sum(file.path(root, "input", "model_input_parameters.xlsx"))),
  file.path(out_rds, "model_input_parameters_md5.txt")
)

cat("\n========================================================\n")
cat("PRIMARY MODEL COMPLETE.\n")
cat(sprintf("  WTP=$%g | Horizon=Year-1\n", wtp))
cat("  Next step: run 03_plots.R (all plots), then 04_scenarios.R\n")
cat("========================================================\n")
