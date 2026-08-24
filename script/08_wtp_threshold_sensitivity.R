# ============================================================
# NPY CUA — SCENARIO: WTP THRESHOLD SENSITIVITY (primary model)
# ============================================================
#
# WHY THIS IS ITS OWN SCRIPT
#   05_scenario_C_premature_death_qaly.R reports the lifetime-QALY
#   model's ICER/P(CE) against both the GDP-per-capita threshold
#   ($2,536) and the Ochalek-lineage opportunity-cost threshold
#   ($487) — but it only ever applies the $487 threshold to the
#   LIFETIME-EXTENDED numbers, never to the PRIMARY (year-1) model on
#   its own. That conflates two separate, independent questions:
#     - horizon sensitivity: does extending to lifetime QALYs change
#       the answer, holding the threshold fixed?
#     - threshold sensitivity: does the primary model, UNCHANGED,
#       still clear a more conservative threshold?
#   Testing only "lifetime QALYs AND the lower threshold together"
#   answers a third, weaker question — "does the best case of both
#   relaxations clear the bar" — without showing which one is doing
#   the work. This script isolates threshold sensitivity alone: same
#   primary model, same PSA, just swap the WTP.
#
# WHAT THIS DOES
#   Takes the primary model's own base case and PSA (from 02_model.R,
#   unmodified — no lifetime-QALY extension, no other change) and
#   reports ICER + P(cost-effective) at both wtp ($2,536) and
#   wtp_opportunity_cost ($487), both from 00_config.R, for all three
#   NPY strategies vs No NPY.
#
# INPUT: <out_root>/rds/model_env.rds, <out_root>/psa/PSA_raw_results.xlsx
#        (out_root is output/; both written by 02_model.R). Does not
#        re-run the PSA or change any base-case number.
# OUTPUT: <out_root>/tables/wtp_threshold_sensitivity.xlsx
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
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

out_tables_path <- file.path(out_root, "tables", "wtp_threshold_sensitivity.xlsx")

cat("\n=== WTP threshold sensitivity — PRIMARY (year-1) model, unchanged ===\n")
cat(sprintf("Thresholds compared: GDP-per-capita %s  |  opportunity-cost %s\n\n", wtp_label, wtp_opportunity_cost_label))

icer_vs_nonpy <- function(cost, qaly, cost0, qaly0) (cost - cost0) / (qaly - qaly0)

strategies <- list(
  "Current NPY"      = list(cost = br$cost_soc, qaly = br$qaly_soc),
  "Realistic Impr."  = list(cost = br$cost_ri,  qaly = br$qaly_ri),
  "Ideal Impr."      = list(cost = br$cost_ii,  qaly = br$qaly_ii)
)

pce_at <- function(w, d_cost, d_qaly) round(100 * mean((w * d_qaly - d_cost) > 0), 1)

results <- do.call(rbind, lapply(names(strategies), function(nm) {
  s <- strategies[[nm]]
  icer <- icer_vs_nonpy(s$cost, s$qaly, br$cost_nonpy, br$qaly_nonpy)

  d_cost <- switch(nm,
    "Current NPY"     = psa_raw$cost_soc - psa_raw$cost_nonpy,
    "Realistic Impr." = psa_raw$cost_ri  - psa_raw$cost_nonpy,
    "Ideal Impr."     = psa_raw$cost_ii  - psa_raw$cost_nonpy)
  d_qaly <- switch(nm,
    "Current NPY"     = psa_raw$qaly_soc - psa_raw$qaly_nonpy,
    "Realistic Impr." = psa_raw$qaly_ri  - psa_raw$qaly_nonpy,
    "Ideal Impr."     = psa_raw$qaly_ii  - psa_raw$qaly_nonpy)

  data.frame(
    Strategy                            = nm,
    ICER_vs_NoNPY                       = round(icer, 2),
    Clears_GDP_threshold_2026           = icer < wtp,
    Clears_OpportunityCost_threshold_2025 = icer < wtp_opportunity_cost,
    P_CE_at_GDP_threshold_2026          = pce_at(wtp, d_cost, d_qaly),
    P_CE_at_OpportunityCost_threshold_2025 = pce_at(wtp_opportunity_cost, d_cost, d_qaly)
  )
}))

print(results, row.names = FALSE)
cat(sprintf("\nGDP threshold = %s | Opportunity-cost threshold = %s\n", wtp_label, wtp_opportunity_cost_label))
write_xlsx(list(
  results = results,
  note = data.frame(Note = c(
    paste0("GDP-per-capita threshold: ", wtp_label, " (India GDP per capita at current prices)."),
    paste0("Opportunity-cost threshold: ", wtp_opportunity_cost_label, " (Ochalek-lineage cross-country update).")
  ))
), out_tables_path)
cat(sprintf("\nOK - saved to %s\n", out_tables_path))

cat("\nThis table is the PRIMARY (year-1) model only. For the lifetime-QALY\n")
cat("model's threshold sensitivity, see 05_scenario_C_premature_death_qaly.R.\n")
