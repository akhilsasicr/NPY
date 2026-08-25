# ============================================================
# NPY CUA — SCENARIO: WTP THRESHOLD SENSITIVITY (primary model)
# ============================================================
#
# WHY THIS IS ITS OWN SCRIPT
#   05_scenario_C_premature_death_qaly.R reports the lifetime-QALY
#   model's ICER/P(CE) against both the GDP-per-capita threshold
#   ($2,536) and the health-system opportunity-cost threshold
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
    Clears_OpportunityCost_threshold_2019 = icer < wtp_opportunity_cost,
    P_CE_at_GDP_threshold_2026          = pce_at(wtp, d_cost, d_qaly),
    P_CE_at_OpportunityCost_threshold_2019 = pce_at(wtp_opportunity_cost, d_cost, d_qaly)
  )
}))

print(results, row.names = FALSE)
cat(sprintf("\nGDP threshold = %s | Opportunity-cost threshold = %s\n", wtp_label, wtp_opportunity_cost_label))

# WTP opportunity-cost threshold candidates and independent Ochalek
# (2026) replication check -- built here so both tables are ready
# before the write_xlsx() call below. Full citations/discussion in
# the comment block further down this file.
wtp_candidates <- data.frame(
  Value_USD   = c(2536, 487, NA, 609, 2534),
  Price_Year  = c(2026, 2019, 2025, 2026, 2019),
  Label       = c(
    "GDP per capita (base case, used throughout this project)",
    "Opportunity cost, Pichon-Riviere et al. 2023 (USED as secondary threshold)",
    "Opportunity cost, Pichon-Riviere et al. 2025 update (IMF-projection method; no exact figure disclosed)",
    "Opportunity cost, Ochalek 2026 preprint (NOT peer-reviewed; not independently reproduced -- see replication below)",
    "Pichon-Riviere et al. 2023's own hypothetical India example (REJECTED -- assumes a health-spend target India never reached)"
  ),
  Source_DOI  = c(
    "IMF WEO April 2025 projection",
    "10.1016/S2214-109X(23)00162-6",
    "10.1017/S0266462325100755",
    "10.64898/2026.03.31.26349880",
    "10.1016/S2214-109X(23)00162-6"
  ),
  Adopted     = c(TRUE, TRUE, FALSE, FALSE, FALSE)
)

beta_ochalek   <- 0.21
ghe_pc_india   <- 33.05
daly_pc_india  <- 0.3356
daly_pc_lo     <- 30382.52 / 100000
daly_pc_hi     <- 37071.39 / 100000

cost_per_daly_central <- ghe_pc_india / (beta_ochalek * daly_pc_india)
cost_per_daly_lo      <- ghe_pc_india / (beta_ochalek * daly_pc_hi)   # higher DALY_pc -> lower cost
cost_per_daly_hi      <- ghe_pc_india / (beta_ochalek * daly_pc_lo)   # lower DALY_pc -> higher cost

replication_row <- data.frame(
  Beta_DALY_elasticity        = beta_ochalek,
  GHE_pc_2023_USD             = ghe_pc_india,
  DALY_pc_2023                = daly_pc_india,
  Replicated_Cost_per_DALY    = round(cost_per_daly_central, 1),
  Replicated_Range_Low        = round(cost_per_daly_lo, 1),
  Replicated_Range_High       = round(cost_per_daly_hi, 1),
  Ochalek_2026_Published      = 609,
  Discrepancy_USD             = round(609 - cost_per_daly_central, 1),
  Note = "See script comments below for full sourcing and discussion."
)

write_xlsx(list(
  results = results,
  note = data.frame(Note = c(
    paste0("GDP-per-capita threshold: ", wtp_label, " (India GDP per capita at current prices)."),
    paste0("Opportunity-cost threshold: ", wtp_opportunity_cost_label, " (health-system opportunity-cost estimate for India, Pichon-Riviere et al. 2023).")
  )),
  wtp_candidates       = wtp_candidates,
  ochalek_replication  = replication_row
), out_tables_path)
cat(sprintf("\nOK - saved to %s\n", out_tables_path))

cat("\nThis table is the PRIMARY (year-1) model only. For the lifetime-QALY\n")
cat("model's threshold sensitivity, see 05_scenario_C_premature_death_qaly.R.\n")

# ============================================================
# WTP OPPORTUNITY-COST THRESHOLD: FULL PROVENANCE, CANDIDATE
# VALUES, AND AN INDEPENDENT REPLICATION CHECK
# ============================================================
#
# BASE CASE VS. "WHAT-IF" COMPARATORS
#   wtp ($2,536, 2026 USD, 00_config.R) is the base case this whole
#   project is built around -- India GDP per capita, IMF World
#   Economic Outlook, April 2025 projection. Every threshold listed
#   below is a secondary, more-conservative "what if" comparator,
#   never a replacement for the base case.
#
# LINEAGE OF THE OPPORTUNITY-COST THRESHOLD (oldest to newest)
#
#   1. Ochalek J, Lomas J, Claxton K (2018). "Estimating health
#      opportunity costs in low-income and middle-income countries:
#      a novel approach and evidence from cross-country data."
#      BMJ Global Health 3(6):e000964.
#      DOI: https://doi.org/10.1136/bmjgh-2018-000964
#      -> Originates the cross-country opportunity-cost methodology.
#         Does not itself report an India figure used in this
#         project; supplies the DALY elasticity (Table 2: -0.21)
#         used in the replication check below.
#
#   2. Pichon-Riviere A, Drummond M, Palacios A, Garcia-Marti S,
#      Augustovski F (2023). "Determining the efficiency path to
#      universal health coverage: cost-effectiveness thresholds for
#      174 countries based on growth in life expectancy and health
#      expenditures." Lancet Global Health 11:e833-e842.
#      DOI: https://doi.org/10.1016/S2214-109X(23)00162-6
#      -> Table 3: India = $487/QALY (95% range $249-$618), reported
#         explicitly in 2019 USD. THIS IS THE VALUE THIS PROJECT
#         USES (wtp_opportunity_cost, 00_config.R). A related but
#         methodologically distinct cross-country approach from
#         Ochalek 2018, not a direct application of it.
#
#   3. Pichon-Riviere A, Drummond M, Garcia Marti S, Augustovski F
#      (2025). "Updated cost-effectiveness threshold estimates for
#      174 countries based on projected growth in life expectancy,
#      health expenditure, and gross domestic product." OP17,
#      International Journal of Technology Assessment in Health
#      Care. DOI: https://doi.org/10.1017/S0266462325100755
#      -> Same authors as #2, but abandons the historical
#         bracket-median method for IMF growth-projection targets.
#         States India's threshold is "adjusted upward" but the
#         available abstract discloses no exact figure. NOT ADOPTED
#         here -- no verifiable number to adopt.
#
#   4. Ochalek J (2026). "Updated health opportunity cost estimates
#      for 92 low- and middle-income countries: implications for
#      global health financing and donor allocation." medRxiv
#      preprint, posted 2 Apr 2026, NOT PEER-REVIEWED.
#      DOI: https://doi.org/10.64898/2026.03.31.26349880
#      -> India = $609 per DALY averted (2026 USD), from her own
#         2018 elasticity (-0.21, unchanged) applied to updated
#         2023 GHE/GBD data. NOT ADOPTED here: (a) preprint status,
#         (b) our own independent replication below does not
#         reproduce this figure from public data.
#
# CANDIDATE VALUES CONSIDERED AND REJECTED (for completeness)
#   - $2,534/QALY: Pichon-Riviere et al. (2023)'s own illustrative
#     India example (paper page e840), assuming India raises health
#     expenditure to 5% of GDP within 5 years at 6% GDP growth. This
#     is a hypothetical target, not an observed trajectory -- India's
#     health spend has stayed ~3-3.5% of GDP for years. REJECTED as
#     not a valid current or realistic estimate.
#
# INDEPENDENT REPLICATION OF OCHALEK (2026)'S $609 FIGURE
#   Formula (Ochalek et al. 2018 elasticity framework):
#     Cost per DALY averted = GHE_pc / (beta * DALY_pc)
#   where beta is the DALY elasticity magnitude and GHE_pc / DALY_pc
#   are government health expenditure and disease-burden per capita.
#
#   Inputs (independently sourced, not from Ochalek's own data/code,
#   which was not accessible beyond the preprint's published text):
#     beta      = 0.21     [Ochalek, Lomas & Claxton 2018, Table 2,
#                           DALY row, average estimate]
#     GHE_pc    = 33.05    [World Bank, indicator SH.XPD.GHED.PC.CD,
#                           "Domestic general government health
#                           expenditure per capita (current US$)",
#                           India, 2023 -- government/compulsory
#                           schemes only, matching Ochalek's
#                           definition, NOT the broader $84.69 CHE
#                           figure used elsewhere in this project's
#                           Pichon-Riviere-equation recalculation]
#     DALY_pc   = 0.3356   [IHME GBD 2023 Results Tool, direct query:
#                           India, All causes, Both sexes, All ages,
#                           2023, Rate = 33,556.58 per 100,000
#                           (95% UI 30,382.52-37,071.39)]
#
#   Result: 33.05 / (0.21 * 0.3356) = ~$469/DALY (2023 USD), range
#   ~$425-$518 using the GBD uncertainty interval on the DALY rate.
#   This is meaningfully BELOW Ochalek's own published $609 (2026
#   USD) -- even allowing ~9-10% forward inflation to 2026 USD only
#   reaches ~$515. The gap is not resolved: possible causes include
#   her averaging across four DALY measures (DALY 1-4) rather than
#   DALY 4 alone, a different GHE/GBD data vintage, or an elasticity
#   not reducible to the single point estimate used here.
#
#   This section does NOT change wtp_opportunity_cost or any model
#   result -- it is a documented, reproducible disclosure only.

cat("\n=== WTP opportunity-cost threshold: candidate values (see script comments for full citations) ===\n")
print(wtp_candidates, row.names = FALSE)
cat("\n=== Independent replication of Ochalek (2026) preprint's India $609/DALY figure ===\n")
print(replication_row, row.names = FALSE)
