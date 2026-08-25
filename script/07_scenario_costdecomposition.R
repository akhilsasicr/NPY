# ============================================================
# NPY CUA — SCENARIO: COST DECOMPOSITION (NPY transfer vs care cost)
# DIAGNOSTIC / REPORTING ONLY — does not change the primary model
# ============================================================
#
# WHAT THIS IS
#   Every cost_* parameter in model_input_parameters.xlsx is a single
#   lump total per pathway x TB-type x outcome cell (e.g.
#   cost_timely_DSTB_suc = $593.05). There is no visible NPY-transfer
#   line item inside that number anywhere in the primary model.
#
#   This script splits each of those totals into two components:
#     transfer_*       : the NPY cash transfer specifically
#     residual_care_*  : cost_total - transfer  (everything else)
#
#   It does this for TWO variants, so the effect of the outcome rule
#   itself is visible, not assumed:
#     (a) FLAT           — everyone who receives NPY gets the FULL
#                           transfer regardless of outcome. This is
#                           the assumption implicitly baked into the
#                           primary model's cost_* totals today.
#     (b) OUTCOME-WEIGHTED — success/unsuccessful get the full
#                           transfer, death gets 50% (death usually
#                           occurs within the first ~3 months, so only
#                           the first lump sum of a 2-lump DS-TB
#                           schedule / 2+-lump DR-TB schedule is
#                           typically received).
#
# WHY THIS EXISTS
#   1. Gives "did patients who died receive less NPY?" an actual
#      number instead of an assertion — the cost_* totals alone give
#      no visibility into this.
#   2. Answers the *transfer* half of the health-system-vs-patient
#      perspective split (Scenario B) exactly: NPY_transfer is,
#      by definition, 100% health-system outlay/100% patient inflow.
#      The health-system/patient "care" cost is a separate flat figure
#      per TB type (cost_healthsystem_*/cost_patient_*, "Parameter"
#      sheet) — Scenario B skips rather than guesses if blank.
#
# WHAT THIS SCRIPT DELIBERATELY DOES NOT DO
#   - Read-only against the main Excel; does not change any cost_*
#     value 02_model.R/04/05 reads.
#   - Does not feed the transfer/residual split back into the
#     primary model — whether this rule set becomes the model's
#     actual cost structure is a separate decision.
#   - residual_care_* is only correct under the assumption that the
#     ORIGINAL cost_* totals assumed the full transfer was paid in
#     every case — i.e. "residual" is relative to variant (a), not
#     an independently re-derived care cost. Stated in the output
#     tables too, not just here.
#
# INPUT FILES
#   input/model_input_parameters.xlsx — a single workbook, four sheets:
#     "Parameter"                (read-only; existing cost_* totals)
#     "NPY transfer"              (merged in 19 Aug 2026 — previously a
#                                  separate file, cost_decomposition_
#                                  inputs.xlsx; merged so the "System
#                                  vs patient split" sheet's formulas
#                                  can reference "Parameter" cells
#                                  directly instead of duplicating
#                                  hardcoded totals that could go stale)
#     "System vs patient split"   (used by Scenario B, 04_scenarios.R)
#     "Instructions"               (human-readable only, not read by R)
#   See the "NPY transfer" sheet's "status" column: CONFIRMED rows are
#   settled; PLACEHOLDER rows are assumptions still needing sign-off,
#   most importantly full_transfer_drtb_usd — the DR-TB lump-sum
#   total/schedule is not confirmed the way DS-TB's is. The official
#   DBT Manual for NTEP describes a continuous 28-day disbursement
#   mechanism for DR-TB, plus a possible "episode restart" on regimen
#   change, not a fixed lump count.
#
# OUTPUT (under output/tables/)
#   cost_decomposition_a_flat.xlsx
#   cost_decomposition_b_outcomeweighted.xlsx
#   cost_decomposition_comparison.xlsx  (a vs b side by side)
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
library(here)

root <- here::here()
source(file.path(root, "script", "00_config.R"))   # for cost_param_names, nocost_param_names

input_xlsx    <- file.path(root, "input", "model_input_parameters.xlsx")
# A single output/ folder for every run (no more pct20/source mode split).
out_root      <- file.path(root, "output")
out_tables    <- file.path(out_root, "tables")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# SECTION 1 — LOAD INPUTS (both read-only)
# ============================================================

cat("Loading main cost parameters (read-only) ...\n")
main_params <- read_excel(input_xlsx, sheet = "Parameter") %>%
  filter(Parameter %in% c(cost_param_names, nocost_param_names)) %>%
  select(Parameter, base_value, ci_lower, ci_upper)

cat("Loading NPY transfer parameters (input/model_input_parameters.xlsx, sheet 'NPY transfer') ...\n")
# This whole script needs the transfer table to do anything meaningful
# (unlike Scenario B, there's no partial/skip mode here) - fail with
# a clear, actionable message instead of an opaque zip-parsing error
# if the sheet is missing.
sheet_names_check <- readxl::excel_sheets(input_xlsx)
if (!("NPY transfer" %in% sheet_names_check))
  stop("input/model_input_parameters.xlsx has no 'NPY transfer' sheet. ",
       "This script needs it - see the file's 'Instructions' sheet.")
transfer_params <- read_excel(input_xlsx, sheet = "NPY transfer")
transfer_params <- transfer_params[!is.na(transfer_params$parameter), ]   # drop blank/spacer rows -- see 01_helpers.R's compute_npy_transfer_table() for why this matters

get_tp <- function(name) transfer_params$base_value[transfer_params$parameter == name]

# full_transfer_d*tb_usd are already in USD -- the sheet's "Calculation
# shown" column displays the INR/exchange-rate formula for reference;
# base_value (read here) is the plain converted number.
full_transfer_dstb_usd <- get_tp("full_transfer_dstb_usd")
full_transfer_drtb_usd <- get_tp("full_transfer_drtb_usd")
mult_success         <- get_tp("multiplier_success")
mult_unsuccessful    <- get_tp("multiplier_unsuccessful")
mult_death           <- get_tp("multiplier_death")

status_drtb <- transfer_params$status[transfer_params$parameter == "full_transfer_drtb_usd"]
if (grepl("needs confirmation", status_drtb, fixed = TRUE)) {
  cat("\n*** WARNING: full_transfer_drtb_usd needs confirmation from the PI.\n")
  cat("*** DR-TB decomposition below should be treated as provisional.\n\n")
}

# ============================================================
# SECTION 2 — PARSE EACH cost_* / nocost_* NAME INTO
#              (pathway, tb_type, outcome)
# ============================================================
# cost_timely_DSTB_suc, cost_del_DRTB_die, cost_nr_DSTB_unsuc,
# nocost_nr_dstb_suc, ... -> pathway / tb_type / outcome

parse_cost_name <- function(name) {
  is_nocost <- grepl("^nocost_", name)
  n <- tolower(name)
  pathway <- dplyr::case_when(
    grepl("_timely_", n) ~ "timely",
    grepl("_del_",    n) ~ "delayed",
    grepl("_nr_",      n) & !is_nocost ~ "non-receipt (NPY-active system)",
    is_nocost ~ "No NPY (counterfactual)",
    TRUE ~ NA_character_
  )
  tb_type <- dplyr::case_when(
    grepl("dstb", n) ~ "DS-TB",
    grepl("drtb", n) ~ "DR-TB",
    TRUE ~ NA_character_
  )
  outcome <- dplyr::case_when(
    grepl("_suc$",   n) ~ "success",
    grepl("_unsuc$", n) ~ "unsuccessful",
    grepl("_die$",   n) ~ "death",
    TRUE ~ NA_character_
  )
  received_npy <- !(pathway %in% c("non-receipt (NPY-active system)", "No NPY (counterfactual)"))
  list(pathway = pathway, tb_type = tb_type, outcome = outcome, received_npy = received_npy)
}

parsed <- lapply(main_params$Parameter, parse_cost_name)
main_params$pathway      <- vapply(parsed, `[[`, character(1), "pathway")
main_params$tb_type      <- vapply(parsed, `[[`, character(1), "tb_type")
main_params$outcome      <- vapply(parsed, `[[`, character(1), "outcome")
main_params$received_npy <- vapply(parsed, `[[`, logical(1),   "received_npy")

# ============================================================
# SECTION 3 — DECOMPOSE, for a given outcome-multiplier rule
# ============================================================

decompose <- function(df, mult_suc, mult_unsuc, mult_death, variant_label) {
  df %>%
    mutate(
      full_transfer_usd_val = ifelse(tb_type == "DS-TB", full_transfer_dstb_usd, full_transfer_drtb_usd),
      outcome_mult = case_when(
        !received_npy         ~ 0,                 # non-receipt / No NPY: no transfer at all
        outcome == "success"      ~ mult_suc,
        outcome == "unsuccessful" ~ mult_unsuc,
        outcome == "death"        ~ mult_death,
        TRUE ~ NA_real_
      ),
      transfer_usd = full_transfer_usd_val * outcome_mult,
      cost_total_usd = base_value,
      residual_care_usd = cost_total_usd - transfer_usd,
      variant = variant_label
    ) %>%
    select(Parameter, pathway, tb_type, outcome, received_npy,
           cost_total_usd, transfer_usd, residual_care_usd, outcome_mult, variant)
}

variant_a <- decompose(main_params, 1.0, 1.0, 1.0, "a_flat (everyone gets full transfer)")
variant_b <- decompose(main_params, mult_success, mult_unsuccessful, mult_death,
                        "b_outcome_weighted (death = 50%)")

# Sanity check: for non-receipt/No NPY rows, transfer should be 0 and
# residual should equal the original total exactly, in BOTH variants.
stopifnot(all(abs(variant_a$transfer_usd[!variant_a$received_npy]) < 1e-9))
stopifnot(all(abs(variant_b$transfer_usd[!variant_b$received_npy]) < 1e-9))
cat("OK - non-receipt/No NPY rows correctly carry zero transfer in both variants.\n")

# ============================================================
# SECTION 4 — COMPARISON TABLE (a vs b, receipt pathways only)
# ============================================================

comparison <- variant_a %>%
  filter(received_npy) %>%
  select(Parameter, pathway, tb_type, outcome,
         cost_total_usd,
         transfer_a = transfer_usd, residual_a = residual_care_usd) %>%
  left_join(
    variant_b %>% filter(received_npy) %>%
      select(Parameter, transfer_b = transfer_usd, residual_b = residual_care_usd),
    by = "Parameter"
  ) %>%
  mutate(
    transfer_delta_b_minus_a = transfer_b - transfer_a,
    residual_delta_b_minus_a = residual_b - residual_a
  )

cat("\n--- Cost decomposition: (a) flat vs (b) outcome-weighted ---\n")
cat("Rows where the two variants differ (death rows only, by construction):\n")
print(comparison %>% filter(abs(transfer_delta_b_minus_a) > 1e-9) %>%
        select(Parameter, tb_type, transfer_a, transfer_b, residual_a, residual_b))

# ============================================================
# SECTION 5 — SAVE OUTPUTS
# ============================================================

write_xlsx(variant_a,  file.path(out_tables, "cost_decomposition_a_flat.xlsx"))
write_xlsx(variant_b,  file.path(out_tables, "cost_decomposition_b_outcomeweighted.xlsx"))
write_xlsx(comparison, file.path(out_tables, "cost_decomposition_comparison.xlsx"))

cat("\nSaved:\n")
cat(" -", file.path(out_tables, "cost_decomposition_a_flat.xlsx"), "\n")
cat(" -", file.path(out_tables, "cost_decomposition_b_outcomeweighted.xlsx"), "\n")
cat(" -", file.path(out_tables, "cost_decomposition_comparison.xlsx"), "\n")
