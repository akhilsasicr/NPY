# ============================================================
# TB Decision Analytic Model — NPY
# VALUE OF INFORMATION: EVPI and EVPPI
# ============================================================
#
# PRE-REQUISITE: run 02_model.R first. It saves
# <out_root>/rds/model_env.rds AND <out_root>/rds/evppi_inputs.rds
# (out_root is output/) — evppi_inputs.rds is a small subsample of PSA
# costs/effects/parameter draws built specifically for this script —
# see 00_config.R, n_psa_evppi, for why it is a subsample rather than
# the full PSA.
# This script does NOT depend on 04_scenarios.R or
# 05_scenario_C_premature_death_qaly.R — it can be run any time after
# 02_model.R, independently, same pattern as Scenario C.
#
# A distinct question from 02_model.R ("what is the answer"): EVPI/
# EVPPI ask "given remaining uncertainty, which parameter (if any) is
# worth researching further" — kept in its own file, same pattern as
# Scenario C being kept out of 04_scenarios.R.
#
# Section 2 — EVPI: max a decision-maker should pay to eliminate ALL
#   parameter uncertainty at once, swept across WTP thresholds.
# Section 3 — EVPPI: how much of that EVPI comes from uncertainty in
#   ONE specific parameter, at the primary WTP — tells the PI which
#   parameter is most worth pinning down with further data.
#
# Both use a subsample (n_psa_evppi rows), not the full PSA:
# dampack::calc_evppi() fits a GAM per parameter, and a moderate
# sample gives a stable estimate at far lower compute (see
# 00_config.R). 02_model.R saves this subsample to evppi_inputs.rds;
# this script rebuilds the dampack PSA object from it.
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
# SECTION 0 — LOAD MODEL ENVIRONMENT + EVPI/EVPPI INPUTS
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

evppi_inputs_path <- file.path(out_rds, "evppi_inputs.rds")
if (!file.exists(evppi_inputs_path))
  stop(evppi_inputs_path, " not found. Please run 02_model.R first ",
       "(it now saves this file alongside model_env.rds).")

evppi_inputs <- readRDS(evppi_inputs_path)
costs_df   <- evppi_inputs$costs_df
effects_df <- evppi_inputs$effects_df
params_df  <- evppi_inputs$params_df
wtp_range  <- evppi_inputs$wtp_range
n_evppi_use <- evppi_inputs$n_psa_evppi_actual
sampled_param_names <- names(params_df)

cat(sprintf("OK - EVPI/EVPPI inputs loaded: %d of %d PSA draws (%d sampled parameters)\n",
            n_evppi_use, n_psa, length(sampled_param_names)))

# out_plots/out_psa/out_tables all come from the RDS above
# (they are the SAME paths 02_model.R used), so EVPI/EVPPI outputs
# land in exactly the same folders they always have — nothing
# downstream that references these paths needs to change.

# ============================================================
# SECTION 1 — dampack PSA OBJECT (rebuilt from the saved subsample)
# ============================================================
psa_obj_evppi <- suppressWarnings(make_psa_obj(
  cost          = costs_df,
  effectiveness = effects_df,
  parameters    = params_df,
  strategies    = c("No NPY", "Current NPY", "Realistic Impr.", "Ideal Impr.")
))
cat(sprintf("OK - EVPI/EVPPI PSA object rebuilt from %d draws\n", n_evppi_use))

# ============================================================
# SECTION 2 — EVPI (aggregate)
# ============================================================
# Expected Value of Perfect Information: the maximum a decision-
# maker should be willing to pay to eliminate ALL parameter
# uncertainty at once, at each WTP threshold. High EVPI signals
# that more research (of any kind) could still change the decision.
# Uses psa_obj_evppi (n_psa_evppi draws) — see the header note above
# for why the subsample, not the full PSA, is used here.
evpi_obj <- calc_evpi(wtp = wtp_range, psa = psa_obj_evppi)
evpi_df  <- as.data.frame(evpi_obj)
y_pos    <- max(evpi_df$EVPI, na.rm = TRUE) * 0.1
if (y_pos == 0) y_pos <- diff(range(wtp_range)) * 0.02

evpi_plot <- ggplot(evpi_df, aes(x = WTP, y = EVPI)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey40") +
  annotate("text", x = wtp * 0.85, y = y_pos, label = paste0("WTP\n", wtp_label),
           size = 3, hjust = 0, colour = "grey40") +
  scale_x_continuous(limits = c(0, ceiling(wtp * 2)),
                     breaks = pretty(c(0, ceiling(wtp * 2)), n = 6), labels = scales::comma) +
  labs(title = "Expected Value of Perfect Information (EVPI)",
       x = "Willingness-to-Pay (USD/QALY)", y = "EVPI (USD)") +
  theme_bw(base_size = 12)
ggsave(file.path(out_plots, "evpi.png"), evpi_plot, width = 8, height = 5, dpi = 150)
cat("OK - EVPI (aggregate) saved\n")

# ============================================================
# SECTION 3 — EVPPI (parameter-level)
# ============================================================
# Expected Value of Partial Perfect Information: how much of the
# aggregate EVPI (above) is attributable to uncertainty in ONE
# specific parameter (or a small group of them). This tells the
# PI which single parameter would be most valuable to pin down with
# further data collection — as opposed to EVPI, which only says
# "there is value in reducing uncertainty overall."
#
# COMPUTED AT THE PRIMARY WTP ONLY ($wtp, not swept across
# wtp_range): EVPPI requires fitting a regression metamodel (a GAM,
# via dampack::calc_evppi) separately for EACH parameter. Sweeping
# that across ~100 WTP values for every parameter would be very
# slow for little practical benefit — a single EVPPI ranking at the
# threshold that actually matters for the decision ($wtp) is the
# standard use case.
#
# NMB used here is Realistic Improvement vs No NPY (the strategy
# the PI is most focused on), matching the outcome already used for
# OWSA in 02_model.R (nmb_base_ri).
#
# If dampack's calc_evppi() signature differs from what is called
# here (it has changed across package versions), this section will
# print a clear warning and skip EVPPI rather than crashing the
# rest of the script.
cat("\n=== Running EVPPI (parameter-level, at WTP=$", round(wtp), ") ===\n", sep = "")

# dampack::calc_evppi() (verified against dampack 1.0.2) returns a
# list of class "evppi" whose numeric result is in the $df_evppi
# element — a 1-row data frame with columns WTP and EVPPI (one row
# since a single wtp is passed). $metamodel_ls holds the fitted GAM
# objects and is not needed here. extract_evppi() below also handles
# two older/alternate return shapes defensively, in case dampack's
# format changes again in a future version.
extract_evppi <- function(res) {
  if (is.list(res) && !is.null(res$df_evppi) && is.data.frame(res$df_evppi))
    return(as.numeric(res$df_evppi$EVPPI[1]))
  # Fallbacks, in case a different dampack version changes the shape:
  if (is.list(res) && !is.null(res$evppi)) {
    v <- res$evppi
    if (is.data.frame(v)) return(as.numeric(v[["EVPPI"]][1]))
    return(as.numeric(v)[1])
  }
  if (is.data.frame(res) && "EVPPI" %in% names(res))
    return(as.numeric(res[["EVPPI"]][1]))
  NA_real_
}

# Each parameter's outcome is tracked with an explicit Status/Error,
# not just a bare NA — so "EVPPI genuinely ~0 for this parameter" and
# "EVPPI could not be computed (GAM fit failed, etc.)" are
# distinguishable in the saved output, instead of both silently
# collapsing into the same missing row.
evppi_results <- tryCatch({
  do.call(rbind, lapply(sampled_param_names, function(pn) {
    out <- tryCatch({
      res <- suppressWarnings(dampack::calc_evppi(
        psa     = psa_obj_evppi,
        params  = pn,
        outcome = "nmb",
        wtp     = wtp,
        type    = "gam"
      ))
      val <- extract_evppi(res)
      if (is.na(val)) {
        list(val = NA_real_, status = "FAILED",
             error = "extract_evppi() returned NA - unrecognised dampack return shape")
      } else {
        list(val = val, status = "OK", error = NA_character_)
      }
    }, error = function(e) list(val = NA_real_, status = "FAILED", error = conditionMessage(e)))
    # one bad parameter must not kill the whole run
    data.frame(Parameter = pn, EVPPI = out$val, Status = out$status, Error = out$error)
  }))
}, error = function(e) {
  cat("WARNING - EVPPI could not be computed with the installed dampack version.\n")
  cat("          Error message:", conditionMessage(e), "\n")
  cat("          Skipping EVPPI; all other outputs are unaffected.\n")
  NULL
})

if (!is.null(evppi_results)) {
  n_ok     <- sum(evppi_results$Status == "OK")
  n_failed <- sum(evppi_results$Status == "FAILED")
  cat(sprintf("OK - EVPPI computed for %d of %d parameters (%d failed - see EVPPI_status.xlsx for why)\n",
              n_ok, nrow(evppi_results), n_failed))
  write_xlsx(evppi_results, file.path(out_psa, "EVPPI_status.xlsx"))
}

evppi_ok <- if (!is.null(evppi_results)) evppi_results %>% filter(Status == "OK") else NULL

if (is.null(evppi_ok) || nrow(evppi_ok) == 0) {
  cat("WARNING - EVPPI produced no usable results; nothing written.\n")
} else {
  write_xlsx(evppi_ok %>% select(Parameter, EVPPI), file.path(out_psa, "EVPPI_by_parameter.xlsx"))
  # Top 15 by EVPPI, for a quick "what to research next" tornado-style plot
  evppi_top <- evppi_ok %>% arrange(desc(EVPPI)) %>% slice_head(n = 15) %>%
    mutate(Parameter = factor(Parameter, levels = rev(Parameter)))
  evppi_plot <- ggplot(evppi_top, aes(x = EVPPI, y = Parameter)) +
    geom_col(fill = "#2E75B6") +
    labs(title = "EVPPI by Parameter (top 15)",
         subtitle = paste0("At WTP=", wtp_label, "/QALY | NMB: Realistic Impr. vs No NPY"),
         x = "EVPPI (USD, population-level per dampack default)", y = NULL) +
    theme_bw(base_size = 11)
  ggsave(file.path(out_plots, "evppi_top15.png"), evppi_plot, width = 9, height = 6, dpi = 150)
  cat("OK - EVPPI saved (per-parameter table + top-15 plot)\n")
}

cat("\n========================================================\n")
cat("VALUE OF INFORMATION (EVPI/EVPPI) COMPLETE.\n")
cat(sprintf("  WTP=$%g | EVPI/EVPPI sample=%d of %d PSA draws\n", wtp, n_evppi_use, n_psa))
cat("========================================================\n")
