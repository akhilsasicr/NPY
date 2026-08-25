# ============================================================
# NPY CUA — CONFIGURATION / TOGGLES
# ============================================================
# Every assumption/setting NOT read from
# input/model_input_parameters.xlsx lives here.
# Sourced by 02_model.R and 04_scenarios.R.
# ============================================================

# ── PSA uncertainty ────────────────────────────────────────────
# Every parameter's PSA distribution (Beta/Gamma/Dirichlet-concentration)
# is fitted from its own real CI in input/model_input_parameters.xlsx.
# There is no longer a separate global "+/-20% for everything" mode
# (removed 19 Aug 2026) — the handful of parameters lacking a genuine
# real-data CI (the receipt-group probabilities p_soc_timely/p_soc_del/
# p_soc_nr, whose sample-derived CI was an artifact of the large
# underlying cohort size rather than real-world variability; and any
# parameter with a missing CI cell) now carry a directly-widened
# +/-20% CI in the Excel itself, or fall back to +/-20% automatically
# at sample time (sample_psa_params(), 01_helpers.R) if a CI cell is
# blank.

# ── Willingness-to-pay thresholds ─────────────────────────────
# GDP-per-capita 2026 (WEO April 2025 Projection), the manuscript's stated threshold.
wtp <- 2536
wtp_year <- 2026

# Health-system opportunity-cost threshold for India (Pichon-Riviere
# et al. 2023, Lancet Glob Health, Table 3; descended from Ochalek
# et al. 2018's cross-country methodology but not a direct
# application of it) — a more conservative alternative to GDP.
# Full lineage, candidate-value comparison, and an independent
# replication check of a newer (2026, not peer-reviewed) update are
# documented in script/08_wtp_threshold_sensitivity.R.
wtp_opportunity_cost <- 487
# Pichon-Riviere et al. (2023, Lancet Glob Health) reports this figure
# in 2019 USD explicitly ("All thresholds ... are reported in US
# dollars (2019)") -- not inflation-adjusted forward to any later
# year in this project. Was mislabelled 2025 until 19 Aug 2026.
wtp_opportunity_cost_year <- 2019

# Shared display strings so every plot/table shows the same
# number+year pairing.
wtp_label                  <- paste0("$", format(wtp, big.mark = ","), " (", wtp_year, ")")
wtp_opportunity_cost_label <- paste0("$", format(wtp_opportunity_cost, big.mark = ","), " (", wtp_opportunity_cost_year, " USD)")

# ── PSA reproducibility ───────────────────────────────────────
psa_seed <- 2025

# ── PSA sample size ────────────────────────────────────────────
# Convergence re-checked 24 Aug 2026, directly against the current
# saved draws. Re-measured MC SE as % of mean NMB, worst strategy per
# scenario, across the primary model and every current scenario
# (B/C/D/E/F/H):
#   n=2,000  primary model FAILS the 5% tolerance (5.3-5.6%)
#   n=5,000  every scenario PASSES, worst case (primary) 3.5%,
#            comfortable margin, not thin

n_psa <- 5000L

# ── EVPI/EVPPI sample size (decoupled from n_psa) ──────────────
# dampack::calc_evppi() fits a GAM per parameter; a moderate sample
# gives a stable estimate at far lower compute than the full n_psa
# (64 parameters x one GAM fit each). Uses the first n_psa_evppi rows
# of the main PSA sample, not a fresh draw.
# Kept at 5,000 now rather than the full PSA, if its changed later.
n_psa_evppi <- 5000

# Groups A-H below are just section labels in file order — they do
# NOT correspond to Scenario A-H (e.g. Group D holds Scenario C's
# config; the two label sets are independent).

# ============================================================
# GROUP A — CHE (Catastrophic Health Expenditure) constants
# ============================================================
# Anchor: proportion of non-receipt DS-TB patients who incur CHE.
# Source: Jeyashree K, Thangaraj JWV, Shanmugasundaram D, et al. Cost
# of TB care and equity in distribution of catastrophic TB care costs
# across income quintiles in India. Glob Health Res Policy 2024;9(1):51.
# doi:10.1186/s41256-024-00392-9. (Cross-verified against two
# independent PI-supplied reference lists, 24 Aug 2026.)
# Also OWSA'd (±20%) via cat_params/cat_ci_lower/cat_ci_upper in
# 02_model.R, under the name p_nr_dstb_cat — owsa_multipliers below
# is a separate, narrower mechanism for the RR/OR structural
# relationships only, not for this anchor value itself.
p_nr_dstb_cat_src <- 0.4255319

# Relative risk of CHE, NPY receipt vs non-receipt.
# Source: Wingfield T, Tovar MA, Huff D, et al. The economic effects
# of supporting tuberculosis-affected households in Peru. Eur Respir J
# 2016;48(5):1396. doi:10.1183/13993003.00066-2016. PMID 27660507.
# (Full citation confirmed 24 Aug 2026 -- previously recorded here only
# as "Wingfield T et al. Eur Respir J 2016", with no volume/pages/DOI;
# cross-verified against the manuscript's own reference list and a
# PI-supplied supplementary appendix, both citing this exact paper.)
rr_npy_che <- 0.71

# Odds ratio of CHE, DR-TB vs DS-TB.
or_drtb_che <- 1.61

# Odds ratio of unfavourable outcome (failure/death) given CHE.
# Sources: Wingfield T et al. PLoS Med 2014;11(7):e1001675
# (doi:10.1371/journal.pmed.1001675, PMID 25025331); Fuady A,
# Houweling TAJ, Mansyur M, Burhan E, Richardus JH. Catastrophic costs
# due to tuberculosis worsen treatment outcomes: a prospective cohort
# study in Indonesia. Trans R Soc Trop Med Hyg 2020;114(9):666-673.
# doi:10.1093/trstmh/traa038. PMID 32511712. (Fuady added 24 Aug 2026 --
# a PI-supplied supplementary appendix reports this OR = 1.7 as
# sourced to BOTH papers jointly, not Wingfield 2014 alone.)
or_che_outcome <- 1.7

# OWSA ranges for the two structural CHE multipliers above.
owsa_multipliers <- list(
  rr_npy_che  = list(base = rr_npy_che,  lo = 0.55, hi = 0.87),
  or_drtb_che = list(base = or_drtb_che, lo = 1.20, hi = 1.90)
)

# ============================================================
# GROUP B — No NPY cost constants
# ============================================================
# Separate from cost_nr_* (Current/RI/Ideal's non-receipt branches):
# these describe a world where NPY never existed, not a patient who
# missed an active programme. Source: Treeage/NoNPY cost.xlsx, June 2026.
nocost_nr_params <- list(
  nocost_nr_dstb_suc   = list(base = 515.54,  lo = 412.44,  hi = 618.65),
  nocost_nr_dstb_unsuc = list(base = 471.46,  lo = 377.17,  hi = 565.75),
  nocost_nr_dstb_die   = list(base = 216.03,  lo = 172.82,  hi = 259.23),
  nocost_nr_drtb_suc   = list(base = 1208.14, lo = 966.51,  hi = 1449.77),
  nocost_nr_drtb_unsuc = list(base = 1458.92, lo = 1167.13, hi = 1750.70),
  nocost_nr_drtb_die   = list(base = 1127.24, lo = 901.79,  hi = 1352.69)
)

# ============================================================
# GROUP C — After-treatment utilities (used in Scenario C lifetime QALY model)
# ============================================================
after_tx_utilities <- list(
  uv_dstb_suc_after  = list(base = 0.9600, lo = 0.8640, hi = 0.9900),
  uv_dstb_fail_after = list(base = 0.8500, lo = 0.7650, hi = 0.9350),
  uv_drtb_suc_after  = list(base = 0.8800, lo = 0.7920, hi = 0.9680),
  uv_drtb_fail_after = list(base = 0.5100, lo = 0.4590, hi = 0.5610)
)
discount_rate_qaly <- 0.03   # annual discount rate for future QALYs

# ============================================================
# GROUP D — Scenario D: DR-TB utility as a ratio off DS-TB
# ============================================================
# Primary model's DR-TB during-treatment utility (0.51, flat across
# success/failure) is imported from Kittikraisak et al. 2012 (PLoS
# ONE 7(1):e29775, PMID 22253777), a Thailand cohort — while DS-TB
# utility (0.93/0.91) is this project's own India data. Kittikraisak
# also reports DS-TB (median EQ-5D 0.69, n=32) in the same
# study/instrument/population, so a within-study ratio can be applied
# to India's own DS-TB values instead of importing Thailand's
# absolute number:
#   dr_ds_utility_ratio = 0.51 / 0.69 = 0.7391
# Scenario D (04_scenarios.R) applies this ratio to each PSA draw's
# sampled uv_dstb_during/uv_dstb_fail_during — DR-TB's uncertainty is
# inherited from DS-TB's sampled uncertainty, scaled.
#
# Caveats: Kittikraisak's MDR stratum is small (n=11, one outlier
# below zero, flagged by the source paper itself). The ratio splits
# by treatment status, not by outcome, so it's a single during-
# treatment ratio applied uniformly to success and failure.
dr_ds_utility_ratio <- 0.51 / 0.69

# ============================================================
# GROUP E — Scenario A: temporal mismatch correction (Rs.500 -> Rs.1000)
# ============================================================
# Dimension 1: relative increase in CHE-protection effectiveness
# (1-RR), not a multiplier on RR directly — a 10% relative increase is
# RR_new = 1 - [(1-RR_base) * 1.10], not RR_base * 0.90.
scenario_c_effectiveness_gain <- 1.10
# Dimension 2: reduction in unfavourable outcomes for receipt groups.
scenario_c_outcome_improve <- 0.90

# ============================================================
# GROUP F — Scenario C: premature-death QALY loss
# ============================================================
# A 1-year decision-tree horizon discards most of a mortality-
# reducing intervention's value. Scenario C (compute_qalys_lifetime()
# in 01_helpers.R) adds a discounted future-QALY term while keeping
# the 1-year tree structure unchanged: survivors accrue post-
# treatment utility for their remaining discounted life expectancy;
# deaths accrue nothing further.
#
# Remaining life expectancy is computed from India's SRS abridged
# life table via discounted_life_expectancy() (01_helpers.R), not a
# flat annuity — it walks forward year by year, tracking survival
# probability, and sums discounted expected life-years.
#
# Life table source: "SRS Based Abridged Life Tables 2020-24," Office
# of the Registrar General & Census Commissioner, India, published
# May 2026. India-Total-both-sexes row (page 19), converted from
# 5-year nqx bands to single-year probabilities via
# q_annual = 1-(1-nqx)^(1/n); the open "85+" band extended with a
# mildly accelerating hazard — NOT an arbitrary assumption: the
# per-year growth rate (a constant 1.05x/year, confirmed by direct
# inspection of input/SRS_lifetable_India_2020-24.csv — a Gompertz-
# law-consistent constant relative hazard increase) was fitted from
# the growth trend of the preceding REAL SRS bands, then extrapolated
# forward past the open-ended interval, standard practice for
# extending a life table past its last reported age group.
# Reconstruction gives life expectancy at birth of 70.3 years vs the
# official 70.6 (small residual gap, most likely from the uniform-
# hazard-within-band conversion formula above being a weaker
# approximation specifically for the age-0 band, where real infant
# deaths are front-loaded within the year rather than uniformly
# spread — a known limitation of this reconstruction method, not
# investigated further since Scenario F uses remaining life
# expectancy FROM cohort_mean_age, not life expectancy at birth, so
# this specific small gap does not propagate into that calculation).
# NOTE: this line needs `root` to already exist in the calling
# environment at SOURCE time (not call time) — every script in this
# project defines `root <- here::here()` before `source("00_config.R")`,
# which is what makes this safe in practice. If 00_config.R is ever
# sourced standalone or before `root` is set, this line errors
# immediately rather than silently using a wrong path.
lifetable_path <- file.path(root, "input", "SRS_lifetable_India_2020-24.csv")

# Modelled cohort (2021 diagnosis-year, N=736,459): mean 38.231,
# median 36, SD 17.935, range 0-100. Age figure supplied directly by
# the PI (Dr. Jeyashree Kathiresan); cross-referenced against
# `previous/correspondence/Age_descriptive_tosent_v1_4 May 2026.docx`
# and `Suppl_Baseline_characteristcs_seconday_data_120526.docx`, which
# describe the same N=736,459 secondary-dataset cohort (2021
# diagnosis-year, drawn from a larger 2018-2022 dataset of 3,712,551
# patients) that `p_soc_timely`/`p_soc_del`/`p_soc_nr` and the
# treatment-outcome probabilities also trace back to — same source
# cohort throughout, not independently derived. A single mean age is
# used since the model has no age structure — an approximation, since
# remaining life expectancy is non-linear in age. cohort_median_age/
# cohort_age_sd exist so Scenario F can sweep across the age
# distribution as a robustness check (same pattern as the SMR sweep).
cohort_mean_age   <- 38.231
cohort_median_age <- 36
cohort_age_sd     <- 17.935

# ── Post-TB excess mortality (SMR) ────────────────────────────
# TB survivors die at a higher rate than the general population for
# years afterwards. An SMR multiplies every age-specific mortality
# HAZARD from the life table (applied as q' = 1-(1-q)^SMR in
# discounted_life_expectancy(), 01_helpers.R -- not q*SMR directly,
# which is demographically approximate), shortening remaining life
# expectancy accordingly.
#
# Base case = 2.3, from Selvaraju S et al., "Long-term Survival of
# Treated Tuberculosis Patients in Comparison to a General Population
# in South India: A Matched Cohort Study." Int J Infect Dis
# 2021;110:385-393. PMID 34333118. doi:10.1016/j.ijid.2021.07.067.
# Indian, matched cohort (4,022 treated vs 12,243 controls), genuinely
# post-treatment (unlike older cohorts that follow from treatment
# start and would double-count year-1 deaths this model already
# counts separately), 95% CI reported for PSA.
#
# SMR=1.0 (no excess mortality) is not used as base case — it is the
# assumption most favourable to NPY, and known to be false.
#
# A flat SMR is applied across all remaining years, not because
# post-TB excess mortality is actually flat over time — time-varying
# data (Kim S et al., Clin Infect Dis 2024, doi:10.1093/cid/ciaf206;
# Cerqueira-Silva T et al., Nat Med 2026, doi:10.1038/s41591-026-
# 04294-w) shows a decline from ~11x in year 1 to a plateau around
# 2.2-2.6 from years 2-10. 2.3 approximates that long-run plateau
# across the whole remaining lifetime.
#
# Confounding caveat: Basham CA & Karim ME, Ann Epidemiol 2021,
# doi:10.1016/j.annepidem.2021.12.009 — an E-value analysis found
# unmeasured confounding of RR>=2.95 on both TB and mortality could
# render most published post-TB mortality associations
# non-significant. Some of SMR 2.3 may reflect shared poverty/
# comorbidity risk factors rather than a causal effect of TB disease
# itself; Selvaraju's matched design partially addresses this via
# age/sex matching.
post_tb_smr <- 2.3

# PSA range — Selvaraju's reported 95% CI. sample_psa_params() draws
# post_tb_smr from a Beta fitted to this mean+CI on every iteration,
# rather than holding SMR fixed, so Scenario F's PSA reflects genuine
# SMR uncertainty too.
post_tb_smr_ci <- c(lo = 1.7, hi = 3.1)

# SMR sensitivity sweep (deterministic, one-way), anchored on
# published estimates:
#   1.00 - no excess mortality (upper bound on survivor LE)
#   1.22 - Quaife M et al., Lancet Respir Med 2020;8(4):332-333,
#          doi:10.1016/S2213-2600(20)30039-4. Conservative India-
#          specific lower bound, from post-TB COPD alone.
#   2.30 - base case (Selvaraju 2021, above)
#   2.91 - Romanowski K et al., Lancet Infect Dis 2019;19(10):1129-
#          1137, doi:10.1016/S1473-3099(19)30309-3. Pooled SMR, all
#          treated, 10 studies, 40,781 people — international cross-
#          check, not a substitute for the Indian estimate.
#   4.20 - Kolappan C et al., Int J Tuberc Lung Dis 2008;12(1):81-86,
#          PMID 18173882. Measured from treatment start (includes
#          on-treatment deaths this model counts separately) — a
#          stress-test value, not a like-for-like estimate.
# Excludes Romanowski's cure-restricted
# 3.76 (higher than the all-treated 2.91, the wrong direction for a
# cured subgroup — likely a selection artifact; Scenario F applies
# SMR to all year-1 survivors, so all-treated is the right population
# match anyway).
post_tb_smr_range <- c(1.00, 1.22, 2.30, 2.91, 4.20)

# Raw remaining-years sweep, alongside the SMR sweep: how few future
# years would need to be credited per averted death before the
# conclusion flips?
life_expectancy_remaining_range <- c(2, 3, 4, 5, 7, 10, 15, 20, 25, 30)

# ============================================================
# GROUP G — Cost-parameter name lists (for Scenario B & cost decomposition)
# ============================================================
cost_param_names <- c(
  "cost_timely_DSTB_suc",   "cost_timely_DSTB_unsuc",  "cost_timely_DSTB_die",
  "cost_timely_DRTB_suc",   "cost_timely_DRTB_unsuc",  "cost_timely_DRTB_die",
  "cost_del_DSTB_suc",      "cost_del_DSTB_unsuc",     "cost_del_DSTB_die",
  "cost_del_DRTB_suc",      "cost_del_DRTB_unsuc",     "cost_del_DRTB_die",
  "cost_nr_DSTB_suc",       "cost_nr_DSTB_unsuc",      "cost_nr_DSTB_die",
  "cost_nr_DRTB_suc",       "cost_nr_DRTB_unsuc",      "cost_nr_DRTB_die"
)
nocost_param_names <- c(
  "nocost_nr_dstb_suc",   "nocost_nr_dstb_unsuc",  "nocost_nr_dstb_die",
  "nocost_nr_drtb_suc",   "nocost_nr_drtb_unsuc",  "nocost_nr_drtb_die"
)

# ============================================================
# GROUP H — PSA convergence diagnostic tolerance
# ============================================================
# Used by check_psa_convergence() (01_helpers.R) for two checks tied
# to one constant: (1) the running mean over the last 10% of
# iterations must stay within this fraction of the final running
# mean; (2) Monte Carlo SE-as-percent-of-mean must not exceed it.
#
# Set to 0.05, not a stricter 0.02: primary-model NMB is small in
# absolute terms (~$25-28), so even a well-behaved PSA has an SE that
# is a large percentage of that mean (a real run: 12.9-14.2%).
# Reaching 2% would need ~42x more iterations for no decision-
# relevant benefit — the actual robustness question is answered by
# Scenario F's ICER/breakeven margin, which is large. Scenario F's
# own PSA (larger NMB scale) sits at 2.6-3.1% and passes even the
# stricter bar.
psa_convergence_tol_pct <- 0.05
