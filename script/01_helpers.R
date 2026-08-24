# ============================================================
# NPY CUA —SHARED HELPER FUNCTIONS
# ============================================================
# Every function in this file is used by BOTH 02_model.R (the
# base/primary model + PSA) and 04_scenarios.R (Scenarios A, B, D,
# plus Scenario C in 05_scenario_C_premature_death_qaly.R).
#
#
# If you are new to this project: read che_branch_split() and
# calc_branch() first — everything else in this file supports
# those two functions (either by feeding them parameters, or by
# generating the many parameter DRAWS used in the probabilistic
# sensitivity analysis, PSA).
# ============================================================

# ============================================================
# EXCEL READING HELPERS
# ============================================================

# safe_arithmetic_eval(): evaluates a simple +-*/() arithmetic
# expression string WITHOUT ever calling eval() on the parsed
# expression directly. str2lang() only parses the text into a syntax
# tree (no execution); this function then walks that tree itself and
# explicitly rejects any node that isn't a numeric literal or one of
# the four whitelisted operators — so even a string engineered to
# call an arbitrary R function is rejected by the walker, not
# executed by an eval().
safe_arithmetic_eval <- function(expr_text) {
  parsed <- tryCatch(str2lang(expr_text), error = function(e) NULL)
  if (is.null(parsed)) return(NA_real_)
  walk <- function(node) {
    if (is.numeric(node)) return(as.numeric(node))
    if (!is.call(node)) stop("disallowed token")
    op <- as.character(node[[1]])
    if (!(op %in% c("+", "-", "*", "/", "("))) stop("disallowed operator/function: ", op)
    args <- lapply(as.list(node)[-1], walk)
    if (op == "(") return(args[[1]])
    if (length(args) == 1 && op == "-") return(-args[[1]])
    Reduce(function(a, b) switch(op, "+" = a + b, "-" = a - b, "*" = a * b, "/" = a / b), args)
  }
  tryCatch(as.numeric(walk(parsed)), error = function(e) NA_real_)
}

# safe_numeric(): converts one Excel cell to a number.
# Some cells in the input file are typed as simple arithmetic
# expressions (e.g. "=126002/736459") rather than a plain number.
# This function first tries a normal numeric conversion; if that
# fails AND the text looks like a safe arithmetic expression
# (only digits, +-*/(). and e/E for scientific notation), it
# evaluates the expression via safe_arithmetic_eval() above (not
# eval(parse())). Anything else (typos, stray text) is left as NA so
# it gets caught by validation, not silently misread.

safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  if (is.na(x))  return(NA_real_)
  result <- suppressWarnings(as.numeric(x))
  if (is.na(result) && is.character(x)) {
    clean_expr <- gsub("^=", "", x)
    if (grepl("^[-0-9.eE+*/() ]+$", clean_expr))
      result <- safe_arithmetic_eval(clean_expr)
  }
  result
}

# to_list(): converts a data-frame column into a named list keyed
# by the Parameter column, e.g. base[["p_soc_timely"]] == 0.1711.
# allow_na = FALSE makes this function STOP the whole script if any
# value is unreadable — this is intentional: a silently-missing
# parameter would produce wrong results without any visible error.
to_list <- function(df, key, val, allow_na = FALSE) {
  vals <- vapply(df[[val]], safe_numeric, numeric(1))
  if (!allow_na && any(is.na(vals))) {
    bad <- df[[key]][is.na(vals)]
    stop(paste0("CRITICAL: Non-numeric value for: ", paste(bad, collapse = ", ")))
  }
  setNames(as.list(vals), df[[key]])
}

# ============================================================
# PARAMETER VALIDATION
# ============================================================
# Every group of probabilities that represents a branch of the
# decision tree (e.g. "goes timely / delayed / non-receipt") must
# sum to 1 — otherwise some fraction of the cohort disappears (or
# is double-counted) in the model. FATAL on any violation (missing
# value, out-of-[0,1] value, or a group not summing to 1 within
# tolerance) — a structurally invalid probability set should stop the
# run, not print a warning that's easy to miss in a long console log.
# na.rm is NOT used in the sum check below: a missing member could
# otherwise be silently offset by the other members already summing
# to ~1, hiding a real data problem.
validate_params <- function(p) {
  errors <- character(0)
  chk <- function(nms, label) {
    vals <- sapply(nms, function(x) as.numeric(p[[x]]))
    if (anyNA(vals)) {
      errors <<- c(errors, sprintf("  %s has missing value(s): %s",
                                   label, paste(nms[is.na(vals)], collapse = ", ")))
      return(invisible(NULL))
    }
    if (any(vals < 0 | vals > 1)) {
      errors <<- c(errors, sprintf("  %s has value(s) outside [0,1]: %s",
                                   label, paste(sprintf("%s=%.4f", nms, vals), collapse = ", ")))
    }
    s <- sum(vals)
    if (abs(s - 1) > 1e-3)
      errors <<- c(errors, sprintf("  %s sums to %.4f (must = 1)", label, s))
  }
  chk(c("p_soc_timely", "p_soc_del",    "p_soc_nr"),                        "SoC receipt")
  chk(c("p_ri_timely",  "p_ri_delayed", "p_ri_nr"),                         "RI receipt")
  chk(c("p_timely_dstb", "p_timely_drtb"),                                  "Timely TB type")
  chk(c("p_del_dstb",    "p_del_drtb"),                                     "Delayed TB type")
  chk(c("p_nr_dstb",     "p_nr_drtb"),                                      "NR TB type")
  chk(c("p_timely_dstb_suc","p_timely_dstb_unsuc","p_timely_dstb_die"),     "Timely DSTB outcomes")
  chk(c("p_del_dstb_suc",   "p_del_dstb_unsuc",   "p_del_dstb_die"),        "Delayed DSTB outcomes")
  chk(c("p_nr_dstb_suc",    "p_nr_dstb_unsuc",    "p_nr_dstb_die"),         "NR DSTB outcomes")
  chk(c("p_timely_drtb_suc","p_timely_drtb_unsuc","p_timely_drtb_die"),     "Timely DRTB outcomes")
  chk(c("p_del_drtb_suc",   "p_del_drtb_unsuc",   "p_del_drtb_die"),        "Delayed DRTB outcomes")
  chk(c("p_nr_drtb_suc",    "p_nr_drtb_unsuc",    "p_nr_drtb_die"),         "NR DRTB outcomes")
  if (length(errors) > 0) {
    stop("CRITICAL - parameter validation failed:\n", paste(errors, collapse = "\n"))
  } else {
    cat("OK - All parameter validation checks passed\n")
  }
}

# ============================================================
# QALY CALCULATORS
# ============================================================

# compute_qalys(): the PRIMARY model's QALY set — "year 1 only".
# The TreeAge tree assigns on-treatment (year-1) utilities at every
# terminal node (confirmed directly from the TreeAge XML export), so
# year-1 is the primary decision-tree horizon, not a simplification —
# the multi-year alternative lives in Scenario B instead.
compute_qalys <- function(p) {
  list(
    dstb_suc  = as.numeric(p$uv_dstb_during),
    dstb_fail = as.numeric(p$uv_dstb_fail_during),
    drtb_suc  = as.numeric(p$uv_drtb_suc_during),
    drtb_fail = as.numeric(p$uv_drtb_fail_during),
    death     = 0
  )
}

# compute_qalys_3yr(): Scenario B's QALY set. Adds two more years of
# POST-treatment utility, each discounted at discount_rate_qaly
# (config.R; 3% by default) per year:
#   Year 1: on-treatment utility (same as the primary model)
#   Year 2: post-treatment utility / (1+r)
#   Year 3: post-treatment utility / (1+r)^2
#   Death : 0 in every year (a dead patient accrues no QALYs)
compute_qalys_3yr <- function(p, discount_rate = discount_rate_qaly) {
  list(
    dstb_suc  = as.numeric(p$uv_dstb_during) +
      as.numeric(p$uv_dstb_suc_after)  / (1 + discount_rate) +
      as.numeric(p$uv_dstb_suc_after)  / (1 + discount_rate)^2,
    dstb_fail = as.numeric(p$uv_dstb_fail_during) +
      as.numeric(p$uv_dstb_fail_after) / (1 + discount_rate) +
      as.numeric(p$uv_dstb_fail_after) / (1 + discount_rate)^2,
    drtb_suc  = as.numeric(p$uv_drtb_suc_during) +
      as.numeric(p$uv_drtb_suc_after)  / (1 + discount_rate) +
      as.numeric(p$uv_drtb_suc_after)  / (1 + discount_rate)^2,
    drtb_fail = as.numeric(p$uv_drtb_fail_during) +
      as.numeric(p$uv_drtb_fail_after) / (1 + discount_rate) +
      as.numeric(p$uv_drtb_fail_after) / (1 + discount_rate)^2,
    death     = 0
  )
}

# ============================================================
# LIFE TABLE — remaining life expectancy for Scenario F
# ============================================================
# These two functions exist so Scenario F can use a REAL, sourced
# figure for "how many more years does a survivor live," rather than
# a hardcoded guess.

# load_lifetable(): reads India's SRS abridged life table.
#
# The file (input/SRS_lifetable_India_2020-24.csv) has two columns:
#   age        — single year of age, 0 to 119
#   qx_annual  — probability that a person currently aged exactly
#                `age` dies before reaching age+1
#
# qx is the standard demographic notation for this quantity. Note the
# values are flat across each SRS-published age band (e.g. ages 35-39
# all share one value) because SRS publishes abridged (banded)
# tables — this file converts each band's multi-year probability to
# an equivalent single-year probability (see 00_config.R, Group F,
# for the exact source and conversion method), so flat runs within a
# band are expected, not an error.

load_lifetable <- function(path = lifetable_path) {
  if (!file.exists(path))
    stop("Life table not found at: ", path,
         "\nScenario F needs input/SRS_lifetable_India_2020-24.csv.")
  lt <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("age", "qx_annual") %in% names(lt)))
    stop("Life table must have columns 'age' and 'qx_annual'; found: ",
         paste(names(lt), collapse = ", "))
  lt[order(lt$age), c("age", "qx_annual")]
}

# discounted_life_expectancy(): expected remaining life-years from a
# given age, discounted, optionally with excess mortality (SMR)
# applied. Unlike a flat annuity factor (present value of N certain
# years), this walks forward year by year from start_age+1, applying
# each year's real death probability (capped at 1, x SMR), crediting
# survivors a full year and decedents a half year (half-cycle
# convention), discounting each, and stopping once the cohort is
# effectively extinct.
#
# Starts at start_age+1, not start_age: start_age is cohort_mean_age,
# the age during Year 1 (on-treatment), which
# compute_qalys_lifetime() already credits via uv_*_during — this
# function only values the years after that. Starting at start_age
# itself would double-apply Year 1's mortality risk and add an
# undiscounted extra life-year on top of what's already counted.
#
# Discounting uses a clean integer year counter (1, 2, 3, ...) from
# that point, matching compute_qalys_3yr()'s (1+r)^1, (1+r)^2 pattern
# — NOT (table_age - start_age), which used to produce a fractional
# first-year exponent (e.g. 0.769 at start_age=38.231) and under-
# discount every subsequent year by the same fractional offset.
#
# ARGUMENTS
#   start_age     : age during Year 1 (cohort_mean_age) — remaining-
#                   years clock starts the year after this
#   smr           : standardised mortality ratio (1.0 = general
#                   population; 2.0 = double the general-pop rate)
#   discount_rate : annual discount rate for future health (3%)
#   lt            : optional pre-loaded life table (avoids re-reading
#                   the file inside a loop)
discounted_life_expectancy <- function(start_age    = cohort_mean_age,
                                        smr          = post_tb_smr,
                                        discount_rate = discount_rate_qaly,
                                        lt           = NULL) {
  if (is.null(lt)) lt <- load_lifetable()
  qx_by_age <- setNames(as.numeric(lt$qx_annual), as.character(lt$age))
  max_age   <- max(lt$age)

  alive <- 1
  total <- 0
  year  <- 0
  for (a in seq(from = floor(start_age) + 1, to = max_age)) {
    year <- year + 1                              # 1, 2, 3, ... — clean integer
                                                    # offsets from start_age+1,
                                                    # matching compute_qalys_3yr()'s
                                                    # (1+r)^1, (1+r)^2 pattern; avoids
                                                    # the fractional (a - start_age)
                                                    # exponent this used to use, which
                                                    # under-discounted the first future
                                                    # year and overlapped with the
                                                    # [start_age, start_age+1) interval
                                                    # already credited as Year 1.
    q <- qx_by_age[[as.character(a)]]
    if (is.null(q) || is.na(q)) q <- 1          # past table end: assume death
    q <- min(q * smr, 1)                         # apply excess mortality, cap at 1
    # life-years lived during this year: survivors get 1, decedents get 0.5
    lived <- alive * (1 - q) + alive * q * 0.5
    total <- total + lived / ((1 + discount_rate)^year)
    alive <- alive * (1 - q)
    if (alive < 1e-9) break                      # cohort effectively extinct
  }
  total
}

# ------------------------------------------------------------
# compute_qalys_lifetime(): Scenario F's QALY set.
# ------------------------------------------------------------
# Adds a discounted future-QALY term on top of the 1-year decision
# tree (no restructuring needed): survivors of year 1 accrue post-
# treatment utility for their remaining life expectancy, discounted;
# deaths accrue nothing further, exactly as in the primary model. The
# difference between those two is the QALY gain from averting a
# death — what a pure 1-year model throws away.
#
# `le_factor` is the discounted remaining life expectancy, normally
# from discounted_life_expectancy() (above) — already accounts for
# mortality risk in each future year, so it's used directly as the
# multiplier on post-treatment utility (~21.3 at age 38, no excess
# mortality). Year 1 itself uses the same on-treatment utilities as
# the primary model, so any difference in results is attributable
# only to the added future term.
#
# Scenario B (compute_qalys_3yr) is the same idea stopped at 3 years;
# both reuse the uv_*_after utilities from 00_config.R Group C.
#
# Known simplifications:
#   1. Successes and failures get the same remaining life expectancy
#      (differing only in post-treatment utility) — no outcome-
#      specific SMR data exists to do otherwise; generous to failures.
#   2. Survivors are assumed to have no further disease events (no
#      relapse, reinfection, later TB death) — same simplification a
#      DALY's YLL component makes. Modelling this properly needs a
#      Markov tail plus matching future retreatment costs; adding
#      future health without future costs would bias in NPY's favour.
#      The SMR sweep (post_tb_smr_range) is the partial answer.
#   3. uv_dstb_suc_after (0.96) is validated only to 3 months post-
#      treatment (Mahalingam V et al., PLOS One 2025;20,
#      doi:10.1371/journal.pone.0328484) and applied to every
#      remaining year (17+). Indian PTLD studies using SGRQ (no
#      Indian EQ-5D data extends past 3 months) find persistent
#      impairment years later — Aggarwal N et al. 2021 (medRxiv,
#      doi:10.1101/2021.10.04.21264524, mean SGRQ 42.3, 93% still
#      symptomatic); Thoker ZA et al. 2023 (Cureus,
#      doi:10.7759/cureus.36354, median SGRQ 45.5). No Indian EQ-5D
#      data tracks utility into the PTLD phase, so no trajectory can
#      be built from local data — the quality-of-life counterpart to
#      limitation 2.
compute_qalys_lifetime <- function(p, le_factor = NULL) {

  # If no discounted life expectancy was supplied, compute it from the
  # SRS life table using the config defaults (cohort_mean_age,
  # post_tb_smr, discount_rate_qaly).
  if (is.null(le_factor)) le_factor <- discounted_life_expectancy()
  af <- le_factor

  list(
    # Year 1 on-treatment utility + all remaining discounted life-years
    dstb_suc  = as.numeric(p$uv_dstb_during)      + as.numeric(p$uv_dstb_suc_after)  * af,
    dstb_fail = as.numeric(p$uv_dstb_fail_during) + as.numeric(p$uv_dstb_fail_after) * af,
    drtb_suc  = as.numeric(p$uv_drtb_suc_during)  + as.numeric(p$uv_drtb_suc_after)  * af,
    drtb_fail = as.numeric(p$uv_drtb_fail_during) + as.numeric(p$uv_drtb_fail_after) * af,
    # A patient who dies accrues no further QALYs. This zero, set
    # against the large survivor values above, IS the mechanism by
    # which averted deaths now carry their full lifetime value.
    death     = 0
  )
}

# ============================================================
# CORE DECISION TREE — che_branch_split() / calc_branch() /
#                       strat_ev() / nonpy_ev()
# ============================================================
# These four functions ARE the decision tree. Read them in order.
#
# TREE STRUCTURE (matches the corrected TreeAge model NPY_9.6.26):
#   Strategy -> Receipt group (timely/delayed/non-receipt)
#             -> TB type (DS-TB/DR-TB)
#               -> CHE (catastrophic health expenditure: yes/no)
#                 -> Treatment outcome (success/failure/death)
#   (No NPY skips the receipt-group level - see nonpy_ev() below.)

# che_branch_split(): splits ONE pathway x TB-type cell's observed
# outcome probabilities into a "no-CHE" sub-branch and a "CHE"
# sub-branch.
#
#   mode = "marginal" (used by the primary model and Scenarios B, C):
#     The no-CHE branch uses the OBSERVED marginal unfavourable
#     probability directly as its baseline (p0 = p_unsuc + p_die).
#     This matches the TreeAge base case exactly.
#
#   mode = "rescale" (used by Scenario A only):
#     Solves a quadratic for p0 such that the WEIGHTED AVERAGE of
#     the CHE and no-CHE branches exactly reproduces the observed
#     marginal (rather than the no-CHE branch alone matching it).
#     This is the more conservative option: the CHE effect on
#     outcomes is slightly smaller.
#
# In both modes, the CHE branch's unfavourable probability (p1) is
# then computed from p0 using the odds-ratio-to-probability formula
# with or_che_outcome (Wingfield et al. 2014, OR=1.7):
#   p1 = (OR * p0) / [(1 - p0) + (OR * p0)]
# If this ever exceeds 0.999 (which can happen for extreme PSA
# draws), it is capped at 0.999 to keep it a valid probability, and
# the event is logged in cap_env so you can see how often it happens.
che_branch_split <- function(p_unsuc, p_die, p_cat_obs, or_che_outcome,
                              mode = c("marginal", "rescale"),
                              node_name = "Unknown", cap_env = NULL) {
  mode  <- match.arg(mode)
  p_obs <- p_unsuc + p_die   # observed marginal unfavourable probability

  if (mode == "marginal") {
    p0 <- p_obs
  } else {
    # Quadratic solve: p_obs = (1-p_cat)*p0 + p_cat*[OR*p0/((1-p0)+OR*p0)]
    a  <- (1 - p_cat_obs) * (or_che_outcome - 1)
    b  <- 1 + (p_cat_obs - p_obs) * (or_che_outcome - 1)
    cc <- -p_obs
    p0 <- if (abs(a) < 1e-8) {
      if (abs(b) < 1e-12) 0.5 else -cc / b   # degenerate a AND b: no real solve possible, midpoint fallback
    } else {
      (-b + sqrt(max(b^2 - 4 * a * cc, 0))) / (2 * a)
    }
    p0 <- max(0, min(p0, 1))
    # Post-solve check: the weighted average of the two branches must
    # actually reproduce the observed marginal p_obs (the whole point
    # of solving for p0) — catches a wrong solve silently propagating
    # into every downstream cost/QALY number instead of erroring.
    p1_check <- (or_che_outcome * p0) / ((1 - p0) + (or_che_outcome * p0))
    weighted_check <- (1 - p_cat_obs) * p0 + p_cat_obs * p1_check
    if (abs(weighted_check - p_obs) > 1e-6)
      stop(sprintf("che_branch_split rescale solve failed at %s: weighted avg %.6f != observed %.6f",
                    node_name, weighted_check, p_obs))
  }

  p1 <- (or_che_outcome * p0) / ((1 - p0) + (or_che_outcome * p0))
  if (p1 > 0.999) {
    if (!is.null(cap_env)) {
      cap_env$count <- cap_env$count + 1L
      cap_env$nodes[[node_name]] <-
        (if (is.null(cap_env$nodes[[node_name]])) 0L else cap_env$nodes[[node_name]]) + 1L
    }
    p1 <- 0.999
  }

  # Within each branch, failure vs death is split preserving the
  # observed proportional shares between the two.
  share_unsuc <- if (p_obs > 0) p_unsuc / p_obs else 0.5
  share_die   <- if (p_obs > 0) p_die   / p_obs else 0.5

  list(
    p_nocat       = 1 - p_cat_obs,
    p_nocat_suc   = 1 - p0,
    p_nocat_unsuc = share_unsuc * p0,
    p_nocat_die   = share_die   * p0,
    p_cat         = p_cat_obs,
    p_cat_suc     = 1 - p1,
    p_cat_unsuc   = share_unsuc * p1,
    p_cat_die     = share_die   * p1
  )
}

# calc_branch(): computes expected cost, QALY, survival, death, and
# CHE probability for ONE receipt-group x TB-type combination (a
# single "cell" of the tree). Takes explicit cost/QALY arguments so
# the same function can be called with different cost sets (e.g.
# nocost_nr_* for the No NPY counterfactual) or different QALY sets
# (year-1 vs 3-year).
#
# There are only THREE direct terminal outcomes per CHE sub-branch:
# success, failure, death — confirmed against the TreeAge tree, which
# has no conditional post-outcome death sub-branch.
calc_branch <- function(p_recv, p_tb, p_unsuc, p_die,
                         cost_suc, cost_unsuc, cost_die,
                         qaly_suc, qaly_fail, qaly_death,
                         p_cat_obs, or_che_outcome,
                         split_mode = "marginal",
                         node_name = "Unknown", cap_env = NULL) {

  s <- che_branch_split(p_unsuc, p_die, p_cat_obs, or_che_outcome,
                         mode = split_mode, node_name = node_name, cap_env = cap_env)

  cost_nocat  <- s$p_nocat * (s$p_nocat_suc * cost_suc +
                                s$p_nocat_unsuc * cost_unsuc +
                                s$p_nocat_die  * cost_die)
  qaly_nocat  <- s$p_nocat * (s$p_nocat_suc  * qaly_suc  +
                                s$p_nocat_unsuc * qaly_fail +
                                s$p_nocat_die  * qaly_death)
  surv_nocat  <- s$p_nocat * (s$p_nocat_suc + s$p_nocat_unsuc)
  death_nocat <- s$p_nocat *  s$p_nocat_die

  cost_cat    <- s$p_cat * (s$p_cat_suc   * cost_suc  +
                              s$p_cat_unsuc  * cost_unsuc +
                              s$p_cat_die    * cost_die)
  qaly_cat    <- s$p_cat * (s$p_cat_suc   * qaly_suc  +
                              s$p_cat_unsuc  * qaly_fail +
                              s$p_cat_die    * qaly_death)
  surv_cat    <- s$p_cat * (s$p_cat_suc + s$p_cat_unsuc)
  death_cat   <- s$p_cat *  s$p_cat_die

  list(
    cost  = p_recv * p_tb * (cost_nocat  + cost_cat),
    qaly  = p_recv * p_tb * (qaly_nocat  + qaly_cat),
    surv  = p_recv * p_tb * (surv_nocat  + surv_cat),
    death = p_recv * p_tb * (death_nocat + death_cat),
    cat   = p_recv * p_tb * p_cat_obs
  )
}

# compute_npy_transfer_table(): the NPY-transfer share of each
# cost_*/nocost_* total, in USD. Rule: full transfer for
# success/unsuccessful, 50% for death (death usually occurs within the
# first ~3 months, before the second lump-sum installment is paid),
# zero for non-receipt/No-NPY cells (they never received NPY). Reads
# input/model_input_parameters.xlsx, sheet "NPY transfer" (merged into
# the main parameter workbook 19 Aug 2026 — previously a separate file,
# cost_decomposition_inputs.xlsx; see that file's "Instructions" sheet).
# Used by 04_scenarios.R's Scenario B (health-system vs patient cost
# split): the transfer itself is exact under this rule; health-system/
# patient "care" cost is a flat figure per TB type (cost_healthsystem_*/
# cost_patient_* rows in the "Parameter" sheet) — real-data-or-skip, see
# 04_scenarios.R for the skip behaviour when either is blank.
#
# death_adjusted: TRUE (default) applies the real death multiplier
# (0.5) — this is what a patient/the system ACTUALLY received, and is
# what should be credited to the health-system side in Scenario B (health-system).
# FALSE forces every outcome's multiplier to 1 ("full transfer
# regardless of outcome") — this is NOT more correct, it exists so
# 04_scenarios.R can compute care_cost = total - full_transfer, which
# correctly isolates the system+patient residual from the model's
# EXISTING cost_* totals. Those totals are built under the assumption
# that every individual received the full NPY amount regardless of
# outcome (a stated modelling assumption, not derived from
# outcome-specific data). If death_adjusted=TRUE's smaller transfer
# were subtracted from a total built assuming the full amount, the
# leftover $ difference would be wrongly counted as system/patient
# cost instead of unspent transfer. Using FALSE for the residual
# calculation, then adding back the TRUE (death-adjusted) transfer
# only on the health-system side, isolates the entire effect of the
# death-timing correction to Scenario B (health-system) — Scenario B (patient cost)
# is mathematically unaffected either way, since the transfer was never
# counted as patient cost in the first place.
compute_npy_transfer_table <- function(root, cost_param_names, nocost_param_names, death_adjusted = TRUE) {
  transfer_xlsx <- file.path(root, "input", "model_input_parameters.xlsx")
  # Fails clearly if the sheet is missing entirely, rather than letting
  # read_excel() throw an opaque "sheet not found" error. Needed for
  # Scenario B (cost perspective split).
  if (!("NPY transfer" %in% readxl::excel_sheets(transfer_xlsx)))
    stop("input/model_input_parameters.xlsx has no 'NPY transfer' sheet. ",
         "This is needed for Scenario B (cost perspective split) - see ",
         "that file's 'Instructions' sheet.")
  tp <- readxl::read_excel(transfer_xlsx, sheet = "NPY transfer")
  tp <- tp[!is.na(tp$parameter), ]   # drop blank/spacer rows: an NA in
  # tp$parameter makes `tp$parameter == name` evaluate to NA (not FALSE)
  # for that row, and indexing with a logical vector containing NA
  # returns an NA element instead of excluding the row, so every lookup
  # below would silently return length 2 (value, NA) instead of length
  # 1 if this filter weren't here.
  get_tp <- function(name) tp$base_value[tp$parameter == name]

  # full_transfer_d*tb_usd are already in USD -- the sheet's
  # "Calculation shown" column displays the INR/exchange-rate formula
  # for reference, but base_value (read here) is the plain converted
  # number, not an INR figure needing further conversion.
  full_dstb_usd <- get_tp("full_transfer_dstb_usd")
  full_drtb_usd <- get_tp("full_transfer_drtb_usd")
  m_suc     <- get_tp("multiplier_success")
  m_unsuc   <- get_tp("multiplier_unsuccessful")
  m_death   <- if (death_adjusted) get_tp("multiplier_death") else 1

  all_names <- c(cost_param_names, nocost_param_names)
  transfer_usd <- vapply(all_names, function(nm) {
    n <- tolower(nm)
    is_nocost <- grepl("^nocost_", nm)
    received  <- !(grepl("_nr_", n) | is_nocost)   # non-receipt / No NPY: no transfer
    if (!received) return(0)
    full_usd <- if (grepl("dstb", n)) full_dstb_usd else full_drtb_usd
    mult <- if (grepl("_suc$", n)) m_suc
            else if (grepl("_unsuc$", n)) m_unsuc
            else m_death   # _die
    full_usd * mult
  }, numeric(1))
  setNames(transfer_usd, all_names)
}

# strat_ev(): sums calc_branch() over all six pathway x TB-type
# cells (timely/delayed/non-receipt x DS-TB/DR-TB) for ONE strategy.
# Used for Current NPY, Realistic Improvement, and Ideal Improvement
# (all three have a receipt-group split; No NPY does not — see
# nonpy_ev() below).
#
# p        : the full parameter list for this run (base case or one
#            PSA draw). For Scenario C this is the D1+D2-adjusted
#            copy of the parameters, not the raw base list.
# qv       : QALY set from compute_qalys() or compute_qalys_3yr()
# split_mode: "marginal" (primary/B/C) or "rescale" (Scenario A)
# cap_env  : optional environment tracking how often the 0.999 CHE
#            probability cap is triggered (see che_branch_split())
strat_ev <- function(p, p_timely, p_delayed, p_nr, qv, or_che_outcome,
                      split_mode = "marginal", cap_env = NULL) {
  branches <- list(
    calc_branch(p_timely,  p$p_timely_dstb,
                p$p_timely_dstb_unsuc, p$p_timely_dstb_die,
                p$cost_timely_DSTB_suc, p$cost_timely_DSTB_unsuc, p$cost_timely_DSTB_die,
                qv$dstb_suc, qv$dstb_fail, qv$death,
                p$p_timely_dstb_cat, or_che_outcome, split_mode, "Timely DS-TB", cap_env),
    calc_branch(p_timely,  p$p_timely_drtb,
                p$p_timely_drtb_unsuc, p$p_timely_drtb_die,
                p$cost_timely_DRTB_suc, p$cost_timely_DRTB_unsuc, p$cost_timely_DRTB_die,
                qv$drtb_suc, qv$drtb_fail, qv$death,
                p$p_timely_drtb_cat, or_che_outcome, split_mode, "Timely DR-TB", cap_env),
    calc_branch(p_delayed, p$p_del_dstb,
                p$p_del_dstb_unsuc, p$p_del_dstb_die,
                p$cost_del_DSTB_suc, p$cost_del_DSTB_unsuc, p$cost_del_DSTB_die,
                qv$dstb_suc, qv$dstb_fail, qv$death,
                p$p_del_dstb_cat, or_che_outcome, split_mode, "Delayed DS-TB", cap_env),
    calc_branch(p_delayed, p$p_del_drtb,
                p$p_del_drtb_unsuc, p$p_del_drtb_die,
                p$cost_del_DRTB_suc, p$cost_del_DRTB_unsuc, p$cost_del_DRTB_die,
                qv$drtb_suc, qv$drtb_fail, qv$death,
                p$p_del_drtb_cat, or_che_outcome, split_mode, "Delayed DR-TB", cap_env),
    calc_branch(p_nr,      p$p_nr_dstb,
                p$p_nr_dstb_unsuc, p$p_nr_dstb_die,
                p$cost_nr_DSTB_suc, p$cost_nr_DSTB_unsuc, p$cost_nr_DSTB_die,
                qv$dstb_suc, qv$dstb_fail, qv$death,
                p$p_nr_dstb_cat, or_che_outcome, split_mode, "NR DS-TB", cap_env),
    calc_branch(p_nr,      p$p_nr_drtb,
                p$p_nr_drtb_unsuc, p$p_nr_drtb_die,
                p$cost_nr_DRTB_suc, p$cost_nr_DRTB_unsuc, p$cost_nr_DRTB_die,
                qv$drtb_suc, qv$drtb_fail, qv$death,
                p$p_nr_drtb_cat, or_che_outcome, split_mode, "NR DR-TB", cap_env)
  )
  list(
    cost  = Reduce("+", lapply(branches, `[[`, "cost")),
    qaly  = Reduce("+", lapply(branches, `[[`, "qaly")),
    surv  = Reduce("+", lapply(branches, `[[`, "surv")),
    death = Reduce("+", lapply(branches, `[[`, "death")),
    cat   = Reduce("+", lapply(branches, `[[`, "cat"))
  )
}

# nonpy_ev(): the No NPY counterfactual. Unlike strat_ev(), there is
# no receipt-group level — the corrected TreeAge tree goes directly
# from "No NPY" to DS-TB/DR-TB (all patients behave like the
# non-receipt group, since NPY never existed in this world). Uses
# the SEPARATE nocost_nr_* cost parameters (p$nocost_nr_*), not the
# cost_nr_* parameters used inside strat_ev()'s non-receipt branch —
# see 00_config.R Group B for why these must be different.
nonpy_ev <- function(p, qv, or_che_outcome, split_mode = "marginal", cap_env = NULL) {
  branches <- list(
    calc_branch(1, p$p_nr_dstb, p$p_nr_dstb_unsuc, p$p_nr_dstb_die,
                p$nocost_nr_dstb_suc, p$nocost_nr_dstb_unsuc, p$nocost_nr_dstb_die,
                qv$dstb_suc, qv$dstb_fail, qv$death,
                p$p_nr_dstb_cat, or_che_outcome, split_mode, "No NPY DS-TB", cap_env),
    calc_branch(1, p$p_nr_drtb, p$p_nr_drtb_unsuc, p$p_nr_drtb_die,
                p$nocost_nr_drtb_suc, p$nocost_nr_drtb_unsuc, p$nocost_nr_drtb_die,
                qv$drtb_suc, qv$drtb_fail, qv$death,
                p$p_nr_drtb_cat, or_che_outcome, split_mode, "No NPY DR-TB", cap_env)
  )
  list(
    cost  = Reduce("+", lapply(branches, `[[`, "cost")),
    qaly  = Reduce("+", lapply(branches, `[[`, "qaly")),
    surv  = Reduce("+", lapply(branches, `[[`, "surv")),
    death = Reduce("+", lapply(branches, `[[`, "death")),
    cat   = Reduce("+", lapply(branches, `[[`, "cat"))
  )
}

# run_model(): runs all FOUR strategies (No NPY, Current NPY,
# Realistic Improvement, Ideal Improvement) for one parameter set
# and returns everything needed for the results tables. This is the
# single entry point used by 02_model.R's base case + PSA, and by
# every scenario in 04_scenarios.R (each scenario just supplies a
# different qv / split_mode / p).
run_model <- function(p, qv, or_che_outcome, split_mode = "marginal", cap_env = NULL) {
  soc   <- strat_ev(p, as.numeric(p$p_soc_timely), as.numeric(p$p_soc_del), as.numeric(p$p_soc_nr),
                     qv, or_che_outcome, split_mode, cap_env)
  ri    <- strat_ev(p, as.numeric(p$p_ri_timely), as.numeric(p$p_ri_delayed), as.numeric(p$p_ri_nr),
                     qv, or_che_outcome, split_mode, cap_env)
  ii    <- strat_ev(p, 1, 0, 0, qv, or_che_outcome, split_mode, cap_env)
  nonpy <- nonpy_ev(p, qv, or_che_outcome, split_mode, cap_env)

  list(
    cost_soc    = soc$cost,    qaly_soc    = soc$qaly,    surv_soc    = soc$surv,
    death_soc   = soc$death,   cat_soc     = soc$cat,
    cost_ri     = ri$cost,     qaly_ri     = ri$qaly,     surv_ri     = ri$surv,
    death_ri    = ri$death,    cat_ri      = ri$cat,
    cost_ii     = ii$cost,     qaly_ii     = ii$qaly,     surv_ii     = ii$surv,
    death_ii    = ii$death,    cat_ii      = ii$cat,
    cost_nonpy  = nonpy$cost,  qaly_nonpy  = nonpy$qaly,  surv_nonpy  = nonpy$surv,
    death_nonpy = nonpy$death, cat_nonpy   = nonpy$cat
  )
}

# new_cap_env(): fresh CHE-probability-cap tracking environment.
# Call this before each new base-case or PSA run so counts from a
# previous run/scenario don't bleed into the next one's diagnostics.
new_cap_env <- function() {
  e <- new.env()
  e$count <- 0L
  e$nodes <- list()
  e
}

# ============================================================
# PSA SAMPLING DISTRIBUTIONS
# ============================================================

# rbeta_ci(): draws from a Beta distribution fitted (by method of
# moments) to a mean (mu) and a 95% CI (lo, hi). Used for
# probabilities and utilities, which must stay within [0, 1].
# If the CI is degenerate (se <= 0) or mu is at a boundary, just
# returns the point estimate n times rather than erroring.
rbeta_ci <- function(n, mu, lo, hi, diag_env = NULL) {
  se <- (hi - lo) / (2 * 1.96)
  if (se <= 0 || mu <= 0 || mu >= 1) return(rep(mu, n))
  phi <- mu * (1 - mu) / se^2 - 1
  if (phi <= 0) {
    if (!is.null(diag_env)) diag_env$beta_phi_fail <- diag_env$beta_phi_fail + 1L
    return(rep(mu, n))
  }
  rbeta(n, mu * phi, (1 - mu) * phi)
}

# rbeta_ci_truncated_upper(): same parameterisation as rbeta_ci(),
# but the draw is constrained to never exceed 'upper'. Used to keep a
# "failure" utility draw from ever exceeding the "success" utility
# draw sampled in the same PSA iteration — successfully treated
# patients should never have a lower quality of life than those whose
# treatment failed, so failure's Beta distribution is truncated at
# whatever success value was just drawn (see the DS-TB/DR-TB PSA
# sampling steps in sample_psa_params() below for where this is
# applied). Method: inverse-CDF (probability integral transform),
# restricted to the probability mass below 'upper'. This is exact
# (not rejection sampling) and always returns a valid draw.
rbeta_ci_truncated_upper <- function(mu, lo, hi, upper) {
  se <- (hi - lo) / (2 * 1.96)
  if (se <= 0 || mu <= 0 || mu >= 1) return(min(mu, upper))
  phi <- mu * (1 - mu) / se^2 - 1
  if (phi <= 0) return(min(mu, upper))
  alpha <- mu * phi
  beta  <- (1 - mu) * phi
  p_upper <- pbeta(upper, alpha, beta)
  if (p_upper < 1e-8) return(upper * 0.999)
  qbeta(runif(1, 0, p_upper), alpha, beta)
}

# rgamma_ci(): draws from a Gamma distribution fitted (by method of
# moments) to a mean and 95% CI. Used for costs, which must stay
# positive (Gamma has no upper bound, unlike Beta).
rgamma_ci <- function(n, mu, lo, hi) {
  se <- (hi - lo) / (2 * 1.96)
  if (se <= 0 || mu <= 0) return(rep(mu, n))
  shape <- (mu / se)^2
  rate  <- mu / se^2
  rgamma(n, shape = shape, rate = rate)
}

# ============================================================
# PSA PARAMETER SAMPLER
# ============================================================
# Draws n parameter sets for the probabilistic sensitivity analysis.
# Each PSA iteration does, in order:
#   1. Dirichlet draws for probability groups (sum-to-1 constraint)
#   2. Beta draws for single (non-grouped) probabilities & utilities
#   3. Beta draws for CHE prevalences
#   4. Gamma draws for Excel cost parameters
#   5. Gamma draws for No NPY costs (nocost_nr_*)
#   6. RI clamp: p_ri_timely must be >= p_soc_timely
#   7. Utility ordering: success utility >= failure utility
#   8. Post-TB excess mortality (SMR) draw — Scenario F only
#
# Step 1 — Dirichlet groups:
#   Dirichlet groups (which include the receipt proportions
#   p_soc_timely/p_soc_del/p_soc_nr) derive their concentration (phi)
#   from the same per-parameter mean+CI logic used for Beta parameters:
#   fit from the Excel ci_lower/ci_upper columns, falling back to
#   +/-20% only when a parameter's CI is genuinely missing. There is
#   no longer a separate "pct20" global mode — every parameter uses
#   its own real Excel CI. p_soc_timely/p_soc_del/p_soc_nr (base
#   0.1711/0.6248/0.2041) previously carried an unrealistically narrow
#   sample-derived CI (an artifact of the large underlying cohort
#   size, not genuine real-world variability); their Excel CI has
#   since been widened to +/-20% directly (2026-08-19), so this
#   function's ordinary CI-fitting logic now handles them correctly
#   without any special-casing.
sample_psa_params <- function(n, base, beta_set, cost_params,
                               dirichlet_groups, dirichlet_group_names,
                               ci_lower, ci_upper, cat_params, cat_ci_lower, cat_ci_upper,
                               nocost_nr_params, diag_env) {
  lapply(seq_len(n), function(i) {
    p <- base

    # ── 1. Dirichlet draws (sum-to-1 groups) ──────────────────
    for (grp in dirichlet_group_names) {
      pnames <- dirichlet_groups$parameter[dirichlet_groups$sum_check_group == grp]
      raw_phi <- sapply(pnames, function(nm) {
        mu <- as.numeric(base[[nm]])
        if (length(mu) == 0 || is.na(mu)) return(NA_real_)
        lo <- as.numeric(ci_lower[[nm]]); hi <- as.numeric(ci_upper[[nm]])
        # A dirichlet-tagged parameter missing real CI data must NOT
        # silently propagate NA into se/phi below (that produces an
        # NA phi, and `if (phi <= 0)` on an NA throws a hard R error
        # -- "missing value where TRUE/FALSE needed" -- crashing the
        # whole PSA loop). Falling back to +/-20% for THIS parameter
        # only keeps every other parameter using real data.
        if (is.na(lo) || is.na(hi)) { lo <- mu * 0.8; hi <- mu * 1.2 }
        se  <- (hi - lo) / (2 * 1.96)
        phi <- mu * (1 - mu) / se^2 - 1
        if (phi <= 0) { diag_env$dir_phi_warn <- diag_env$dir_phi_warn + 1L; phi <- 1 }
        phi
      })
      valid_phi  <- raw_phi[!is.na(raw_phi)]
      phi_common <- if (length(valid_phi) > 0) length(valid_phi) / sum(1 / valid_phi) else 1000
      alphas <- sapply(seq_along(pnames), function(j)
        max(as.numeric(base[[ pnames[j] ]]) * phi_common, 0.01))
      samp <- as.numeric(gtools::rdirichlet(1, alphas))
      for (j in seq_along(pnames)) p[[pnames[j]]] <- samp[j]
    }

    # ── 2. Beta draws (utilities and single probabilities) ────
    for (nm in beta_set) {
      mu <- as.numeric(base[[nm]])
      lo <- as.numeric(ci_lower[[nm]]); hi <- as.numeric(ci_upper[[nm]])
      # Falls back to +/-20% whenever real CI data is missing for THIS
      # parameter. Without this, a parameter with no ci_lower/ci_upper
      # would silently fail the is.na() check below and never get
      # sampled at all -- freezing it at its base value for every
      # single PSA iteration (zero uncertainty, no warning, easy to
      # miss).
      if (is.na(lo) || is.na(hi)) { lo <- mu * 0.8; hi <- mu * 1.2 }
      if (isTRUE(!is.na(lo)) && isTRUE(!is.na(hi)) && isTRUE(mu > 0) && isTRUE(mu < 1))
        p[[nm]] <- rbeta_ci(1, mu, lo, hi, diag_env)
    }

    # ── 3. Beta draws for CHE prevalences (+/-20% on derived values)
    for (nm in names(cat_params)) {
      mu <- cat_params[[nm]]
      lo <- cat_ci_lower[[nm]]; hi <- cat_ci_upper[[nm]]
      if (mu > 0 && mu < 1) p[[nm]] <- rbeta_ci(1, mu, lo, hi, diag_env)
    }

    # ── 4. Gamma draws for Excel cost parameters ──────────────
    # Same per-parameter fallback pattern as step 2 (Beta draws): use
    # the real Excel CI wherever it's actually present, and fall back
    # to +/-20% only when that specific parameter's CI is missing.
    for (nm in cost_params) {
      mu <- as.numeric(base[[nm]])
      lo <- as.numeric(ci_lower[[nm]]); hi <- as.numeric(ci_upper[[nm]])
      if (is.na(lo) || is.na(hi)) { lo <- mu * 0.8; hi <- mu * 1.2 }
      if (isTRUE(mu > 0)) p[[nm]] <- rgamma_ci(1, mu, lo, hi)
    }

    # ── 5. Gamma draws for No NPY costs ────────────────────────
    # nocost_nr_* have their own real CI bounds from PI-provided
    # data, so those are used directly rather than +/-20%.
    for (nm in names(nocost_nr_params)) {
      vals <- nocost_nr_params[[nm]]
      p[[nm]] <- rgamma_ci(1, vals$base, vals$lo, vals$hi)
    }

    # ── 6. RI clamp: timely receipt must not fall below SoC ───
    # In PSA, Dirichlet sampling can occasionally produce an RI
    # timely proportion LOWER than the status quo — which would
    # mean "improvement" is actually worse. This clamp prevents
    # that by pinning RI-timely to SoC-timely (if it fell below)
    # and rescaling RI-delayed/RI-nr proportionally so they still
    # sum to 1.
    soc_t <- as.numeric(p[["p_soc_timely"]])
    ri_t  <- as.numeric(p[["p_ri_timely"]])
    if (ri_t < soc_t) {
      diag_env$clamp_count <- diag_env$clamp_count + 1L
      p[["p_ri_timely"]] <- soc_t
      remaining      <- 1 - soc_t
      current_others <- p[["p_ri_delayed"]] + p[["p_ri_nr"]]
      if (current_others > 0) {
        p[["p_ri_delayed"]] <- remaining * (p[["p_ri_delayed"]] / current_others)
        p[["p_ri_nr"]]      <- remaining * (p[["p_ri_nr"]]      / current_others)
      } else {
        br_rem <- as.numeric(base[["p_ri_delayed"]]) + as.numeric(base[["p_ri_nr"]])
        p[["p_ri_delayed"]] <- remaining * (as.numeric(base[["p_ri_delayed"]]) / br_rem)
        p[["p_ri_nr"]]      <- remaining * (as.numeric(base[["p_ri_nr"]])      / br_rem)
      }
    }

    # ── 7. Utility ordering: success >= failure ────────────────
    # A successfully treated patient's quality of life cannot be
    # LOWER than a treatment-failure patient's — that would be a
    # logical impossibility. Independently-sampled Beta draws can
    # violate this when the two distributions overlap (a naive
    # post-hoc swap of the two draws would "fix" the ordering but
    # distorts both distributions' shapes). Instead:
    #   DS-TB : success sampled freely; failure sampled from a
    #           Beta TRUNCATED at whatever success value was drawn.
    #   DR-TB : success and failure share an identical base value
    #           and CI in the source data (outcome is unknown
    #           during treatment) — so ONE shared draw is used for
    #           both, guaranteeing ordering by construction.
    # Each pair's CI uses the same per-parameter fallback pattern as
    # steps 2/4 above (real Excel CI, +/-20% only when missing).
    ci_pair <- function(nm) {
      mu <- as.numeric(base[[nm]])
      lo <- as.numeric(ci_lower[[nm]]); hi <- as.numeric(ci_upper[[nm]])
      if (is.na(lo) || is.na(hi)) { lo <- mu * 0.8; hi <- mu * 1.2 }
      list(mu = mu, lo = lo, hi = hi)
    }
    dstb_suc_ci <- ci_pair("uv_dstb_during")
    p$uv_dstb_during     <- rbeta_ci(1, dstb_suc_ci$mu, dstb_suc_ci$lo, dstb_suc_ci$hi, diag_env)
    dstb_fail_ci <- ci_pair("uv_dstb_fail_during")
    p$uv_dstb_fail_during <- rbeta_ci_truncated_upper(
                                      dstb_fail_ci$mu, dstb_fail_ci$lo, dstb_fail_ci$hi,
                                      upper = p$uv_dstb_during)
    drtb_suc_ci <- ci_pair("uv_drtb_suc_during")
    uv_drtb_shared       <- rbeta_ci(1, drtb_suc_ci$mu, drtb_suc_ci$lo, drtb_suc_ci$hi, diag_env)
    p$uv_drtb_suc_during  <- uv_drtb_shared
    p$uv_drtb_fail_during <- uv_drtb_shared

    # ── 7b. Same ordering rule for AFTER-treatment utilities ──────
    # Scenario B extends the horizon to 3 years using uv_*_after
    # parameters, which need the same success >= failure ordering
    # constraint as the year-1 utilities above. The DS-TB pair's CIs
    # overlap (success 0.96 [0.864, 0.990] vs failure 0.85 [0.765,
    # 0.935]), so independent draws would occasionally produce a
    # FAILURE utility higher than the SUCCESS utility — the same
    # truncated-Beta treatment used above is applied here too.
    #   DR-TB after-treatment has no CI overlap (success 0.88 [0.792,
    #   0.968] vs failure 0.51 [0.459, 0.561]), but is handled by the
    #   same guarded code below so the rule holds if those inputs
    #   ever change.
    # Only runs when these parameters are actually being sampled
    # (i.e. Scenario B); the primary year-1 model never touches them.
    if ("uv_dstb_suc_after" %in% beta_set && "uv_dstb_fail_after" %in% beta_set) {
      dstb_suc_after_ci <- ci_pair("uv_dstb_suc_after")
      p$uv_dstb_suc_after  <- rbeta_ci(1, dstb_suc_after_ci$mu, dstb_suc_after_ci$lo, dstb_suc_after_ci$hi, diag_env)
      dstb_fail_after_ci <- ci_pair("uv_dstb_fail_after")
      p$uv_dstb_fail_after <- rbeta_ci_truncated_upper(
                                        dstb_fail_after_ci$mu, dstb_fail_after_ci$lo, dstb_fail_after_ci$hi,
                                        upper = p$uv_dstb_suc_after)
    }
    if ("uv_drtb_suc_after" %in% beta_set && "uv_drtb_fail_after" %in% beta_set) {
      drtb_suc_after_ci <- ci_pair("uv_drtb_suc_after")
      p$uv_drtb_suc_after  <- rbeta_ci(1, drtb_suc_after_ci$mu, drtb_suc_after_ci$lo, drtb_suc_after_ci$hi, diag_env)
      drtb_fail_after_ci <- ci_pair("uv_drtb_fail_after")
      p$uv_drtb_fail_after <- rbeta_ci_truncated_upper(
                                        drtb_fail_after_ci$mu, drtb_fail_after_ci$lo, drtb_fail_after_ci$hi,
                                        upper = p$uv_drtb_suc_after)
    }

    # ── 8. Post-TB excess mortality (SMR) — Scenario F only ───────
    # SMR is a positive ratio with no upper bound of 1 (values run
    # 1.0-4.2+ in the sweep), so it is sampled via
    # rgamma_ci() — the same distribution family used for costs —
    # rather than rbeta_ci(), which assumes a 0-1 range and would be
    # the wrong tool here. post_tb_smr and post_tb_smr_ci come from
    # 00_config.R (Group F), read as globals since this function is
    # not otherwise parameterised for scenario-specific constants.
    # Stored unconditionally: the primary model and Scenarios A, B,
    # D never read p$post_tb_smr, so this has no effect on
    # them. Only Scenario C's model_C()
    # (05_scenario_C_premature_death_qaly.R) uses it, to recompute a
    # per-iteration discounted life expectancy instead of holding the
    # life-expectancy factor fixed across the whole PSA.
    smr_lo <- post_tb_smr_ci[["lo"]]; smr_hi <- post_tb_smr_ci[["hi"]]
    p$post_tb_smr <- rgamma_ci(1, post_tb_smr, smr_lo, smr_hi)

    # Ideal improvement is always fixed at 100% timely — never sampled.
    p$p_ii_timely <- 1; p$p_ii_delayed <- 0; p$p_ii_nr <- 0
    p
  })
}

# ============================================================
# AFTER-TREATMENT UTILITY INJECTION
# ============================================================
# inject_after_tx_utilities(): adds Scenario C's after-treatment
# utilities (00_config.R, Group C) into a base parameter list plus its
# CI lookups. Used by 05_scenario_C_premature_death_qaly.R (Scenario C).
inject_after_tx_utilities <- function(base, ci_lower, ci_upper) {
  for (nm in names(after_tx_utilities)) {
    base[[nm]]     <- after_tx_utilities[[nm]]$base
    ci_lower[[nm]] <- after_tx_utilities[[nm]]$lo
    ci_upper[[nm]] <- after_tx_utilities[[nm]]$hi
  }
  list(base = base, ci_lower = ci_lower, ci_upper = ci_upper)
}

# ============================================================
# OWSA HELPER
# ============================================================
# normalize_group(): used by one-way sensitivity analysis (OWSA) to
# move a single parameter within a sum-to-1 group (e.g. push
# p_soc_timely up) while rescaling the OTHER members of that group
# proportionally so the group still sums to 1. Returns NULL if the
# requested value is out of range or would force another member
# negative (the caller should skip that OWSA point).
normalize_group <- function(p, base, nm, val, prob_groups) {
  grp <- Filter(function(g) nm %in% g, prob_groups)
  if (length(grp) == 0) { p[[nm]] <- val; return(p) }
  if (!is.finite(val) || val < 0 || val > 1) return(NULL)
  grp <- grp[[1]]
  others <- setdiff(grp, nm)
  base_others_sum <- sum(sapply(others, function(x) as.numeric(base[[x]])))
  new_others_sum  <- 1 - val
  if (base_others_sum <= 0 || new_others_sum < 0) return(NULL)
  scale <- new_others_sum / base_others_sum
  p[[nm]] <- val
  for (o in others) p[[o]] <- as.numeric(base[[o]]) * scale
  vals <- sapply(grp, function(x) as.numeric(p[[x]]))
  if (any(!is.finite(vals)) || any(vals < 0) || any(vals > 1)) return(NULL)
  p
}

# ============================================================
# REPORTING HELPERS (used by both model + scenarios for consistent
# tables/plots)
# ============================================================

# print_icer(): builds, prints, and saves the standard 4-strategy
# ICER table (vs No NPY) plus the dampack sequential-dominance
# ("efficiency frontier") table. strats/costs/qalys/cats must be
# in the fixed order [No NPY, Current NPY, Realistic Impr., Ideal Impr.].
# icer_xlsx_path / frontier_xlsx_path are the exact output file
# paths to write to (not a folder — callers pass the full path so
# the primary model and each scenario can use whatever folder
# layout they want without this function guessing subfolder names).
print_icer <- function(label, strats, costs, qalys, cats, wtp,
                        icer_xlsx_path, frontier_xlsx_path) {
  df <- data.frame(
    Strategy      = strats,
    Cost          = round(costs, 4),
    QALY          = round(qalys, 6),
    Incr_Cost     = round(costs - costs[1], 4),
    Incr_QALY     = round(qalys - qalys[1], 6),
    ICER_vs_NoNPY = c(NA, round(ifelse(qalys[-1] == qalys[1], NA,
                                        (costs[-1] - costs[1]) / (qalys[-1] - qalys[1])), 2)),
    NMB           = round(wtp * (qalys - qalys[1]) - (costs - costs[1]), 2),
    Prob_CHE      = round(cats, 6),
    CHE_averted   = round(cats[1] - cats, 6)
  )
  cat(sprintf("\n=== %s ===\n", label))
  print(df, row.names = FALSE)
  write_xlsx(df, icer_xlsx_path)
  icer_damp <- suppressWarnings(dampack::calculate_icers(
    cost = costs, effect = qalys, strategies = strats))
  print(icer_damp)
  write_xlsx(as.data.frame(icer_damp), frontier_xlsx_path)
  invisible(df)
}

# run_scenario_psa(): runs the PSA loop for one scenario given a list
# of per-iteration parameter draws and a function that turns one draw
# into one set of strategy cost/QALY results. Shared by every scenario
# script (04_scenarios.R, 05_scenario_C_premature_death_qaly.R) so the
# loop, progress reporting, and output shape are defined in exactly one
# place — previously duplicated identically in both files.
#
# At large n this loop can run for minutes with zero output otherwise,
# which makes it hard to tell a slow-but-working run from a stuck one.
# Prints ~10 progress ticks (iteration count, %, elapsed seconds) plus
# a final elapsed-time summary; the tick interval is computed from n
# so it scales sensibly at any sample size.
run_scenario_psa <- function(n, param_draws, model_call, label = "PSA") {
  t0 <- Sys.time()
  progress_step <- max(1, floor(n / 10))
  cat(sprintf("  [%s] starting %d PSA iterations...\n", label, n))
  results <- lapply(seq_along(param_draws), function(i) {
    if (i %% progress_step == 0 || i == n) {
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      cat(sprintf("  [%s] %d / %d (%.0f%%) - %.0fs elapsed\n", label, i, n, 100 * i / n, elapsed))
    }
    r <- model_call(param_draws[[i]])
    data.frame(cost_nonpy = r$cost_nonpy, qaly_nonpy = r$qaly_nonpy,
               cost_soc   = r$cost_soc,   qaly_soc   = r$qaly_soc,
               cost_ri    = r$cost_ri,    qaly_ri    = r$qaly_ri,
               cost_ii    = r$cost_ii,    qaly_ii    = r$qaly_ii)
  })
  out <- do.call(rbind, results)
  elapsed_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  [%s] complete: %d iterations in %.1fs (%.0f/sec)\n",
              label, n, elapsed_total, n / max(elapsed_total, 0.001)))
  out
}

# report_pce(): prints the PSA-based probability each NPY strategy is
# cost-effective vs No NPY, at the primary WTP threshold. Shared by
# every scenario script (04_scenarios.R, 05_scenario_C_premature_
# death_qaly.R) — previously duplicated identically in both files,
# same as run_scenario_psa() above.
report_pce <- function(psa_df, label) {
  cat(sprintf("Prob SoC cost-effective (%s) at WTP=$%.0f: %.1f%%\n", label, wtp,
              100 * mean((wtp * (psa_df$qaly_soc - psa_df$qaly_nonpy) - (psa_df$cost_soc - psa_df$cost_nonpy)) > 0)))
  cat(sprintf("Prob RI  cost-effective (%s) at WTP=$%.0f: %.1f%%\n", label, wtp,
              100 * mean((wtp * (psa_df$qaly_ri  - psa_df$qaly_nonpy) - (psa_df$cost_ri  - psa_df$cost_nonpy)) > 0)))
  cat(sprintf("Prob II  cost-effective (%s) at WTP=$%.0f: %.1f%%\n", label, wtp,
              100 * mean((wtp * (psa_df$qaly_ii  - psa_df$qaly_nonpy) - (psa_df$cost_ii  - psa_df$cost_nonpy)) > 0)))
}

# make_ceac_plot(): builds and saves a pairwise CEAC (each NPY
# strategy vs No NPY) from a PSA output data frame. psa_df must
# have columns: cost_nonpy, qaly_nonpy, cost_soc, qaly_soc,
# cost_ri, qaly_ri, cost_ii, qaly_ii.
# png_path / xlsx_path: exact output file paths (see print_icer note).
make_ceac_plot <- function(psa_df, wtp, wtp_range, title_suffix, png_path, xlsx_path) {
  pw <- do.call(rbind, lapply(wtp_range, function(w) {
    data.frame(
      WTP      = w,
      Strategy = c("Current NPY", "Realistic Impr.", "Ideal Impr."),
      P_CE     = c(
        mean((w * (psa_df$qaly_soc - psa_df$qaly_nonpy) -
                (psa_df$cost_soc - psa_df$cost_nonpy)) > 0),
        mean((w * (psa_df$qaly_ri  - psa_df$qaly_nonpy) -
                (psa_df$cost_ri  - psa_df$cost_nonpy)) > 0),
        mean((w * (psa_df$qaly_ii  - psa_df$qaly_nonpy) -
                (psa_df$cost_ii  - psa_df$cost_nonpy)) > 0)
      )
    )
  })) %>%
    mutate(Strategy = factor(Strategy,
                             levels = c("Current NPY", "Realistic Impr.", "Ideal Impr.")))

  p <- ggplot(pw, aes(x = WTP, y = P_CE, colour = Strategy, group = Strategy)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey40") +
    annotate("text", x = wtp * 0.85, y = 0.05,
             label = paste0("WTP\n", wtp_label), size = 3, hjust = 0, colour = "grey40") +
    scale_x_continuous(limits = c(0, ceiling(wtp * 2)),
                       breaks = pretty(c(0, ceiling(wtp * 2)), n = 6),
                       labels = scales::comma) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_colour_manual(values = c("Current NPY"     = "#377EB8",
                                   "Realistic Impr." = "#4DAF4A",
                                   "Ideal Impr."     = "#984EA3")) +
    labs(title    = paste0("CEAC - ", title_suffix),
         subtitle = paste0("Pairwise, each strategy vs No NPY | n=", nrow(psa_df)),
         x = "Willingness-to-Pay (USD/QALY)",
         y = "Probability cost-effective vs No NPY",
         colour = "Strategy") +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")

  ggsave(png_path, p, width = 8, height = 5, dpi = 150)
  write_xlsx(pw, xlsx_path)
  invisible(p)
}

# make_ceac_plot_vs_current(): same idea as make_ceac_plot() above, but
# referenced against CURRENT NPY instead of No NPY — answers "is Ideal
# actually worth it over Current," not just "over No NPY." Only
# Realistic Impr. and Ideal Impr. have a meaningful line (Current NPY
# vs itself is trivially 0 at every WTP, so it's not plotted).
make_ceac_plot_vs_current <- function(psa_df, wtp, wtp_range, title_suffix, png_path, xlsx_path) {
  pw <- do.call(rbind, lapply(wtp_range, function(w) {
    data.frame(
      WTP      = w,
      Strategy = c("Realistic Impr.", "Ideal Impr."),
      P_CE     = c(
        mean((w * (psa_df$qaly_ri - psa_df$qaly_soc) -
                (psa_df$cost_ri  - psa_df$cost_soc)) > 0),
        mean((w * (psa_df$qaly_ii - psa_df$qaly_soc) -
                (psa_df$cost_ii  - psa_df$cost_soc)) > 0)
      )
    )
  })) %>%
    mutate(Strategy = factor(Strategy, levels = c("Realistic Impr.", "Ideal Impr.")))

  p <- ggplot(pw, aes(x = WTP, y = P_CE, colour = Strategy, group = Strategy)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey40") +
    annotate("text", x = wtp * 0.85, y = 0.05,
             label = paste0("WTP\n", wtp_label), size = 3, hjust = 0, colour = "grey40") +
    scale_x_continuous(limits = c(0, ceiling(wtp * 2)),
                       breaks = pretty(c(0, ceiling(wtp * 2)), n = 6),
                       labels = scales::comma) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_colour_manual(values = c("Realistic Impr." = "#4DAF4A",
                                   "Ideal Impr."     = "#984EA3")) +
    labs(title    = paste0("CEAC vs Current NPY - ", title_suffix),
         subtitle = paste0("Pairwise vs Current NPY, not No NPY | n=", nrow(psa_df)),
         x = "Willingness-to-Pay (USD/QALY)",
         y = "Probability cost-effective vs Current NPY",
         colour = "Strategy") +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")

  ggsave(png_path, p, width = 8, height = 5, dpi = 150)
  write_xlsx(pw, xlsx_path)
  invisible(p)
}

# make_multi_ceac_plot(): a TRUE multi-strategy CEAC — probability
# each strategy has the HIGHEST net monetary benefit among ALL FOUR
# strategies simultaneously (No NPY, Current, Realistic, Ideal), swept
# across wtp_range. This is different from make_ceac_plot() above,
# which only asks "does this strategy beat No NPY," strategy by
# strategy, independently. A true CEAC answers "which one strategy
# would you actually pick," and is the only one of the three CEAC
# functions in this file where No NPY appears as its own curve.
make_multi_ceac_plot <- function(psa_df, wtp_range, title_suffix, png_path, xlsx_path) {
  strategy_names <- c("No NPY", "Current NPY", "Realistic Impr.", "Ideal Impr.")

  pw <- do.call(rbind, lapply(wtp_range, function(w) {
    nmb_nonpy <- w * psa_df$qaly_nonpy - psa_df$cost_nonpy
    nmb_soc   <- w * psa_df$qaly_soc   - psa_df$cost_soc
    nmb_ri    <- w * psa_df$qaly_ri    - psa_df$cost_ri
    nmb_ii    <- w * psa_df$qaly_ii    - psa_df$cost_ii

    nmb_mat <- cbind(nmb_nonpy, nmb_soc, nmb_ri, nmb_ii)
    optimal_idx <- max.col(nmb_mat, ties.method = "first")

    data.frame(
      WTP      = w,
      Strategy = factor(strategy_names, levels = strategy_names),
      P_Optimal = vapply(seq_along(strategy_names),
                          function(i) mean(optimal_idx == i), numeric(1))
    )
  }))

  wtp_vline_x <- wtp_range[which.min(abs(wtp_range - wtp))]
  p <- ggplot(pw, aes(x = WTP, y = P_Optimal, colour = Strategy, group = Strategy)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = wtp_vline_x, linetype = "dashed", colour = "grey40") +
    annotate("text", x = wtp_vline_x * 0.85, y = 0.05,
             label = paste0("WTP\n", wtp_label), size = 3, hjust = 0, colour = "grey40") +
    scale_x_continuous(limits = c(0, max(wtp_range)),
                       breaks = pretty(c(0, max(wtp_range)), n = 6),
                       labels = scales::comma) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_colour_manual(values = c("No NPY" = "#E41A1C", "Current NPY" = "#377EB8",
                                   "Realistic Impr." = "#4DAF4A", "Ideal Impr." = "#984EA3")) +
    labs(title    = paste0("Multi-strategy CEAC - ", title_suffix),
         subtitle = paste0("P(strategy has the single highest NMB, all four at once)\n",
                            "n=", nrow(psa_df)),
         x = "Willingness-to-Pay (USD/QALY)",
         y = "Probability of being the optimal (highest-NMB) strategy",
         colour = "Strategy") +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")

  ggsave(png_path, p, width = 8, height = 5, dpi = 150)
  write_xlsx(pw, xlsx_path)
  invisible(p)
}

# ============================================================
# PSA CONVERGENCE DIAGNOSTIC
# ============================================================
# check_psa_convergence(): checks whether the PSA ran with enough
# iterations for its results to be numerically stable — a PSA mean
# carries Monte Carlo sampling error like a survey estimate, shrinking
# only as 1/sqrt(n) (Briggs, Claxton & Sculpher, "Decision Modelling
# for Health Economic Evaluation"). Two checks, per strategy's NMB:
#   1. Tail stability — does the running mean (average of draws 1..i)
#      over the last tail_frac of iterations stay within
#      psa_convergence_tol_pct of the final running mean?
#   2. Monte Carlo SE as % of mean (sd/sqrt(n)) — flagged "WARN" if it
#      exceeds se_warn_pct, or if the tail isn't stable; else "PASS".
# Saves a running-mean-vs-iteration plot (one line per strategy) and
# a summary table (final mean NMB, SE, SE%, tail-stability, status).
#
# se_warn_pct defaults to psa_convergence_tol_pct (00_config.R) so
# both checks share one tolerance constant. Relaxed from an initial
# 2% to 5%: the primary model's NMB is small (~$25-28), so even a
# well-converged PSA has an SE that's a large percentage of that
# small mean — see 00_config.R Group H for the full numbers.
#
# ARGUMENTS
#   psa_df         : PSA output data frame (e.g. psa_raw, psa_A...)
#                    with the NMB columns named in nmb_cols.
#   nmb_cols       : NMB column names, e.g. c("nmb_soc","nmb_ri","nmb_ii").
#   strategy_labels: display names, same length/order as nmb_cols.
#   title_suffix   : appended to the plot title (e.g. "Scenario A").
#   png_path       : exact output PNG path.
#   xlsx_path      : exact output XLSX path for the summary table.
#   tail_frac      : fraction of iterations (from the end) checked
#                    for stability (0.10 = last 10%).
#   se_warn_pct    : SE-as-%-of-mean threshold for "WARN".
check_psa_convergence <- function(psa_df, nmb_cols, strategy_labels,
                                   title_suffix = "", png_path, xlsx_path,
                                   tail_frac = 0.1,
                                   se_warn_pct = psa_convergence_tol_pct * 100) {

  n <- nrow(psa_df)
  tail_n <- max(1, floor(n * tail_frac))

  running_list  <- vector("list", length(nmb_cols))
  summary_rows  <- vector("list", length(nmb_cols))

  for (i in seq_along(nmb_cols)) {
    col   <- nmb_cols[i]
    label <- strategy_labels[i]
    x     <- psa_df[[col]]

    running_mean <- cumsum(x) / seq_along(x)
    final_mean   <- running_mean[n]

    # Tail-stability check: does the running mean, over the LAST
    # tail_n iterations, stay within +/- psa_convergence_tol_pct of
    # the final (all-iterations) running-mean value?
    tail_vals   <- running_mean[(n - tail_n + 1):n]
    tol         <- psa_convergence_tol_pct * abs(final_mean)
    tail_stable <- all(abs(tail_vals - final_mean) <= tol)

    mc_se        <- sd(x) / sqrt(n)
    se_pct       <- 100 * mc_se / abs(final_mean)
    status       <- if (se_pct > se_warn_pct || !tail_stable) "WARN" else "PASS"

    # WHY THIS EXISTS: a bare "WARN" tells a reader something is off
    # but not what to do about it, and the two ways this check can
    # fail have very different implications. This builds a one-line,
    # human-readable explanation so the console output and the saved
    # summary table are self-explanatory without having to re-derive
    # the reasoning from se_pct/tail_stable by hand every time.
    #   - tail unstable: the running mean was still visibly drifting
    #     in the last tail_frac of iterations — a genuine sign the run
    #     has not settled. More iterations would likely change the
    #     answer. This is the more serious failure mode.
    #   - tail stable but SE% high: the running mean HAS settled, but
    #     the Monte Carlo standard error is still a large PERCENTAGE
    #     of the mean — almost always because the mean itself is
    #     small (comparing strategies with similar outcomes gives a
    #     small NMB difference, so the same absolute noise looks like
    #     a big percentage). This is expected for small-effect
    #     comparisons (e.g. Scenario A, whose NMB values are the
    #     smallest of any scenario) and is not, by itself, evidence
    #     the estimate is unreliable.
    reason <- if (status == "PASS") {
      "Converged: running mean settled and Monte Carlo SE is small relative to the mean."
    } else if (!tail_stable && se_pct > se_warn_pct) {
      sprintf("WARN: running mean had NOT settled in the last %.0f%% of iterations, AND Monte Carlo SE is %.1f%% of the mean (> %.1f%% threshold). Likely needs more PSA iterations.",
              100 * tail_frac, se_pct, se_warn_pct)
    } else if (!tail_stable) {
      sprintf("WARN: running mean had NOT settled in the last %.0f%% of iterations, though SE (%.1f%%) is within tolerance. Likely needs more PSA iterations.",
              100 * tail_frac, se_pct)
    } else {
      sprintf("WARN: running mean has settled, but Monte Carlo SE is %.1f%% of the mean (> %.1f%% threshold). Usually means the mean itself is small (similar-outcome strategies), not that the run is unstable -- check final_mean_nmb before assuming this needs more iterations.",
              se_pct, se_warn_pct)
    }

    running_list[[i]] <- data.frame(
      iteration    = seq_len(n),
      running_mean = running_mean,
      strategy     = label
    )
    summary_rows[[i]] <- data.frame(
      strategy         = label,
      final_mean_nmb   = round(final_mean, 2),
      mc_se            = round(mc_se, 4),
      se_pct_of_mean   = round(se_pct, 4),
      tail_stable      = tail_stable,
      status           = status,
      reason           = reason
    )
    cat(sprintf("  [%s] %s: %s\n", title_suffix, label, reason))
  }

  running_df <- do.call(rbind, running_list)
  summary_df <- do.call(rbind, summary_rows)

  running_df$strategy <- factor(running_df$strategy, levels = strategy_labels)

  p <- ggplot(running_df, aes(x = iteration, y = running_mean,
                               colour = strategy, group = strategy)) +
    geom_line(linewidth = 0.8) +
    labs(title    = paste0("PSA Convergence - ", title_suffix),
         subtitle = paste0("Running mean NMB vs PSA iteration | n=", n),
         x = "PSA iteration",
         y = "Running mean NMB (USD)",
         colour = "Strategy") +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")

  ggsave(png_path, p, width = 8, height = 5, dpi = 150)
  write_xlsx(summary_df, xlsx_path)

  invisible(list(plot = p, summary = summary_df, running_data = running_df))
}
