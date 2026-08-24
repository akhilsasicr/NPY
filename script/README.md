# NPY Cost-Utility Analysis


If you're new: read this file top to bottom, then open `02_model.R` and
read the comments in order — every block explains what it does and
why, not just what the code says.

## Folder contents, and what order to run them in

```
setup.R          RUN THIS FIRST, ONCE, on any new machine (new
                 laptop, a co-author's computer, a cloud R
                 environment). Checks/installs required packages,
                 creates the output/ folder tree, and checks the
                 required input data files are present, with a clear
                 message about what to copy in if not. Not sourced by
                 anything else — a one-time bootstrap, which is why it
                 has no numeric prefix (unlike the files below, which
                 form an execution sequence).

Every script below writes its output under output/ — a single folder
for every run (there is no longer a per-uncertainty-mode split).

00_config.R      Every toggle / assumption / hardcoded constant that
                 is NOT read from the Excel input file. If you want
                 to know "what number did the model use for X", or
                 change an assumption, look here FIRST. Also where
                 n_psa (PSA iteration count) lives, with a comment
                 documenting the real convergence figures behind that
                 choice.

01_helpers.R     Every function shared across the base model and all
                 scenarios. This is where the actual decision-tree
                 math lives (che_branch_split, calc_branch, strat_ev,
                 nonpy_ev, run_model), plus the PSA sampling
                 distributions and reporting helpers.

02_model.R       RUN THIS SECOND (after setup.R). Reads the Excel
                 input, runs the base case + main PSA + OWSA +
                 coverage sweep, and saves every result as a table
                 (.xlsx) or RDS. Produces NO plots itself — writes
                 output/rds/model_env.rds for the scripts below, the
                 full primary-model PSA draws to
                 output/rds/psa_raw_primary.rds (same data as
                 output/psa/PSA_raw_results.xlsx, just faster to
                 reload), and a small subsample of PSA parameter
                 draws to output/rds/evppi_inputs.rds for
                 06_value_of_information.R. Every .rds file this
                 project writes lives under output/rds/ — see that
                 folder for a fast-reload copy of anything also
                 available as .xlsx.

03_plots.R       RUN THIS THIRD. Reads the tables/RDS that 02_model.R
                 saved and produces every plot for the primary model:
                 CE-plane (combined + faceted), incremental CE-plane
                 (combined + faceted), three different CEACs (each
                 strategy vs No NPY; Realistic/Ideal vs Current NPY
                 directly; a true multi-strategy CEAC where No NPY
                 gets its own curve), PSA convergence diagnostic,
                 tornado chart, the NPY coverage scale-up plot, and a
                 paired PSA noise test (Ideal vs Realistic Impr.,
                 within-iteration NMB difference — quantifies whether
                 their apparent NMB gap is distinguishable from PSA
                 noise). Does not re-run the PSA or any sweep — pure
                 reporting. See that file's header for why each plot
                 exists.

04_scenarios.R   RUN THIS FOURTH. Loads output/rds/model_env.rds, runs
                 Scenarios A, B, and D (see "The scenarios" below —
                 Scenario B is skipped cleanly if the real
                 health-system/patient cost split isn't in the Excel
                 input; no placeholder is ever used). Produces its own
                 plots/tables under output/scenarios/, and also saves
                 each scenario's raw PSA draws to
                 output/rds/psa_raw_<letter>.rds (e.g. psa_raw_A.rds;
                 Scenario B saves two, psa_raw_B_hs.rds and
                 psa_raw_B_pt.rds, one per cost perspective) — same
                 data as that scenario's
                 output/scenarios/<name>/tables/*psa_raw.xlsx, just
                 faster to reload for anything that only needs to
                 re-plot, not re-sample.

05_scenario_C_premature_death_qaly.R
                 CAN BE RUN ANY TIME AFTER 02_model.R (independently
                 of 03/04/06 — only depends on 02_model.R's RDS).
                 Numbered to sit right after 04_scenarios.R in run
                 order because it's the same kind of scenario, just
                 materially bigger. Scenario C: adds a discounted
                 premature-death QALY term on top of the 1-year
                 decision tree. Kept in its own file rather than
                 folded into 04_scenarios.R because it is a materially
                 bigger piece of work (a sourced Indian life table, a
                 literature-derived excess-mortality adjustment, a
                 crude-vs-refined method comparison, two robustness
                 sweeps) than a one-line scenario variant — see that
                 file's own header for the full reasoning.

06_value_of_information.R
                 CAN BE RUN ANY TIME AFTER 02_model.R (independently
                 of 03/04/05). Computes EVPI (aggregate value of
                 eliminating all parameter uncertainty) and EVPPI
                 (per-parameter breakdown — which parameter is most
                 worth researching further). Kept out of 02_model.R
                 because "which parameter is worth more research" is
                 a distinct analytical question from "did the primary
                 analysis converge / what's the answer." Loads
                 output/rds/evppi_inputs.rds (written by 02_model.R)
                 to rebuild its PSA object rather than re-running the
                 PSA itself.

07_scenario_costdecomposition.R
                 CAN BE RUN ANY TIME AFTER 02_model.R. Diagnostic-only
                 decomposition of each cost_* total into an explicit
                 NPY-transfer component + residual care cost. Does
                 NOT change any base-case number in 02/04/05/06 —
                 read-only against the main Excel, output-only. See
                 that file's header for the (a) flat vs (b)
                 outcome-weighted variants, and for which input file
                 it currently reads (this has changed recently — see
                 that script's own header for the authoritative
                 answer, since this README is not guaranteed to be
                 updated on the same day as an input-file rename).
                 Independent of Scenario B: no file read/write overlap
                 — Scenario B uses its own inline transfer-table logic
                 via compute_npy_transfer_table(), 01_helpers.R.

08_wtp_threshold_sensitivity.R
                 CAN BE RUN ANY TIME AFTER 02_model.R. Tests the
                 PRIMARY (year-1, unmodified) model's ICER/P(CE)
                 against both WTP thresholds (GDP-per-capita $2,536
                 and the Ochalek-lineage opportunity-cost figure
                 $487) in isolation — separating "does a lower
                 threshold alone change the answer" from Scenario C's
                 "does extending to lifetime QALYs change the
                 answer," which 05 conflates by only ever testing the
                 lower threshold against the lifetime-extended
                 numbers. See that file's header for the full
                 reasoning.
```

`02_model.R`, `03_plots.R`, `04_scenarios.R`,
`05_scenario_C_premature_death_qaly.R`, `06_value_of_information.R`,
and `08_wtp_threshold_sensitivity.R` each `source()` `00_config.R` and
`01_helpers.R` automatically — you never need to run those two files
directly.
(`07_scenario_costdecomposition.R` is the one exception: it sources
only `00_config.R`, since it is fully self-contained and doesn't use
any shared function from `01_helpers.R`.)
None of these scripts source each other by filename (each
independently sources `00_config.R`/`01_helpers.R` and loads
`output/rds/model_env.rds` by path), so the numeric prefixes are a
run-order label, not a functional dependency. `setup.R` is separate:
run it by itself, once, before any of the numbered scripts.

## Input files this project reads

```
input/model_input_parameters.xlsx   All probabilities, utilities and
                                    costs, sheet "Parameter" — including
                                    the four flat health-system/patient
                                    cost figures Scenario B needs
                                    (cost_healthsystem_DSTB,
                                    cost_patient_DSTB,
                                    cost_healthsystem_DRTB,
                                    cost_patient_DRTB). Scenario B is
                                    skipped cleanly if any of the four
                                    are missing — no placeholder or
                                    guessed split is ever used.

input/SRS_lifetable_India_2020-24.csv
                                    India's SRS abridged life table
                                    (Registrar General of India, "SRS
                                    Based Abridged Life Tables
                                    2020-24," published May 2026):
                                    age-specific annual probability of
                                    death (qx_annual). Used ONLY by
                                    Scenario C, to work out how many
                                    more years a TB survivor is
                                    expected to live.
```

## The scenarios

| | What it changes vs the primary model |
|---|---|
| A | Temporal mismatch correction: Rs.1000 current costs vs Rs.500-era effectiveness data — models the effectiveness gain expected from the larger transfer (CHE protection + treatment outcomes), holding coverage and cost constant |
| B | Cost perspective, health-system and patient (one scenario, two views): re-runs the same tree with only the health-system share or only the patient share of cost. Skipped entirely if the real flat cost-split figures aren't in `model_input_parameters.xlsx` — no placeholder is ever used |
| C | Premature-death QALY loss — see below (own script, `05_scenario_C_premature_death_qaly.R`) |
| D | DR-TB utility derived as a ratio off India's own DS-TB utility, instead of importing a Thailand study's absolute value directly — see `dr_ds_utility_ratio`, `00_config.R` Group D, for the citation and caveats |

A 3-year QALY-horizon variant and a temporal-mismatch-only "outcomes
unchanged" variant were both tried and dropped (see `WORKLOG.md`) —
neither changed any conclusion and neither is carried forward as a
scenario.

### Scenario C in particular

A 1-year decision-tree horizon discards most of a mortality-reducing
intervention's value: preventing a death is worth decades of life,
but a 1-year model only ever scores one year. Scenario C's remedy is
to keep the 1-year decision tree exactly as it is, but change what a
terminal node is worth:

- someone who **survives** year 1 also gets credited with their
  remaining discounted life expectancy, at post-treatment utility
- someone who **dies** still gets 0, unchanged

Remaining life expectancy is not guessed. It is computed from
`input/SRS_lifetable_India_2020-24.csv` by `discounted_life_expectancy()`
(`01_helpers.R`), which walks forward year by year from the cohort's
mean age, tracking the probability of still being alive and applying
a half-year correction for people who die during a year.

Scenario C reports the ICER against two thresholds — the
GDP-per-capita figure ($2,536) and the Ochalek-lineage
opportunity-cost figure ($487) — and runs three robustness sweeps:

1. **SMR sweep** — multiplies every age-specific death probability by
   a standardised mortality ratio, to represent the excess mortality
   TB survivors experience. Re-derives life expectancy from the same
   life table each time, so the figures stay actuarially real.
2. **Raw-years sweep** — a blunter version: how few future years
   would we have to credit per averted death before the conclusion
   flips?
3. **Age-distribution sweep** — the calculation uses a single mean
   cohort age (38.231); this sweep re-derives life expectancy at
   several ages spanning the real distribution (mean ± SD, and the
   median), to check how much the ICER moves versus using the mean
   alone.

All three exist to answer "doesn't assuming TB survivors live a
normal lifespan (or that everyone is exactly the mean age) inflate
your result?" without committing to a single borrowed estimate.
