# NPY Cost-Utility Analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22092766.svg)](https://doi.org/10.5281/zenodo.22092766)

R decision-analytic cost-utility model for the Nikshay Poshan Yojana
(NPY) tuberculosis cash-transfer programme in India.

Compares four strategies (No NPY, Current NPY, Realistic Improvement,
Ideal Improvement) via a decision tree with probabilistic sensitivity
analysis, scenario analyses (temporal mismatch correction,
health-system/patient cost perspectives, premature-death QALY loss,
DR-TB utility ratio), value-of-information analysis, and
willingness-to-pay threshold sensitivity.

## Getting started

```
Rscript script/setup.R          # one-time bootstrap: packages, folders, input checks
Rscript script/02_model.R       # base case, primary PSA, OWSA, coverage sweep
Rscript script/03_plots.R       # primary-model figures
Rscript script/04_scenarios.R   # scenarios A, B, D
Rscript script/05_scenario_C_premature_death_qaly.R   # scenario C (lifetime extension)
Rscript script/06_value_of_information.R              # EVPI / EVPPI
Rscript script/07_scenario_costdecomposition.R        # cost decomposition
Rscript script/08_wtp_threshold_sensitivity.R         # WTP threshold sweep
```

See **[script/README.md](script/README.md)** for a full description of
every file and what each script does.

## Data

- `input/model_input_parameters.xlsx` — model probabilities, utilities,
  and costs
- `input/SRS_lifetable_India_2020-24.csv` — India's official
  age-specific mortality table, used by the lifetime-extension scenario
