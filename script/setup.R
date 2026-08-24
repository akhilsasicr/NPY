# ============================================================
# NPY CUA —SETUP (run this FIRST, once, on any new machine)
# ============================================================
# WHO THIS IS FOR
#   Anyone opening this project for the first time — a new machine, 
#   or a cloud R environment (Posit Cloud,RStudio Server, etc.). 
#   Run this script once before 02_model.R.
#   It does not run the model itself — it only checks/prepares
#   everything the model needs, and tells you clearly if something
#   is missing, instead of letting the pipeline fail with a cryptic
#   error partway through a run.
#
# WHAT IT DOES, IN ORDER
#   1. Confirms the project root is found correctly (via here::here(),
#      anchored by NPY.Rproj at the project root — this is what makes
#      every other script's file.path(root, ...) calls work
#      regardless of which OS or working directory you're in).
#   2. Checks every R package this project uses is installed, and
#      installs any that are missing.
#   3. Creates the full folder trees.
#   4. Checks that the required INPUT DATA FILES are present, and if
#      not, tells you exactly what to copy in.
#   5. Prints basic environment info (OS, R version), useful if
#      something behaves differently on a different machine.
#
# CROSS-PLATFORM NOTE
#   Every path in this project is built with file.path() (not raw
#   "/" or "\\" strings), and the project root is found via
#   here::here() rather than a hardcoded absolute path — both of
#   these are what make the scripts run unmodified on Windows, macOS,
#   Linux, and cloud R environments. Nothing else needs to change
#   between platforms.
# ============================================================

cat("========================================================\n")
cat("NPY — SETUP\n")
cat("========================================================\n\n")

# ── Step 1: locate the project root ───────────────────────────
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
root <- here::here()
cat(sprintf("Project root detected as:\n  %s\n", root))
if (!file.exists(file.path(root, "NPY.Rproj"))) {
  cat("WARNING - NPY.Rproj not found at the detected root.\n")
  cat("  here::here() may have anchored to the wrong folder. Open this\n")
  cat("  project via NPY.Rproj (double-click it, or File > Open Project\n")
  cat("  in RStudio) rather than opening a loose script file, so the\n")
  cat("  project root is detected reliably on every machine.\n")
} else {
  cat("OK - NPY.Rproj found; project root is reliable.\n")
}

# ── Step 2: check/install required packages ───────────────────
cat("\n-- Checking required packages --\n")
required_packages <- c("readxl", "writexl", "dplyr", "tidyr", "ggplot2",
                       "gtools", "dampack", "scales", "here")
missing_packages <- character(0)
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  OK   %s\n", pkg))
  } else {
    cat(sprintf("  MISSING   %s\n", pkg))
    missing_packages <- c(missing_packages, pkg)
  }
}
if (length(missing_packages) > 0) {
  cat(sprintf("\nInstalling %d missing package(s): %s\n",
              length(missing_packages), paste(missing_packages, collapse = ", ")))
  install.packages(missing_packages)
  # Re-check after install, in case any failed silently (e.g. no internet, 
  # or a package needing compilation tools not present on this machine 
  # — this is the most common cross-platform snag).
  still_missing <- missing_packages[!vapply(missing_packages, requireNamespace,
                                             logical(1), quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop("Could not install: ", paste(still_missing, collapse = ", "),
         "\nInstall these manually before running 02_model.R.")
  }
  cat("OK - All packages now installed.\n")
} else {
  cat("OK - All required packages already installed.\n")
}

# ── Step 3: create the folder structure ───────────────────────
cat("\n-- Creating folder structure --\n")
# A single output/ folder for every run (no more pct20/source mode
# split — removed 19 Aug 2026; every parameter now uses its own real
# Excel CI, see 00_config.R). setup.R pre-creates the tree as a
# convenience/sanity check; the numbered scripts also create their
# own folders at runtime regardless (recursive=TRUE dir.create calls).
scenario_subdirs <- c(
  "A_temporal_mismatch", "B_cost_perspective",
  "C_premature_death_qaly", "D_dr_utility_ratio"
)
out_root <- "output"
dirs <- c("input", out_root,
          file.path(out_root, "tables"),
          file.path(out_root, "plots"),
          file.path(out_root, "psa"),
          # output/rds/ holds every .rds file this project writes:
          # model_env.rds, evppi_inputs.rds (06's EVPI/EVPPI subsample),
          # and each scenario's raw PSA draws (psa_raw_primary.rds,
          # psa_raw_A.rds, ...), saved alongside the equivalent .xlsx.
          file.path(out_root, "rds"),
          file.path(out_root, "scenarios", scenario_subdirs, "tables"),
          file.path(out_root, "scenarios", scenario_subdirs, "plots"))
for (d in dirs) dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
cat(sprintf("OK - %d folders ready under %s\n", length(dirs), root))

# ── Step 4: check required input data files ───────────────────
# WHAT TO COPY IN, AND FROM WHERE:
#   input/model_input_parameters.xlsx
#     The main parameter file: probabilities, utilities, costs.
#     Sheet "Parameter". This has no external public source — it is
#     this project's own compiled dataset. Copy it from wherever the
#     project's shared drive / machine keeps it.
#   input/SRS_lifetable_India_2020-24.csv
#     India's official age-specific mortality table, used only by
#     Scenario C (05_scenario_C_premature_death_qaly.R). Derived from
#     "SRS Based Abridged Life Tables 2020-24," Office of the
#     Registrar General & Census Commissioner, India (May 2026),
#     https://censusindia.gov.in/nada/index.php/catalog/45558 — see
#     00_config.R (Group F) for exactly how it was built from that
#     source. If missing, only Scenario C is blocked; 02_model.R and
#     04_scenarios.R (Scenarios A, B, D) do not need this file.
cat("\n-- Checking required input files --\n")
required_files <- list(
  list(path = "input/model_input_parameters.xlsx",
       note = "Main parameter file. Required for EVERYTHING. No public source -- copy from the project's shared drive."),
  list(path = "input/SRS_lifetable_India_2020-24.csv",
       note = "India life table. Required ONLY for Scenario C (05_scenario_C_premature_death_qaly.R). See this script's comments for the official source.")
)
# NOTE: NPY-transfer amounts and the health-system/patient cost split
# now live as sheets ("NPY transfer", "Parameter") inside
# model_input_parameters.xlsx itself (merged 19 Aug 2026), not a
# separate file. Scenario B (04_scenarios.R) and
# 07_scenario_costdecomposition.R skip cleanly with a clear printed
# message if cost_healthsystem_*/cost_patient_* are blank.
all_present <- TRUE
for (f in required_files) {
  full_path <- file.path(root, f$path)
  if (file.exists(full_path)) {
    cat(sprintf("  OK       %s\n", f$path))
  } else {
    cat(sprintf("  MISSING  %s\n", f$path))
    cat(sprintf("           %s\n", f$note))
    all_present <- FALSE
  }
}
if (all_present) {
  cat("OK - All required input files present.\n")
} else {
  cat("\nWARNING - Copy the missing file(s) above into the input/ folder\n")
  cat("  before running 02_model.R. The folder now exists and is ready\n")
  cat("  to receive them: ", file.path(root, "input"), "\n", sep = "")
}

# ── Step 5: environment info ───────────────────────────────────
cat("\n-- Environment --\n")
cat(sprintf("  OS            : %s\n", Sys.info()[["sysname"]]))
cat(sprintf("  R version     : %s\n", R.version.string))
cat(sprintf("  Working dir   : %s\n", getwd()))

cat("\n========================================================\n")
if (all_present && length(missing_packages) == 0) {
  cat("SETUP COMPLETE. Next: run 02_model.R.\n")
} else if (all_present) {
  cat("SETUP COMPLETE (packages were installed). Next: run 02_model.R.\n")
} else {
  cat("SETUP INCOMPLETE — copy the missing input file(s) listed above,\n")
  cat("then re-run this script to confirm before running 02_model.R.\n")
}
cat("========================================================\n")
