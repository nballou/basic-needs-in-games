# R Scripts and Utilities

## `targets/`
Functions used by the `targets` pipeline (`_targets.R`) and sourced directly
from `preprocessing.qmd`/`imputation.qmd`/`modelling.qmd`. Each file covers
one pipeline stage's shared, genuinely-reused logic (data loading is
one-off and stays inline in the qmd chunks; only mechanics called from
multiple places live here):

- `telemetry.R` -- `get_dst_offset()` (vendored from the Zenodo archive's own
  helpers, used by `preprocessing.qmd`)
- `imputation.R` -- `build_wide_survey()`, `setup_mice_inputs()`, `run_mice()`
  (used by `imputation.qmd`, and reused for the survey-completion-threshold
  sensitivity variants in the supplement)
- `derive.R` -- `derive_analysis_dat()` (post-imputation centering, shared by
  the main analysis dataset and the threshold variants)
- `models_main.R` -- `fit_mi_models()`, the parallelized (`furrr`) fit-and-pool
  loop used by every multiple-imputation model across H1/H2/H3 and the S1-S9
  sensitivity analyses
- `export.R` -- the shared object-name lists (`preprocessing_export_names`
  etc.) used both by each qmd's export chunk and by `_targets.R`'s
  `tar_eval()`-generated export targets

## `helpers.R`
Reporting/presentation helpers used by `index.qmd` (the manuscript) and `supplement.qmd`
(`report_wb_estimate()`, `report_lmer_term()`, `plot_relationship()`,
`clean_results()`, `format_mean_sd()`, `format_n_pct()`,
`create_categorical_section()`). Pure functions operating on
already-computed model/data objects -- no data loading, no side effects.

## `unit_tests.R`
**Currently broken** -- references `R/plot_marginal_effects.R`, which no
longer exists (the function it tests was renamed to `plot_relationship()`
and moved into `helpers.R`). Needs updating to test the current function;
left as-is here since fixing test content is out of scope for a
reorganization pass.

## `utils/`
Standalone, manually-run scripts that are **not** part of the `targets`
pipeline -- each produces a static input file that the pipeline then reads
as-is:

- `categorize_activities.qmd` -- one-off LLM-based classification of
  free-text activity descriptions, produces `data/activity_categories.csv`.
  Its cache/output live in sibling `categorize_activities_cache/` and
  `categorize_activities_files/` directories (gitignored, standard Quarto
  convention); `categorize_activities_support/` holds its progress log and
  its own README. Known stale reference: line ~333 points at
  `data/open-play-v1.0.0`, an older data version than the rest of the
  project now uses (`v1.2.5`) -- not fixed here, flagging for a follow-up.
- `determine_m_imputations.qmd` -- one-off helper for selecting the number
  of imputations (M) via von Hippel's (2020) protocol. **Stale**: still
  documents the old `index.qmd` workflow ("run the mice chunk in
  index.qmd..."); needs updating to read from the `imputation.qmd` pipeline
  stage (e.g. via `targets::tar_load()`) instead.
- `diagnose_imputation.R` -- standalone imputation diagnostics script.
