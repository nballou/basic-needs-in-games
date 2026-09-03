# archive/

## `index.qmd`

The original monolithic manuscript, before the analysis was split into the
`targets` pipeline (`preprocessing.qmd` → `imputation.qmd` → `modelling.qmd`,
`studyb.qmd`) plus `manuscript.qmd` and `supplement.qmd`.

Kept for reference only. It is **not** run by `tar_make()` and is not in the
`_quarto.yml` render list. It no longer executes as-is: it reads intermediate
files that were removed in the migration (`data/imputation_analytical.csv.gz`,
`data/imputation_full.csv.gz`). Every number it produced was verified to
reproduce in the split documents before it was retired (see commit history
and `PLAN-manuscript-split.md`).
