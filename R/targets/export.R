# Names of objects each pipeline-stage document computes that later stages
# and/or manuscript.qmd/supplement.qmd need. Defined once here, used both by
# each document's final export chunk (saveRDS loop) and by _targets.R's
# tar_eval() calls (which generate one individually-tar_read()-able target
# per name). Extend these vectors as later pipeline phases (PWS, S1-S9) add
# more exported objects.

# preprocessing.qmd: data loading, telemetry, survey enrichment.
# intake + the two telemetry aggregates feed manuscript.qmd's Method-section
# tables and descriptive figures (participants table, hour-of-day and
# platform-usage panels, platform-hours prose). full_eligible_pids is NOT
# exported -- it is just unique(surveys$pid), derived where needed.
preprocessing_export_names <- c(
  "surveys",
  "intake",
  "hourly_telemetry",
  "daily_telemetry"
)

# imputation.qmd: MICE + within/between-person centering (tar_load()s from
# preprocessing.qmd). imp_diag is a slim long frame (including .imp == 0, the
# observed data) for supplement.qmd's observed-vs-imputed diagnostic figure.
imputation_export_names <- c(
  "dat",
  "dat_desc",
  "m_imputations",
  "imp_diag"
)

# modelling.qmd: H1/H2/H3 (+ later PWS, S1-S9) model fitting (tar_load()s
# from imputation.qmd)
modelling_export_names <- c(
  "h1_pooled",
  "h1mod",
  "h2_pooled",
  "h2mod",
  "h3_pooled",
  "h3mod"
)
