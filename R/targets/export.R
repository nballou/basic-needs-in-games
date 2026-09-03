# Names of objects each pipeline-stage document computes that later stages
# and/or manuscript.qmd/supplement.qmd need. Defined once here, used both by
# each document's final export chunk (saveRDS loop) and by _targets.R's
# tar_eval() calls (which generate one individually-tar_read()-able target
# per name). Extend these vectors as later pipeline phases (PWS, S1-S9) add
# more exported objects.

# preprocessing.qmd: data loading, telemetry, survey enrichment.
# intake + the two telemetry aggregates feed manuscript.qmd's Method-section
# tables and descriptive figures (participants table, hour-of-day and
# platform-usage panels, platform-hours prose). activity_categories carries
# the free-text activity classification (activity_label) for the H3
# displaced-category breakdown and the S9 by-domain models.
# full_eligible_pids is NOT exported -- it is just unique(surveys$pid).
preprocessing_export_names <- c(
  "surveys",
  "intake",
  "hourly_telemetry",
  "daily_telemetry",
  "activity_categories"
)

# imputation.qmd: MICE + within/between-person centering (tar_load()s from
# preprocessing.qmd). dat_observed is the .imp == 0 (non-imputed) slice of
# the long frame, for the observed-vs-imputed diagnostic and the
# complete-case sensitivity analyses.
imputation_export_names <- c(
  "dat",
  "dat_desc",
  "m_imputations",
  "dat_observed"
)

# studyb.qmd: PowerWash Simulator session prep + the two exploratory
# behavioural models. Independent of the Study A pipeline. pws_wide is
# exported so manuscript.qmd can build prediction grids for the Study B
# figure; pws_m1/pws_m2 also feed supplement.qmd's full-model-output table.
studyb_export_names <- c(
  "pws_m1",
  "pws_m2",
  "pws_wide",
  "pws_n_players",
  "pws_n_sessions"
)

# modelling.qmd: H1/H2/H3 (+ later S1-S9) model fitting (tar_load()s from
# imputation.qmd)
modelling_export_names <- c(
  "h1_pooled",
  "h1mod",
  "h2_pooled",
  "h2mod",
  "h3_pooled",
  "h3mod"
)
