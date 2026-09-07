# Names of objects each pipeline-stage document computes that later stages
# and/or the manuscript need. Defined once here, used both by each document's
# final export chunk (the saveRDS loop) and by _targets.R's tar_eval() calls
# (which generate one individually-tar_read()-able target per name).
#
# NB: R/targets/export.R is listed in every tar_quarto() call's extra_files,
# so any edit here invalidates the whole pipeline. Settle each stage's export
# list before writing chunks; batch changes.

# preprocessing.qmd: data loading, telemetry, survey enrichment.
# intake + the two telemetry aggregates feed the manuscript's Method-section
# tables and descriptive figures. activity_categories carries the free-text
# activity classification (activity_label) for the H3 displaced-category
# breakdown and the S9 by-domain models.
# full_eligible_pids is NOT exported -- it is just unique(surveys$pid).
preprocessing_export_names <- c(
  "surveys",
  "intake",
  "hourly_telemetry",
  "daily_telemetry",
  "activity_categories"
)

# imputation.qmd: MICE + within/between-person centering. Two imputations are
# run: the primary one on all eligible participants (>=1 completed survey --
# the preregistered target), and a restricted one on the >=15-survey subsample
# used for the sample-construction sensitivity analysis (S4) and the Bayesian
# random-slopes check (S2). dat_complete is the .imp == 0 (non-imputed) slice
# of the long frame, for the observed-vs-imputed diagnostic and the
# complete-case analyses; dat_restricted_complete is its subsample counterpart.
imputation_export_names <- c(
  "dat",
  "dat_desc",
  "m_imputations",
  "dat_complete",
  "dat_restricted",
  "dat_restricted_complete"
)

# studyb.qmd: PowerWash Simulator session prep + the two exploratory
# behavioural models. Independent of the Study A pipeline.
studyb_export_names <- c(
  "pws_m1",
  "pws_m2",
  "pws_wide",
  "pws_n_players",
  "pws_n_sessions"
)

# modelling.qmd: H1/H2/H3 confirmatory model fitting. Each hypothesis is fit
# three ways: on the primary (full) sample (h*_pooled -- the headline result),
# on the >=15-survey subsample (h*_restricted_pooled), and on complete cases
# with no imputation (h*_cc_pooled). The subsample and complete-case fits are
# the sensitivity analyses reported alongside each primary estimate in text.
# h*mod is the first-imputation primary fit, kept for the complete model-summary
# tables in the appendix.
modelling_export_names <- c(
  "h1_pooled",
  "h1mod",
  "h1_restricted_pooled",
  "h1_cc_pooled",
  "h2_pooled",
  "h2mod",
  "h2_restricted_pooled",
  "h2_cc_pooled",
  "h3_pooled",
  "h3mod",
  "h3_restricted_pooled",
  "h3_cc_pooled"
)

# sensitivity.qmd: fits the S1-S9 sensitivity models; exports the pooled /
# summarised results the manuscript's appendix renders as tables and figures.
# The heavy fitted objects (esp. the S2 brms fit) stay in data/models/; only
# the small summaries cross the boundary.
sensitivity_export_names <- c(
  "sens1_pooled", # S1: 12h post-survey window
  "sens1b_pooled", # S1: 6h post-survey window
  "sens1c_pooled", # S1: 24h pre-survey window
  "s2_bayes_fixed", # S2: Bayesian random-slopes fixed effects (>=15 subsample)
  "s2_h2a_pooled", # S2: random slope for game NS only (>=15 subsample)
  "s2_h2b_pooled", # S2: random slope for global NF only (>=15 subsample)
  "sens3_pooled", # S3: continuous play-volume outcome
  "sample_results", # S4: combined sample-construction / imputation table
  "sens5b_pooled", # S5: game NS x global NF interaction
  "sens8_linear", # S6: linear H2 model (for the linear-vs-spline comparison)
  "sens8_spline", # S6: natural-spline H2 model
  "sens9_h2_aut", # S7: autonomy component
  "sens9_h2_com", # S7: competence component
  "sens9_h2_rel", # S7: relatedness component
  "sens10_pooled", # S8: game need frustration -> play
  "s10_pooled" # S9: H3 by specific displaced life domain (named list)
)
