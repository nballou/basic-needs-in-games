library(targets)
library(tarchetypes)

source("R/targets/export.R")

# Generate one individually-tar_read()-able target per name in `names`, each
# depending on `report_name` (the tar_quarto() target for the document that
# produced them) and reading its saved data/exports/<name>.rds. Used for each
# pipeline stage's exported objects.
export_targets <- function(names, report_name) {
  values <- tibble::tibble(
    target_name = rlang::syms(names),
    file_name = names,
    report_sym = rep(list(rlang::sym(report_name)), length(names))
  )
  tar_eval(
    tar_target(target_name, {
      report_sym
      readRDS(file.path("data/exports", paste0(file_name, ".rds")))
    }),
    values = values
  )
}

# --use-freezer: tar_quarto() renders one document at a time, and Quarto
# *always* re-executes an incremental single-file render -- `freeze: auto`
# only applies to whole-project renders. So without this flag, anything that
# invalidates a report target (a styles.css / _quarto.yml / _extensions edit,
# a deleted _site/*.html, an extra_files change) forces a full re-execution,
# e.g. imputation.qmd's ~20-min MICE. `--use-freezer` makes the single-file
# render restore computations from the committed _freeze/ instead, guarded by
# a source-hash check (a genuine .qmd edit still re-executes). Requires that
# _freeze/<doc>/ is committed and in sync with <doc>.qmd -- see WORKFLOW.md.
QUARTO_ARGS <- "--use-freezer"

list(
  # Data loading, telemetry processing, survey enrichment
  #
  # extra_files: tar_quarto()'s automatic dependency detection only scans for
  # tar_load()/tar_read() calls (target deps) and Quarto-level resources like
  # templates (via quarto::quarto_inspect()) -- it does NOT parse R code for
  # source() calls. Any R/targets/*.R file a document sources must be listed
  # here explicitly, or editing that function silently fails to invalidate
  # the documents that use it.
  tar_quarto(
    preprocessing_report,
    path = "preprocessing.qmd",
    extra_files = c("R/targets/telemetry.R", "R/targets/export.R"),
    quarto_args = QUARTO_ARGS
  ),
  export_targets(preprocessing_export_names, "preprocessing_report"),

  # MICE imputation + within/between-person centering
  tar_quarto(
    imputation_report,
    path = "imputation.qmd",
    extra_files = c("R/targets/imputation.R", "R/targets/derive.R", "R/targets/export.R"),
    quarto_args = QUARTO_ARGS
  ),
  export_targets(imputation_export_names, "imputation_report"),

  # H1/H2/H3 confirmatory model fitting
  tar_quarto(
    modelling_report,
    path = "modelling.qmd",
    extra_files = c("R/targets/models_main.R", "R/targets/export.R"),
    quarto_args = QUARTO_ARGS
  ),
  export_targets(modelling_export_names, "modelling_report"),

  # S1-S9 sensitivity model fitting. Separate from modelling.qmd so a
  # sensitivity analysis can be revised without re-running H1-H3. Each fit
  # keeps its own data/models/ cache. Exports the pooled/summarised results
  # the manuscript's appendix renders.
  tar_quarto(
    sensitivity_report,
    path = "sensitivity.qmd",
    extra_files = "R/targets/export.R",
    quarto_args = QUARTO_ARGS
  ),
  export_targets(sensitivity_export_names, "sensitivity_report"),

  # Study B (PowerWash Simulator): session prep + two behavioural models.
  # Independent branch -- shares no data with the Study A stages above.
  tar_quarto(
    studyb_report,
    path = "studyb.qmd",
    extra_files = "R/targets/export.R",
    quarto_args = QUARTO_ARGS
  ),
  export_targets(studyb_export_names, "studyb_report"),

  # Manuscript (index.qmd -- the site home page): introduction, Study A
  # method + confirmatory results, Study B, discussion, and the full Appendix
  # (design table, complete model outputs, imputation diagnostics, and the
  # S1-S9 sensitivity tables/figures). Fits nothing -- every computed object
  # is tar_load()ed from the pipeline stages above. No exported objects.
  tar_quarto(
    manuscript_report,
    path = "index.qmd",
    extra_files = c("R/helpers.R", "R/targets/imputation.R"),
    quarto_args = QUARTO_ARGS
  )
)
